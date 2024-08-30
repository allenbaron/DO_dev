# KEGG xref analysis -- 2024-08-28
#   - prompted by https://github.com/DiseaseOntology/HumanDiseaseOntology/issues/1374

library(DO.utils) # from GitHub (install devtools or remotes, then install_github("DiseaseOntology/DO.utils"), also requires Java 11 & ROBOT (http://robot.obolibrary.org/)
library(tidyverse)
library(KEGGREST) # from Bioconductor (install BiocManager, then use BiocManager::install)

do_xref <- DO.utils::robot_query(
    input = "~/Documents/Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl",
    query = '#id: DOq017
    #description: List all cross-references in DO to KEGG (active diseases only)
    #input: doid.owl
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>

    SELECT ?id ?label ?xref
    WHERE {
      ?class a owl:Class ;
        oboInOwl:hasOBONamespace "disease_ontology" ;
        oboInOwl:id ?id ;
        rdfs:label ?label ;
        oboInOwl:hasDbXref ?xref .

      FILTER(STRSTARTS(?xref, "KEGG:"))
      FILTER NOT EXISTS { ?class owl:deprecated true }
    }
    ORDER BY ?id ?xref',
    tidy_what = "everything"
) %>%
    dplyr::mutate(
        lui = stringr::str_remove_all(xref, ".*:"),
        lui_code = stringr::str_remove_all(xref, "KEGG:|[0-9]+"),
        lui_num = stringr::str_extract(xref, "[0-9]+")
    )

# check format of KEGG xrefs
dplyr::count(do_xref, lui_code)
    # most with no lui_code, some with 'H' codes ('H' is what corresponds to
    #   disease lui in KEGG
    # I checked a few codes manually and the few I tried without H codes correspond
    #   to pathways... not sure why DO xrefs KEGG pathways instead of KEGG diseases

# get KEGG pathway & disease IDs
kpd <- KEGGREST::keggLink("pathway", "disease") %>%
    tibble::tibble(pathway = ., disease = names(.)) %>%
    dplyr::mutate(
        p_prefix = stringr::str_remove(pathway, ":.+"),
        p_lui = stringr::str_remove(pathway, ".+:"),
        p_lui_code = stringr::str_remove_all(pathway, ".*:|[0-9]+"),
        p_lui_num = stringr::str_extract(pathway, "[0-9]+"),
        d_prefix = stringr::str_remove(disease, ":.+"),
        d_lui = stringr::str_remove(disease, ".+:"),
        d_lui_code = stringr::str_remove_all(disease, ".*:|[0-9]+"),
        d_lui_num = stringr::str_extract(disease, "[0-9]+")
    )

# convert to long form to match either disease or pathway
kpd_long <- kpd %>%
    tidyr::pivot_longer(
        cols = dplyr::contains("lui"),
        names_to = "type",
        values_to = "lui"
    )



# skip this... didn't work, keep for info ---------------------------------

# match on full lui - including 'H' where it is part of the lui
do_kegg <- dplyr::left_join(do_xref, kpd_long, by = "lui")
    # hm... only pathway lui numbers matched; makes me wonder if those 'H'
    #   numbers are actually pathways with artificial "H" added to the front

kegg2 <- dplyr::left_join(do_xref, kpd_long, by = c("lui_num" = "lui"))
    # nope still no matches to lui's starting with 'H'... going to have to check
    #   manually --> okay, the one I checked WAS a disease... maybe I only
    #   pulled those diseases from KEGG that corresponded to pathways?



# full pathway/disease search ---------------------------------------------

kd <- KEGGREST::keggList("disease") %>%
    tibble::tibble(d_lui = names(.), d_label = .)

# pathways without link start with 'map' instead of human identifier 'hsa'
kp <- KEGGREST::keggList("pathway") %>%
    tibble::tibble(p_lui2 = names(.), p_label = .) %>%
    dplyr::mutate(
        p_lui_code2 = stringr::str_remove(p_lui2, "[0-9]+"),
        p_lui_num = stringr::str_extract(p_lui2, "[0-9]+"),
    )

kpd1 <- dplyr::full_join(kpd, kd, by = "d_lui") %>%
    dplyr::full_join(kp, by = "p_lui_num")

kpd1l <- kpd1 %>%
    tidyr::pivot_longer(
        cols = dplyr::contains("lui"),
        names_to = "type",
        values_to = "lui",
        values_drop_na = TRUE
    )

do_kegg <- dplyr::left_join(do_xref, kpd1l, by = "lui") %>%
    tidyr::pivot_wider(
        names_from = type,
        values_from = lui
    )



# share KEGG pathway-disease (with DO data) -------------------------------

share <- kpd1l %>%
    dplyr::filter(type %in% c("p_lui_num", "d_lui")) %>%
    dplyr::full_join(
        dplyr::rename(do_xref, doid = id, do_label = label, do_xref = xref),
        by = "lui"
    ) %>%
    tidyr::pivot_wider(
        names_from = type,
        values_from = lui
    ) %>%
    dplyr::mutate(
        kegg_pathway_id = dplyr::if_else(
            is.na(pathway) & !is.na(p_lui_num),
            paste0("hsa", p_lui_num),
            stringr::str_remove(pathway, "path:")
        ),
        kegg_disease_id = dplyr::if_else(
            is.na(disease) & !is.na(d_lui),
            d_lui,
            stringr::str_remove(disease, "ds:")
        ),
        do = is.na(doid),
        kpd = !is.na(kegg_pathway_id) & !is.na(kegg_disease_id)
    ) %>%
    dplyr::arrange(do, dplyr::desc(kpd), do_label, d_label, p_label) %>%
    dplyr::select(
        doid, do_label, do_xref,
        kegg_pathway_id, kegg_pathway_label = p_label,
        kegg_disease_id, kegg_disease_label = d_label
    )

readr::write_csv(share, "data/mapping/DO-KEGG-mapping.csv")
