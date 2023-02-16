# identify definitions where a gene identifier might be needed

library(here)
library(DO.utils)
library(tidyverse)
library(tidytext)

do_owl <- DO.utils::owl_xml(
    here::here("../Ontologies/HumanDiseaseOntology/src/ontology/doid.owl")
)

do_def <- do_owl$query(
    "SELECT ?id ?label ?def (GROUP_CONCAT(?child; SEPARATOR='|') AS ?children)
    WHERE {
        ?class a owl:Class ;
            oboInOwl:id ?id ;
            rdfs:label ?label ;
            obo:IAO_0000115 ?def .
        FILTER NOT EXISTS { ?class owl:deprecated ?any . }
        OPTIONAL {
            ?class2 rdfs:subClassOf ?class ;
                rdfs:label ?child .
        }
    }
    GROUP BY ?id ?label ?def"
) %>%
    DO.utils::tidy_sparql()

# identify common words in definitions with has_material_basis_in & gene IDs-ish
#   most common: mutation, chromosome, gene, characterized, hommozygous, etc.
def_w_gene <- do_def %>%
    dplyr::mutate(
        id = paste0(label, " (", id, ")"),
        def = stringr::str_replace_all(
            def,
            c(
                "[^[:space:][:punct:]][0-9]+[^[:space:][:punct:]]" = " ",
                purrr::set_names(
                    rep(" ", length(obo_prefix)),
                    nm = paste0(names(obo_prefix), "[:_]")
                )
            )
        )
    ) %>%
    dplyr::select(-label) %>%
    dplyr::filter(
        stringr::str_detect(def, "[A-Z0-9]{3,7}[ .]"),
        stringr::str_detect(def, "has.material.basis.in")
    ) %>%
    tidytext::unnest_tokens("word", def, to_lower = FALSE) %>%
    dplyr::anti_join(tidytext::get_stopwords(), by = "word")

gene_common <- dplyr::count(def_w_gene, word, sort = TRUE)
gene_def_regex <- gene_common %>%
    dplyr::filter(
        n > 300,
        stringr::str_length(word) > 5,
        !word %in% c("disease", "syndrome", "characterized",
                     "has_material_basis_in")
    ) %>%
    .$word %>%
    DO.utils::vctr_to_string(delim = "|")

need_gene <- do_def %>%
    dplyr::mutate(
        def = stringr::str_replace_all(
            def,
            c(
                "[^[:space:][:punct:]][0-9]+[^[:space:][:punct:]]" = " ",
                purrr::set_names(
                    rep(" ", length(obo_prefix)),
                    nm = paste0(names(obo_prefix), "[:_]")
                )
            )
        )
    ) %>%
    dplyr::filter(
        !stringr::str_detect(def, "[A-Z0-9]{3,7}[ .]"),
        stringr::str_detect(def, "has.material.basis.in"),
        stringr::str_detect(def, gene_def_regex),
        !stringr::str_detect(children, "( [A-Z0-9]+\\|)| [A-Z0-9]+$")
    ) %>%
    dplyr::arrange(dplyr::desc(children), id)

gs <- googlesheets4::gs4_create("defs_missing_gene")
googledrive::drive_mv(gs, path = "My_Work/Curation/definitions/", overwrite = FALSE)

googlesheets4::write_sheet(need_gene, gs, "2023-02-16")
