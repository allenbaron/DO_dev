# get initial set of data for FMHA (M. Clarkson)
# - urinary / reproductive hierarchy, also including kidney and gonadal (sexual
# dysmorphisms) diseases
# - asserted & inferred with anatomy terms

library(here)
library(DO.utils)
library(tidyverse)
library(janitor)
library(googlesheets4)

dm_owl <- here::here("../Ontologies/HumanDiseaseOntology/src/ontology/doid-merged.owl")

# get disease-anatomy relationships
export_file <- tempfile(fileext = ".tsv")
DO.utils::robot(
    "export",
    i = dm_owl,
    header = '"ID|LABEL|Equivalent Class [ID]|SubClass Of [ID ANON]"',
    export = export_file
)

class_axioms <- readr::read_tsv(export_file) %>%
    janitor::clean_names()

doid_anat <- class_axioms %>%
    tidyr::pivot_longer(
        cols = tidyselect::contains("class"),
        names_to = "type",
        values_to = "lgl_axiom"
    ) %>%
    dplyr::filter(stringr::str_detect(lgl_axiom, "UBERON")) %>%
    dplyr::mutate(
        uberon = stringr::str_extract_all(lgl_axiom, "UBERON:[0-9]+")
    ) %>%
    tidyr::unnest(uberon) %>%
    dplyr::select(doid = id, do_label = label, uberon_id = uberon) %>%
    unique() %>%
    dplyr::left_join(
        dplyr::select(class_axioms, uberon_id = id, uberon_label = label),
        by = c("uberon_id")
    ) %>%
    dplyr::mutate(anatomy = paste0(uberon_label, " (", uberon_id, ")")) %>%
    dplyr::select(doid, anatomy) %>%
    DO.utils::collapse_col(anatomy, delim = ", ", na.rm = TRUE)


# identify desired diseases & add anatomy terms
doid_file <- tempfile(fileext = ".tsv")
dis_query <- "
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>
PREFIX DOID: <http://purl.obolibrary.org/obo/DOID_>

SELECT ?id ?label ?anat_system ?anat_subsystem ?parent
WHERE {
    ?class oboInOwl:id ?id ;
        rdfs:label ?label ;
        rdfs:subClassOf ?parent ;
        rdfs:subClassOf* ?anat_subsystem_uri .

    ?anat_subsystem_uri rdfs:label ?anat_subsystem ;
        rdfs:subClassOf ?anat_system_uri .

    ?anat_system_uri rdfs:label ?anat_system ;
        rdfs:subClassOf DOID:7 .

    FILTER( !isBlank( ?parent ) )
    FILTER( ?anat_system_uri IN ( DOID:15, DOID:18 ) || ?anat_subsystem_uri = DOID:2277 )
    FILTER NOT EXISTS { ?class owl:deprecated ?any . }
}" # limits results to descendants of anatomy/repro branches, or gonadal disease sub-branch
dis_q_file <- tempfile(fileext = ".rq")
readr::write_lines(dis_query, dis_q_file)
robot(
    "query",
    i = here::here("../Ontologies/HumanDiseaseOntology/src/ontology/doid.owl"),
    query = dis_q_file,
    doid_file
)

diseases <- readr::read_tsv(doid_file) %>%
    DO.utils::tidy_sparql() %>%
    dplyr::mutate(
        dplyr::across(dplyr::everything(), ~ stringr::str_remove(.x, "@en"))
    ) %>%
    DO.utils::collapse_col(anat_subsystem, delim = "/", na.rm = TRUE) %>%
    dplyr::mutate(
        do_anatomy_group = paste(anat_system, anat_subsystem, sep = " > ")
    ) %>%
    dplyr::select(-tidyselect::starts_with("anat")) %>%
    DO.utils::collapse_col(
        .cols = c(parent, do_anatomy_group),
        delim = ", ",
        na.rm = TRUE
    ) %>%
    dplyr::rename(
        doid = id, do_label = label, "parent(s)" = parent,
        "do_anatomy_group(s)" = do_anatomy_group
    ) %>%
    dplyr::left_join(doid_anat, by = "doid")

readr::write_tsv(diseases, "data/external/FMHA-disease_anatomy-urinary_repro.tsv")
