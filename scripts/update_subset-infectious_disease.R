# Create ROBOT template to add terms to infectious disease subset

library(here)
library(DO.utils)
library(tidyverse)
library(googlesheets4)

# Identify infectious diseases to add to subset ---------------------------

# convert edit file to OWL for SPARQL query
de_path <- here::here("../Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl")
de_tmp <- tempfile(fileext = ".owl")
system2("robot", glue::glue("convert --input {de_path} --output {de_tmp}"))

# query to identify terms
owl <- DO.utils::owl_xml(de_tmp)

inf_dis_rq <- "
PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>
PREFIX DOID: <http://purl.obolibrary.org/obo/DOID_>
PREFIX doid: <http://purl.obolibrary.org/obo/doid#>

SELECT ?class
WHERE {
    ?class a owl:Class ;
        rdfs:subClassOf* DOID:0050117 .

    FILTER NOT EXISTS { ?class oboInOwl:inSubset doid:DO_infectious_disease_slim . }
    FILTER NOT EXISTS { ?class owl:deprecated ?any . }
}"

add_to_subset <- owl$query(inf_dis_rq) %>%
    tidy_sparql() %>%
    dplyr::rename_with(toupper) %>%
    dplyr::mutate(subset = "DO_infectious_disease_slim")


# Create robot template ---------------------------------------------------
# load generic template

robot_template <- readr::read_csv(
    here::here("data/robot_template/ROBOT_template-explainer.csv"),
    col_types = "c"
)

rt_needed <- robot_template %>%
    dplyr::filter(title %in% c("ID", "subset"))

infectious_dis_template <- tibble::as_tibble(
    purrr::set_names(
        as.list(rt_needed$robot_pattern),
        rt_needed$title
    )
) %>%
    dplyr::bind_rows(add_to_subset)

readr::write_tsv(
    infectious_dis_template,
    here::here(
        "data",
        "robot_template",
        paste0(
            "ROBOT_template-infectious_disease_subset-",
            today_datestamp(),
            ".tsv"
        )
    )
)

