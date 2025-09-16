# identify all characters in latest release of doid-international.owl

library(DO.utils)
library(glue)
library(here)
library(tidyverse)


# MANUAL INPUTS -----------------------------------------------------------

repo_path <- here::here("../Ontologies/HumanDiseaseOntology")

text_pred <- "rdfs:label"



# STANDARD PROCESSING -----------------------------------------------------

query <- glue::glue(
    'PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>

    PREFIX dc: <http://purl.org/dc/elements/1.1/>
    PREFIX dcterms: <http://purl.org/dc/terms/>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>

    PREFIX DOID: <http://purl.obolibrary.org/obo/DOID_>
    PREFIX doid: <http://purl.obolibrary.org/obo/doid#>

    PREFIX IAO: <http://purl.obolibrary.org/obo/IAO_>
    PREFIX RO: <http://purl.obolibrary.org/obo/RO_>

    SELECT ?iri ?predicate ?text ?lang
    WHERE {
        VALUES ?predicate { @text_pred@ }
        ?iri a owl:Class ;
            ?predicate ?text_lang .
        BIND(str(?text_lang) AS ?text)
        BIND(lang(?text_lang) AS ?lang)
        FILTER NOT EXISTS { ?iri owl:deprecated ?any }
    }',
    .sep = " ",
    .open = "@",
    .close = "@"
)

di_df <- DO.utils::robot_query(
    file.path(repo_path, "src", "ontology", "releases", "translations", "doid-international.owl"),
    query,
    tidy_what = "everything"
) |>
    dplyr::mutate(lang = dplyr::coalesce(.data$lang, "en"))

all_chr <- di_df$text |>
    stringr::str_split("") |>
    unlist() |>
    unique() |>
    sort()


# OPTIONAL PROCESSING -----------------------------------------------------

#### identify examples of potentially problematic characters ####

# choose number of examples to return of each character (per lang)
examples_n <- 2

chr_check <- all_chr[!stringr::str_detect(all_chr, "^[A-Za-z0-9]+$")]

# priortize DOID namespace over others and text predicates as ordered in initial
#  input
di_df2 <- di_df |>
    dplyr::mutate(
        ns = stringr::str_remove(.data$iri, ":.*"),
        priority = dplyr::if_else(.data$ns == "DOID", 0, 1) +
            as.integer(factor(.data$predicate, levels = text_pred)) - 1
    )

di_check <- purrr::map(
    chr_check,
    ~ di_df2 |>
        dplyr::filter(stringr::str_detect(.data$text, stringr::str_escape(.x))) |>
        dplyr::filter(dplyr::row_number(.data$priority) <= examples_n, .by = "lang")
) |>
    purrr::set_names(chr_check) |>
    dplyr::bind_rows(.id = "chr") |>
    dplyr::arrange(.data$chr, .data$lang, .data$priority)

View(di_check)
