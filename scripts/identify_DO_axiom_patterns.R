# Script to list/count types of logical axioms in DO
# By: J. Allen Baron
# Created: 2022-08-26
# Updated: 2022-08-31

library(tidyverse)
library(DO.utils) # >= v.0.2.4.9000
library(googlesheets4)


# paths -------------------------------------------------------------------
repo_path <- "../Ontologies/HumanDiseaseOntology/"
gs <- "1zN8Rxv31anXBi_vAsNL4YhTwSoow1xNnPp8FgMKhOZ4"

# extract axioms & obj prop labels -----------------------------------------
axioms <- DO.utils:::extract_class_axiom(repo_path)

# get property labels for axiom readability
repo <- DO.utils::DOrepo(repo_path)
prop_df <- repo$doid_merged$query(
    'SELECT ?property ?type ?label
    WHERE {
        VALUES ?type { owl:ObjectProperty owl:AnnotationProperty }
        ?property a ?type ;
            rdfs:label ?label .
    }'
) %>%
    tibble::as_tibble()


# identify patterns -------------------------------------------------------

axiom_patterns <- axioms %>%
    unlist() %>%
    DO.utils::format_axiom(prop_df, generify_obo = TRUE)

pattern_count <- axiom_patterns %>%
    tibble::tibble(axiom = .) %>%
    dplyr::count(axiom, sort = TRUE) %>%
    dplyr::mutate(
        type = dplyr::if_else(
            stringr::str_detect(axiom, "SubClassOf"),
            "SubClassOf",
            "EquivalentClass"
        ),
    ) %>%
    dplyr::group_by(type) %>%
    dplyr::mutate(
        pct = round(n / sum(n) * 100, 1),
        pct = dplyr::if_else(pct < 1, "<1", as.character(pct))
    ) %>%
    dplyr::select(type, axiom, pct, n) %>%
    dplyr::arrange(type, dplyr::desc(n), axiom)


# write to google sheet ---------------------------------------------------

googlesheets4::write_sheet(
    pattern_count,
    gs,
    "all_patterns"
)
