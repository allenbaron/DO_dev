# Fuzzy string matching between ICD-O and DO cancer terms
# By J. Allen Baron
# 2022-02-08


# Setup -------------------------------------------------------------------

# libraries
library(here)
library(tidyverse)
library(DO.utils)
library(reticulate)

# py_rdf workaround & custom functions
py_rdf <- reticulate::import_from_path(
    "py_rdf",
    system.file("python", package = "DO.utils", mustWork = TRUE)
)

# Standardize terms labels
#   1. Replace all 1+ groups of punctuation/spaces with single space
#   2. Convert all to lowercase
standardize_label <- function(term) {
    term %>%
        stringr::str_replace_all("[[:punct:][:space:]]+", " ") %>%
        stringr::str_to_lower() %>%
        stringr::str_squish()
}


# Files, input ------------------------------------------------------------

icdo_xl_file <- here::here("data/mapping/2021_ICDO_update_preferred terms.xlsx")
do_owl_file <- here::here("../Ontologies/HumanDiseaseOntology/src/ontology/doid.owl")
sparql_cell_prolif <- here::here("sparql/DO-cell_prolif-id_label.rq")


# Initiate test to estimate time required for matching --------------------
#   1. limit ICD-O to leukemia (5 terms only)
#   2. limit DO to cancer branch (all terms)

# Read ICD-O, leukemia terms (NOS previously removed)
icdo_data <- readxl::read_excel(
    icdo_xl_file,
    sheet = "leukemia",
    col_types = "text"
)

# Extract DO cancer terms (release: v2022-01-31)
do_owl <- py_rdf$read(do_owl_file)
do_cp <- py_rdf$sparql_query(do_owl, query = sparql_cell_prolif) %>%
    tibble::as_tibble()


# Standardize terms in both resources (see custom function definition above
#   for details)
icdo_data <- icdo_data %>%
    dplyr::mutate(term_std = standardize_label(Term))
do_cp <- do_cp %>%
    dplyr::mutate(term_std = standardize_label(label))

# Calculate average string length to set maximum distance parameter,
#   NOTE: There are MANY more terms in DO, so I'll just check DO and use the
#       3rd quartile.
est_max_dist <- purrr::map_int(do_cp$term_std, stringr::str_length) %>%
    summary() %>%
    .[["3rd Qu."]]


# Test Fuzzy String Matching with 5 ICD-O leukemia terms to 2,736 DO cancer
#   terms
time5 <- system.time(
    icdo5_do <- DO.utils::match_fz(
        icdo_data$term_std[1:5],
        do_cp$term_std,
        maxDist = est_max_dist
    )
)

######### RESULT: test took only 0.011 seconds ###############
# Full ICD-O set should take ~ 2.5s (1,143 / 5 * 0.011s)
time5
icdo5_do
