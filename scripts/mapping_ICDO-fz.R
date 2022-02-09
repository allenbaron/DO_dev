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


# File paths --------------------------------------------------------------

# Input
icdo_xl_file <- here::here("data/mapping/2021_ICDO_update_preferred terms.xlsx")
do_owl_file <- here::here("../Ontologies/HumanDiseaseOntology/src/ontology/doid.owl")
sparql_cp <- here::here("sparql/DO-cell_prolif-id_label_exsyn.rq")

# Output
match_res_file <- here::here("data/mapping/do_icdo-fz_match.csv")



# Load & standardize data  ------------------------------------------------

# Read all ICD-O terms, including synonyms
icdo_data <- readxl::read_excel(
    icdo_xl_file,
    sheet = "all terms",
    col_types = "text"
) %>%
    ##### tidy
    # column names
    dplyr::rename(icdo_id = `ICDO3.2`, type = Level, icdo_label = Term) %>%
    # labels
    dplyr::mutate(
        # remove NOS (only found in ICD-O)
        term_no_nos = stringr::str_remove(icdo_label, "[, ]*NOS"),
        term_std = standardize_label(term_no_nos),
    # type - to match DO cellular prolif data
        type = stringr::str_to_lower(type)
    )


# Extract DO cancer terms & exact synonyms (release: v2022-01-31)
do_owl <- py_rdf$read(do_owl_file)
do_cp <- py_rdf$sparql_query(do_owl, query = sparql_cp) %>%
    tibble::as_tibble() %>%
    ##### tidy
    # column names
    dplyr::rename(doid = id, do_label = label) %>%
    dplyr::mutate(
    # labels
        term_std = standardize_label(do_label),
    # type - to match ICD-O data
        type = stringr::str_remove(type, "exact_")
    )


# Complete fuzzy string matching ------------------------------------------

# Set maximum allowable string match distance to 3rd quartile of string length
#   across both resources.
max_dist_3q <- c(
    purrr::map_int(do_cp$term_std, stringr::str_length),
    purrr::map_int(icdo_data$term_std, stringr::str_length)
) %>%
    summary() %>%
    .[["3rd Qu."]]


# match ICD-O to DO, since there are more DO terms and the extra matches that
#   will result from the reverse comparison are undesirable (will only make for
#   more work).
system.time(
    icdo_do <- DO.utils::match_fz(
        # ignore label duplicates to speed up matching
        unique(icdo_data$term_std),
        unique(do_cp$term_std),
        maxDist = max_dist_3q
    )
)   # takes ~ 22s


# Tidy and save data ------------------------------------------------------

tidy_df <- icdo_do %>%
    dplyr::left_join(
        dplyr::select(icdo_data, -term_no_nos),
        by = c("x" = "term_std")
    ) %>%
    dplyr::rename(icdo_std = x, icdo_type = type) %>%
    dplyr::left_join(do_cp, by = c("table_match" = "term_std")) %>%
    dplyr::rename(do_std = table_match, do_type = type) %>%
    dplyr::select(
        # icdo info
        icdo_id, icdo_type, icdo_label,
        # match info
        do_std, dist, icdo_std,
        # do info
        doid, do_type, do_label
    ) %>%
    dplyr::arrange(icdo_id, icdo_type, dist, do_type, doid)


readr::write_csv(tidy_df, match_res_file)
