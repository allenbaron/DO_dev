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

# Calculate score
# Goal: Create score between 0 & 1 (1 = best) that reflect quality of match
# Considerations:
#   - distance of zero (i.e. perfect match) should always be 1
#   - should account for overall string lengths, since changes are more likely
#       to be necessary even for true matches in longer strings
# Parameters:
#   - dist = distance between strings
#   - x = first set of strings
#   - y = second set of strings which x was matched to
score_match <- function(dist, x, y) {
    x_len <- stringr::str_length(x)
    y_len <- stringr::str_length(y)
    min_len <- dplyr::if_else(x_len < y_len, x_len, y_len)

    # '+ 1' in denom to avoid 1/0 = Inf
    1 / (dist / min_len + 1)
}

# get max except where NA (to silence warnings)
max_na <- function(x) {
    ifelse(all(is.na(x)), NA_real_, max(x, na.rm = TRUE))
}


# File paths --------------------------------------------------------------

# Input
icdo_xl_file <- here::here("data/mapping/2021_ICDO_update_preferred terms.xlsx")
do_owl_file <- here::here("../Ontologies/HumanDiseaseOntology/src/ontology/doid.owl")
sparql_cp <- here::here("sparql/DO-cell_prolif-id_label_exsyn.rq")
sparql_xref <- here::here("sparql/DO-xref.rq")

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
    ) %>%
    # drop related terms & headers
    dplyr::filter(
        !stringr::str_detect(icdo_id, "-"),
        type %in% c("preferred", "synonym")
    )


# Extract DO cancer terms & exact synonyms (commit: 85b3e650)
#   Used this commit instead of latest release because synonym errors had to be
#   corrected.
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


# Remove ICD-O & DO terms that are already xrefs --------------------------

do_icdo_xref <- py_rdf$sparql_query(do_owl, query = sparql_xref) %>%
    tibble::as_tibble() %>%
    dplyr::filter(stringr::str_detect(xref, "^ICDO")) %>%
    dplyr::mutate(icdo_id = stringr::str_remove(xref, "ICDO:"))

icdo_data <- icdo_data %>%
    dplyr::filter(!icdo_id %in% do_icdo_xref$icdo_id)

do_cp <- do_cp %>%
    dplyr::filter(!doid %in% do_icdo_xref$id)


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
    ) %>%
        # replace distance with score
        dplyr::mutate(
            score = score_match(dist, x, table_match),
            dist = NULL
        )
)   # takes < 15s


# Reduce output to best ICDO-DOID matches, tidy, & save -------------------

# Fill in ICDO & DO preferred terms and synonyms for all standardize results
icdo_info <- icdo_data %>%
    dplyr::select(-term_no_nos) %>%
    tidyr::pivot_wider(
        id_cols = icdo_id,
        names_from = type,
        names_prefix = "icdo_",
        values_from = icdo_label,
        values_fn = ~ unique_to_string(.x, delim = " | ")
    ) %>%
    dplyr::left_join(
        dplyr::select(icdo_data, icdo_id, term_std),
        by = "icdo_id"
    )

do_info <- do_cp %>%
    dplyr::select(doid:do_label) %>%
    tidyr::pivot_wider(
        id_cols = doid,
        names_from = type,
        names_prefix = "do_",
        values_from = do_label,
        values_fn = ~ unique_to_string(.x, delim = " | ")
    ) %>%
    dplyr::left_join(
        dplyr::select(do_cp, doid, term_std),
        by = "doid"
    )


# Tidy & reduce
tidy_df <- icdo_do %>%
    # add info
    dplyr::left_join(icdo_info, by = c("x" = "term_std")) %>%
    dplyr::rename(icdo_std = x) %>%
    dplyr::left_join(do_info, by = c("table_match" = "term_std")) %>%
    dplyr::rename(do_std = table_match) %>%
    # drop duplicates
    unique() %>%
    # set up to arrange by best matches
    dplyr::group_by(icdo_id) %>%
    dplyr::mutate(overall_max = max_na(score)) %>%
    dplyr::ungroup() %>%
    # get only best score for each ICDO-DOID (id, type) pairing
    dplyr::arrange(dplyr::desc(score)) %>%
    DO.utils::collapse_col_flex(icdo_std, do_std, score, method = "first") %>%
    dplyr::arrange(
        dplyr::desc(overall_max), icdo_id, dplyr::desc(score), doid
    ) %>%
    dplyr::select(-overall_max)

# save
readr::write_csv(tidy_df, match_res_file)
