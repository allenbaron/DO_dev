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
    # calculate score between 0 & 1
    #   - "(... + 1)" to avoid 1/0 error, "dist/
    #   - "dist/mean(dist)" to normalize scores
    #   - "1/" so more changes (higher dist) = lower score
    dplyr::mutate(
        score = score_match(dist, x, table_match),
        dist = NULL
    ) %>%
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
        icdo_std, score, do_std,
        # do info
        doid, do_type, do_label
    ) %>%
    # sort by best scoring ICD-O terms, keeping all matches together
    dplyr::group_by(icdo_id) %>%
    dplyr::arrange(dplyr::desc(score), do_type, icdo_type, doid, .by_group = TRUE) %>%
    dplyr::mutate(
        max_score = ifelse(
            !all(is.na(score)),
            max(score, na.rm = TRUE),
            NA_real_
        )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
        dplyr::desc(max_score), icdo_id, dplyr::desc(score), do_type, doid
    ) %>%
    dplyr::select(-max_score)


readr::write_csv(tidy_df, match_res_file)
