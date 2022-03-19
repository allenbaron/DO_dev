# ICD-O in DO: Before & After
# Completed for Glioma Reclassification Poster in collaboration with CIViC and
# presented by Jason Saliba.
#
# J. Allen Baron
# 2022-03-18


# Setup -------------------------------------------------------------------

# libraries
library(here)
library(tidyverse)
library(DO.utils) # >= 0.1.7.900, pyDOID 0.1.4


# Inputs ------------------------------------------------------------------

## tags
before_tag <- "v2020-12-22"
after_tag <- "v2022-03-02"

## access repo
do_repo <- DO.utils::DOrepo("~/Documents/Ontologies/HumanDiseaseOntology")
head <- do_repo$capture_head()

# icdo file
icdo_xl_file <- here::here("data/mapping/2021_ICDO_update_preferred terms.xlsx")

# sparql queries
sparql_cp <- here::here("sparql/DO-cell_prolif-id_label_exsyn.rq")
sparql_xref_w_obs <- here::here("sparql/DO-xref-w_obs.rq")


# Custom functions --------------------------------------------------------

# Standardize terms labels
#   1. Replace all 1+ groups of punctuation/spaces with single space
#   2. Convert all to lowercase
standardize_label <- function(term) {
    term %>%
        stringr::str_replace_all("[[:punct:][:space:]]+", " ") %>%
        stringr::str_to_lower() %>%
        stringr::str_squish()
}

tidy_cp <- function(cp_df) {
    cp_df %>%
        tibble::as_tibble() %>%
        dplyr::rename(doid = id, do_label = label, do_type = type) %>%
        dplyr::mutate(
            term_std = standardize_label(do_label),
            do_type = stringr::str_remove(do_type, "exact_")
        )
}

tidy_xref <- function(xref_df) {
    xref_df %>%
    tibble::as_tibble() %>%
        dplyr::filter(stringr::str_detect(xref, "^ICDO")) %>%
        dplyr::mutate(icdo_id = stringr::str_remove(xref, "ICDO:")) %>%
        tidyr::unnest(obsolete, keep_empty = TRUE) %>%
        tidyr::replace_na(list(obsolete = FALSE))
}

# only counts synonyms where preferred term matches don't exist
count_matches <- function(match_df) {
    pp <- dplyr::filter(
        match_df,
        do_type == "preferred" & icdo_type == "preferred"
    )
    sp <- dplyr::filter(
        match_df,
        !doid %in% pp$doid & !icdo_id %in% pp$icdo_id,
        icdo_type == "preferred"
    )
    ps <- dplyr::filter(
        match_df,
        !doid %in% pp$doid & !icdo_id %in% pp$icdo_id,
        !doid %in% sp$doid & !icdo_id %in% sp$icdo_id,
        do_type == "preferred"
    )
    ss <- dplyr::filter(
        match_df,
        !doid %in% pp$doid & !icdo_id %in% pp$icdo_id,
        !doid %in% sp$doid & !icdo_id %in% sp$icdo_id,
        !doid %in% ps$doid & !icdo_id %in% ps$icdo_id
    )

    c(
        pp = nrow(pp),
        sp = nrow(sp),
        p_total = sum(nrow(pp), nrow(sp)),
        ps = nrow(ps),
        ss = nrow(ss),
        s_total = sum(nrow(ps), nrow(ss))
    )
}

# counts all preferred terms & synonyms in DO
count_icdo_in_do <- function(match_df) {
    p <- dplyr::filter(
        match_df,
        icdo_type == "preferred"
    )
    s <- dplyr::filter(
        match_df,
        icdo_type == "synonym"
    )

    p_n <- dplyr::count(p, do_type)
    s_n <- dplyr::count(s, do_type)

    df <- dplyr::bind_rows(
        list(preferred = p_n, synonym = s_n),
        .id = "icdo_type"
    ) %>%
        tidyr::pivot_wider(
            names_from = do_type,
            names_prefix = "do_",
            values_from = n
        )

    rf <- dplyr::rowwise(df) %>%
        dplyr::mutate(TOTAL = sum(dplyr::c_across(dplyr::starts_with("do"))))

    rf
}


# rename_file <- function(input, suffix) {
#     new_file <- stringr::str_replace(
#         input,
#         "\\.csv",
#         paste0("_", suffix, ".csv")
#     )
#     success <- file.rename(input, new_file)
#     message("file rename successful: ", success)
#     new_file
# }


# Read inl ICD-O preferred terms & synonyms -------------------------------

icdo_data <- readxl::read_excel(
    icdo_xl_file,
    sheet = "all terms",
    col_types = "text"
) %>%
    ##### tidy
    # column names
    dplyr::rename(icdo_id = `ICDO3.2`, icdo_type = Level, icdo_label = Term) %>%
    # labels
    dplyr::mutate(
        # remove NOS (only found in ICD-O)
        term_no_nos = stringr::str_remove(icdo_label, "[, ]*NOS"),
        term_std = standardize_label(term_no_nos),
        # type - to match DO cellular prolif data
        icdo_type = stringr::str_to_lower(icdo_type)
    ) %>%
    # drop related terms & headers
    dplyr::filter(
        !stringr::str_detect(icdo_id, "-"),
        icdo_type %in% c("preferred", "synonym")
    )


# Extract DO data at "before" tag -----------------------------------------

do_repo$checkout_tag(before_tag)
before_cp <- do_repo$doid$query(query = sparql_cp, reload = TRUE) %>%
    tidy_cp()
before_xref <- do_repo$doid$query(query = sparql_xref_w_obs) %>%
    tidy_xref()


# Extract DO data at "after" tag -----------------------------------------

do_repo$checkout_tag(after_tag)
after_cp <- do_repo$doid$query(query = sparql_cp, reload = TRUE) %>%
    tidy_cp()
after_xref <- do_repo$doid$query(query = sparql_xref_w_obs) %>%
    tidy_xref()


# Restore repo ------------------------------------------------------------

do_repo$restore_head()


# Establish DO-ICDO exact (not fuzzy) matches -----------------------------

before_exact <- dplyr::left_join(
    before_cp,
    icdo_data,
    by = c("do_label" = "icdo_label")
) %>%
    dplyr::filter(!is.na(icdo_id))

after_exact <- dplyr::left_join(
    after_cp,
    icdo_data,
    by = c("do_label" = "icdo_label")
) %>%
    dplyr::filter(!is.na(icdo_id))


# Calculate exact counts --------------------------------------------------

# ICD-O xrefs in DO
bx_n <- dplyr::n_distinct(before_xref$xref)
ax_n <- dplyr::n_distinct(after_xref$xref)
diff_x <- ax_n - bx_n

# ICD-O preferred/synonyms matching DO preferred/synonyms
bem_n <- count_term_matches(before_exact)
aem_n <- count_term_matches(after_exact)
diff_em <- aem_n - bem_n
em_df <- dplyr::bind_rows(bem_n, aem_n, diff_em)


# Relaxed comparison & counts ---------------------------------------------
# allow for differences in capitalization & punctuation

before_relax <- dplyr::left_join(
    before_cp,
    icdo_data,
    by = "term_std"
) %>%
    dplyr::filter(!is.na(icdo_id))

after_relax <- dplyr::left_join(
    after_cp,
    icdo_data,
    by = "term_std"
) %>%
    dplyr::filter(!is.na(icdo_id))

# Counts ICD-O preferred/synonyms matching DO preferred/synonyms
brm_n <- count_icdo_in_do(before_relax)
arm_n <- count_icdo_in_do(after_relax)


# Print Counts ------------------------------------------------------------
bx_n
ax_n
diff_x

em_df

brm_n
arm_n
