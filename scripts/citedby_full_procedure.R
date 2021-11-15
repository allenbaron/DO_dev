# Temporary script for citedby data

library(here)
library(tidyverse)
library(keyring)
library(rentrez)
library(lubridate)
library(DO.utils) # requires >= v0.1.6

merge_citedby_file <- here::here("data", "citedby", "DO_citedby-20211112.csv")

# PubMed cited by data ----------------------------------------------------

cb_pm_raw_file <- here::here("data", "citedby", "do_cb_pm_summary_by_id.rda")

#  Load Data or Get/Save Data if it isn't available
if (file.exists(cb_pm_raw_file)) {
    load(file = cb_pm_raw_file)
} else {
    # set API key
    rentrez::set_entrez_key(keyring::key_get("ENTREZ_KEY"))

    pmid_raw <- DO_pubs$pmid[1:8] %>% # exclude 2022 paper, no citations yet
        citedby_pmid(by_id = TRUE)
    pmid <- extract_pmid(pmid_raw)

    do_cb_pm_summary_by_id <- pubmed_summary(pmid)
    save(do_cb_pm_summary_by_id, file = cb_pm_raw_file)
}

cb_pm_by_id <- as_tibble(do_cb_pm_summary_by_id) %>%
    DO.utils:::hoist_ArticleIds()

# prepare for merge
cb_pm_merge <- cb_pm_by_id %>%
    dplyr::left_join(
        dplyr::select(DO_pubs, cites = pmid, internal_id),
        by = "cites"
    ) %>%
    dplyr::mutate(
        pub_type = purrr::map_chr(PubType, vctr_to_string, delim = "|"),
        pub_date = lubridate::date(SortPubDate),
        cites = NULL
    ) %>%
    dplyr::select(
        first_author = SortFirstAuthor, title = Title, journal = Source,
        pub_date, doi, pmid, pmcid, cites = internal_id, pub_type
    ) %>%
    dplyr::mutate(source = "pubmed") %>%
    # collapse cited by records that cite multiple DO_pubs
    collapse_col(cites)


# Scopus cited by data ----------------------------------------------------

cb_scop_raw_file <- here::here("data", "citedby", "do_cb_scop_by_id.rda")

# get data if it isn't available
if (file.exists(cb_scop_raw_file)) {
    load(file = cb_scop_raw_file)
} else {
    do_cb_scop_by_id <- citedby_scopus(
        title = DO_pubs$title[1:8], # exclude 2022 paper, no citations yet
        id = DO_pubs$internal_id[1:8],
        by_id = TRUE,
        # set API key
        api_key = keyring::key_get("Elsevier_API"),
        # set institution token
        headers = rscopus::inst_token_header(
            keyring::key_get("Elsevier_insttoken")
        ),
        verbose = FALSE
    )

    save(do_cb_scop_by_id, cb_scop_raw_file)
}

cb_scop_by_id <- as_tibble(do_cb_scop_by_id)

# prepare for merge
cb_scop_merge <- cb_scop_by_id %>%
    dplyr::mutate(
        scopus_eid = stringr::str_remove(eid, "2-s2.0-"),
        first_author = stringr::str_remove_all(`dc:creator`, "\\."),
        pub_type = paste(
            `prism:aggregationType`,
            subtypeDescription,
            sep = "|"
        ),
        pub_date = lubridate::date(`prism:coverDate`),
    ) %>%
    dplyr::select(
        first_author, title = "dc:title", journal = "prism:publicationName",
        pub_date, doi = "prism:doi", pmid = "pubmed-id", scopus_eid, cites,
        pub_type, added
    ) %>%
    dplyr::mutate(source = "scopus") %>%
    # collapse cited by records that cite multiple DO_pubs
    collapse_col(c(cites, added))


# Merge -------------------------------------------------------------------
# prefer pubmed data
match_index <- match_citations(cb_pm_merge, cb_scop_merge)

cb_merge <- cb_pm_merge %>%
    dplyr::mutate(
        source = dplyr::if_else(is.na(match_index), source, "pubmed|scopus"),
        added = cb_scop_merge$added[match_index]
    ) %>%
    dplyr::bind_rows(cb_scop_merge[-na.omit(match_index), ])

readr::write_csv(cb_merge, merge_citedby_file)

# improvements needed
#   1. abbreviated titles for Scopus data
#   2. retain scopus_eid for pubmed-scopus matches
#   3. added date for pubmed records
#   4. keep only oldest added date for record
#   5. export as xlsx or googlesheet
#       - convert IDs to links

