# Temporary script for citedby data

library(here)
library(tidyverse)
library(keyring)
library(rentrez)
library(lubridate)
library(DO.utils) # requires >= v0.1.6


# Identify files ----------------------------------------------------------

citedby_dir <- here::here("data", "citedby")

# cited by raw output files
cb_pm_raw_file <- file.path(citedby_dir, "do_cb_pm_summary_by_id.rda")
cb_scop_raw_file <- file.path(citedby_dir, "do_cb_scop_by_id.rda")

# collections raw input and output files
collection_pm_raw_file <- file.path(citedby_dir, "do_collection_pm_summary.rda")
collection_pmc_raw_file <- file.path(
    citedby_dir,
    "do_collection_pmc_summary.rda"
)

# final tidied file
merge_citedby_file <- file.path(citedby_dir, "DO_citedby-20211112.csv")


# PubMed cited by data ----------------------------------------------------

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


# PubMed collection data --------------------------------------------------

# get "cited by" articles identified in MyNCBI collection; enough of these are
#   not redundant with the PubMed + Scopus cited by results to make this
#   necessary

# MANUAL STEP REQUIRED!!!
# Download data manually to data/citedby_collection.txt by selecting:
#   'Send to:' > 'File' > 'Summary (text)' (Format drop down) > 'Create File'
#   URL: https://www.ncbi.nlm.nih.gov/myncbi/browse/collection/49204559/

# extract PubMed IDs & get data from Entrez API to make formatting easier
collection_txt <- read_pubmed_txt(
    here::here("data", "citedby", "citedby_collection.txt")
)

collection_id <- tibble::tibble(
    n = 1:length(collection_txt),
    pmid = stringr::str_extract(
        collection_txt,
        "PMID: [0-9]{8}"
    ),
    pmcid = stringr::str_extract(
        collection_txt,
        "PMCID: PMC[0-9]+"
    )
) %>%
    dplyr::mutate(
        pmid = stringr::str_remove(pmid, "PMID: "),
        pmcid = stringr::str_remove(pmcid, "PMCID: ")
    )

# Get Entrez API records for collection records with PubMed IDs
if (file.exists(collection_pm_raw_file)) {
    load(file = collection_pm_raw_file)
} else {
    col_pmid <- collection_id$pmid %>%
        na.omit() %>%
        unique()
    do_col_pm_summary <- pubmed_summary(col_pmid)
    save(do_col_pm_summary, file = collection_pm_raw_file)
}

col_pm <- as_tibble(do_col_pm_summary) %>%
    DO.utils:::hoist_ArticleIds()

col_pm_merge <- col_pm %>%
    dplyr::mutate(
        pub_type = purrr::map_chr(PubType, vctr_to_string, delim = "|"),
        pub_date = lubridate::date(SortPubDate)
    ) %>%
    dplyr::select(
        first_author = SortFirstAuthor, title = Title, journal = Source,
        pub_date, doi, pmid, pmcid, pub_type
    ) %>%
    dplyr::mutate(source = "ncbi_col-pubmed")


# Get Entrez API records for collection records with PubMed Central IDs
if (file.exists(collection_pmc_raw_file)) {
    load(file = collection_pmc_raw_file)
} else {
    # get pmc summary only where no PubMed record exists
    col_pmcid <- dplyr::filter(collection_id, is.na(pmid)) %>%
        .$pmcid %>%
        na.omit() %>%
        unique()
    do_col_pmc_summary <- pmc_summary(col_pmcid)
    save(do_col_pmc_summary, file = collection_pmc_raw_file)
}

col_pmc <- as_tibble(do_col_pmc_summary) %>%
    DO.utils:::hoist_ArticleIds()

col_pmc_merge <- col_pmc %>%
    tidyr::hoist(
        .col = Authors,
        first_author = list(1L, 1L)
    ) %>%
    dplyr::mutate(
        pub_date = lubridate::date(SortDate),
        pmid = dplyr::if_else(
            stringr::str_length(pmid) < 8,
            NA_character_,
            pmid
        )
    ) %>%
    dplyr::select(
        first_author, title = Title, journal = Source, pub_date, doi, pmid,
        pmcid
    ) %>%
    # drop columns without values
    dplyr::select(where(~!all(is.na(.x)))) %>%
    dplyr::mutate(source = "ncbi_col-pmc")


# Merge -------------------------------------------------------------------

# PubMed and Scopus cited by results (prefer PubMed data for matches)
match_scop <- match_citations(cb_pm_merge, cb_scop_merge)

cb_merge <- cb_pm_merge %>%
    dplyr::mutate(
        source = dplyr::if_else(
            is.na(match_scop),
            source,
            paste(source, "scopus", sep = "; ")
        ),
        added = cb_scop_merge$added[match_scop]
    ) %>%
    dplyr::bind_rows(cb_scop_merge[-na.omit(match_scop), ])

## ...and Collection PubMed results (prefer previously merged data)
match_col_pm <- match_citations(cb_merge, col_pm_merge)

cb_col_merge1 <- cb_merge %>%
    dplyr::mutate(
        source = dplyr::if_else(
            is.na(match_col_pm),
            source,
            paste(source, "ncbi_col-pubmed", sep = "; ")
        )
    ) %>%
    dplyr::bind_rows(col_pm_merge[-na.omit(match_col_pm), ])

# ...and Collection PMC results (prefer PubMed data for matches)
match_pmc <- match_citations(cb_col_merge1, col_pmc_merge)

final_merge <- cb_col_merge1 %>%
    dplyr::mutate(
        source = dplyr::if_else(
            is.na(match_pmc),
            source,
            paste(source, "ncbi_col-pmc", sep = "; ")
        )
    ) %>%
    dplyr::bind_rows(col_pmc_merge[-na.omit(match_pmc), ])

# ...and SAVE
readr::write_csv(final_merge, merge_citedby_file)

# convert IDs to links and save for Excel
merge_w_links <- final_merge %>%
    dplyr::mutate(
        doi = DO.utils:::append_to_url(doi, url = DO.utils:::get_url("doi")),
        pmid = DO.utils:::append_to_url(pmid, url = DO.utils:::get_url("pubmed")),
        pmcid = DO.utils:::append_to_url(pmcid, url = DO.utils:::get_url("pmc"))
    )

readr::write_csv(
    merge_w_links,
    file.path(citedby_dir, "DO_citedby-20211112-w_links.csv")
)

# improvements needed
#   1. abbreviated titles for Scopus data
#   2. retain scopus_eid, citedby, etc for matches (not straight preference)
#   3. added date for NCBI records
#   4. keep only oldest added date for record
