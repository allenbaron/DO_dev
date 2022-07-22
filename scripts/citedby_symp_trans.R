# Temporary script for citedby data

library(here)
library(tidyverse)
library(keyring)
library(rentrez)
library(lubridate)
library(DO.utils) # requires >= v0.1.7.900
library(googlesheets4)

# improvements needed
#   1. abbreviated titles for Scopus data
#   2. retain scopus_eid, citedby, etc for matches (not straight preference)
#   3. parse & format Lens collection data
#   4. start SYMP/TRANS NCBI collection? may be redundant with Lens (code in
#       place, but commented out, just in case)


# Files -------------------------------------------------------------------

# Google sheet - Final place for storage & review
gs <- "1wG-d0wt-9YbwhQTaelxqRzbm4qnu11WDM2rv3THy5mY"
cb_sheet <- "SYMP_TRANS_citedby"

##### local files ######
citedby_dir <- here::here("data", "citedby")

# input collection files
# ncbi_collection_file <- file.path(citedby_dir, "NCBI-ST_collection.txt")
lens_collection_file <- file.path(citedby_dir, "Lens-ST_collections.csv")

# cited by raw output files
cb_pm_raw_file <- file.path(citedby_dir, "st_cb_pm_summary_by_id.rda")
cb_scop_raw_file <- file.path(citedby_dir, "st_cb_scop_by_id.rda")
# collection_pm_raw_file <- file.path(citedby_dir, "st_collection_pm_summary.rda")
# collection_pmc_raw_file <- file.path(
#     citedby_dir,
#     "st_collection_pmc_summary.rda"
# )

# final tidied file
merge_citedby_file <- file.path(citedby_dir, "ST_citedby.csv")


# PubMed cited by data ----------------------------------------------------

#  Load Data or Get/Save Data if it isn't available
if (file.exists(cb_pm_raw_file)) {
    load(file = cb_pm_raw_file)
} else {
    # set API key
    rentrez::set_entrez_key(keyring::key_get("ENTREZ_KEY"))

    st_cb_pm_summary_by_id <- DO.utils::citedby_pubmed(
        DO.utils::ST_pubs$pmid,
        by_id = TRUE
    )
    save(st_cb_pm_summary_by_id, file = cb_pm_raw_file)
}

cb_pm_by_id <- as_tibble(st_cb_pm_summary_by_id) %>%
    DO.utils:::hoist_ArticleIds()

# prepare for merge
cb_pm_merge <- cb_pm_by_id %>%
    dplyr::left_join(
        dplyr::select(ST_pubs, cites = pmid, internal_id),
        by = "cites"
    ) %>%
    dplyr::mutate(
        pub_type = purrr::map_chr(PubType, DO.utils::vctr_to_string, delim = "|"),
        pub_date = lubridate::date(SortPubDate),
        cites = NULL
    ) %>%
    dplyr::select(
        first_author = SortFirstAuthor, title = Title, journal = Source,
        pub_date, doi, pmid, pmcid, cites = internal_id, pub_type
    ) %>%
    dplyr::mutate(source = "pubmed") %>%
    # collapse cited by records that cite multiple DO_pubs
    DO.utils::collapse_col(cites) %>%
    # note added time
    dplyr::mutate(added = lubridate::now(tzone = "UTC"))


# Scopus cited by data ----------------------------------------------------

# get data if it isn't available
if (file.exists(cb_scop_raw_file)) {
    load(file = cb_scop_raw_file)
} else {
    st_cb_scop_by_id <- citedby_scopus(
        title = DO.utils::ST_pubs$title,
        id = DO.utils::ST_pubs$internal_id,
        by_id = TRUE,
        # set API key
        api_key = keyring::key_get("Elsevier_API"),
        # set institution token
        headers = rscopus::inst_token_header(
            keyring::key_get("Elsevier_insttoken")
        ),
        verbose = FALSE
    )

    save(st_cb_scop_by_id, file = cb_scop_raw_file)
}

cb_scop_by_id <- as_tibble(st_cb_scop_by_id)

# prepare for merge
cb_scop_merge <- cb_scop_by_id %>%
    dplyr::mutate(
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
        pub_date, doi = "prism:doi", pmid = "pubmed-id", scopus_eid = eid, cites,
        pub_type, added
    ) %>%
    dplyr::mutate(source = "scopus") %>%
    # collapse cited by records that cite multiple DO_pubs
    DO.utils::collapse_col_flex(cites = "unique", added = "first")


# # PubMed collection data --------------------------------------------------
#
# # extract PubMed IDs & get data from Entrez API to make formatting easier
# collection_txt <- read_pubmed_txt(ncbi_collection_file)
#
# collection_id <- tibble::tibble(
#     n = 1:length(collection_txt),
#     pmid = stringr::str_extract(
#         collection_txt,
#         "PMID: [0-9]{8}"
#     ),
#     pmcid = stringr::str_extract(
#         collection_txt,
#         "PMCID: PMC[0-9]+"
#     )
# ) %>%
#     dplyr::mutate(
#         pmid = stringr::str_remove(pmid, "PMID: "),
#         pmcid = stringr::str_remove(pmcid, "PMCID: ")
#     )
#
# # Get Entrez API records for collection records with PubMed IDs
# if (file.exists(collection_pm_raw_file)) {
#     load(file = collection_pm_raw_file)
# } else {
#     col_pmid <- collection_id$pmid %>%
#         na.omit() %>%
#         unique()
#     st_col_pm_summary <- pubmed_summary(col_pmid)
#     save(st_col_pm_summary, file = collection_pm_raw_file)
# }
#
# col_pm <- as_tibble(st_col_pm_summary) %>%
#     DO.utils:::hoist_ArticleIds()
#
# col_pm_merge <- col_pm %>%
#     dplyr::mutate(
#         pub_type = purrr::map_chr(PubType, vctr_to_string, delim = "|"),
#         pub_date = lubridate::date(SortPubDate)
#     ) %>%
#     dplyr::select(
#         first_author = SortFirstAuthor, title = Title, journal = Source,
#         pub_date, doi, pmid, pmcid, pub_type
#     ) %>%
#     dplyr::mutate(
#         source = "ncbi_col-pubmed",
#         # note added time
#         added = lubridate::now(tzone = "UTC")
#     )
#
#
# # Get Entrez API records for collection records with PubMed Central IDs
# if (file.exists(collection_pmc_raw_file)) {
#     load(file = collection_pmc_raw_file)
# } else {
#     # get pmc summary only where no PubMed record exists
#     col_pmcid <- dplyr::filter(collection_id, is.na(pmid)) %>%
#         .$pmcid %>%
#         na.omit() %>%
#         unique()
#     st_col_pmc_summary <- pmc_summary(col_pmcid)
#     save(st_col_pmc_summary, file = collection_pmc_raw_file)
# }
#
# col_pmc <- as_tibble(st_col_pmc_summary) %>%
#     DO.utils:::hoist_ArticleIds()
#
# col_pmc_merge <- col_pmc %>%
#     tidyr::hoist(
#         .col = Authors,
#         first_author = list(1L, 1L)
#     ) %>%
#     dplyr::mutate(
#         pub_date = lubridate::date(SortDate),
#         pmid = dplyr::if_else(
#             stringr::str_length(pmid) < 8,
#             NA_character_,
#             pmid
#         )
#     ) %>%
#     dplyr::select(
#         first_author, title = Title, journal = Source, pub_date, doi, pmid,
#         pmcid
#     ) %>%
#     # drop columns without values
#     dplyr::select(where(~!all(is.na(.x)))) %>%
#     dplyr::mutate(
#         source = "ncbi_col-pmc",
#         # note added time
#         added = lubridate::now(tzone = "UTC")
#     )


# Lens collection data --------------------------------------------------

# NEED TO WRITE!!!
#   - read_lens_text not written (necessary?)
#   - add processing here (or in DO.utils)
#   - final output = lens_collection
# lens_collection_txt <- read_lens_txt(lens_collection_file)
# lens_collection <- ...

# Merge -------------------------------------------------------------------

# PubMed and Scopus cited by results (prefer PubMed data for matches)
match_scop <- match_citations(cb_pm_merge, cb_scop_merge)

cb_merge <- cb_pm_merge %>%
    dplyr::mutate(
        source = dplyr::if_else(
            is.na(match_scop),
            source,
            paste(source, "scopus", sep = "; ")
        )
    ) %>%
    dplyr::bind_rows(cb_scop_merge[-na.omit(match_scop), ])

## ...and NCBI Collection PubMed results (prefer previously merged data)
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

# ...and NCBI Collection PMC results (prefer PubMed data for matches)
match_pmc <- match_citations(cb_col_merge1, col_pmc_merge)

cb_col_merge2 <- cb_col_merge1 %>%
    dplyr::mutate(
        source = dplyr::if_else(
            is.na(match_pmc),
            source,
            paste(source, "ncbi_col-pmc", sep = "; ")
        )
    ) %>%
    dplyr::bind_rows(col_pmc_merge[-na.omit(match_pmc), ])

# ...and Lens Collection (prefer previously merged data)
# NEED TO WRITE!!!

final_merge <- dplyr::bind_rows(cb_col_merge2, lens_collection) %>%
    # final collapse to ensure unique
    DO.utils::collapse_col_flex(
        first_author = "unique",
        pub_date = "first",
        source = "unique",
        pub_type = "unique",
        added = "first"
    )

# ...and SAVE
readr::write_csv(final_merge, merge_citedby_file)


# Access data in DO-uses,  cited_by google sheet --------------------------

# change sheet location and tidy more for next run in April 2022!!!!
gs_data <- googlesheets4::read_sheet(
    gs,
    cb_sheet,
    col_types = "ccccccccccccTcccDccccccccccccc" # ensure proper formats!!
)



# Identify new publications -----------------------------------------------

new <- match_citations(final_merge, gs_data) %>%
    is.na()
new_df <- dplyr::filter(final_merge, new)


# Add new data to GS data & count -----------------------------------------

updated <- dplyr::bind_rows(gs_data, new_df)

to_count <- rep(TRUE, nrow(updated))
counts <- updated %>%
    dplyr::select(pmid, pmcid, scopus_eid, doi) %>%
    purrr::map_int(
        function(.col) {
            n <- .col[to_count] %>%
                stats::na.omit() %>%
                dplyr::n_distinct()
            to_count <<- dplyr::if_else(to_count == FALSE, FALSE, is.na(.col))
            n
        }
    )
counts
sum(counts)
nrow(updated)


# Append new data to GS ---------------------------------------------------

# custom function to set hyperlinks
set_hyperlink <- function(x, type) {
    link <- DO.utils:::append_to_url(x, url = DO.utils:::get_url(type))
    formula <- dplyr::if_else(
        is.na(x),
        x,
        as.character(glue::glue('=HYPERLINK("{link}", "{x}")'))
    )
    googlesheets4::gs4_formula(formula)
}

# identify columns in GS missing from new data
cols_missing <- names(gs_data)[!names(gs_data) %in% names(new_df)]
cols_add <- rep(NA_character_, length(cols_missing))
names(cols_add) <- cols_missing

# format new data
new_df <- new_df %>%
    dplyr::mutate(
       pmid = set_hyperlink(pmid, "pubmed"),
       doi = set_hyperlink(doi, "doi"),
       pmcid = set_hyperlink(pmcid, "pmc_article"),
    ) %>%
    tibble::add_column(!!!cols_add) %>%
    dplyr::select(dplyr::one_of(names(gs_data)))

googlesheets4::sheet_append(gs, new_df, cb_sheet)
