# Script to get citedby data

library(here)
library(tidyverse)
library(keyring)
library(rentrez)
library(lubridate)
library(DO.utils) # requires v0.2.5
library(googlesheets4)

###### MANUAL STEP REQUIRED!!! ######
# Download data manually to data/citedby/collection.txt by selecting:
#   'Send to:' > 'File' > 'Summary (text)' (Format drop down) > 'Create File'
#   URL: https://www.ncbi.nlm.nih.gov/myncbi/browse/collection/49204559/
#####################################

# Desired Improvements
#   1. Abbreviated titles for Scopus data
#   2. Retain scopus_eid, citedby, etc for matches (not straight preference)
#   3. Incorporate unexported DO.utils functions used here into appropriate
#       exported parent function.


# Files -------------------------------------------------------------------

# Google sheet - Final place for storage & review
gs <- "1wG-d0wt-9YbwhQTaelxqRzbm4qnu11WDM2rv3THy5mY"
cb_sheet <- "cited_by"

##### local files ######
citedby_dir <- here::here("data", "citedby")

# input collection file
collection_file <- file.path(citedby_dir, "collection.txt")

# cited by raw output files
cb_pm_raw_file <- file.path(citedby_dir, "do_cb_pm_summary_by_id.rda")
cb_scop_raw_file <- file.path(citedby_dir, "do_cb_scop_by_id.rda")
collection_pm_raw_file <- file.path(citedby_dir, "do_collection_pm_summary.rda")
collection_pmc_raw_file <- file.path(
  citedby_dir,
  "do_collection_pmc_summary.rda"
)

# final tidied file
merge_citedby_file <- file.path(citedby_dir, "DO_citedby.csv")


# PubMed cited by data ----------------------------------------------------

#  Load Data or Get/Save Data if it isn't available
if (file.exists(cb_pm_raw_file)) {
  load(file = cb_pm_raw_file)
} else {
  # set API key
  rentrez::set_entrez_key(keyring::key_get("ENTREZ_KEY"))

  do_cb_pm_summary_by_id <- DO.utils::citedby_pubmed(
    DO.utils::DO_pubs$pmid,
    by_id = TRUE
  )
  save(do_cb_pm_summary_by_id, file = cb_pm_raw_file)
}

cb_pm_by_id <- DO.utils::as_tibble(do_cb_pm_summary_by_id) %>%
  DO.utils:::hoist_ArticleIds()

# prepare for merge
cb_pm_merge <- cb_pm_by_id %>%
  dplyr::left_join(
    dplyr::select(DO.utils::DO_pubs, cites = pmid, internal_id),
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
  dplyr::mutate(added_dt = lubridate::now(tzone = "UTC"))


# Scopus cited by data ----------------------------------------------------

# get data if it isn't available
if (file.exists(cb_scop_raw_file)) {
  load(file = cb_scop_raw_file)
} else {
  do_cb_scop_by_id <- citedby_scopus(
    title = DO.utils::DO_pubs$title,
    id = DO.utils::DO_pubs$internal_id,
    by_id = TRUE,
    # set API key
    api_key = keyring::key_get("Elsevier_API"),
    # set institution token
    headers = rscopus::inst_token_header(
      keyring::key_get("Elsevier_insttoken")
    ),
    verbose = FALSE
  )

  save(do_cb_scop_by_id, file = cb_scop_raw_file)
}

cb_scop_by_id <- DO.utils::as_tibble(do_cb_scop_by_id)

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
    pub_type, added_dt = added
  ) %>%
  dplyr::mutate(source = "scopus") %>%
  # collapse cited by records that cite multiple DO_pubs
  DO.utils::collapse_col_flex(cites = "unique", added_dt = "first")


# PubMed collection data --------------------------------------------------

# Get "cited by" articles identified in MyNCBI collection; enough of these are
#   not redundant with the PubMed + Scopus cited by results to make this
#   worthwhile.

# extract IDs & get data from Entrez API to make formatting easier
collection_txt <- DO.utils::read_pubmed_txt(collection_file)

# Get Entrez API records for collection records with PubMed IDs
if (file.exists(collection_pm_raw_file)) {
  load(file = collection_pm_raw_file)
} else {
  col_pmid <- collection_txt$pmid %>%
    na.omit() %>%
    unique()
  do_col_pm_summary <- DO.utils::pubmed_summary(col_pmid)
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
  dplyr::mutate(
    source = "ncbi_col-pubmed",
    # note added time
    added_dt = lubridate::now(tzone = "UTC")
  )


# Get Entrez API records for collection records with PubMed Central IDs
if (file.exists(collection_pmc_raw_file)) {
  load(file = collection_pmc_raw_file)
} else {
  # get pmc summary only where no PubMed record exists
  col_pmcid <- dplyr::filter(collection_txt, is.na(pmid)) %>%
    .$pmcid %>%
    na.omit() %>%
    unique()
  do_col_pmc_summary <- DO.utils::pmc_summary(col_pmcid)
  save(do_col_pmc_summary, file = collection_pmc_raw_file)
}

col_pmc <- DO.utils::as_tibble(do_col_pmc_summary) %>%
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
  dplyr::mutate(
    source = "ncbi_col-pmc",
    # note added time
    added_dt = lubridate::now(tzone = "UTC")
  )


# Merge -------------------------------------------------------------------

# PubMed and Scopus cited by results (prefer PubMed data for matches)
match_scop <- DO.utils::match_citations(cb_pm_merge, cb_scop_merge)

cb_merge <- cb_pm_merge %>%
  dplyr::mutate(
    source = dplyr::if_else(
      is.na(match_scop),
      source,
      paste(source, "scopus", sep = "; ")
    )
  ) %>%
  dplyr::bind_rows(cb_scop_merge[-na.omit(match_scop), ])

## ...and Collection PubMed results (prefer previously merged data)
match_col_pm <- DO.utils::match_citations(cb_merge, col_pm_merge)

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
match_pmc <- DO.utils::match_citations(cb_col_merge1, col_pmc_merge)

final_merge <- cb_col_merge1 %>%
  dplyr::mutate(
    source = dplyr::if_else(
      is.na(match_pmc),
      source,
      paste(source, "ncbi_col-pmc", sep = "; ")
    )
  ) %>%
  dplyr::bind_rows(col_pmc_merge[-na.omit(match_pmc), ]) %>%
  # final collapse to ensure unique
  DO.utils::collapse_col_flex(
    first_author = "unique",
    pub_date = "first",
    source = "unique",
    pub_type = "unique",
    added_dt = "first"
  )

# ...and SAVE
readr::write_csv(final_merge, merge_citedby_file)


# Access data in DO-uses, cited_by google sheet --------------------------

gs_data <- googlesheets4::read_sheet(
  gs,
  cb_sheet,
  col_types = "c" # ensure proper formats!!
) %>%
  dplyr::mutate(
    dplyr::across(dplyr::matches("_(dt|date)$"), readr::parse_guess)
  )


# Identify new publications -----------------------------------------------

new <- DO.utils::match_citations(final_merge, gs_data) %>%
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

# identify columns in GS missing from new data
cols_missing <- names(gs_data)[!names(gs_data) %in% names(new_df)]
cols_add <- rep(NA_character_, length(cols_missing))
names(cols_add) <- cols_missing

# format new data
new_df <- new_df %>%
  dplyr::mutate(
    pmid = DO.utils::build_hyperlink(pmid, "pubmed", "gs"),
    doi = DO.utils::build_hyperlink(doi, "doi", "gs"),
    pmcid = DO.utils::build_hyperlink(pmcid, "pmc_article", "gs"),
  ) %>%
  tibble::add_column(!!!cols_add) %>%
  dplyr::select(dplyr::one_of(names(gs_data)))

googlesheets4::sheet_append(gs, new_df, cb_sheet)
