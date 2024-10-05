# submit up to 1500 URLs to ATOMSEO / day @ https://error404.atomseo.com/
#   --> then grab URL of page with output and parse with this script (free
#       version of ATOMSEO results not downloadable, but URLs are unique)
# 2024-09-25

library(here)
library(httr)
library(rvest)
library(tibble)
library(googlesheets4)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)


# address to google sheet for current URL audit ---------------------------

gs <- "https://docs.google.com/spreadsheets/d/16bXSsqrTkllBK73jnsscNfqg4yuwjb7VlckMGdaPgCY/edit?gid=2088402368#gid=2088402368"
submit_sheet <- paste0("atomseo-", format(Sys.Date(), "%Y%m%d"))


# scrape & save error data from ATOMSEO -----------------------------------
# everything else submitted (on `submit_sheet` passed the check)
url <- "https://error404.atomseo.com/SeoListCheck/Report/LR241510"
atomseo_scrape <- httr::GET(url)

save(
    atomseo_scrape,
    file = here::here(
        paste0("data/atomseo_scrape-", format(Sys.Date(), "%Y%m%d"), ".Rdata")
    )
)


# parse error content -----------------------------------------------------

error_content <- httr::content(atomseo_scrape)
col_nm <- rvest::html_nodes(error_content, "tr th") |>
    rvest::html_text()

df <- tibble::tibble(
    url = rvest::html_nodes(error_content, "tr .mdl-data-table__cell--non-numeric a") |>
        rvest::html_attr('href') |>
        # ATOMSEO encodes URLs and decode doesn't fully decode them with single run
        utils::URLdecode() |>
        utils::URLdecode() |>
        stringr::str_remove("^[^=]*="),
    status = rvest::html_nodes(error_content, "tr td:nth-of-type(2)") |>
        rvest::html_text(),
    impact = rvest::html_nodes(error_content, "tr td:nth-of-type(3)") |>
        rvest::html_text()
)


# get total submission list from google sheet & merge data ----------------

orig_df <- googlesheets4::read_sheet(gs, submit_sheet, col_names = "orig_url") |>
    # some original URLs are (partially) encoded and decoding makes comparison work better
    dplyr::mutate(url_decode = utils::URLdecode(orig_url))

# fuzzy match where exact matches don't exist
orig_no_match <- dplyr::anti_join(orig_df, df, by = c("url_decode" = "url"))
no_match_df <- dplyr::anti_join(df, orig_df, by = c("url" = "url_decode")) |>
    dplyr::mutate(
        pos_match = purrr::map(
            .data$url,
            function(.p) agrep(.p, orig_no_match$url_decode)
        )
    )

# ensure 1 match / URL (just choose in order if approx gives >1... will check it
#   in a sec
uniq_pos <- unique(unlist(no_match_df$pos_match))
no_match_df$pos_match <- purrr::map_int(
    no_match_df$pos_match,
    function(.l) {
        out <- head(.l, 1)
        while (!out %in% uniq_pos) {
            .l <- .l[-1]
            out <- .l[1]
        }
        uniq_pos <<- uniq_pos[uniq_pos != out]
        out
    }
)

approx_match <- no_match_df |>
    dplyr::bind_cols(orig_no_match[no_match_df$pos_match, "url_decode"])

# approximate matches look okay?
View(approx_match)
continue <- readline("Do all approximate matches look okay? y/n")

# combine all matches back into a single df (should be same length as orig_df)
if (continue == "y") {
    # get matching DOID
    do_data <- googlesheets4::read_sheet(gs, "audit_20240923") |>
        dplyr::select(id, url)

    res_df <- df |>
        # get exact URL matches
        dplyr::filter(!url %in% approx_match$url) |>
        # get confirmed approx URL matches
        dplyr::bind_rows(
            dplyr::select(approx_match, url = url_decode, status:impact)
        ) |>
        dplyr::full_join(orig_df, by = c("url" = "url_decode")) |>
        dplyr::relocate(orig_url, .before = 1) |>
        dplyr::rename(url_decode = url) |>
        # mark tested URLs that did not error
        tidyr::replace_na(list(impact = "OK")) |>
        # add DOIDs
        dplyr::left_join(do_data, by = c("orig_url" = "url")) |>
        dplyr::relocate(id, .before = 1) |>
        DO.utils::collapse_col("id")

    dplyr::count(res_df, status, sort = TRUE)
}

# save if same number of rows & nothing in anti_join
fix_df <- dplyr::anti_join(orig_df, res_df, by = "orig_url")

if (nrow(unique(res_df)) == nrow(orig_df) && nrow(fix_df) == 0) {
    # save back to google sheet
    googlesheets4::write_sheet(
        res_df,
        gs,
        paste0("atomseo-", format(Sys.Date(), "%Y%m%d"))
    )

    # remove atomseo tested urls from 'remaining' sheet & resave
    remain <- googlesheets4::read_sheet(gs, "remaining")

    remain_rw <- remain |>
        dplyr::anti_join(res_df, by = c("url_unique" = "orig_url"))

    googlesheets4::write_sheet(remain_rw, gs, "remaining")
}
