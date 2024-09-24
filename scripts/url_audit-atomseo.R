# submit up to 1500 URLs to ATOMSEO / day @ https://error404.atomseo.com/
#   --> then grab URL and parse with this script (free version not downloadable)

library(httr)
library(xml2)
library(rvest)
library(here)


# scrape error data from atomseo after run... everything else submitted is okay
url <- "https://error404.atomseo.com/SeoListCheck/Report/LR448224"
z <- httr::GET(url)

atomseo_scrape <- z
save(
    atomseo_scrape,
    file = here::here(
        paste0("data/atomseo_scrape-", format(Sys.Date(), "%Y%m%d"), ".Rdata")
    )
)

zcon <- httr::content(z)
col_nm <- rvest::html_nodes(zcon, "tr th") |>
    rvest::html_text()

df <- tibble::tibble(
    url = rvest::html_nodes(zcon, "tr .mdl-data-table__cell--non-numeric a") |>
        rvest::html_attr('href') |>
        utils::URLdecode() |>
        utils::URLdecode() |>
        stringr::str_remove("^[^=]*="),
    status = rvest::html_nodes(zcon, "tr td:nth-of-type(2)") |>
        rvest::html_text(),
    impact = rvest::html_nodes(zcon, "tr td:nth-of-type(3)") |>
        rvest::html_text()
)

# get total submission list from google sheet & merge data
gs <- "https://docs.google.com/spreadsheets/d/16bXSsqrTkllBK73jnsscNfqg4yuwjb7VlckMGdaPgCY/edit?gid=2088402368#gid=2088402368"

orig_df <- googlesheets4::read_sheet(gs, "atomseo-submit-20240924") |>
    dplyr::filter(dplyr::row_number() <= 1500) |>
    dplyr::mutate(url_decode = utils::URLdecode(orig_url))

orig_no_match <- dplyr::anti_join(orig_df, df, by = c("url_decode" = "url"))
no_match_df <- dplyr::anti_join(df, orig_df, by = c("url" = "url_decode")) |>
    dplyr::mutate(
        pos_match = purrr::map_int(
            no_match_df$url,
            function(.p) agrep(.p, orig_no_match$url_decode)
        )
    )

approx_match <- no_match_df |>
    dplyr::bind_cols(orig_no_match[no_match_df$pos_match, "url_decode"])

res_df <- df |>
    dplyr::filter(!url %in% approx_match$url) |>
    dplyr::bind_rows(
        dplyr::select(approx_match, url = url_decode, status:impact)
    ) |>
    dplyr::full_join(orig_df, by = c("url" = "url_decode")) |>
    dplyr::relocate(orig_url, .before = 1) |>
    dplyr::rename(url_decode = url) |>
    tidyr::replace_na(list(impact = "OK"))

dplyr::count(res_df, status, sort = TRUE)

# save to new sheet
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


