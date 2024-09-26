# submit up to 1500 URLs to ATOMSEO / day @ https://error404.atomseo.com/
#.   --> use this script to collect up to 1500 URLs for list submission

library(googlesheets4)
library(httr)
library(dplyr)

gs <- "https://docs.google.com/spreadsheets/d/16bXSsqrTkllBK73jnsscNfqg4yuwjb7VlckMGdaPgCY/edit?gid=2088402368#gid=2088402368"

# custom function(s) ------------------------------------------------------

extract_domain <- function(url) {
    x <- httr::parse_url(url)
    x[!names(x) %in% c("scheme", "hostname")] <- NULL
    xnew <- httr::build_url(x)
    xnew
}

remain <- googlesheets4::read_sheet(gs, "remaining") |>
    unique() |>
    dplyr::mutate(domain = purrr::map_chr(.data$url_unique, extract_domain)) |>
    # count remaining URLs in domain
    dplyr::mutate(
        dom_n = dplyr::n_distinct(.data$url_unique),
        .by = "domain"
    )

# select <= 30 URLs from domains with >50 for testing
submit <- remain |>
    dplyr::slice_sample(n = 75, by = "domain") |>
    dplyr::arrange(dplyr::desc("dom_n")) |>
    dplyr::filter(dplyr::row_number(url_unique) <= 1500)

submit |>
    dplyr::select("url_unique") |>
    googlesheets4::write_sheet(
        gs,
        paste0("atomseo-", format(Sys.Date(), "%Y%m%d"))
    )
