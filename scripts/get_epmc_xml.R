library(europepmc)
library(here)
library(tidyverse)
library(xml2)


# Manual inputs -----------------------------------------------------------

terms <- "alpha-gal syndrome"

out_dir <- here::here("data/disease_info")

# whether terms should be quoted and/or full text saved to xml files locally
quote_terms <- FALSE
save_xml <- TRUE

# if planning to download full text to xml files locally, set up subdirectory
out_subdir <- file.path(out_dir, gsub("[-[:space:]]", "_", terms[1]))
if (!dir.exists(out_subdir)) {
    dir.create(out_subdir, recursive = TRUE)
}


# Custom functions --------------------------------------------------------

# get_ftxt_safely() will automatically get PMC articles and will NOT
#   fail if on errors caused by individual download failures
safe_epmc_ftxt <- purrr::safely(europepmc::epmc_ftxt, otherwise = NA, quiet = FALSE)

get_ftxt_safely <- function(pmcid) {
    out <- list(safe_epmc_ftxt(pmcid))
    cat(".")
    out
}

# drops retractions/pre-prints/anything without pmcid (e.g. books), reduces columns, and formats dates
tidy_epmc_search <- function(.df) {
    tidy_df <- .df |>
        dplyr::filter(
            !stringr::str_detect(pubType, "retract|preprint"),
            !is.na(pmcid)
        ) |>
        dplyr::select(
            "id", "title", "pubYear", pubDate = "firstPublicationDate", "pmcid"
        ) |>
        dplyr::mutate(
            pubDate = lubridate::as_date(pubDate),
            pubYear = lubridate::year(pubDate)
        )
    tidy_df
}

sample_open_access <- function(tidy_epmc, nmax = 5000) {
    # sample maintaining general pattern over time, if too many
    oa_total <- nrow(tidy_epmc)
    if (oa_total > nmax) {
        warning(
            paste0(
                oa_total, " open access publications found; ",
                "sampling ≤ ", nmax
            ),
            immediate. = TRUE
        )
        n_yr <- dplyr::count(tidy_epmc, pubYear, name = "yr_total") |>
            dplyr::mutate(
                frac = .data$yr_total / sum(.data$yr_total),
                frac_n = DO.utils::round_down(.data$frac * 5000)
            )
        tidy_epmc <- purrr::map2(
            n_yr$pubYear,
            n_yr$frac_n,
            ~ tidy_epmc |>
                dplyr::filter(.data$pubYear == .x) |>
                dplyr::slice_sample(n = .y)
        ) |>
            dplyr::bind_rows()
    }

    full_text <- tidy_epmc |>
        dplyr::rowwise() |>
        dplyr::mutate(ft_xml = get_ftxt_safely(.data$pmcid)) |>
        dplyr::mutate(ft = parse_ftxt_xml(ft_xml, "//body"))

    full_text
}

# parse_ftxt_xml() parses results from get_ftxt_safely()
parse_ftxt_xml <- function(safe_ftxt_xml, xml_accessor) {
    if (!is.null(safe_ftxt_xml$error)) {
        return(paste0("ERROR: ", safe_ftxt_xml$error$message))
    }
    out <- safe_ftxt_xml$result |>
        xml2::xml_find_all(xml_accessor) |>
        xml2::xml_text()

    if (length(out) == 0) {
        out <- paste0(
            "ERROR [NO BODY]: ",
            xml2::xml_text(safe_ftxt_xml$result)
        )
        if (length(out) == 0) {
            out <- "ERROR: No text extractable"
        }
    } else if (length(out) > 1) {
        out <- DO.utils::vctr_to_string(out, delim = "%%%%%") |>
            paste0("WARNING: Multilength output, separated by %%%%%.")
    }

    out
}

# possibly_write_xml() will attempt to write XML to file, returning TRUE if
#   successful, FALSE if not (name will be {pmcid}.xml in outdir)
possibly_write_xml <- function(ft_xml, pmcid, outdir, quiet = TRUE) {
    outfile <- file.path(outdir, paste0(pmcid, ".xml"))
    res <- tryCatch(
        xml2::write_xml(ft_xml$result, outfile),
        error = function(e) {
            if (!quiet) message("Error: ", conditionMessage(e))
            FALSE
        }
    )
    if (is.null(res)) return(TRUE)
    res
}


# Main code ----------------------------------------------------------------

if (quote_terms) {
    search_terms <- DO.utils::sandwich_text(terms, '"')
} else {
    search_terms <- terms
}

search_str <- search_terms |>
    paste0(collapse = " OR ") |>
    DO.utils::sandwich_text(c('OPEN_ACCESS:y AND (', ')'))

data_file <- file.path(out_subdir, "epmc_results.rda")
if (!file.exists(data_file)) {
    res <- europepmc::epmc_search(search_str, synonym = FALSE, limit = 20000)
    save(res, file = data_file)
} else {
    load(data_file)
}

res_tidy <- tidy_epmc_search(res)

if (!exists("res_ftxt")) {
    res_ftxt <- sample_open_access(res_tidy)

    if (save_xml) {
        res_ftxt <- res_ftxt |>
            dplyr::mutate(
                xml_saved = possibly_write_xml(
                    .data$ft_xml,
                    .data$pmcid,
                    out_subdir
                )
            )
        dplyr::count(res_ftxt, .data$xml_saved)
    }

    save(res, res_ftxt, file = data_file)
}

# write results to TSV
df_out <- res_ftxt |>
    dplyr::select(dplyr::where(~ !is.list(.x)))
readr::write_tsv(
    df_out,
    file.path(out_subdir, "epmc_results.tsv"),
    quote = "needed"
)

