# Parse github release details, including term & def counts
# Updated: 2021-08-10

# !!!!! NOTE !!!!! #
# Release number of version may be out of order for releases that occurred
# on the same date.

library(here)
library(tidyverse)
library(DO.utils)
library(lubridate)

release_dir <- "data/releases"

# load raw details (copied from github, must start at tag and end with "Assets")
release_details_raw <- readr::read_lines(
    here::here(release_dir, "do_releases-details.txt")
)

# custom parsing functions
parse_msg <- function(x, start, end, delim = "\n") {
    DO.utils::vctr_to_string(x[start:end], delim = delim)
}

parse_release_details <- function(x) {

    x_clean <- DO.utils::drop_blank(x)
    # use git hash to identify
    hash_pos <- which(
        stringr::str_detect(x_clean, "^[a-z0-9]{7}$")
    )
    assets_pos <- which(
        stringr::str_detect(x_clean, "^Assets")
    )

    release_df <- tibble::tibble(
        tag = x_clean[hash_pos - 1],
        git_hash = x_clean[hash_pos],
        name = x_clean[hash_pos + 1],
        user = stringr::str_extract(x_clean[hash_pos + 2], "@[^ ]+"),
        msg = purrr::map2_chr(
            .x = hash_pos + 3,
            .y = assets_pos -1,
            function(start, end) {
                parse_msg(x_clean, start, end, delim = " ")
            }
            )
        )

    release_df
}

# parse
release_df <- parse_release_details(release_details_raw)

# add date & release number
DO_release_tidy <- release_df %>%
    dplyr::mutate(
        # remove all extra characters (version identifiers)
        date = stringr::str_remove_all(tag, "^v+|[_.v]+[0-9]$") %>%
            # fix specific date
            stringr::str_replace("07015", "07-15") %>%
            lubridate::ymd(),
        rel_num = dplyr::row_number(date),
        # create yr-month "version"
        rel_yr_month = paste(
            stringr::str_remove(lubridate::year(date), "^[0-9]{2}"),
            lubridate::month(date, label = TRUE, abbr = TRUE),
            sep = "-"
        )
    ) %>%
    # add number series for releases in same yr-month
    dplyr::group_by(rel_yr_month) %>%
    dplyr::mutate(
        rel_version = dplyr::if_else(
            dplyr::row_number(date) > 1,
            paste(rel_yr_month, dplyr::row_number(date), sep = ".v"),
            rel_yr_month
        )
    ) %>%
    dplyr::ungroup()


# Add release term & def counts (calc from git tags in do_release_stats.py)
dt_counts <- readr::read_csv(
    here::here(release_dir, "do_term_def_counts.csv")
) %>%
    dplyr::rename(tag = X1) %>%
    dplyr::rename_with(.fn = ~paste0("n_", .x), .cols = -tag)

# Join
DO_release_tidy <- dplyr::left_join(
    DO_release_tidy,
    dt_counts,
    by = "tag"
) %>%
    dplyr::select(tag, rel_num, rel_version, date, n_terms, n_defs,
                  git_hash, name, user, msg)

# write
readr::write_excel_csv(
    DO_release_tidy,
    here::here(release_dir, "do_releases-details.csv")
)
