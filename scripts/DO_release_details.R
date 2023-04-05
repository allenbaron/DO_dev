# Get DO release details directly from Github API;
#   as per https://docs.github.com/en/rest/reference/repos#list-releases
# Updated: 2021-11-22

## BE SURE TO MAKE A GITHUB PAT AVAILABLE!!!

library(here)
library(tidyverse)
library(gh)
library(lubridate)

clean_up <- function(x) {
     stringr::str_replace_all(x, c("[\r\n]" = " ")) %>%
        DO.utils::replace_blank() %>%
        stringr::str_squish()
}

release_dir <- here::here("data/DO_release")
details_file <- file.path(release_dir, "DO_release_details.csv")


# load previous release details
if (file.exists(details_file)) {
    release_df <- readr::read_csv(
        details_file,
        col_types = readr::cols(
            created_at = "T", published_at = "T",
            author_site_admin = "l", draft = "l", prerelease = "l",
            .default = "c"
        )
    )
} else {
    release_df <- NULL
}
    # was necessary to clean up original records but should no longer be
# ) %>%
#     mutate(across(where(is.character), clean_up))


# get new details (last 10 releases)
release_raw <- gh::gh(
    "/repos/{owner}/{repo}/releases",
    owner = "DiseaseOntology",
    repo = "HumanDiseaseOntology",
    .limit = 10
)

release_tidy <- release_raw %>%
    tibble::enframe(name = "tmp") %>%
    tidyr::unnest_wider(value) %>%
    dplyr::select(-tmp) %>%
    tidyr::unnest_wider(author, names_sep = "_") %>%
    dplyr::mutate(
        dplyr::across(.cols = dplyr::everything(), clean_up),
        dplyr::across(.cols = dplyr::ends_with("_at"), lubridate::ymd_hms),
        dplyr::across(
            .cols = c(author_site_admin, draft, prerelease),
            as.logical
        )
    ) %>%
    dplyr::select(tag_name, name, author_login, created_at, body, everything())

if (is.null(release_df)) {
    release_updated <- release_tidy
} else {
    release_updated <- release_df %>%
        dplyr::bind_rows(release_tidy) %>%
        unique()
}

finish <- readline(
    prompt = paste(
        nrow(release_updated) - nrow(release_df),
        "new releases identified. Save? (yes/no)"
    )
)


# write
if (finish == "yes") {
    dir.create(dirname(details_file), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(release_updated, details_file)
}
