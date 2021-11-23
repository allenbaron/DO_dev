# Get DO release details directly from Github API;
#   as per https://docs.github.com/en/rest/reference/repos#list-releases
# Updated: 2021-11-22

## BE SURE TO MAKE A GITHUB PAT AVAILABLE!!!

library(here)
library(tidyverse)
library(gh)

release_dir <- here::here("data/DO_release")
details_file <- file.path(release_dir, "DO_release_details.csv")

# get raw details
release_raw <- gh::gh(
    "/repos/{owner}/{repo}/releases",
    owner = "DiseaseOntology",
    repo = "HumanDiseaseOntology",
    .limit = Inf
)

release_df <- release_raw %>%
    tibble::enframe(name = "tmp") %>%
    tidyr::unnest_wider(value) %>%
    dplyr::select(-tmp) %>%
    tidyr::unnest_wider(author, names_sep = "_") %>%
    dplyr::select(tag_name, name, author_login, created_at, body, everything())


# write
readr::write_excel_csv(release_df, details_file)
