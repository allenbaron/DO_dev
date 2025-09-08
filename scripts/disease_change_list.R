# Returns a list of new & obsoleted diseases between two releases of the DO

library(DO.utils)
library(dplyr)
library(git2r)
library(here)
library(purrr)
library(stringr)


# MANUAL INPUTS -----------------------------------------------------------

releases <- c("v2023-08-08", "v2025-08-29")
repo_path <- "~/Documents/Ontologies/HumanDiseaseOntology"



# Custom functions --------------------------------------------------------

query_tags <- function(tags, repo_path, rel_doid_path, query) {
    init <- git2r::repository_head(repo_path)
    on.exit(git2r::checkout(init))

    doid_path <- file.path(repo_path, rel_doid_path)

    purrr::map(
        tags,
        ~ query_tag(.x, doid_path, query)
    )
}

query_tag <- function(tag, doid_path, query) {
    git2r::checkout(tag)

    DO.utils::robot_query(
        i = doid_path,
        query = query,
        tidy_what = c("header", "lgl_NA_FALSE", "rm_lang_tag")
    )
}

#' Create a Range from Two Years
#'
#' Creates a range from two specified years, dropping digits that are equivalent
#' while respecting a minimum number of digits to keep (from right).
#' @param ... The years of the range (only first & last will be used).
#' @param min_retain The minimum number of digits from right to retain.
years_as_range <- function(..., min_retain = 2) {
    # standardize to character with no extra spaces & take only first & last year
    years <- sort(stringr::str_trim(c(...)))
    y1 <- years[1]
    y2 <- years[length(years)]

    if (stringr::str_length(y1) != 4) {
        stop(paste0("`y1` must be a 4-digit year, not ", y1))
    }
    if (stringr::str_length(y2) != 4) {
        stop(paste0("`y2` must be a 4-digit year, not ", y2))
    }
    if (y1 == y2) stop("`y1` and `y2` must be different")
    if (!DO.utils::is_scalar_whole_number(min_retain)) {
        stop("`min_retain` must be a single, whole number")
    }

    # Find where the digits differ
    d1 <- strsplit(y1, "")[[1]]
    d2 <- strsplit(y2, "")[[1]]
    diff_pos <- which(d1 != d2)[1]

    last_keep <- 5 - min_retain
    if (last_keep > diff_pos) last_keep <- diff_pos
    paste0(y1, "-", substring(y2, last_keep))
}



# Identify New & Obsoleted Terms ------------------------------------------

tags <- git2r::tags(repo_path)
rel_tags <- tags[releases]

tag_missing <- purrr::map_lgl(rel_tags, is.null)
if (any(tag_missing)) {
    stop_msg <- paste0(
        "Release(s) not found in the repository: ",
        paste(releases[tag_missing], collapse = ", ")
    )
    stop(stop_msg)
}

all_disease_query <- '
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>

    SELECT ?id ?label ?deprecated
    WHERE {
      ?iri oboInOwl:id ?id ;
        rdfs:label ?label .

      OPTIONAL { ?iri owl:deprecated ?deprecated }
    }'

list_df <- query_tags(
    rel_tags,
    repo_path,
    "src/ontology/doid.owl",
    all_disease_query
)
comparison <- list_df |>
    dplyr::bind_rows(.id = "release") |>
    DO.utils::collapse_col(c("label", "deprecated", "release")) |>
    dplyr::mutate(
        change = dplyr::case_when(
            .data$release == releases[2] & .data$deprecated == "FALSE" ~ "new",
            .data$release == releases[2] & .data$deprecated == "TRUE" ~ "new-obsoleted",
            stringr::str_detect(.data$release, "\\|") & .data$deprecated == "FALSE|TRUE" ~ "obsoleted",
            stringr::str_detect(.data$release, "\\|") & .data$deprecated == "TRUE|FALSE" ~ "unobsoleted",
            stringr::str_detect(.data$release, "\\|") & !stringr::str_detect(.data$deprecated, "\\|") ~ "none",
            TRUE ~ "ERROR"
        )
    )

if (any(comparison$change == "ERROR")) {
    stop("There are errors in the change column. Please check the data.")
}

dplyr::count(comparison, .data$change)


# Save full dataset
yr_range <- stringr::str_extract(releases, "[0-9]{4}") |>
    years_as_range()
comparison_file <- paste0("diseases_", yr_range, ".rda")
save(comparison, file = here::here("data/DO_release", comparison_file))


# Save selected dataset to HDO repo
out <- comparison |>
    dplyr::filter(.data$change != "none") |>
    # rename change to status
    dplyr::select("id", "label", status = "change") |>
    dplyr::mutate(
        label = stringr::str_remove(.data$label, ".*\\|"),
        # list new-obsoleted terms as just obsoleted
        status = stringr::str_remove(.data$status, "^new-"),
        # use "added" instead of "new"
        status = dplyr::if_else(.data$status == "new", "added", .data$status)
    ) |>
    dplyr::arrange(.data$status, .data$label)

readr::write_tsv(
    out,
    file.path(repo_path, "DOreports", paste0("diseases_", yr_range, ".tsv"))
)
