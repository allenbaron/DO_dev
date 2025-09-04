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

query_tags <- function(tags, repo_path) {
    init <- git2r::repository_head(repo_path)
    on.exit(git2r::checkout(init))

    doid_path <- file.path(repo_path, "src/ontology/doid.owl")
    query <- '
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX owl: <http://www.w3.org/2002/07/owl#>
        PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>

        SELECT ?id ?label ?deprecated
        WHERE {
          ?iri oboInOwl:id ?id ;
            rdfs:label ?label .

          OPTIONAL { ?iri owl:deprecated ?deprecated }
        }
        '

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



# Comparison --------------------------------------------------------------

tags <- git2r::tags(repo_path)
rel_tags <- tags[releases]
# names(rel_tags) <- stringr::str_extract(names(rel_tags), "[0-9]{4}")
rel_nm <- names(rel_tags)

if (any(is.na(rel_nm))) {
    stop_msg <- paste0(
        "Release(s) not found in the repository: ",
        paste(releases[!releases %in% rel_nm], collapse = ", ")
    )
    stop(stop_msg)
}

list_df <- query_tags(rel_tags, repo_path)
rdf <- list_df |>
    dplyr::bind_rows(.id = "release") |>
    DO.utils::collapse_col(c("label", "deprecated", "release")) |>
    dplyr::mutate(
        change = dplyr::case_when(
            .data$release == rel_nm[2] & .data$deprecated == "FALSE" ~ "new",
            .data$release == rel_nm[2] & .data$deprecated == "TRUE" ~ "new-obsoleted",
            stringr::str_detect(.data$release, "\\|") & .data$deprecated == "FALSE|TRUE" ~ "obsoleted",
            stringr::str_detect(.data$release, "\\|") & .data$deprecated == "TRUE|FALSE" ~ "unobsoleted",
            stringr::str_detect(.data$release, "\\|") & !stringr::str_detect(.data$deprecated, "\\|") ~ "none",
            TRUE ~ "ERROR"
        )
    )

if (any(rdf$change == "ERROR")) {
    stop("There are errors in the change column. Please check the data.")
}

dplyr::count(rdf, .data$change)


# Save results ------------------------------------------------------------

# full dataset saved here
outfile <- paste0(
    "diseases_",
    paste0(stringr::str_extract(releases, "[0-9]{4}"), collapse = "-"),
    ".rda"
)
save(rdf, file = here::here("data/DO_release/2023-2025.rda"))

out <- rdf |>
    dplyr::filter(.data$change != "none") |>
    dplyr::mutate(
        label = stringr::str_remove(.data$label, ".*\\|"),
        # list new-obsoleted terms as just obsoleted
        change = stringr::str_remove(.data$change, "^new-")
    ) |>
    dplyr::select("id", "label", "change")


