# Comparison of RGD's DO IDs to true DOIDs
# J. Allen Baron
# 2022-07-15

library(here)
library(tidyverse)
library(DO.utils)

# Inputs ------------------------------------------------------------------

rgd_do_url <- "https://download.rgd.mcw.edu/data_release/ontology_obo_files/disease/RDO.owl"
rgd_do_path <- here::here("data/mapping/RDO.owl")
do_repo_path <- here::here("../Ontologies/HumanDiseaseOntology")
output <- here::here("data/mapping/RGD_DO_compare.tsv")
out_stats <- here::here("data/mapping/RGD_DO_compare-stats.tsv")

# Custom functions --------------------------------------------------------

# borrowed from DO.utils (not released yet)
unnest_cross <- function(data, cols, ...) {
    .df_out <- data
    .cols <- tidyselect::eval_select(rlang::enquo(cols), data)
    purrr::walk(
        .cols,
        function(col) {
            .df_out <<- tidyr::unnest(.df_out, {{ col }}, ...)
        }
    )
    .df_out
}

format_result <- function(data, cols = dplyr::everything(), ...) {
    .df <- data %>%
        as_tibble() %>%
        unnest_cross(cols = cols, ...) %>%
        dplyr::mutate(
            dplyr::across(
                where(is.logical),
                tidyr::replace_na,
                replace = FALSE
            )
        )

    col_list <- c("parent", "alt_id")
    cols_collapse <- col_list[col_list %in% names(.df)]
    if (length(cols_collapse) > 0) {
        DO.utils::collapse_col(.df, cols_collapse, delim = " | ")
    } else {
        .df
    }
}


# Load Data ---------------------------------------------------------------

# Load DO (if it hasn't already been loaded)
if (!exists("repo")) repo <- DO.utils::DOrepo(do_repo_path)

# Load +/- download RGD Disease Ontology (if it doesn't already exist)
if (!file.exists(rgd_do_path)) {
    download.file(url = rgd_do_url, destfile = rgd_do_path)
    rgd_owl <- DO.utils::owl_xml(rgd_do_path)
} else if (!exists("rgd_owl")) {
    rgd_owl <- DO.utils::owl_xml(rgd_do_path)
}


# SPARQL queries ----------------------------------------------------------

q_rgd <- "
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>

SELECT ?class ?label ?parent ?alt_id ?deprecated
WHERE {
    ?URI a owl:Class .

    FILTER(CONTAINS(str(?URI), 'DOID'))

    BIND(REPLACE(str(?URI), '.*DOID_', 'DOID:') AS ?class)

    OPTIONAL {
        ?URI rdfs:subClassOf ?superclass .
        BIND(REPLACE(str(?superclass), '.*DOID_', 'DOID:') AS ?parent)
    }
    OPTIONAL { ?URI rdfs:label ?label . }
    OPTIONAL { ?URI owl:deprecated ?deprecated . }
    OPTIONAL { ?URI oboInOwl:hasAlternativeId ?alt_id . }
}"

q_do <- "
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>

SELECT ?class ?label ?parent ?deprecated
WHERE {
    ?URI a owl:Class .

    FILTER(CONTAINS(str(?URI), 'DOID'))

    BIND(REPLACE(str(?URI), '.*DOID_', 'DOID:') AS ?class)

    OPTIONAL {
        ?URI rdfs:subClassOf ?superclass .
        BIND(REPLACE(str(?superclass), '.*DOID_', 'DOID:') AS ?parent)
    }
    OPTIONAL { ?URI rdfs:label ?label . }
    OPTIONAL { ?URI owl:deprecated ?deprecated . }
}"

# Compare -----------------------------------------------------------------

rgd <- rgd_owl$query(q_rgd) %>%
    format_result(keep_empty = TRUE)
do <- repo$doid$query(q_do) %>%
    format_result(keep_empty = TRUE)

vs <- dplyr::full_join(
    rgd,
    dplyr::rename_with(
        do,
        .cols = dplyr::everything(),
        .fn = ~ paste0("do_", .x)
    ),
    by = c("class" = "do_class")
) %>%
    dplyr::mutate(
        same_label = label == do_label,
        same_parent = parent == do_parent,
        same_dep = deprecated == do_deprecated,
        diff = factor(
            dplyr::case_when(
                is.na(do_label) & !deprecated ~ "RGD_only",
                is.na(do_label) & deprecated  ~ "RGD_only-dep",
                is.na(label) & !do_deprecated ~ "DO_only",
                is.na(label) & do_deprecated ~ "DO_only-dep",
                do_deprecated & !deprecated ~ "DO_dep",
                deprecated & !do_deprecated~ "RGD_dep",
                !same_label & !same_parent ~ "label_parent",
                !same_label ~ "label",
                !same_parent ~ "parent",
                TRUE ~ "none"
            ),
            levels = c("RGD_only", "DO_only", "RGD_dep", "label_parent", "label", "parent",
                       "DO_dep", "RGD_only-dep", "DO_only-dep", "none")
        )
    ) %>%
    dplyr::select(-dplyr::starts_with("same")) %>%
    dplyr::arrange(diff)

diff_stats <- dplyr::count(vs, diff)


# Save Output -------------------------------------------------------------

readr::write_tsv(vs, output)
readr::write_tsv(diff_stats, out_stats)

