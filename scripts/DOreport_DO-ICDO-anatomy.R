# Script to produce DO-ICDO-anatomy DOreport

library(here)
library(tidyverse)
library(DO.utils) # >= v.0.2.4.9000


# paths -------------------------------------------------------------------
repo_path <- here::here("../Ontologies/HumanDiseaseOntology/")


# extract data ------------------------------------------------------------
axioms <- DO.utils::extract_class_axiom(repo_path)

# get import labels for anatomy readability
repo <- DO.utils::DOrepo(repo_path)
imports <- repo$doid_merged$query(
    'SELECT ?class ?label
    WHERE {
        ?class a owl:Class .
        OPTIONAL { ?class rdfs:label ?label . }
        FILTER(!CONTAINS(str(?class), "DOID"))
    }'
) %>%
    DO.utils::tidy_sparql() %>%
    dplyr::mutate(id = DO.utils::to_curie(class)) %>%
    dplyr::select(id, label)

# get DO terms in 'disease of cellular proliferation' branch
cp <- repo$doid_merged$query(
    'SELECT ?id ?label (GROUP_CONCAT(?xref; SEPARATOR="|") AS ?xrefs)
    WHERE {
        ?class a owl:Class ;
            oboInOwl:id ?id ;
            rdfs:label ?label ;
            rdfs:subClassOf* obo:DOID_14566 .

        FILTER NOT EXISTS { ?class owl:deprecated ?any . }
        FILTER(CONTAINS(str(?class), "DOID"))

        OPTIONAL {
            ?class oboInOwl:hasDbXref ?xref .
            FILTER(CONTAINS(str(?xref), "ICDO"))
        }
    } GROUP BY ?id ?label'
) %>%
    DO.utils::tidy_sparql() %>%
    dplyr::mutate(xrefs = dplyr::if_else(xrefs == "", NA_character_, xrefs))



# Custom functions --------------------------------------------------------

extract_axiom_doid <- function(x) {
    stringr::str_extract(x, "DOID_[0-9]+") %>%
        stringr::str_replace("_", ":")
}

map_by_element <- function(.list, .f, ..., .type = NULL) {
    if (!is.null(.type) && !.type %in% c("int", "chr")) {
        rlang::abort('`.type` must be NULL, "int", or "chr"')
    }
    if (is.null(.type)) {
        purrr::map(.list, function(input) purrr::map(input, .f, ...))
    } else if (.type == "int") {
        purrr::map(.list, function(input) purrr::map_int(input, .f, ...))
    } else if (.type == "chr") {
        purrr::map(.list, function(input) purrr::map_chr(input, .f, ...))
    }
}

map_by_element2 <- function(.list1, .list2, .f, ..., .type = NULL) {
    if (!is.null(.type) && !.type %in% c("int", "chr")) {
        rlang::abort('`.type` must be NULL, "int", or "chr"')
    }
    if (is.null(.type)) {
        purrr::map2(
            .list1, .list2,
            function(i1, i2) purrr::map2(i1, i2, .f, ...)
        )
    } else if (.type == "int") {
        purrr::map2(
            .list1, .list2,
            function(i1, i2) purrr::map2_int(i1, i2, .f, ...)
        )
    } else if (.type == "chr") {
        purrr::map2(
            .list1, .list2,
            function(i1, i2) purrr::map2_chr(i1, i2, .f, ...)
        )
    }
}

extract_location_axiom <- function(x) {
    doid <- extract_axiom_doid(x)
    some_obj <- stringr::str_replace_all(x, "ObjectSomeValuesFrom", "%")
    some_list <- stringr::str_extract_all(some_obj, "%\\(obo:RO_0004026[^%]+")
    open_paren <- purrr::map(some_list, stringr::str_count, pattern = "\\(")
    keep_pattern <- map_by_element(
        open_paren,
        .type = "chr",
        ~ paste0(rep(".*?\\)", .x), collapse = "")
    )
    axiom <- map_by_element2(
        some_list,
        keep_pattern,
        .type = "chr",
        ~ stringr::str_extract(.x, .y) %>%
            stringr::str_remove_all("^%\\(|\\)$")
    )
    purrr::set_names(axiom, doid)
}

extract_axiom_anatomy_id <- function(x) {
    loc_axiom <- extract_location_axiom(x)
    anatomy <- purrr::map(
        loc_axiom,
        ~ stringr::str_remove(.x, "obo:RO_0004026") %>%
            stringr::str_extract_all("obo:[^ )\"']+") %>%
            unlist() %>%
            unique() %>%
            tibble::tibble(anatomy_id = .)
    )
    anatomy <- anatomy %>%
        restore_names(loc_axiom) %>%
        dplyr::bind_rows(.id = "doid") %>%
        unique()
    anatomy
}



# Add anatomy to 'disease of cellular proliferation' data -----------------

location_axioms <- axioms %>%
    unlist() %>%
    `[`(., stringr::str_detect(., "RO_0004026"))

anatomy_df <- extract_axiom_anatomy_id(location_axioms) %>%
    dplyr::mutate(
        anatomy_id = stringr::str_replace_all(
            anatomy_id,
            c("obo:" = "", "_" = ":")
        )
    ) %>%
    dplyr::left_join(imports, by = c("anatomy_id" = "id")) %>%
    dplyr::rename(anatomy_label = label)

cp <- cp %>%
    dplyr::left_join(anatomy_df, by = c("id" = "doid"))

# write to google sheet ---------------------------------------------------

readr::write_tsv(cp, file.path(repo_path, "DOreports/DO-ICDO-anatomy.tsv"))


# EXTRA - Unused functions ------------------------------------------------

# extract_axiom_outmost_object <- function(x) {
#     stringr::str_extract(x, "Object[^(]+\\(.+\\)") %>%
#         stringr::str_remove_all("^Object[^(]+\\(|\\)$")
# }

# location_prop_outside_object <- function(x) {
#     stringr::str_detect(x, "Object[^(]+\\([^(]+obo:RO_0004026")
# }
