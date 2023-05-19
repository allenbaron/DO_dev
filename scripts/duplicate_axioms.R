# identify axioms on same disease that differ only by grouping

library(here)
library(tidyverse)
library(DO.utils) # >= 0.2.7

# extract EQ & subClass axioms
tmp_export <- tempfile(fileext = ".tsv")
DO.utils::robot(
    "export",
    i = here::here("../Ontologies/HumanDiseaseOntology/src/ontology/doid.owl"),
    header = '"ID|LABEL|Equivalent Class|SubClass Of [ANON]"',
    export = tmp_export
)

do_export <- readr::read_tsv(tmp_export, col_types = "c") %>%
    dplyr::rename_with(
        .fn = ~ stringr::str_remove(.x, " [^ ]+ANON.*"),
        .cols = tidyselect::matches("[Cc]lass")
    ) %>%
    janitor::clean_names(case = "small_camel")

axiom <- do_export %>%
    dplyr::mutate(
        dplyr::across(
            .cols = c(equivalentClass, subClassOf),
            .fns = ~ stringr::str_remove_all(.x, "[[:punct:]]"),
            .names = "{.col}"
        )
    ) %>%
    tidyr::pivot_longer(
        cols = c(equivalentClass, subClassOf),
        names_to = "type",
        values_to = "axiom",
        values_drop_na = TRUE
    ) %>%
    DO.utils::lengthen_col(cols = axiom)

axiom_dup <- axiom %>%
    dplyr::group_by(id, type) %>%
    dplyr::filter(duplicated(axiom)) %>%
    dplyr::ungroup()
