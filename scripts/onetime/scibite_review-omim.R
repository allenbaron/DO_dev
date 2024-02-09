# Review SciBite OMIM mappings

library(here)
library(googlesheets4)
library(tidyverse)
library(DO.utils)


# Load data ---------------------------------------------------------------

### LOCATIONS ###
de_file <- here::here("../Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl")
omim_gs <- "https://docs.google.com/spreadsheets/d/1DFEqe1_jDgQNuJjgx-uIZUicNxHXI0br6DR669-F8Z8/edit#gid=771734333"

### LOAD FILES ###
sb_omim <- purrr::map(
    c("Exact", "Broad"),
    function(.s) {
        out <- googlesheets4::read_sheet(omim_gs, sheet = .s) %>%
            unique() %>%
            dplyr::rename(
                omim = OMIM, doid_scibite = "DOID ID",
                do_label_scibite = "DOID Name"
            ) %>%
            dplyr::mutate(
                omim = stringr::str_replace(omim, "^MIM", "OMIM"),
                doid_scibite = stringr::str_replace(doid_scibite, "^DOID:?", "DOID:"),
            )
        class(out) <- c("omim_tbl", class(out))
        out
    }
) %>%
    purrr::set_names(nm = c("exact", "broad"))


# Inventory SciBite OMIM suggestions --------------------------------------

omim_inv <- sb_omim %>%
    purrr::map(
        ~ DO.utils::inventory_omim(de_file, .x)
    )

## quick look - 98% exact OMIM already in DO, 30% broad already in DO
ois <- purrr::map(omim_inv, DO.utils::elucidate)
# --> write to sheet for reference
ois_full <- purrr::map(
    ois,
    function(.x) { class(.x) <- class(.x)[-1] ; .x}
) %>%
    dplyr::bind_rows(.id = "match")
googlesheets4::write_sheet(ois_full, omim_gs, "stats")


# Compare OMIM from SciBite with inventory --------------------------------

omim_inv <- purrr::map(
    omim_inv,
    ~ dplyr::mutate(
        .x,
        doid_same = doid_scibite == doid,
        do_label_same = doid_same & do_label_scibite == do_label
    )
)

# how many unique OMIM IDs are the same
ois_same <- purrr::map(
    omim_inv,
    ~ dplyr::summarize(
        .x,
        n = dplyr::n_distinct(omim),
        .by = c(doid_same, do_label_same)
    )
) %>%
    dplyr::bind_rows(.id = "match")
googlesheets4::write_sheet(ois_same, omim_gs, "stats-comparison")


# write to new sheets
purrr::walk2(
    omim_inv,
    names(omim_inv),
    ~ DO.utils::write_gs(
        .x,
        omim_gs,
        datestamp = .y,
        hyperlink_curie = c(omim, doid, doid_scibite)
    )
)
