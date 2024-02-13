# Review SciBite OMIM mappings

library(here)
library(googlesheets4)
library(tidyverse)
library(DO.utils)



# Data paths/locations ----------------------------------------------------

# OMIM_DOID_Mapping = main data from SciBite mapping for analysis
omim_gs <- "https://docs.google.com/spreadsheets/d/1DFEqe1_jDgQNuJjgx-uIZUicNxHXI0br6DR669-F8Z8/edit#gid=771734333"

# doid-edit.owl file
de_file <- here::here("../Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl")

# MANUALLY search for 'susceptibility' at omim.org and download file to
# following location
susc_file <- here::here("data/mapping/src/omim_susceptibility.tsv")

# 'OMIM updates from MGI' google sheet
mgi_omim <- "https://docs.google.com/spreadsheets/d/1SUiFOMGeO3QJfnevpXAPFN90UmAaeoIHF7u0X5lvgc4/edit#gid=686803456"

# download OMIM entry names in phenotypic series (only if file older 7 days)
omimps_file <- here::here("data/external/omim/phenotypicSeries.txt")
if (
    !file.exists(omimps_file) ||
    Sys.time() - file.info(omimps_file)$ctime > as.difftime(7, units = "days")
) {
    omimps_file <- DO.utils::download_omim(
        "phenotypicSeries",
        dirname(omimps_file),
        api_key = keyring::key_get("omim_key")
    )
}


# Load data ---------------------------------------------------------------

# scibite omim mapping
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

omim_susc <- DO.utils::read_omim(
    susc_file,
    keep_mim = c("#", "%", "^", "none")
) %>%
    dplyr::select(omim, name.from_susc = title) %>%
    DO.utils::collapse_col(name.from_susc)

omim_ps <- DO.utils::read_omim(omimps_file, col_types = "c") %>%
    dplyr::rename(
        ps = phenotypic_series_number,
        omim = mim_number,
        name.from_ps = phenotype
    ) %>%
    dplyr::mutate(
        omim = as.character(omim),
        omim = paste0(
            "OMIM:",
            dplyr::if_else(is.na(omim), ps, omim)
        )
    ) %>%
    dplyr::select(omim, name.from_ps) %>%
    DO.utils::collapse_col(name.from_ps)

# new term names from 'OMIM updates from MGI' google sheet
omim_new <- googlesheets4::read_sheet(
    ss = mgi_omim,
    sheet = "OMIM new terms"
) %>%
    dplyr::select(omim = id, name.from_new = name) %>%
    DO.utils::collapse_col(name.from_new)

# parent child relationships in DO
dh_file <- tempfile(fileext = ".tsv")
q_hier <- '
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>

SELECT ?id (GROUP_CONCAT(DISTINCT ?ancestor;separator="|") AS ?ancestors)
    (GROUP_CONCAT(DISTINCT ?descendant;separator="|") AS ?descendants)
WHERE {
    ?class oboInOwl:id ?id ;
        rdfs:subClassOf+ ?ancestor_uri .
    ?descendant_uri rdfs:subClassOf+ ?class ;
        oboInOwl:id ?descendant .
    ?ancestor_uri oboInOwl:id ?ancestor .
    FILTER(STRSTARTS(?id, "DOID"))
    FILTER NOT EXISTS { ?class owl:deprecated true }
}
GROUP BY ?id'
qh_file <- tempfile(fileext = ".rq")
readr::write_lines(q_hier, qh_file)
DO.utils::robot("query", i = de_file, query = qh_file, dh_file)
do_hier <- readr::read_tsv(dh_file, show_col_types = FALSE) %>%
    DO.utils::tidy_sparql() %>%
    tidyr::pivot_longer(
        cols = c(ancestors, descendants),
        names_to = "relationship",
        values_to = "id2"
    ) %>%
    dplyr::mutate(
        relationship = dplyr::recode(
            relationship,
            ancestors = "narrow",
            descendants = "broad"
        )
    ) %>%
    DO.utils::lengthen_col(id2, delim = "|")


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
    dplyr::bind_rows(.id = "scibite_set") %>%
    dplyr::mutate(
        dataset = stringr::str_extract(report, "omim|doid"),
        report = stringr::str_remove(report, "(omim|doid)_")
    ) %>%
    tidyr::pivot_wider(
        names_from = report,
        values_from = n
    ) %>%
    dplyr::relocate(present, .before = absent)
googlesheets4::write_sheet(ois_full, omim_gs, "stats")


# Compare OMIM from SciBite with inventory --------------------------------

omim_inv <- purrr::map(
    omim_inv,
    ~ # identify relationship (broad/narrow) scibite -> DOID, if any
        dplyr::left_join(
            .x,
            do_hier,
            by = c("doid_scibite" = "id", "doid" = "id2")
        ) %>%
        dplyr::mutate(
            # identify exact matches
            relationship = dplyr::case_when(
                doid_scibite == doid & do_label_scibite != do_label ~ "exact (diff label)",
                doid_scibite == doid ~ "exact",
                TRUE ~ relationship
            )
        ) %>%
        dplyr::rename(do_scibite_inventory_relationship = relationship)
)

# how many unique OMIM IDs are the same
ois_compare <- purrr::map(
    omim_inv,
    ~ dplyr::mutate(
        .x,
        do_scibite_inventory_relationship = tidyr::replace_na(
            do_scibite_inventory_relationship,
            "none"
        ),
        do_scibite_inventory_relationship = factor(
            do_scibite_inventory_relationship,
            levels = c("exact", "exact (diff label)", "broad", "narrow", "none")
        )
    ) %>%
        dplyr::summarize(
            n = dplyr::n_distinct(omim),
            .by = do_scibite_inventory_relationship
        )
) %>%
    dplyr::bind_rows(.id = "scibite_set") %>%
    dplyr::arrange(
        dplyr::desc(scibite_set),
        do_scibite_inventory_relationship,
    )
googlesheets4::write_sheet(ois_compare, omim_gs, "stats-comparison")


# Add OMIM names from various sources -------------------------------------

omim_inv <- purrr::map(
    omim_inv,
    ~ dplyr::left_join(.x, omim_susc, by = "omim", relationship = "many-to-many") %>%
        dplyr::left_join(omim_ps, by = "omim", relationship = "many-to-many") %>%
        dplyr::left_join(omim_new, by = "omim", relationship = "many-to-many") %>%
        dplyr::mutate(
            omim_title = dplyr::coalesce(name.from_susc, name.from_ps, name.from_new)
        ) %>%
        dplyr::select(-dplyr::starts_with("name.from"))
)


# Write output data to new sheets -----------------------------------------

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
