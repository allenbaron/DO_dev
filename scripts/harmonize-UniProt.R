# Harmonize DO & UniProt diseases (first pass, 2023-05-30)

library(here)
library(tidyverse)
library(janitor)
library(DO.utils)
library(googlesheets4)


# File paths --------------------------------------------------------------

# doid-edit.owl file
de_path <- here::here("../Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl")

# uniprot xref data
up_data_path <- here::here("data/mapping/uniprot-download_true_fields_id_2Cname_2Ckeywords_2Ccross_references-2023.05.23-13.38.57.88.tsv")

# curation google sheet
up_gs <- "https://docs.google.com/spreadsheets/d/1yUVActLsW_NDpMuocPN6J7AS9O2F4EyF-Ml70Je8DqY/edit#gid=0"



# Load data ---------------------------------------------------------------

# uniprot data
up_data <- readr::read_tsv(up_data_path) %>%
    janitor::clean_names()

up_tidy <- up_data %>%
    dplyr::rename(
        up_id = disease_entry_id,
        up_label = name,
        up_xref_lui = cross_reference
    ) %>%
    DO.utils::lengthen_col(up_xref_lui, delim = ",")


# DO data
de_tmp <- tempfile(fileext = ".owl")
DO.utils::robot("convert", i = de_path, o = de_tmp)

de_owl <- DO.utils::owl_xml(de_tmp)

do_data <- de_owl$query("sparql/for_mapping.rq") %>%
    DO.utils::tidy_sparql()

do_tidy <- do_data %>%
    # currently don't have prefixes to indicate namespaces for uniprot xrefs -->
    # going to identify by perfect match to LUI only
    #
    # NOTE: UniProt appears to have only MIM, MedGen, and MeSH xrefs; if all of
    # DO's xrefs are included, aberrant matches between UniProt xref LUIs will
    # be made with DO xref LUIs from Orphanet and SnomedCT are generated
    dplyr::filter(stringr::str_detect(xref, "MIM|MESH|UMLS")) %>%
    dplyr::mutate(
        xref_lui = stringr::str_remove(xref, ".*:"),
        dplyr::across(
            dplyr::contains("_type"),
            DO.utils::to_curie
        )
    ) %>%
    DO.utils::collapse_col(.cols = xref_type, delim = ",") %>%
    dplyr::rename_with(.cols = dplyr::everything(), .fn = ~ paste0("do_", .x))



# COMPARISON!!! -----------------------------------------------------------

# xref comparison only

up_xref <- up_tidy %>%
    dplyr::select(up_id, up_label, up_xref_lui) %>%
    dplyr::distinct()

do_xref <- do_tidy %>%
    dplyr::select(do_id:do_xref, do_xref_lui) %>%
    dplyr::distinct()

xref_comp <- dplyr::full_join(
    up_xref,
    do_xref,
    by = c("up_xref_lui" = "do_xref_lui"),
    na_matches = "never",
    relationship = "many-to-many"
) %>%
    dplyr::select(dplyr::contains("do_"), dplyr::everything()) %>%
    DO.utils::collapse_col(
        .cols = c(up_xref_lui, do_xref, do_xref_type),
        na.rm = TRUE
    ) %>%
    dplyr::arrange(do_id, up_id, up_xref_lui) %>%
    # remove DO deprecated diseases that don't match UniProt
    dplyr::filter(
        is.na(do_dep) | do_dep & !is.na(up_id)
    ) %>%
    # identify UniProt diseases with NO match in DO
    dplyr::group_by(up_id) %>%
    dplyr::mutate(noDO = all(is.na(do_id))) %>%
    # identify DO diseases with NO match in UniProt
    dplyr::group_by(do_id) %>%
    dplyr::mutate(noUP = all(is.na(up_id))) %>%
    dplyr::ungroup()

# split out datasets for review
xref_out <- list(
    # UniProt diseases with no DO match
    not_in_DO = xref_comp %>%
        dplyr::filter(noDO) %>%
        dplyr::select(dplyr::contains("up_")) %>%
        dplyr::rename_with(.fn = ~ stringr::str_remove(.x, "up_")),
    # DO diseases with no UniProt match
    not_in_UniProt = xref_comp %>%
        dplyr::filter(noUP) %>%
        dplyr::select(dplyr::contains("do_")) %>%
        dplyr::rename_with(.fn = ~ stringr::str_remove(.x, "do_")) %>%
        dplyr::select(id, label, xref, xref_type),
    # diseases with xref matches
    in_both = xref_comp %>%
        dplyr::filter(!noDO, !noUP) %>%
        dplyr::select(-dplyr::starts_with("no"))
)

# save to curation Google Sheet
purrr::walk2(
    xref_out,
    names(xref_out),
    ~ googlesheets4::write_sheet(
        data = .x,
        ss = up_gs,
        sheet = paste(.y, DO.utils::today_datestamp(), sep = "-")
    )
)


# EXTRA -------------------------------------------------------------------

# # count xrefs from DO in each namespace
# xref_comp_final %>%
#     dplyr::filter(!is.na(up_id)) %>%
#     .$do_xref %>%
#     stringr::str_split(coll("|")) %>%
#     purrr::map(~ stringr::str_remove(.x, ":.*")) %>%
#     unlist() %>%
#     table()

# # join approach returning only matches between DO & UniProt
# xref_inner <- dplyr::inner_join(
#     up_xref,
#     do_xref,
#     by = c("up_xref_lui" = "do_xref_lui"),
#     na_matches = "never",
#     relationship = "many-to-many"
# ) %>%
#     dplyr::select(
#         dplyr::contains("up_"),
#         do_xref,
#         dplyr::everything(),
#         -do_xref_prefix
#     ) %>%
#     DO.utils::collapse_col(.cols = c(up_xref_lui, do_xref, do_xref_type))

# # join approach for finding differences
# xref_noDO <- dplyr::anti_join(
#     up_xref,
#     do_xref,
#     by = c("up_xref_lui" = "do_xref_lui")
# )
