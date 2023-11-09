# check UMLS git diff

library(tidyverse)
library(DO.utils)
library(googlesheets4)


# MANUAL data prep --------------------------------------------------------

# 1. replace doid-edit.owl in HumanDiseaseOntology/src/ontology with
# new_doid-edit.owl produced by UMLS update, then run:
# git diff > DEL-actual_change.txt

# 2. Add link to google sheet with manual review below, should include sheet
# named "for_manual_review" with the corresponding data and the added headers:
# "DOID", "label", "change_comment", "UMLS", "pipeline_action", etc.
gs <- "https://docs.google.com/spreadsheets/d/1clk9S0HH-4xJ0kgHLC9rAyQ554eCSmQgst82Yq2pFco/edit#gid=784851601"


# Process diff ------------------------------------------------------------

umls <- tibble::tibble(
    raw = readr::read_lines(
        "../Ontologies/HumanDiseaseOntology/src/ontology/DEL-actual_change.txt"
    )
) %>%
    # keep only xref lines that changed
    dplyr::filter(stringr::str_detect(raw, "^[+\\-][^+\\-]")) %>%
    dplyr::mutate(
        strip = stringr::str_remove_all(raw, "^[+\\-]|[+\\-]$"),
        # identify position only changes in file
        pos_only = DO.utils::all_duplicated(strip),
        # identify xref changes (excluding those on defs & synonyms)
        xref = stringr::str_detect(raw, stringr::coll("oboInOwl:hasDbXref")) &
            !stringr::str_detect(raw, "IAO_0000115|nonym")
    ) %>%
    dplyr::filter(xref, !pos_only) %>%
    dplyr::select(raw)


umls_diff <- umls %>%
    tidyr::separate_wider_regex(
        cols = raw,
        patterns = c(
            change = '^[+\\-]',
            '.*hasDbXref obo:',
            doid = 'DOID_[0-9]+',
            ' "',
            ns = '.+',
            ':',
            lui = '[^"]+',
            '"\\)'
        ),
        cols_remove = TRUE,
        too_few = "error"
    ) %>%
    dplyr::mutate(
        date = stringr::str_extract(ns, "20[0-9]{2}_[0-9]{2}_[0-9]{2}$"),
        ns = stringr::str_remove(ns, "_20[0-9]{2}_[0-9]{2}_[0-9]{2}$")
    ) %>%
    tidyr::unite(col = "stmt", doid, ns, lui, sep = "@", remove = TRUE) %>%
    DO.utils::collapse_col(.cols = c(change, date), delim = " >> ") %>%
    tidyr::separate_wider_delim(
        cols = stmt,
        delim = "@",
        names = c("DOID", "ns", "lui")
    ) %>%
    dplyr::mutate(
        change = dplyr::case_when(
            change == "+" ~ "added",
            change == "-" ~ "removed",
            stringr::str_detect(change, ">>") ~ "updated"
        ),
        DOID = DO.utils::build_hyperlink(
            x = stringr::str_remove(DOID, ".*_"),
            url = "DOID",
            txt = DOID,
            as = "gs"
        ),
        xref = DO.utils::build_hyperlink(
            x = lui,
            url = ns,
            txt = paste0(
                ns,
                dplyr::if_else(
                    is.na(date),
                    "",
                    paste0(
                        "_",
                        stringr::str_extract(date, "20[0-9]{2}_[0-9]{2}_[0-9]{2}$")
                    )
                ),
                ":",
                lui
            ),
            as = "gs"
        )
    ) %>%
    dplyr::select(DOID, xref, change, date)

# write to auto_review sheet
googlesheets4::write_sheet(umls_diff, ss = gs, sheet = "auto_review")



# Add links to manual review ----------------------------------------------

manual <- googlesheets4::read_sheet(ss = gs, sheet = "for_manual_review") %>%
    dplyr::mutate(
        DOID = DO.utils::build_hyperlink(
            x = stringr::str_remove(DOID, ".*_"),
            url = "DOID",
            txt = DOID,
            as = "gs"
        ),
        UMLS = DO.utils::build_hyperlink(
            x = stringr::str_remove(UMLS, ".*:"),
            url = "UMLS_CUI",
            txt = UMLS,
            as = "gs"
            )
        )

googlesheets4::write_sheet(data = manual, ss = gs, sheet = "for_manual_review")
