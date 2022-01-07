# R wrapper script for mapping_ICDO.py
# Created: 2022-01-06


# Setup -------------------------------------------------------------------

# R libraries
library(tidyverse)
library(reticulate)
library(readxl)
library(DO.utils) # requires feature/mapping branch on/after 2022-01-07
library(googlesheets4)


# Mapping -----------------------------------------------------------------

# Read ICD-O file info
icdo_xl_file <- here::here("data/mapping/2021_ICDO_update_preferred terms.xlsx")
sh_names <- readxl::excel_sheets(icdo_xl_file)

icdo_data <- purrr::map(
    sh_names,
    ~ readxl::read_excel(icdo_xl_file, sheet = .x, col_types = "text")
) %>%
    purrr::set_names(sh_names)


# Set namespace for comparison to ICD-O terms
ns <- "DOID"


# Identify mappings to preferred terms
preferred_mappings <- icdo_data$`preferred terms` %>%
    dplyr::mutate(
        predicted = DO.utils::pyobo_map(`Term (NOS removed)`, ns)
    ) %>%
    DO.utils::unnest_mapping(
        predicted,
        prefix = ns,
        best_only = TRUE
    ) %>%
    # Identify exact matches (ignoring case) and add source and added to DO notes
    dplyr::mutate(
        dplyr::across(
            c(Term, `Term (NOS removed)`, term),
            ~ stringr::str_remove_all(
                stringr::str_to_lower(.x),
                "[[:punct:]]"
            ),
            .names = "simple_{.col}"
        ),
        Match_Type = dplyr::case_when(
            is.na(id) ~ NA_character_,
            simple_term == simple_Term | simple_term == `simple_Term (NOS removed)` ~
                "exact match via biomappings",
            TRUE ~ "recommended by biomappings"
        ),
        Added_to_DO = dplyr::if_else(
            Match_Type == "exact match via biomappings",
            TRUE,
            NA
        )
    ) %>%
    # prettify
    dplyr::select(-tidyselect::starts_with("simple")) %>%
    dplyr::rename(DOID = id, DO_Term = term, Match_Score = score)


# copy mappings to leukemia, and brain sheets
icdo_out <- list()
icdo_out$`preferred terms` <- preferred_mappings
icdo_out[c("leukemia", "brain")] <- purrr::map(
    icdo_data[c("leukemia", "brain ")],
    function(df) {
        dplyr::left_join(
            df,
            preferred_mappings
        ) %>%
            dplyr::select(-`Term (NOS removed)`)
    })


# count mappings
purrr::map(
    icdo_out,
    ~ count(.x, Match_Type)
) %>%
    purrr::set_names(names(icdo_out)) %>%
    dplyr::bind_rows(.id = "curation_set") %>%
    tidyr::pivot_wider(
        names_from = curation_set,
        values_from = n
    ) %>%
    dplyr::arrange(Match_Type)


# show exact match duplicates (none should exist for loading by ROBOT)
preferred_mappings %>%
    dplyr::filter(Match_Type == "exact match via biomappings") %>%
    dplyr::mutate(
        icdo_dup = DO.utils::all_duplicated(ICDO3.2),
        doid_dup = DO.utils::all_duplicated(DOID)
    ) %>%
    dplyr::filter(icdo_dup | doid_dup)

# count total & unique exact matches (same as above; just to be sure)
preferred_mappings %>%
    dplyr::filter(Match_Type == "exact match via biomappings") %>%
    summarize(
        .,
        total = nrow(.),
        icdo_uniq = n_distinct(ICDO3.2)
        , doid_uniq = n_distinct(DOID)
    )


# Save sheets to new Google Sheet file ------------------------------------

gs_out <- "https://docs.google.com/spreadsheets/d/1n-wfB6q-W6mZeuO7KvpO5i-1uc6csEUADd4vZz7ZQFU/edit?usp=sharing"

purrr::walk2(
    .x = icdo_out,
    .y = names(icdo_out),
    function(df, sh) {
        googlesheets4::write_sheet(
            data = df,
            ss = gs_out,
            sheet = sh
        )
    }
)


# Save exact matches to ROBOT template ------------------------------------

robot_gs <- "https://docs.google.com/spreadsheets/d/1dlYhGQwgfDlBwCQaR__f2oHFX5i5VQR-RQGDieKU_-c/edit?usp=sharing"
robot_sheet <- "preferred_exact_matches-20220107"

## custom function to write to sheet
write_robot_range <- function(.data, .range, .col_names) {
    googlesheets4::range_write(
        ss = robot_gs, sheet = robot_sheet, range = .range,
        data = .data,
        col_names = .col_names
    )
}

## format data for ROBOT (as google sheet; expects data.frames)
exact_mappings <- preferred_mappings %>%
    dplyr::filter(Match_Type == "exact match via biomappings") %>%
    dplyr::mutate(ICDO3.2 = paste0("ICDO:", ICDO3.2)) %>%
    dplyr::select(DOID, DO_Term, xref_ICDO = ICDO3.2, skos_ICDO = ICDO3.2,
                  ICDO_label = Term, Match_Score)

human_headers <- names(exact_mappings) %>%
    matrix(nrow = 1) %>%
    as.data.frame()
robot_headers <- c("ID", "DO_label*", "A oboInOwl:hasDbXref", "A skos:exactMatch",
                "xref_label*", "score*")
header_note <- matrix(
    "* columns for convenience -> remove before running ROBOT template"
) %>%
    as.data.frame()

## execute write
write_robot_range(human_headers, "A1", .col_names = FALSE)
write_robot_range(header_note, "G1", .col_names = FALSE)
write_robot_range(
    purrr::set_names(exact_mappings, robot_headers),
    "A2",
    .col_names = TRUE
)

