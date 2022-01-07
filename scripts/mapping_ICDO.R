# R wrapper script for mapping_ICDO.py
# Created: 2022-01-06


# Setup -------------------------------------------------------------------

# R libraries
library(tidyverse)
library(reticulate)
library(readxl)
library(DO.utils) # requires feature/mapping branch on/after 2022-01-07


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


# Identify mappings to full set of data
all_mappings <- icdo_data$`all terms` %>%
    dplyr::mutate(predicted = pyobo_map(Term, ns)) %>%
    unnest_mapping(
        predicted,
        prefix = ns,
        best_only = TRUE,
        warn_best_gt1 = TRUE
    )

# copy mappings to preferred, leukemia, and brain sheets


# save updated sheets to Excel file
