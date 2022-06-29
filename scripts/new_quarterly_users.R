# Get new ontologies, resources, methodologies published in last quarter for
# Buzz Newsletter
# Author: J. Allen Baron
# 2022-05-23


# NOTE: Should be run after updating cited_by sheet and curating new users.

library(tidyverse)
library(googlesheets4)
library(DO.utils)
library(lubridate)


# Get Data ----------------------------------------------------------------

uses_gs <- "1wG-d0wt-9YbwhQTaelxqRzbm4qnu11WDM2rv3THy5mY"

citedby <- googlesheets4::read_sheet(
    uses_gs,
    "cited_by",
    range = "A:T",
    col_types = "c"
) %>%
    tidyr::separate(
        user_ID,
        into = paste0(
            "user_ID-",
            seq(max(stringr::str_detect(.$user_ID, "|"), na.rm = TRUE) + 1)
        ),
        sep = "\\|",
        fill = "right",
        remove = TRUE
    ) %>%
    tidyr::pivot_longer(
        cols = dplyr::starts_with("user_ID"),
        names_to = ".delete",
        names_pattern = "^[^0-9]*([0-9]+)$",
        values_to = "user_ID"
    ) %>%
    dplyr::filter(.delete == "1" | !is.na(user_ID)) %>%
    dplyr::select(-.delete) %>%
    dplyr::mutate(
        user_ID = readr::parse_integer(user_ID),
        added = readr::parse_datetime(added),
        pub_date = readr::parse_date(pub_date)
    )

users <- googlesheets4::read_sheet(
    uses_gs,
    "DO_website_user_list",
    range = "A:G",
    col_types = "c"
) %>%
    dplyr::mutate(
        added = readr::parse_logical(added),
        user_ID = readr::parse_integer(user_ID)
    )


# Identify new additions this quarter -------------------------------------

# Ontologies, Resources, & Methodologies
new_res_pubs <- citedby %>%
    dplyr::filter(
        stringr::str_detect(review, "added"),
        # added to cited_by list in the last 3 months
        added >= lubridate::today() - lubridate::dmonths(3)
    )

# QC #1 - new_res_pubs must all have a user_ID
dplyr::filter(new_res_pubs, is.na(user_ID))

new_resources <- users %>%
    dplyr::filter(user_ID %in% new_res_pubs$user_ID)

# QC #2
# A. new_resources were all copied to users list (same number as ID'd by pubs)
nrow(new_resources) == nrow(new_res_pubs)
# B. new_resources all have matching user_ID to new_res_pubs
dplyr::filter(new_res_pubs, !user_ID %in% new_resources$user_ID)
# C. new_resources full names all match new_res_pubs
new_resources %>%
    dplyr::mutate(
        nm = dplyr::if_else(is.na(full_name), name, full_name)
    ) %>%
    dplyr::filter(!nm %in% new_res_pubs$tool_name)


new_resources <- dplyr::left_join(
    new_resources,
    dplyr::select(new_res_pubs, -tool_name, -url, added_to_cb = added),
    by = "user_ID"
)

# count by type of resource
dplyr::count(new_resources, type)

# Primary Research
new_primary <- citedby %>%
    dplyr::filter(
        pub_date >= lubridate::today() - lubridate::dmonths(3),
        is.na(tool_name) & is.na(user_ID),
        !stringr::str_detect(pub_type, "[Rr]eview")
    )
