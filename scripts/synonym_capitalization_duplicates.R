# identify synonyms that differ only by capitalization

library(googlesheets4)
library(here)
library(tidyverse)
library(DO.utils)


# Establish file names ----------------------------------------------------

# DO_synonym_capitalization_dups google sheet
gs <- googlesheets4::gs4_get("1oIMi55zpShoWVyS509SHQCRZ-aasM-CO80adPtzbNxA")
sheet_ids <- c(
    # order matters!!
    "do_same", # comparisons within same DOID
    "do_diff", # comparison across DOIDs
    "do_imp" # comparison between DOIDs & imports
)
sheet_names <- paste0(
    sheet_ids,
    "-",
    DO.utils::today_datestamp()
)

syn_query <- here::here("sparql/id_label_synonym.rq")

# Create files if analysis not run already today --------------------------

# avoid overwrite
sheets_in_gs <- googlesheets4::sheet_names(gs)
missing_sheet <- !sheet_names %in% sheets_in_gs

if (any(missing_sheet)) {
    r <- DO.utils::DOrepo("../Ontologies/HumanDiseaseOntology")

    syn <- r$doid_merged$query(syn_query) %>%
        tibble::as_tibble() %>%
        DO.utils::unnest_cross(where(is.list), keep_empty = TRUE)

    syn_compare <- syn %>%
        tidyr::pivot_wider(
            names_from = syn_type,
            values_from = synonym,
            values_fn = list
        ) %>%
        DO.utils::unnest_cross(where(is.list), keep_empty = TRUE) %>%
        tidyr::pivot_longer(
            cols = c(label, dplyr::contains("Synonym")),
            names_to = "txt_type",
            values_to = "txt",
            values_drop_na = TRUE
        ) %>%
        dplyr::mutate(
            txt_std = stringr::str_squish(stringr::str_to_lower(txt))
        ) %>%
        dplyr::left_join(
            dplyr::select(syn, id, label),
            by = "id"
        ) %>%
        dplyr::select(id, label, dplyr::everything()) %>%
        unique()

    do_only <- syn_compare %>%
        dplyr::filter(stringr::str_detect(id, "DOID"))

    dup_do <- do_only %>%
        dplyr::filter(DO.utils::all_duplicated(txt_std)) %>%
        dplyr::arrange(txt_std, id, dplyr::desc(txt_type))

    # Within DOID match file - easiest to handle
    if (missing_sheet[1]) {
        dup_do_same <- dup_do %>%
            dplyr::group_by(txt_std) %>%
            dplyr::filter(dplyr::n_distinct(id) == 1) %>%
            dplyr::group_by(id, txt_std) %>%
            # add recommendation on degree of certainty for removal
            dplyr::mutate(
                remove = dplyr::case_when(
                    txt_type == "label" ~ "no",
                    any(txt_type == "label") & txt_type != "label" ~ "yes",
                    any(txt_type == "hasExactSynonym") &
                            txt_type != "hasExactSynonym" ~ "yes",
                    any(txt_type != "hasExactSynonym") &
                        txt_type == "hasExactSynonym" ~ "no",
                    dplyr::n_distinct(txt_type) > 1 &
                        txt != txt_std ~ "preferred - type diff",
                    txt != txt_std ~ "preferred",
                    TRUE ~ "maybe"
                )
            ) %>%
            dplyr::ungroup() %>%
            # format for easier review:
            #   - drop labels from dataset (those won't be removed)
            #   - order rows/columns for ease
            dplyr::filter(txt_type != "label") %>%
            dplyr::arrange(id, txt_std, dplyr::desc(remove)) %>%
            dplyr::select(id:label, remove, dplyr::everything())

        googlesheets4::write_sheet(dup_do_same, gs, sheet_names[1])
    }

    # Cross DOID match file
    if (missing_sheet[2]) {
        dup_do_diff <- dup_do %>%
            dplyr::group_by(txt_std) %>%
            dplyr::filter(dplyr::n_distinct(id) > 1) %>%
            dplyr::ungroup()
        googlesheets4::write_sheet(dup_do_diff, gs, sheet_names[2])
    }

    # DO-import match file - hardest to handle
    if (missing_sheet[3]) {
        do_info <- do_only %>%
            dplyr::select(id, label, txt_std, txt_type) %>%
            dplyr::rename_with(.cols = c(id, label, txt_type), ~ paste0("do_", .x))

        dup_do_imp <- syn_compare %>%
            dplyr::filter(DO.utils::all_duplicated(txt_std)) %>%
            dplyr::arrange(txt_std, id, dplyr::desc(txt_type)) %>%
            dplyr::group_by(txt_std) %>%
            dplyr::filter(
                any(stringr::str_detect(id, "DOID")),
                !stringr::str_detect(id, "DOID")
            ) %>%
            dplyr::ungroup() %>%
            dplyr::left_join(do_info, by = "txt_std") %>%
            dplyr::select(do_id:do_txt_type, txt_std, id, txt, txt_type) %>%
            dplyr::arrange(do_id, id)

        googlesheets4::write_sheet(dup_do_imp, gs, sheet_names[3])
    }

    rlang::inform(
        c(
            paste0(
                "Review new synonym capitalization duplicate sheet(s) in ",
                gs$name,
                " at\n",
                gs$spreadsheet_url
            ),
            setNames(
                sheet_names[missing_sheet],
                rep("i", sum(missing_sheet))
            )
        )
    )
}



# EXTRA -------------------------------------------------------------------

stop("DONE!")

# if automating update of doid-edit.owl ... decided against it since manual
#   review is necessary anyway
do_same_continue <- function() {
    answer <- NULL
    while (!answer %in% c("yes", "no")) {
        answer <- readline(
            paste0(
                strwrap(
                    paste0(
                        "Has ",
                        comparison_output["do_same"],
                        " been reviewed & prepared for use in processing doid-edit.owl? yes/no"
                    ),
                    exdent = 4
                ),
                collapse = "\n"
            )
        )
    }
    answer
}

same_continue <- do_same_continue()
if (same_continue) {
    # edit doid-edit.owl automatically here
}
