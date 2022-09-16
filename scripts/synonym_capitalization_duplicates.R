# identify synonyms that differ only by capitalization

library(here)
library(tidyverse)
library(DO.utils)


# Establish file names ----------------------------------------------------

output_dir <- here::here("data/DO")
output_names <- c(
    do_same = "syn_cap_dup-same_id", # comparisons within same DOID
    do_diff = "syn_cap_dup-diff_id", # comparison across DOIDs
    do_imp = "syn_cap_dup-DO_import" # comparison between DOIDs & imports
)
comparison_output <- paste0(
    output_names,
    "-",
    DO.utils::today_datestamp(),
    ".csv"
) %>%
    purrr::map_chr(~ file.path(output_dir, .x)) %>%
    purrr::set_names(nm = names(output_names))
syn_query <- here::here("sparql/id_label_synonym.rq")

# Create files if analysis not run already today --------------------------

# DO self comparison files - within ID, across IDs (diff_id), with imports
missing_file <- !file.exists(comparison_output)
names(missing_file) <- names(output_names)

if (any(missing_file)) {
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
    if (missing_file["do_same"]) {
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
            dplyr::arrange(id, dplyr::desc(remove)) %>%
            dplyr::select(id:label, remove, dplyr::everything())

        readr::write_csv(dup_do_same, comparison_output["do_same"])
    }

    # Cross DOID match file
    if (missing_file["do_diff"]) {
        dup_do_diff <- dup_do %>%
            dplyr::group_by(txt_std) %>%
            dplyr::filter(dplyr::n_distinct(id) > 1) %>%
            dplyr::ungroup()
        readr::write_csv(dup_do_diff, comparison_output["do_diff"])
    }

    # DO-import match file - hardest to handle
    if (missing_file["do_imp"]) {
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

        readr::write_csv(dup_do_imp, comparison_output["do_imp"])
    }

    rlang::inform(
        c("Review new synonym capitalization duplicate file(s):",
          setNames(
              comparison_output[missing_file],
              rep("i", sum(missing_file))
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
