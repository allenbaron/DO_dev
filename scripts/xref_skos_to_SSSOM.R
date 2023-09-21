# extract all mappings (xrefs + skos) from doid-edit.owl & return as ~SSSOM (2023-03-27)

library(here)
library(tidyverse)
library(DO.utils)
library(googlesheets4)

de_path <- here::here("../Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl")
gs <- "1cKEXlbdrNIT5KgML3H5lRPdgx4p5Lc1yWGRuKTmViZg" # curated_DOID_mappings Google Sheet
gs_sheet <- "all_mappings"

tmp_file <- tempfile(fileext = ".csv")

system2(
    "robot",
    args = c(
    "export -i",
    de_path,
    "--header 'ID|LABEL|oboInOwl:hasDbXref|skos:exactMatch|skos:broadMatch|skos:narrowMatch|skos:relatedMatch|skos:closeMatch'",
    "--export",
    tmp_file
    )
)

export_res <- readr::read_csv(
    tmp_file,
    col_types = readr::cols(.default = readr::col_character())
)

mappings <- export_res %>%
    dplyr::rename(subject_id = ID, subject_label = LABEL) %>%
    tidyr::pivot_longer(
        cols = tidyselect::contains(":"),
        names_to = "predicate_id",
        values_to = "object_id"
    ) %>%
    dplyr::filter(!is.na(object_id)) %>%
    DO.utils::lengthen_col(cols = object_id, delim = "|")

# will re-create xrefs if oboInOwl:hasDbXref, skos:exactMatch, or skos:closeMatch
# remove xref if has skos:(exact|close)Match
mappings_keep <- mappings %>%
    dplyr::group_by(subject_id, object_id) %>%
    dplyr::filter(
        stringr::str_detect(predicate_id, "skos") |
        (stringr::str_detect(predicate_id, "Xref") & !any(stringr::str_detect(predicate_id, "exact|close")))
    ) %>%
    dplyr::ungroup() %>%
    # label problematic mappings (xref + skos:(broad/narrow)Match for review
    #   --> identified as simple duplicates (shouldn't be any)
    dplyr::mutate(
        subject_id = DO.utils::build_hyperlink(
        needs_review = DO.utils::all_duplicated(paste0(subject_id, object_id))
        )
    ) %>%
    # add curation columns and re-arrange
    DO.utils::append_empty_col(
        c("subject_id", "subject_label", "predicate_id", "object_id",
          "object_label", "mapping_justification", "contributor", "date",
          "needs_review", "notes", "review_doc"),
        order = TRUE
    )

# save to
googlesheets4::write_sheet(mappings_keep, ss = gs, sheet = gs_sheet)

mappings_rm <- mappings %>%
    dplyr::anti_join(
        mappings_keep,
        by = c("subject_id", "predicate_id", "object_id")
    ) %>%
    dplyr::arrange(subject_id, object_id, predicate_id) %>%
    dplyr::left_join(
        dplyr::select(mappings_keep, subject_id, object_id, replaced_by = predicate_id),
        by = c("subject_id", "object_id")
    )

# redundant mappings stats (only removed for manual curation... added back
# automatically during build
dplyr::count(mappings_rm, predicate_id, replaced_by)

