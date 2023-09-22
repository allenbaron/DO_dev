# extract all mappings (xrefs + skos) from doid-edit.owl & return as ~SSSOM (2023-03-27)

library(here)
library(tidyverse)
library(DO.utils)
library(googlesheets4)

de_path <- here::here("../Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl")
gs <- "1qAzDm9_jFe_a0gqxDpWI9ik8FeSdI-aYr7smbGnSNpk" # curated_DOID_mappings Google Sheet
gs_sheet <- "in_DOID"

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
        mapping_justification = dplyr::if_else(
            stringr::str_detect(object_id, "ORDO|OMIM|GARD|EFO|KEGG|ICDO|MEDDRA"),
            "semapv:ManualMappingCuration",
            NA_character_
        ),
        other = paste0(
            "needs_review: ",
            DO.utils::all_duplicated(paste0(subject_id, object_id)),
            ",\nreview_date: '',\n",
            "subject_obsoleted_version: '',\n",
            "object_obsoleted_version: ''"
        )
    ) %>%
    # add curation columns and re-arrange
    DO.utils::append_empty_col(
        c("subject_id", "subject_label", "predicate_id", "predicate_modifier",
          "object_id", "object_label", "mapping_justification", "confidence",
          "comment", "author_id", "mapping_date", "reviewer_id",
          "issue_tracker", "see_also", "curation_rule", "other"
        ),
        order = TRUE
    )

# save to Google Sheet with links for quicker review
mappings_gs <- mappings_keep %>%
    dplyr::mutate(
        subject_id = DO.utils::build_hyperlink(
            x = stringr::str_remove(subject_id, ".*:"),
            url = "DOID",
            txt = subject_id,
            as = "gs"
        ),
        object_id = DO.utils::build_hyperlink(
            x = stringr::str_remove(object_id, ".*:"),
            url = stringr::str_remove(object_id, "(_[0-9]{4}_[0-9]{2}_[0-9]{2})?:.*"),
            txt = object_id,
            as = "gs"
        )
    )

googlesheets4::write_sheet(mappings_gs, ss = gs, sheet = gs_sheet)


# capture redundant removed mappings
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

# redundant mappings stats (only removed for manual curation... plan to add back
# automatically during build
dplyr::count(mappings_rm, predicate_id, replaced_by)

# "slots" in SSSOM spec
sssom <- c(
    "author_id", "author_label", "comment", "confidence", "creator_id",
    "creator_label", "curation_rule", "curation_rule_text", "documentation",
    "homepage", "imports", "issue_tracker", "issue_tracker_item",
    "last_updated", "license", "literal", "literal_datatype",
    "literal_preprocessing", "literal_source", "literal_source_version",
    "local_name", "mapping_cardinality", "mapping_date",
    "mapping_justification", "mapping_provider", "mapping_registry_description",
    "mapping_registry_id", "mapping_registry_title", "mapping_set_description",
    "mapping_set_group", "mapping_set_id", "mapping_set_references",
    "mapping_set_source", "mapping_set_title", "mapping_set_version",
    "mapping_source", "mapping_tool", "mapping_tool_version", "mappings",
    "match_string", "mirror_from", "object_category", "object_id",
    "object_label", "object_match_field", "object_preprocessing",
    "object_source", "object_source_version", "object_type", "other",
    "predicate_id", "predicate_label", "predicate_modifier", "predicate_type",
    "publication_date", "registry_confidence", "reviewer_id", "reviewer_label",
    "see_also", "semantic_similarity_measure", "semantic_similarity_score",
    "similarity_measure", "similarity_score", "subject_category", "subject_id",
    "subject_label", "subject_match_field", "subject_preprocessing",
    "subject_source", "subject_source_version", "subject_type"
)

# slots desired for curation but NOT in SSSOM spec; NOTE could be in spec for
# SSSOM if formatted as key-value list in "other" column
# handle these at build time by:
#   1. Dropping any rows with values in .subject_obsoleted_version (primarily
#   retained for historical purposes)
#   2. Keeping rows with .object_obsoleted_version and annotating axiom with
#   something to indicate deprecation status; alternatively could append to
#   term comment in programmatically accessible way -> "obsoleted_xrefs: <xref1>, ..."
not_sssom <- c(
    ".review_date", ".needs_review", ".subject_obsoleted_version",
    ".object_obsoleted_version"
)
