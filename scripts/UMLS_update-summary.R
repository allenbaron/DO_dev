# Check UMLS build results and update doid-edit.owl file

# OUTPUT: A `manual_review` file will be created in `umls_build_dir` as a new
# Google Sheet.

# NOTES:
# - subject_source_version & mapping_date will be date of commit from which
# doid-edit.owl was used (since between releases): `commit_date`


# MANUALLY specify data ---------------------------------------------------

umls_version <- "2025AB"

# specify latest UMLS_build dir in Google Drive
umls_build_dir <- "https://drive.google.com/drive/folders/1bNK5iBzS0_5ZXCuiT0Zpjrsvewpnd4jq"

# local DO repo
do_repo <- "~/Documents/Ontologies/HumanDiseaseOntology"

# commit used for UMLS update
commit <- "8a80588764f4c998a0223b30b29858ef00cefbdb"

# DOID_SSSOM curated Google Sheet -- for comparison
sssom_gs <- "https://docs.google.com/spreadsheets/d/1qAzDm9_jFe_a0gqxDpWI9ik8FeSdI-aYr7smbGnSNpk/"
sssom_sheet <- "mapping_curated"



######################## PROGRAMMATIC SETUP ##############################

library(DO.utils)
library(gert)
library(googledrive)
library(googlesheets4)
library(tidyverse)

# expected SSSOM columns in output
# used internally by various functions
sssom_cols <- c(
    "subject_id", "subject_label", "predicate_id", "predicate_modifier",
    "object_id", "object_label", "mapping_justification", "curation_rule",
    "comment", "author_id", "mapping_date",
    "subject_source_version", "object_source_version",
    "reviewer_id"
)


# `parse_*()` functions ---------------------------------------------------

#' @param x character vector of lines from new_cuis_from_xrefs.txt report
#' @param sssom tibble of existing SSSOM mappings, prior to incorporation of
#' UMLS updater changes
parse_new_cui <- function(x, sssom) {
    parsed <- stringr::str_match(
        x,
        "Found new CUI \\((?<cui>C[0-9]+)\\) using (?<source>[^ ]+) code \\((?<lui>[^ ]+)\\) on this term: (?<doid>DOID_[0-9]+)"
    ) |>
        tibble::as_tibble(.name_repair = name_by_pos)

    checked <- check_parsing(x, parsed)

    out <- checked |>
        dplyr::mutate(
            doid = stringr::str_replace(.data$doid, "_", ":"),
            cui = stringr::str_replace(
                .data$cui,
                "^(UMLS_CUI:)?",
                "UMLS_CUI:"
            ),
        ) |>
        tidyr::unite(
            col = "source_xref",
            "source",
            "lui",
            sep = ":",
            remove = FALSE
        ) |>
        dplyr::select(-"lui") |>
        # add DO labels
        dplyr::left_join(
            dplyr::select(
                sssom,
                doid = "subject_id",
                dplyr::starts_with("subject"),
            ),
            by = "doid",
            relationship = "many-to-many"
        ) |>
        # add SSSOM metadata
        dplyr::mutate(
            predicate_id = "oboInOwl:hasDbXref",
            mapping_justification = "semapv:MappingChaining",
            curation_rule = "map1",
            comment	= paste0("mapped from", .data$source_xref),
            author_id = "UMLS updater",
            mapping_date = commit_date,
            subject_source_version = commit_date,
            object_source_version = umls_version
        ) |>
        # reformat to SSSOM & add missing cols
        dplyr::rename(subject_id = "doid", object_id = "cui") |>
        add_sssom_cols() |>
        dplyr::relocate(dplyr::all_of(sssom_cols), .before = 1) |>
        dplyr::relocate(
            "source", "source_xref",
            .before = "mapping_justification"
        )

    # check if new_cui's already existed
    out_warn <- dplyr::mutate(
        out,
        not_new = paste0(.data$subject_id, .data$object_id) %in%
            paste0(sssom$subject_id, sssom$object_id)
    )
    if (any(out_warn$not_new)) {
        warning(
            "Some 'new' CUIs already existed in doid-edit.owl mappings",
            "\n`not_new` column (boolean) included in output to identify these lines"
        )
        return(out_warn)
    }

    out
}


# Can't read as CSV because of unquoted labels with commas --> regex parse
# 3 patterns:
# 1. DOID,label (possible commas, no "),No entry in the UMLS for CUI,UMLS_CUI:<cui>, removing this xref
# 2. DOID,label (possible commas, no "),Unable to find any UMLS CUIs for this term.
# 3. DOID,label (possible commas, no "),Multiple CUIs found for <source>,<lui>,Found CUI,<cui>
#.    --> order of this one is different with action BEFORE cui, requires different regex

#' @param x character vector of lines from for_manual_review.txt report
#' @param new_cui tibble of new CUIs data from `parse_new_cui()`
#' @param sssom tibble of existing SSSOM mappings, prior to incorporation of
#' UMLS updater changes
parse_man_rev <- function(x, new_cui, sssom) {
    x_no_umls <- stringr::str_remove(x, "(CUI|UMLS).*")
    # create recurring comma regex pattern for each individual label
    label_comma <- stringr::str_count(x_no_umls, ",") - 2
    pattern_start <- glue::glue(
        "^(?<doid>DOID[_:][0-9]+),(?:(?:[^,]+,){@label_comma@}[^,]+),",
        .open = "@",
        .close = "@"
    )
    # final patterns differ for 'found' (out of order = single pattern impossible)
    patterns <- dplyr::if_else(
        stringr::str_detect(x, "Found CUI"),
        paste0(pattern_start, "(?<comment>[^,]+) for (?<source>[^,]+),(?<lui>[^,]+),(?<action>[^,]+),(?<cui>.+)$"),
        paste0(pattern_start, "(?<comment>[^,]+)(?:,(?<cui>UMLS_CUI:[^,]+), *(?<action>.+))?$")
    )

    parsed <- purrr::map2(
        x,
        patterns,
        ~ tibble::as_tibble(
            stringr::str_match(.x, .y),
            .name_repair = name_by_pos
        )
    ) |>
        dplyr::bind_rows()

    checked <- check_parsing(x, parsed, c("doid", "comment"))

    out <- checked |>
        dplyr::mutate(
            doid = stringr::str_replace(.data$doid, "_", ":"),
            cui = stringr::str_replace(
                .data$cui,
                "^(UMLS_CUI:)?",
                "UMLS_CUI:"
            ),
        ) |>
        # reformat to SSSOM & add relevant info
        dplyr::rename(
            subject_id = "doid", object_id = "cui", updater_comment = "comment",
            updater_action = "action"
        ) |>
        # drop values listed in new_cui (avoid duplication, easier to review there)
        dplyr::anti_join(
            new_cui,
            by = c("subject_id", "object_id")
        )

    # drop source & lui if all empty after new_cui anti_join, otherwise
    # process if they exist
    opt_cols <- c("source", "lui")
    if (all(opt_cols %in% names(out))) {
        all_na <- purrr::map_lgl(
            out[, opt_cols],
            ~ all(is.na(.x))
        )
        if (any(all_na)) {
            out <- dplyr::select(out, -dplyr::all_of(names(all_na)[all_na]))
        } else {
            out <- out |>
                tidyr::unite(
                    col = "source_xref",
                    "source",
                    "lui",
                    sep = ":",
                    remove = FALSE,
                    na.rm = TRUE
                ) |>
                dplyr::mutate(source_xref = dplyr::na_if(.data$source_xref, "")) |>
                dplyr::select(-"lui")
        }
    }

    # add existing mapping info from doid-edit.owl SSSOM
    sssom_label <- dplyr::select(sssom, "subject_id", "subject_label") |>
        dplyr::distinct()
    label_recode <- purrr::set_names(
        sssom_label$subject_label,
        sssom_label$subject_id
    )
    out <- dplyr::left_join(
        out,
        sssom,
        by = c("subject_id", "object_id"),
        relationship = "one-to-many"
    ) |>
        # add any missing DOID labels
        dplyr::mutate(
            subject_label = dplyr::if_else(
                is.na(.data$subject_label),
                dplyr::recode(.data$subject_id, !!!label_recode),
                .data$subject_label
            )
        ) |>
        # !!NOTE: UMLS updater does NOT currently update SKOS mappings
        #   TEMPORARY ACTION: drop updater_action from any SKOS
        dplyr::mutate(
            updater_action = dplyr::if_else(
                .data$predicate_id == "oboInOwl:hasDbXref",
                .data$updater_action,
                NA_character_
            )
        ) |>
        # add missing sssom cols & re-order
        DO.utils::append_empty_col(sssom_cols) |>
        dplyr::relocate(dplyr::all_of(sssom_cols), .before = 1) |>
        dplyr::relocate(dplyr::starts_with("updater"), .after = "comment") |>
        tibble::add_column(
            subject_obsoleted_version = NA_character_,
            object_obsoleted_version = NA_character_,
            .after = "object_source_version"
        ) |>
        # define groups for curation based on updater comments
        dplyr::mutate(
            group = dplyr::case_when(
                stringr::str_detect(.data$updater_comment, "No entry") ~
                    "cui_removed",
                stringr::str_detect(.data$updater_comment, "Unable to find") ~
                    "no_cui_found",
                TRUE ~ "other"
            ),
            .before = 1
        )

    out
}


#' @param repo_path path to DO git repo (for git diff); MUST BE ON UMLS UPDATE BRANCH
#' @param new_cui tibble of new CUIs data from `parse_new_cui()`
#' @param man_rev tibble of manual review data from `parse_man_rev()`
#' @param new_sssom tibble of all mappings in new doid-edit.owl SSSOM, AFTER
#' UMLS updater changes incorporated
parse_de_updated_diff <- function(repo_path, new_cui, man_rev, new_sssom) {
    all_diff_lines <- gert::git_diff_patch(repo = repo_path) |>
        stringr::str_split("\n") |>
        unlist()

    diff_relevant <- tibble::tibble(raw = all_diff_lines) |>
        # keep only xref lines that changed
        dplyr::filter(stringr::str_detect(.data$raw, "^[+\\-][^+\\-]")) |>
        dplyr::mutate(
            strip = stringr::str_remove_all(.data$raw, "^[+\\-]|[+\\-]$"),
            # identify position only changes in file
            pos_only = DO.utils::all_duplicated(.data$strip),
            # identify xref changes (excluding those on defs & synonyms)
            xref = stringr::str_detect(.data$raw, "oboInOwl:hasDbXref") &
                !stringr::str_detect(.data$raw, "IAO_0000115|nonym")
        ) |>
        dplyr::filter(.data$xref, !.data$pos_only) |>
        dplyr::select("raw") |>
        unlist(use.names = FALSE)

    out <- tibble::tibble(raw = diff_relevant) |>
        tidyr::separate_wider_regex(
            cols = "raw",
            patterns = c(
                change = '^[+\\-]',
                '.*hasDbXref obo:',
                subject_id = 'DOID_[0-9]+',
                ' "',
                source = '.+',
                ':',
                lui = '[^"]+',
                '"\\)'
            ),
            cols_remove = TRUE,
            too_few = "error"
        ) |>
        dplyr::mutate(
            subject_id = stringr::str_replace(.data$subject_id, "_", ":"),
            date = stringr::str_extract(
                .data$source,
                "20[0-9]{2}_[0-9]{2}_[0-9]{2}$"
            ),
            source = stringr::str_remove(
                .data$source,
                "_20[0-9]{2}_[0-9]{2}_[0-9]{2}$"
            )
        ) |>
        tidyr::unite(
            col = "stmt",
            "subject_id",
            "source",
            "lui",
            sep = "@",
            remove = TRUE
        ) |>
        DO.utils::collapse_col(.cols = c(change, date), delim = " >> ") |>
        tidyr::separate_wider_delim(
            cols = "stmt",
            delim = "@",
            names = c("subject_id", "source", "lui")
        ) |>
        dplyr::mutate(
            subject_id = stringr::str_replace(.data$subject_id, "_", ":"),
            change = dplyr::case_when(
                .data$change == "+" ~ "added",
                .data$change == "-" ~ "removed",
                stringr::str_detect(.data$change, ">>") ~ "updated"
            ),
            object_source_version_PRIOR = stringr::str_replace_all(
                .data$date,
                c(" >> .+" = "", "_" = "-")
            ),
            date = NULL
        ) |>
        tidyr::unite(
            col = "object_id",
            "source",
            "lui",
            sep = ":",
            remove = TRUE
        ) |>
        dplyr::left_join(
            new_sssom,
            by = c("subject_id", "object_id"),
            relationship = "one-to-many"
        ) |>
        add_sssom_cols() |>
        dplyr::relocate(dplyr::all_of(sssom_cols), .after = "change") |>
        dplyr::relocate(
            "object_source_version_PRIOR",
            .before = "object_source_version"
        ) |>
        # drop mapping records already_capture in new_cui or man_rev
        # !!NOTE: not 100% sure this is desirable
        dplyr::anti_join(new_cui, by = c("subject_id", "object_id")) |>
        dplyr::anti_join(man_rev, by = c("subject_id", "object_id"))
}


# `parse_*()` function helpers --------------------------------------------

#' Validate Regex-Parsing Success
#'
#' Used by regex_parsing functions (`parse_new_cui()` & `parse_man_rev()`)
#'
#' @param unparsed character vector of original lines (for error messages)
#' @param parsed tibble of parsed lines (with col1 = full_match, etc.)
#' @param warn_col column(s) to check for partial parsing (default = all except
#' col1)
#'
#' @keywords internal
check_parsing <- function(unparsed, parsed, warn_col = -"col1") {
    failed <- is.na(parsed$col1)
    if (any(failed)) {
        n <- sum(failed)
        err_list <- purrr::set_names(
            paste0("line ", which(failed), ": ", unparsed[failed]),
            rep("x", n)
        )
        msg <- glue::glue("Failed to parse {n} lines")
        rlang::abort(c(msg, err_list))
    }

    issue <- parsed |>
        dplyr::rowwise() |>
        dplyr::mutate(partial = any(is.na(dplyr::c_across({{ warn_col }})))) |>
        dplyr::ungroup()
    if (any(issue$partial)) {
        n <- sum(issue$partial)
        msg <- glue::glue(
            "Parsed successfully but {n} lines may be missing data",
            "\n`partial` column (boolean) included in output to identify these lines"
        )
        warning(msg)
        out <- issue
    } else {
        out <- dplyr::select(parsed, -"col1")
    }

    out
}


#' Assign Column Names by Position
#'
#' Assigns *missing* column names by position (e.g. col1, col2, etc.), when used
#' as `.name_repair` within `tibble::as_tibble()`.
#'
#' @param col_nm character vector of column names (some may be NA or empty)
#'
#' @keywords internal
name_by_pos <- function(col_nm) {
    if (is.null(col_nm)) {
        out <- paste0("col",  seq_along(col_nm))
    } else {
        out <- col_nm
        missing <- stringr::str_trim(col_nm) == ""
        out[missing] <- if (any(missing)) paste0("col", which(missing))
    }

    out
}


#' Add SSSOM columns
#'
#' Adds any missing SSSOM cols from `sssom_cols`.
#'
#' @param .df tibble to add missing SSSOM cols to
#'
#' @keywords internal
add_sssom_cols <- function(.df) {
    missing <- setdiff(sssom_cols, names(.df))
    DO.utils::append_empty_col(.df, missing)
}


#' Write UMLS Update Data to Google Sheet
#'
#' Formats CURIEs as hyperlinks and splits data into sheets to aid review of
#' UMLS updater changes.
#'
#' @param ss Google Sheet ID or URL for Google Sheet
#' @param ... One or more tibbles of UMLS update data. Names are not necessary
#' but can be included for identification purposes. These must be one of:
#' `new_cui`, `man_rev`, or `de_diff`. When absent, data will be identified by
#' the presence of absence of columns as follows:
#'
#' * `new_cui`: determined by absence of `group` and `change` columns
#'
#' * `man_rev`: determined by inclusion of `group` column
#'
#' * `de_diff`: determined by inclusion of `change` column
#'
#' @param debug boolean for whether to skip writing to Google Sheet
#' (default = FALSE)
#'
#' @section Reason for `...`:
#' `...` is used instead of specific, named arguments for each data element to
#' allow for re-writing individual sheets, if needed.
#'
#' @returns Data as a list of tibbles with corresponding sheet names, invisibly.
write_umls_to_gs <- function(ss, ..., debug = FALSE) {
    # identify data & identify as needed
    df_list <- list(...)
    stopifnot(
        "Too many inputs to `...`. Only 3 possible inputs are accepted" =
            length(df_list) <= 3,
        "`...` must not be empty" = length(df_list) > 0,
        "All inputs to `...` must be data.frames" =
            all(purrr::map_lgl(df_list, is.data.frame))
    )
    df_nm <- names(df_list)
    df_nm <- if (is.null(df_nm)) rep("", length(df_list))
    if (any(df_nm == "")) {
        df_nm <- purrr::map2_chr(
            df_list,
            df_nm,
            ~ if (.y == "") {
                col_nm <- names(.x)
                dplyr::case_when(
                    "group" %in% col_nm ~ "man_rev",
                    "change" %in% col_nm ~ "de_diff",
                    TRUE ~ "new_cui"
                )
            } else {
                .y
            }
        )
    }
    stopifnot(
        "Names could not be identified for some `...` inputs" = all(df_nm != ""),
        "Unrecognized names for `...` input; use only 'new_cui', 'man_rev', or 'de_diff'" =
            all(df_nm %in% c("new_cui", "man_rev", "de_diff"))
    )
    names(df_list) <- df_nm

    # split manual review data into separate sheets by type of updater comment
    if ("man_rev" %in% names(df_list)) {
        mr_nested <- make_curie_links(
            df_list$man_rev,
            c(
                "subject_id",
                if ("source_xref" %in% names(man_rev)) "source_xref",
                "object_id"
            )
        ) |>
            tidyr::nest(.by = "group")

        sheet_data <- purrr::set_names(
            mr_nested$data,
            mr_nested$group
        )

        # for removed, replace UMLS_CUI direct link with search link
        sheet_data$cui_removed <- sheet_data$cui_removed |>
            dplyr::mutate(
                object_id = dplyr::if_else(
                    stringr::str_detect(.data$updater_action, stringr::coll("remov")),
                    googlesheets4::gs4_formula(
                        stringr::str_replace(
                            .data$object_id,
                            "https://uts.nlm.nih.gov/uts/umls/concept/",
                            "https://uts.nlm.nih.gov/uts/umls/searchResults?searchString="
                        )
                    ),
                    .data$object_id
                ),
                term_replaced_by = NA_character_
            )
    } else {
        sheet_data <- list()
    }

    if ("new_cui" %in% names(df_list)) {
        sheet_data$new_cui <- make_curie_links(
            df_list$new_cui,
            c("subject_id", "source_xref", "object_id")
        )
    }

    if ("de_diff" %in% names(df_list)) {
        sheet_data$de_diff <- make_curie_links(
            df_list$de_diff,
            c("subject_id", "object_id")
        )
    }

    if (!debug & length(sheet_data) > 0) {
        purrr::walk2(
            sheet_data,
            names(sheet_data),
            ~ googlesheets4::write_sheet(
                data = .x,
                ss = ss,
                sheet = .y
            )
        )
    }

    invisible(sheet_data)
}


#' Makes CURIEs into Hyperlinks
#'
#' Simplifies formatting CURIEs as hyperlinks in Google Sheets, used by
#' `write_umls_to_gs()`.
#'
#' @param .df tibble containing CURIE columns to format as hyperlinks
#' @param .cols character vector of column names containing CURIEs to format as
#' hyperlinks
#'
#' @section NOTES:
#' May already exist in DO.utils... maybe `hyperlink_curie()`?
#'
#' @keywords internal
make_curie_links <- function(.df, .cols) {
    dplyr::mutate(
        .df,
        dplyr::across(
            dplyr::any_of(.cols),
            ~ DO.utils::build_hyperlink(
                x = stringr::str_remove(.x, ".*[:_]"),
                url = stringr::str_match(.x, "(.+)[:_]")[, 2],
                text = .x,
                as = "gs"
            )
        )
    )
}



######################### DATA PROCESSING #################################

# Set up new git branch ('umls_update') from UMLS-compared commit ---------

umls_branch <- gert::git_branch_create(
    branch = "umls_update",
    repo = do_repo,
    ref = commit,
    checkout = TRUE
)


# Get prior SSSOM mapping data from doid-edit.owl (for comparison) -------

de_path <- file.path(do_repo, "src", "ontology", "doid-edit.owl")

# use commit date as subject_source_version & UMLS updater mapping_date
commit_date <- system2(
    "cd",
    args = c(do_repo, "&& git show -s --format=%cd --date=short HEAD"),
    stdout = TRUE
)

de_sssom <- DO.utils::extract_obo_mappings(de_path) |>
    dplyr::rename(subject_status = "status") |>
    dplyr::mutate(subject_source_version = commit_date) |>
    dplyr::mutate(predicate_modifier = NA_character_, .after = "predicate_id")


# Get UMLS update data from Google Drive ----------------------------------

local_dir <- file.path(tempdir(), "umls_update")
res <- dir.create(local_dir, showWarnings = FALSE)
if (!res) {
    rlang::abort("Failed to create local directory for UMLS update data")
}

umls_data <- googledrive::drive_ls(umls_build_dir)
umls_paths <- purrr::map2_chr(
    umls_data$id,
    umls_data$name,
    function(.x, .y) {
        fpath <- file.path(local_dir, .y)
        googledrive::drive_download(
            file = .x,
            path = fpath,
            overwrite = TRUE
        )
        fpath
    }
)
umls_paths <- purrr::set_names(umls_paths, basename(umls_paths))


# Incorporate UMLS updater changes into doid-edit.owl in DO repo ----------

res <- file.copy(
    from = umls_paths["new_doid-edit.owl"],
    to = de_path,
    overwrite = TRUE
)
if (!res) {
    rlang::abort("Failed to replace doid-edit.owl with new_doid-edit.owl")
}

new_de <- readr::read_file(de_path) |>
    # !!NOTE: remove ^^xsd:string & re-write (FIX UMLS build script to avoid
    #   in future)
    stringr::str_replace_all("\\^\\^xsd:string", "") # remove if fixed
readr::write_file(new_de, de_path)                   # remove if fixed



# PROCESS UMLS & ONTOLOGY DATA --------------------------------------------

### parse UMLS build reports ###

# new cui report
new_cui_raw <- readr::read_lines(umls_paths["new_cuis_from_xrefs.txt"])
new_cui <- parse_new_cui(new_cui_raw, de_sssom)

# manual review report
man_rev_raw <- readr::read_lines(umls_paths["for_manual_review.txt"])
man_rev <- parse_man_rev(man_rev_raw, new_cui, de_sssom)


### analyze git diff (as SSSOM with new mapping info) ###
de_sssom_new <- DO.utils::extract_obo_mappings(de_path) |>
    dplyr::rename(subject_status = "status") |>
    dplyr::mutate(subject_source_version = commit_date) |>
    dplyr::mutate(predicate_modifier = NA_character_, .after = "predicate_id")

de_diff <- parse_de_updated_diff(do_repo, new_cui, man_rev, de_sssom_new)


# check for class status issues
status_list <- purrr::map(
    list(new_cui = new_cui, man_rev = man_rev, de_diff = de_diff),
    ~ dplyr::filter(.x, .data$subject_status != "active")
)
status_list <- status_list[purrr::map_int(status_list, nrow) > 0]

if (length(status_list > 0)) {
    stop("Unexpected 'subject_status' in data. REVIEW in `status_list`")
}

# !!NOTE: update to compare against existing SSSOM mappings
#  (from Google Sheet or DO repo???)
# sssom <- googlesheets4::read_sheet(
#     sssom_gs,
#     sssom_sheet,
#     col_types = "c"
# )


# Write to new 'curator_review' Google Sheet in umls_build_dir ------------

# create file
new_rev <- googledrive::drive_create(
    name = "curator_review",
    type = "spreadsheet",
    path = googledrive::as_id(umls_build_dir),
    overwrite = FALSE
)

# copy SSSOM sheet from Curator-Templates-MAIN
googlesheets4::sheet_copy(
    from_ss = "https://docs.google.com/spreadsheets/d/1Zn6p5xkVHUwbWe1N8FUa3fNcEkAOoE9P4ADb12f69hQ/",
    from_sheet = "sssom-template",
    to_ss = new_rev,
    to_sheet = "sssom-curated",
    .before = 1
)

# drop default 'Sheet1'
googlesheets4::sheet_delete(new_rev, sheet = "Sheet1")

res <- write_umls_to_gs(ss = new_rev, new_cui, man_rev, de_diff, debug = TRUE)
