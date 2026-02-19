# Check UMLS build results and update doid-edit.owl file

# OUTPUT: A `manual_review` file will be created in `umls_build_dir` as a new
# Google Sheet.

# MANUALLY specify data locations -----------------------------------------

# specify latest UMLS_build dir in Google Drive
umls_build_dir <- "https://drive.google.com/drive/folders/1bNK5iBzS0_5ZXCuiT0Zpjrsvewpnd4jq"

# local DO repo
do_repo <- "~/Documents/Ontologies/HumanDiseaseOntology"

# commit used for UMLS update
commit <- "8a80588764f4c998a0223b30b29858ef00cefbdb"


# PROGRAMMATIC SETUP ------------------------------------------------------

library(DO.utils)
library(gert)
library(googledrive)
library(googlesheets4)
library(tidyverse)

#' @param unparsed character vector of original lines (for error messages)
#' @param parsed tibble of parsed lines (with V1 = full match, etc.)
#' @param warn_col column(s) to check for partial parsing (default = all except
#' V1)
check_parsing <- function(unparsed, parsed, warn_col = -"V1") {
    failed <- is.na(parsed$V1)
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
        out <- dplyr::select(parsed, -"V1")
    }

    out
}

parse_new_cuis <- function(x) {
    parsed <- stringr::str_match(
        x,
        "Found new CUI \\((?<cui>C[0-9]+)\\) using (?<source>[^ ]+) code \\((?<lui>[^ ]+)\\) on this term: (?<doid>DOID_[0-9]+)"
    ) |>
        tibble::as_tibble()

    checked <- check_parsing(x, parsed)

    out <- checked |>
        dplyr::mutate(
            doid = stringr::str_replace(.data$doid, "_", ":"),
            cui = stringr::str_replace(
                .data$cui,
                "^(UMLS_CUI:)?",
                "UMLS_CUI:"
            )
        ) |>
        tidyr::unite(
            col = "source_xref",
            source,
            lui,
            sep = ":",
            remove = TRUE
        ) |>
        dplyr::rename(umls_cui = "cui") |>
        dplyr::relocate("doid", "source_xref", "umls_cui", .before = 1)

    out
}

# 3 patterns:
# 1. DOID,label (possible commas, no "),No entry in the UMLS for CUI,UMLS_CUI:<cui>, removing this xref
# 2. DOID,label (possible commas, no "),Unable to find any UMLS CUIs for this term.
# 3. DOID,label (possible commas, no "),Multiple CUIs found for <source>,<lui>,Found CUI,<cui>
#.    --> order of this one is different with action BEFORE cui, requires different regex
parse_man_rev <- function(x) {
    # deal with unquote labels that might have commas
    x_no_umls <- stringr::str_remove(x, "(CUI|UMLS).*")
    label_comma <- stringr::str_count(x_no_umls, ",") - 2
    pattern_start <- glue::glue(
        "^(?<doid>DOID[_:][0-9]+),(?<label>(?:[^,]+,){@label_comma@}[^,]+),",
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
        ~ tibble::as_tibble(stringr::str_match(.x, .y))
    ) |>
        dplyr::bind_rows()

    checked <- check_parsing(x, parsed, c("doid", "label", "comment"))
    out <- checked |>
        tidyr::unite(
            col = "source_xref",
            source,
            lui,
            sep = ":",
            remove = TRUE
        ) |>
        dplyr::mutate(
            source_xref = dplyr::na_if(.data$source_xref, "NA:NA"),
            doid = stringr::str_replace(.data$doid, "_", ":"),
            cui = stringr::str_replace(
                .data$cui,
                "^(UMLS_CUI:)?",
                "UMLS_CUI:"
            )
        ) |>
        dplyr::rename(umls_cui = "cui") |>
        dplyr::relocate("action", "umls_cui", .before = "comment")

    if ("source_xref" %in% names(out)) {
        out <- dplyr::relocate(out, "source_xref", .before = "comment")
    }
    out
}


# Set up new git branch ---------------------------------------------------

umls_branch <- gert::git_branch_create(
    branch = "umls_update",
    repo = do_repo,
    ref = commit,
    checkout = TRUE
)


# Get UMLS update data ----------------------------------------------------

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

# Update doid-edit.owl in DO repo (on new 'umls_update' branch) -----------

de_path <- file.path(do_repo, "src", "ontology", "doid-edit.owl")
res <- file.copy(
    from = umls_paths["new_doid-edit.owl"],
    to = de_path,
    overwrite = TRUE
)
if (!res) {
    rlang::abort("Failed to replace doid-edit.owl with new_doid-edit.owl")
}

new_de <- readr::read_file(de_path) |>
    # remove ^^xsd:string & re-write (FIX UMLS build script to avoid in future)
    stringr::str_replace_all("\\^\\^xsd:string", "") # remove if fixed
readr::write_file(new_de, de_path)                   # remove if fixed



# PROCESS UMLS & ONTOLOGY DATA --------------------------------------------

# parse UMLS build reports
new_cui_raw <- readr::read_lines(umls_paths["new_cuis_from_xrefs.txt"])
new_cui <- parse_new_cuis(new_cui_raw) |>

man_rev_raw <- readr::read_lines(umls_paths["for_manual_review.txt"])
man_rev <- parse_man_rev(man_rev_raw)


# analyze git diff
diff_lines <- gert::git_diff_patch(repo = do_repo) |>
    stringr::str_split("\n") |>
    unlist()

umls <- tibble::tibble(raw = diff_lines) |>
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
    dplyr::select("raw")


umls_diff <- umls |>
    tidyr::separate_wider_regex(
        cols = "raw",
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
    ) |>
    dplyr::mutate(
        date = stringr::str_extract(.data$ns, "20[0-9]{2}_[0-9]{2}_[0-9]{2}$"),
        ns = stringr::str_remove(.data$ns, "_20[0-9]{2}_[0-9]{2}_[0-9]{2}$")
    ) |>
    tidyr::unite(col = "stmt", "doid", "ns", "lui", sep = "@", remove = TRUE) |>
    DO.utils::collapse_col(.cols = c(change, date), delim = " >> ") |>
    tidyr::separate_wider_delim(
        cols = "stmt",
        delim = "@",
        names = c("DOID", "ns", "lui")
    ) |>
    dplyr::mutate(
        change = dplyr::case_when(
            .data$change == "+" ~ "added",
            .data$change == "-" ~ "removed",
            stringr::str_detect(.data$change, ">>") ~ "updated"
        ),
        DOID = DO.utils::build_hyperlink(
            x = stringr::str_remove(.data$DOID, ".*_"),
            url = "DOID",
            text = .data$DOID,
            as = "gs"
        ),
        xref = DO.utils::build_hyperlink(
            x = .data$lui,
            url = .data$ns,
            text = paste0(
                .data$ns,
                dplyr::if_else(
                    is.na(.data$date),
                    "",
                    paste0(
                        "_",
                        stringr::str_extract(
                            .data$date,
                            "20[0-9]{2}_[0-9]{2}_[0-9]{2}$"
                        )
                    )
                ),
                ":",
                .data$lui
            ),
            as = "gs"
        )
    ) |>
    dplyr::select("DOID", "xref", "change", "date")


# Write to new 'curator_review' Google Sheet in umls_build_dir ------------

new_rev <- googledrive::drive_create((
    name = "curator_review",
    type = "spreadsheet",
    path = googledrive::as_id(umls_build_dir),
    ovewrite = FALSE
)

sheet_data <- list(
    for_manual_review = dplyr::mutate(
        man_rev,
        dplyr::across(
            c("doid", "source_xref", "umls_cui"),
            ~ DO.utils::build_hyperlink(
                x = stringr::str_remove(.x, ".*[:_]"),
                url = stringr::str_match(.x, "(.+)[:_]")[, 2],
                text = .x,
                as = "gs"
            )
        ),
    new_cuis_from_xrefs = dplyr::mutate(
        new_cui,
        dplyr::across(
            c("doid", "source_xref", "umls_cui"),
            ~ DO.utils::build_hyperlink(
                x = stringr::str_remove(.x, ".*[:_]"),
                url = stringr::str_match(.x, "(.+)[:_]")[, 2],
                text = .x,
                as = "gs"
            )
        )
    umls_diff = umls_diff
)
purrr::map2(
    sheet_data,
    names(sheet_data),
    ~ googlesheets4::write_sheet(
        data = .x,
        ss = new_rev,
        sheet = .y
    )
)



manual <- googlesheets4::read_sheet(ss = gs, sheet = "for_manual_review")

googlesheets4::write_sheet(
    data = manual,
    ss = gs,
    sheet = paste0("for_manual_review-", today)
)
