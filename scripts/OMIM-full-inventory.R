# Inventory all OMIM phenotypes to identify needed updates to DO

library(DO.utils)
library(dplyr)
library(here)
library(keyring)
library(rlang)
library(stringr)
library(tidyr)


# MANUAL INPUTS ------------------------------------------------------------

data_dir <- here::here("data", "mapping", "omim")
omim_data <- c(
    "mimTitles",
    "phenotypicSeries",
    "genemap2",
    "mim2gene",
    "morbidmap"
)
doid_edit_path <- here::here(
    "../Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl"
)
review_gs <- "https://docs.google.com/spreadsheets/d/1nxbjt3Kq5vVA9yvHXfdu2rFeaNgEZ05h-AFIXsf2SN0/"
curation_gs <- "https://docs.google.com/spreadsheets/d/1gVfdXwVMGO2-vD09Kdu67KX89eucYTY8C7gibOY5RUg/"


# FUNCTIONS ----------------------------------------------------------------

# Age is determined from the "# Generated: YYYY-MM-DD" header line, which OMIM
# sets to the download time
omim_header_date <- function(path) {
    header <- readLines(path, n = 20L, warn = FALSE)
    date_line <- grep("^#\\s*Generated:", header, value = TRUE)
    if (length(date_line) == 0L) {
        rlang::abort(c(
            paste0(
                "Cannot determine age of '",
                basename(path),
                "': no 'Generated:' line found in first 20 lines."
            ),
            i = "Delete the file and re-run to force a fresh download."
        ))
    }
    date_str <- stringr::str_extract(date_line[[1L]], "\\d{4}-\\d{2}-\\d{2}")
    if (is.na(date_str)) {
        rlang::abort(c(
            paste0(
                "Cannot parse date from header of '",
                basename(path),
                "': '",
                date_line[[1L]],
                "'"
            ),
            i = "Delete the file and re-run to force a fresh download."
        ))
    }
    as.Date(date_str)
}

omim_age_days <- function(path) {
    if (!file.exists(path)) {
        return(Inf)
    }
    as.numeric(Sys.Date() - omim_header_date(path))
}

### Helper functions for flag handling ###
flag_order <- c("susceptibility", "locus_mim", "no_hgnc_symbol", "provisional")

# Helper: sort a flag column vector into flag_order priority.
sort_flags <- function(x) {
    vapply(
        x,
        function(s) {
            if (is.na(s)) {
                return(NA_character_)
            }
            vals <- trimws(strsplit(s, " | ", fixed = TRUE)[[1]])
            idx <- match(vals, flag_order)
            paste(
                vals[order(ifelse(is.na(idx), length(flag_order) + 1L, idx))],
                collapse = " | "
            )
        },
        character(1L)
    )
}

# Helper: merge two flag column values within the same row, where either or
# both may already be " | "-delimited strings
merge_flags <- function(a, b) {
    mapply(
        function(x, y) {
            vals <- unique(na.omit(c(
                unlist(strsplit(x, " | ", fixed = TRUE)),
                unlist(strsplit(y, " | ", fixed = TRUE))
            )))
            if (length(vals) == 0L) {
                return(NA_character_)
            }
            idx <- match(vals, flag_order)
            vals <- vals[order(ifelse(
                is.na(idx),
                length(flag_order) + 1L,
                idx
            ))]
            paste(vals, collapse = " | ")
        },
        a,
        b,
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
    )
}

map_inheritance <- function(x) {
    vapply(
        x,
        function(s) {
            if (is.na(s)) {
                return(NA_character_)
            }
            vals <- trimws(strsplit(s, " | ", fixed = TRUE)[[1L]])
            mapped <- unique(na.omit(geno_map[vals]))
            if (length(mapped) == 0L) {
                NA_character_
            } else {
                paste(mapped, collapse = "|")
            }
        },
        character(1L)
    )
}

# Convert a 1-based column number to a spreadsheet column letter (A, B, ..., Z,
# AA, AB, ...).
num_to_col_letter <- function(num) {
    result <- ""
    while (num > 0) {
        remainder <- (num - 1) %% 26
        result <- paste0(LETTERS[remainder + 1], result)
        num <- (num - remainder - 1) %/% 26
    }
    result
}

# Return a vector of cell references for a given column letter, for use inside
# dplyr::mutate() (uses row_number()). row_offset accounts for non-data rows
# above the data in the sheet (e.g. 2: 1 header + 1 ROBOT instruction row).
form_cell_ref <- function(num, nrow, .row_offset = 1L) {
    paste0(num_to_col_letter(num), seq_len(nrow) + .row_offset)
}


# Build a gs4_formula vector for use inside dplyr::mutate(). Pass string
# literals and bare column names interleaved in ...; column names are replaced
# with per-row cell references (e.g. "D3", "D4", ...). Uses dplyr::pick() to
# access the data being mutated. .row_offset accounts for non-data rows above
# the data in the sheet (default 1 for a single header row).
build_gs_formula <- function(..., .row_offset = 1L) {
    .df <- dplyr::pick(dplyr::everything())
    tidy_dots <- rlang::enquos(...)
    n <- nrow(.df)
    input_refs <- purrr::map(tidy_dots, function(q) {
        if (rlang::quo_is_symbol(q)) {
            pos <- tidyselect::eval_select(
                rlang::sym(rlang::as_name(q)),
                .df,
                strict = FALSE
            )
            if (length(pos) == 1L) {
                return(form_cell_ref(pos[[1L]], n, .row_offset))
            }
        }
        rlang::eval_tidy(q)
    })
    googlesheets4::gs4_formula(do.call(paste0, input_refs))
}

#' Curation Status
#'
#' Values used to establish `status` data validation in Google Sheet for
#' robot templates.
#'
#' * `incomplete`: curation of the entry is not yet complete.
#' * `ready to add`: entry is ready to be added to the ontology.
#' * `in edit.owl`: entry has been added to the ontology.
#' * `exclude`: entry should be excluded from the ontology.
curation_status <- c("incomplete", "ready to add", "in edit.owl", "exclude")

# Add a dropdown list validation rule to a specified range in a Google Sheet.
range_add_dropdown <- function(
    ss,
    sheet = NULL,
    range,
    values,
    msg = "Choose a valid value",
    reject_input = TRUE,
    display_arrow = TRUE,
    quiet = TRUE
) {
    rule <- googlesheets4:::new(
        "DataValidationRule",
        condition = googlesheets4:::new_BooleanCondition(
            type = "ONE_OF_LIST",
            values = values
        ),
        inputMessage = msg,
        strict = reject_input,
        showCustomUi = display_arrow
    )

    if (quiet) {
        .fn <- function(...) {
            googlesheets4::with_gs4_quiet(googlesheets4:::range_add_validation(
                ...
            ))
        }
    } else {
        .fn <- function(...) {
            googlesheets4:::range_add_validation(...)
        }
    }

    .fn(ss = ss, sheet = sheet, range = range, rule = rule)
}

# background color for
blue_background <- googlesheets4:::CellData(
    userEnteredFormat = googlesheets4:::new(
        "CellFormat",
        backgroundColor = googlesheets4:::new(
            "Color",
            red = 159 / 255,
            green = 183 / 255,
            blue = 196 / 255
        )
    )
)
range_flood(ss, range = "I:J", cell = blue_background)

# Format a robot_template sheet with curation status dropdowns & frozen header.
format_rt_sheet <- function(robot_data, col, ss, sheet, .row_offset = 1L, ...) {
    # add status validation
    col_ltr <- tidyselect::eval_select(col, robot_data) |>
        num_to_col_letter()
    stat_range <- paste0(col_ltr, .row_offset + 1, ":", col_ltr)
    range_add_dropdown(ss, sheet, stat_range, values = curation_status, ...)

    # freeze first two columns
    googlesheets4::with_gs4_quiet(
        googlesheets4:::sheet_freeze(ss, sheet = sheet, nrow = .row_offset)
    )
}


# 1. Download OMIM files --------------------------------------------------

dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

omim_files <- setNames(
    file.path(data_dir, paste0(omim_data, ".txt")),
    omim_data
)

to_download <- omim_files[vapply(omim_files, omim_age_days, numeric(1)) > 30] |>
    names()

if (length(to_download) > 0L) {
    api_key <- keyring::key_get("OMIM_API_KEY")
    if (!nzchar(api_key)) {
        rlang::abort(
            c(
                "OMIM API key not found in credentials store.",
                i = 'Use keyring::key_set("OMIM_API_KEY") and paste the API key when prompted'
            )
        )
    }
    message(
        "Downloading ",
        paste(to_download, collapse = " and "),
        " from OMIM..."
    )
    DO.utils::download_omim(
        to_download,
        dest_dir = data_dir,
        api_key = api_key
    )
} else {
    age_msgs <- paste0(
        "  ",
        names(omim_files),
        " : ",
        vapply(
            omim_files,
            function(f) format(omim_header_date(f)),
            character(1L)
        )
    )
    message(
        "OMIM files are recent (< 30 days old); skipping download.\n",
        paste(age_msgs, collapse = "\n")
    )
}


# 2. Load and parse OMIM files --------------------------------------------

mt <- DO.utils::read_omim(omim_files["mimTitles"], keep_mim = NULL)
ps_raw <- DO.utils::read_omim(omim_files["phenotypicSeries"], keep_mim = NULL)
mm <- DO.utils::read_omim(omim_files["morbidmap"], keep_mim = NULL)
m2g <- DO.utils::read_omim(omim_files["mim2gene"], keep_mim = NULL)
gm <- DO.utils::read_omim(omim_files["genemap2"], keep_mim = NULL)


# 2a. mimTitles: phenotype universe ----------------------------------------
# Asterisk (*) = gene only; NULL prefix = "predominantly phenotypes".
# Caret (^) = moved/removed -- included separately below.
phenotype_prefixes <- c("Number Sign", "Percent", "NULL")

mt_primary <- mt |>
    dplyr::filter(prefix %in% phenotype_prefixes) |>
    dplyr::transmute(
        omim = paste0("MIM:", mim_number),
        omim_type = dplyr::case_when(
            prefix == "Number Sign" ~ "phenotype",
            prefix == "Percent" ~ "phenotype_unknown_molecular_basis",
            TRUE ~ "predominantly_phenotypes"
        ),
        # Strip gene symbol after ";" to keep only the disease name portion
        label = stringr::str_extract(preferred_title_symbol, "^[^;]+"),
        flag = NA_character_
    )

# Moved/removed entries: included so inventory_omim() can detect stale DO mappings.
mt_moved <- mt |>
    dplyr::filter(prefix == "Caret") |>
    dplyr::transmute(
        omim = paste0("MIM:", mim_number),
        omim_type = "moved_removed",
        label = stringr::str_extract(preferred_title_symbol, "^[^;]+"),
        flag = NA_character_
    )

# Keep "INCLUDED" entities (share the parent MIM#) for reference
mt_included <- mt |>
    dplyr::filter(
        prefix %in% phenotype_prefixes,
        !is.na(included_title_s_symbols)
    ) |>
    dplyr::select(mim_number, included_title_s_symbols) |>
    tidyr::separate_rows(included_title_s_symbols, sep = ";;") |>
    dplyr::mutate(
        label = stringr::str_trim(included_title_s_symbols),
        label = stringr::str_remove(label, ",?\\s*INCLUDED$"),
        label = stringr::str_trim(label),
        label = stringr::str_extract(label, "^[^;]+"),
        omim = paste0("MIM:", mim_number),
        omim_type = "included_entity",
        flag = NA_character_
    ) |>
    dplyr::select(omim, omim_type, label, flag)

# 2b. phenotypicSeries: PS records + membership lookup ---------------------
# Header rows (mim_number is NA) become standalone PS inventory records.
ps_records <- ps_raw |>
    dplyr::filter(is.na(mim_number)) |>
    dplyr::transmute(
        omim = paste0("MIM:", phenotypic_series_number),
        omim_type = "phenotypic_series",
        label = phenotype
    )

# Non-header rows map each individual phenotype MIM# to its PS(es).
ps_lookup <- ps_raw |>
    dplyr::filter(!is.na(mim_number)) |>
    dplyr::transmute(
        omim = paste0("MIM:", mim_number),
        ps = paste0("MIM:", phenotypic_series_number)
    ) |>
    DO.utils::collapse_col(ps, delim = " | ", method = "unique", na.rm = TRUE)

# 2c. mim2gene: HGNC approved symbol lookup --------------------------------
hgnc_lookup <- m2g |>
    dplyr::filter(!is.na(approved_gene_symbol_hgnc)) |>
    dplyr::select(mim_number, gene_symbol = approved_gene_symbol_hgnc)

# 2d. morbidmap: parse phenotype string, filter QTLs, add gene info --------
# Phenotype field format: [BRACKET]name[/BRACKET], MIM# (KEY)
#   BRACKET: { = susceptibility, [ = non_disease/QTL (excluded), ? = provisional
# Inheritance is NOT present in morbidmap -- extracted from genemap2 below.
mm_parsed <- mm |>
    dplyr::mutate(
        bracket = stringr::str_extract(phenotype, "^[?{\\[]"),
        flag = dplyr::case_when(
            bracket == "{" ~ "susceptibility",
            bracket == "[" ~ "non_disease",
            bracket == "?" ~ "provisional",
            .default = NA_character_
        ),
        # 6-digit phenotype MIM# is immediately before " (KEY)"
        pheno_mim_number = as.integer(
            stringr::str_match(phenotype, "(\\d{6}) \\([1-4]\\)")[, 2]
        ),
        mapping_key = as.integer(
            stringr::str_match(phenotype, "\\d{6} \\((\\d)\\)")[, 2]
        ),
        # When no MIM# is embedded in the phenotype string, fall back to the
        # mim_number column, which for ~98% of these rows is itself a
        # phenotype-type MIM# per mim2gene. Flag for review.
        flag = dplyr::if_else(
            is.na(pheno_mim_number),
            merge_flags(flag, "locus_mim"),
            flag
        ),
        pheno_mim_number = dplyr::if_else(
            is.na(pheno_mim_number),
            as.integer(mim_number),
            pheno_mim_number
        ),
        omim = paste0("MIM:", pheno_mim_number)
    ) |>
    dplyr::filter(is.na(flag) | !grepl("non_disease", flag)) |>
    # Join HGNC approved symbol by gene MIM#; fall back to first listed symbol.
    # Flag rows where fallback was used -- gene symbol may be non-standard.
    dplyr::left_join(hgnc_lookup, by = "mim_number") |>
    dplyr::mutate(
        first_locus_symbol = stringr::str_extract(
            gene_locus_and_other_related_symbols,
            "^[^,]+"
        ),
        flag = dplyr::if_else(
            is.na(gene_symbol) & !is.na(first_locus_symbol),
            merge_flags(flag, "no_hgnc_symbol"),
            flag
        ),
        gene_symbol = dplyr::coalesce(gene_symbol, first_locus_symbol)
    )

# Collapse to one row per phenotype MIM#. lengthen_col() splits compound flag
# strings before collapsing so that atomic values are not duplicated across rows.
mm_summary <- mm_parsed |>
    dplyr::select(omim, flag, gene_symbol, cyto_location, mapping_key) |>
    dplyr::mutate(mapping_key = as.character(mapping_key)) |>
    DO.utils::lengthen_col(flag, delim = " | ") |>
    DO.utils::collapse_col(
        c(flag, gene_symbol, cyto_location, mapping_key),
        delim = " | ",
        method = "unique",
        na.rm = TRUE
    ) |>
    dplyr::rename(mm_flag = flag)

# 2e. genemap2: extract inheritance per phenotype MIM# --------------------
# genemap2 phenotypes column is "; "-delimited; each entry:
#   [bracket]name, MIM# (KEY), Inheritance[/bracket]
gm_inheritance <- gm |>
    dplyr::filter(!is.na(phenotypes), nchar(trimws(phenotypes)) > 0L) |>
    dplyr::select(phenotypes) |>
    tidyr::separate_rows(phenotypes, sep = ";\\s+") |>
    dplyr::mutate(
        phenotypes = stringr::str_trim(phenotypes),
        pheno_mim = as.integer(
            stringr::str_match(phenotypes, "(\\d{6}) \\([1-4]\\)")[, 2]
        ),
        inheritance = stringr::str_match(
            phenotypes,
            "\\d{6} \\([1-4]\\), (.+)$"
        )[, 2]
    ) |>
    dplyr::filter(!is.na(pheno_mim)) |>
    dplyr::select(pheno_mim, inheritance) |>
    DO.utils::collapse_col(
        inheritance,
        delim = " | ",
        method = "unique",
        na.rm = TRUE
    ) |>
    dplyr::mutate(omim = paste0("MIM:", pheno_mim)) |>
    dplyr::select(omim, inheritance)


# 3. Assemble inventory table ---------------------------------------------

omim_pheno <- dplyr::bind_rows(mt_primary, mt_included, mt_moved) |>
    dplyr::left_join(ps_lookup, by = "omim") |>
    dplyr::left_join(mm_summary, by = "omim") |>
    dplyr::left_join(gm_inheritance, by = "omim") |>
    dplyr::mutate(
        flag = sort_flags(merge_flags(flag, mm_flag))
    ) |>
    dplyr::select(-mm_flag) |>
    # Append PS records as their own rows (no gene/inheritance/mapping_key data)
    dplyr::bind_rows(
        ps_records |>
            dplyr::mutate(
                ps = omim,
                flag = NA_character_,
                gene_symbol = NA_character_,
                cyto_location = NA_character_,
                mapping_key = NA_character_,
                inheritance = NA_character_
            )
    ) |>
    dplyr::relocate(ps, .after = label)


# 4. DO inventory ---------------------------------------------------------

# inventory_omim() requires an omim_tbl; add the class to our assembled table
class(omim_pheno) <- c("omim_tbl", class(omim_pheno))

oi <- DO.utils::inventory_omim(doid_edit_path, omim_pheno)

DO.utils::elucidate(oi)


# 5. Review Output --------------------------------------------------------

# no review category for existing DOIDs that are susceptibilities/included_entity
rev_df <- oi |>
    dplyr::mutate(
        category = dplyr::case_when(
            .data$exists &
                .data$omim_type == "moved_removed" ~ "removed-review",
            .data$omim_type == "moved_removed" ~ "removed-ignore",
            stringr::str_detect(
                .data$flag,
                "susceptibility"
            ) ~ "susceptibility",
            .data$omim_type == "included_entity" ~ "included",
            .data$exists & !is.na(.data$multimaps) ~ "in_DO-multimaps",
            .data$exists &
                stringr::str_detect(
                    mapping_type,
                    "skos:(close|broad|narrow|related)"
                ) ~ "in_DO-not_exact",
            .data$exists ~ "in_DO-simple",
            .default = "review"
        ),
        category = factor(
            .data$category,
            levels = c(
                # priority order for review
                "review",
                "removed-review",
                "in_DO-multimaps",
                # low priority or no action needed
                "in_DO-not_exact",
                "in_DO-simple",
                "susceptibility",
                "included",
                "removed-ignore"
            )
        )
    ) |>
    tidyr::nest(.by = "category", .key = "data") |>
    dplyr::arrange(.data$category)

# special handling for "review" category: add `quick_review` column
review <- rev_df |>
    dplyr::filter(.data$category == "review") |>
    tidyr::unnest(cols = "data") |>
    dplyr::select("omim":"inheritance") |>
    dplyr::mutate(quick_review = NA_character_, .before = "flag")

DO.utils::write_gs(
    review,
    ss = review_gs,
    sheet = "review",
    hyperlink_curie = c("omim", "ps")
)
quick_rev_ltr <- which(names(review) == "quick_review") |>
    num_to_col_letter()
quick_rev_range <- paste0(quick_rev_ltr, 2, ":", quick_rev_ltr)
range_add_dropdown(
    ss = review_gs,
    sheet = "review",
    range = quick_rev_range,
    values = c("add", "not_disease", "remove", "exclude", "ignore", "review")
)


# write other categories to individual sheets
rev_df <- rev_df |>
    dplyr::filter(category != "review")

purrr::walk2(
    as.character(rev_df$category),
    rev_df$data,
    ~ DO.utils::write_gs(.y, ss = review_gs, sheet = .x)
)

# 6. Robot template for missing phenotypes --------------------------------

# 6a. Direct DO parent for each DOID (from doid-edit.owl subClassOf axioms).
# Used to check whether all DO-mapped members of a PS share the same parent,
# which is taken as the suggested parent for missing siblings.
do_parents <- DO.utils::robot_query(
    doid_edit_path,
    query = '
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    SELECT ?id ?parent_id ?parent_label
    WHERE {
        ?id rdfs:subClassOf ?parent_id .
        ?parent_id rdfs:label ?parent_label
        FILTER(!isBlank(?parent_id))
    }',
    tidy_what = c("header", "uri_to_curie")
)

# 6b. For each PS, record the parent only when ALL DO-mapped members agree.
ps_parent <- oi |>
    dplyr::filter(!is.na(.data$ps), .data$exists) |>
    tidyr::separate_longer_delim("ps", delim = " | ") |>
    dplyr::select(ps, doid) |>
    dplyr::left_join(do_parents, by = c("doid" = "id")) |>
    dplyr::group_by(ps) |>
    dplyr::summarise(
        n_unique_parents = dplyr::n_distinct(parent_id, na.rm = TRUE),
        parent_curie = dplyr::first(na.omit(parent_id)),
        parent_label = dplyr::first(na.omit(parent_label)),
        .groups = "drop"
    ) |>
    dplyr::filter(n_unique_parents == 1L, !is.na(parent_curie)) |>
    dplyr::select(ps, parent_curie, parent_label)

# 6c. Propagate parent suggestion to each missing phenotype.
# If a phenotype belongs to multiple PS that disagree → leave blank.
pheno_parent <- oi |>
    dplyr::filter(!.data$exists, !is.na(.data$ps)) |>
    dplyr::select(omim, ps) |>
    tidyr::separate_longer_delim("ps", delim = " | ") |>
    dplyr::left_join(ps_parent, by = "ps") |>
    dplyr::group_by(omim) |>
    dplyr::summarise(
        n_suggestions = dplyr::n_distinct(parent_curie, na.rm = TRUE),
        parent_curie = dplyr::first(na.omit(parent_curie)),
        parent_label = dplyr::first(na.omit(parent_label)),
        .groups = "drop"
    ) |>
    dplyr::filter(n_suggestions <= 1L) |>
    dplyr::select(omim, parent_curie, parent_label)

# 6d. Parse DO-formatted names and acronyms from mimTitles.
# parse_omim_name() takes the raw "name; ABBREVIATION" OMIM string and returns
# a DO-style lowercased/rearranged name and the abbreviation separately.
name_primary <- mt |>
    dplyr::filter(prefix %in% c(phenotype_prefixes, "Caret")) |>
    dplyr::select(mim_number, preferred_title_symbol) |>
    DO.utils::parse_omim_name(col = "preferred_title_symbol") |>
    dplyr::transmute(
        omim = paste0("MIM:", mim_number),
        do_label = name,
        primary_acronym = abbreviation
    )

# PS names from the phenotypicSeries header rows (no abbreviation present).
name_ps <- ps_raw |>
    dplyr::filter(is.na(mim_number)) |>
    dplyr::transmute(
        omim = paste0("MIM:", phenotypic_series_number),
        preferred_title_symbol = phenotype
    ) |>
    DO.utils::parse_omim_name(col = "preferred_title_symbol") |>
    dplyr::transmute(omim, do_label = name, primary_acronym = abbreviation)

# Alternative titles → additional exact synonyms and acronyms.
name_alt <- mt |>
    dplyr::filter(
        prefix %in% phenotype_prefixes,
        !is.na(alternative_title_s_symbol_s)
    ) |>
    dplyr::select(mim_number, alternative_title_s_symbol_s) |>
    tidyr::separate_rows(alternative_title_s_symbol_s, sep = ";;") |>
    dplyr::mutate(
        alternative_title_s_symbol_s = stringr::str_trim(
            alternative_title_s_symbol_s
        )
    ) |>
    dplyr::filter(nchar(alternative_title_s_symbol_s) > 0L) |>
    DO.utils::parse_omim_name(col = "alternative_title_s_symbol_s") |>
    dplyr::mutate(omim = paste0("MIM:", mim_number)) |>
    dplyr::group_by(omim) |>
    dplyr::summarise(
        syn_exact = paste(name, collapse = "|"),
        alt_acronym = {
            abbrevs <- na.omit(abbreviation)
            if (length(abbrevs) == 0L) {
                NA_character_
            } else {
                paste(abbrevs, collapse = "|")
            }
        },
        .groups = "drop"
    )

# Combine primary + PS names, merge acronyms (primary | alternative).
name_parsed <- dplyr::bind_rows(name_primary, name_ps) |>
    dplyr::left_join(name_alt, by = "omim") |>
    dplyr::mutate(
        acronym = dplyr::case_when(
            !is.na(primary_acronym) & !is.na(alt_acronym) ~
                paste(primary_acronym, alt_acronym, sep = "|"),
            !is.na(primary_acronym) ~ primary_acronym,
            !is.na(alt_acronym) ~ alt_acronym,
            .default = NA_character_
        )
    ) |>
    dplyr::select(omim, do_label, acronym, syn_exact)

# 6e. Map genemap2 inheritance strings to GENO class labels.
# Values not listed here (somatic mutation, isolated cases, pseudoautosomal,
# somatic mosaicism) have no GENO equivalent and map to NA implicitly.
geno_map <- c(
    "Autosomal recessive" = "autosomal recessive inheritance",
    "Autosomal dominant" = "autosomal dominant inheritance",
    "X-linked recessive" = "X-linked recessive inheritance",
    "X-linked dominant" = "X-linked dominant inheritance",
    "X-linked" = "X-linked inheritance",
    "Y-linked" = "Y-linked inheritance",
    "Mitochondrial" = "mitochondrial inheritance",
    "Multifactorial" = "multifactorial inheritance",
    "Digenic dominant" = "digenic inheritance",
    "Digenic recessive" = "digenic inheritance"
)


# 6f. Assemble robot template rows.
# Excludes: entries already in DO, susceptibility entries, included_entity.
# Includes: phenotype, phenotype_unknown_molecular_basis,
#           predominantly_phenotypes, phenotypic_series not in DO.
robot_data <- oi |>
    dplyr::filter(
        !.data$exists,
        omim_type %in%
            c(
                "phenotype",
                "phenotype_unknown_molecular_basis",
                "predominantly_phenotypes",
                "phenotypic_series"
            ),
        is.na(flag) | !stringr::str_detect(flag, "susceptibility")
    ) |>
    dplyr::select("omim":"inheritance") |>
    dplyr::left_join(pheno_parent, by = "omim") |>
    dplyr::left_join(name_parsed, by = "omim") |>
    dplyr::mutate(sc_inheritance = map_inheritance(inheritance)) |>
    dplyr::transmute(
        # Curation-assist columns (no ROBOT instruction; blank = ignored by ROBOT)
        status = NA_character_,
        curation_notes = NA_character_,
        link = omim,
        def_article = NA_character_,
        parent_label,
        `characterized by` = NA_character_,
        `genetic basis` = NA_character_,
        # ROBOT columns
        iri = NA_character_,
        parent_curie,
        label = do_label,
        def = NA_character_,
        def_src = NA_character_,
        def_type = NA_character_,
        syn_exact,
        acronym,
        annotate_acronym = NA_character_,
        skos_exact = omim,
        xrefs = NA_character_,
        `sc_axiom-INHERITANCE` = sc_inheritance,
        `sc_axiom-ANATOMY` = NA_character_,
        `sc_axiom-ONSET` = NA_character_,
        subset = NA_character_,
        obo_id = NA_character_,
        Namespace = NA_character_
    ) |>
    # Add Google Sheets formulas for computed columns.
    dplyr::mutate(
        def_article = build_gs_formula(
            "=if(\n",
            "  isblank(",
            parent_label,
            "), iferror(1/0),\n",
            "  if(regexmatch(\"^[AEIOUaeiou]\", ",
            parent_label,
            "), \"An\", \"A\")\n",
            ")"
        ),
        def = build_gs_formula(
            "=concatenate(\n",
            "  if(not(isblank(",
            def_article,
            ")), concat(",
            def_article,
            ",\" \"), iferror(1/0)),\n",
            "  if(not(isblank(",
            parent_label,
            ")),",
            parent_label,
            ", iferror(1/0)),\n",
            "  if(not(isblank(",
            `characterized by`,
            ")), concatenate(\" characterized by \",",
            `characterized by`,
            "), iferror(1/0)),\n",
            "  if(not(isblank(",
            `genetic basis`,
            ")), concat(\" that has_material_basis_in \",",
            `genetic basis`,
            "), iferror(1/0))\n",
            ")"
        ),
        annotate_acronym = build_gs_formula(
            "=if(isblank(",
            acronym,
            "), iferror(1/0), \"acronym\")"
        ),
        # skos_exact = build_gs_formula("=", link),
        xrefs = build_gs_formula(
            "=if(isblank(",
            skos_exact,
            "), iferror(1/0), ",
            skos_exact,
            ")"
        ),
        obo_id = build_gs_formula("=", iri),
        Namespace = build_gs_formula(
            "=if(isblank(",
            iri,
            "), iferror(1/0), \"disease_ontology\")"
        )
    )

# Write data without ROBOT instruction row
DO.utils::write_gs(
    robot_data[1:10, ],
    ss = curation_gs,
    sheet = "new-robot_template",
    hyperlink_curie = c("link", "parent_curie")
)

# Insert instructions as a separate block at the top of the sheet, to avoid
# coercion errors due to binding > writing

# ROBOT instruction row: written as first data row; ROBOT reads rows 1 + 2.
# Curation-assist columns are NA (ROBOT ignores blank cells in column 2).
robot_instructions <- tibble::tibble_row(
    status = NA_character_,
    curation_notes = NA_character_,
    link = NA_character_,
    def_article = NA_character_,
    parent_label = NA_character_,
    `characterized by` = NA_character_,
    `genetic basis` = NA_character_,
    iri = "ID",
    parent_curie = "SC %",
    label = "A rdfs:label",
    def = "A obo:IAO_0000115",
    def_src = ">A oboInOwl:hasDbXref SPLIT=|",
    def_type = ">AI dc11:type SPLIT=|",
    syn_exact = "A oboInOwl:hasExactSynonym SPLIT=|",
    acronym = "A oboInOwl:hasExactSynonym SPLIT=|",
    annotate_acronym = ">A oboInOwl:SynonymTypeProperty",
    skos_exact = "A skos:exactMatch SPLIT=|",
    xrefs = "A oboInOwl:hasDbXref SPLIT=|",
    `sc_axiom-INHERITANCE` = "SC 'has material basis in' some %",
    `sc_axiom-ANATOMY` = "SC 'disease has location' some %",
    `sc_axiom-ONSET` = "SC 'has material basis in' some %",
    subset = "AI oboInOwl:inSubset",
    obo_id = "A oboInOwl:id",
    Namespace = "A oboInOwl:hasOBONamespace"
)

googlesheets4::sheet_insert_rows(
    curation_gs,
    sheet = "new-robot_template",
    before = 2L
)

googlesheets4::range_write(
    curation_gs,
    robot_instructions,
    sheet = "new-robot_template",
    range = "A2",
    col_names = FALSE
)

format_rt_sheet(
    robot_data,
    col = status,
    ss = curation_gs,
    sheet = "new-robot_template",
    .row_offset = 2L
)

# add highlight
message("Robot template written to Google Sheets: ", nrow(robot_data), " rows")
