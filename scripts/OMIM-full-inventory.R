# Inventory all OMIM phenotypes to identify needed updates to DO

library(here)
library(DO.utils)

# MANUAL INPUTS ------------------------------------------------------------

data_dir <- here::here("data", "mapping", "omim")
omim_data <- c("mimTitles", "phenotypicSeries", "genemap2", "mim2gene", "morbidmap")
doid_edit_path <- here::here(
    "../Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl"
)

# SET UP -------------------------------------------------------------------

api_key <- keyring::key_get("OMIM_API_KEY")
if (!nzchar(api_key)) {
    rlang::abort(
        c(
            "OMIM API key not found in credentials store.",
            i = 'Use keyring::key_set("OMIM_API_KEY") and paste the API key when prompted'
        )
    )
}

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


# 1. Download OMIM files --------------------------------------------------

dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

omim_files <- setNames(
    file.path(data_dir, paste0(omim_data, ".txt")),
    omim_data
)

to_download <- omim_files[vapply(omim_files, omim_age_days, numeric(1)) > 30] |>
    names()

if (length(to_download) > 0L) {
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
        vapply(omim_files, function(f) format(omim_header_date(f)), character(1L))
    )
    message(
        "OMIM files are recent (< 30 days old); skipping download.\n",
        paste(age_msgs, collapse = "\n")
    )
}

# 2. Load and parse OMIM files --------------------------------------------

mt     <- DO.utils::read_omim(omim_files["mimTitles"],         keep_mim = NULL)
ps_raw <- DO.utils::read_omim(omim_files["phenotypicSeries"],  keep_mim = NULL)
mm     <- DO.utils::read_omim(omim_files["morbidmap"],         keep_mim = NULL)
m2g    <- DO.utils::read_omim(omim_files["mim2gene"],          keep_mim = NULL)
gm     <- DO.utils::read_omim(omim_files["genemap2"],          keep_mim = NULL)

# Helper: merge two flag column values within the same row, where either or
# both may already be " | "-delimited strings. Not replaceable by collapse_col()
# (which collapses multiple rows) or lengthen_col() without a pivot.
merge_flags <- function(a, b) {
    mapply(function(x, y) {
        vals <- sort(unique(na.omit(c(
            unlist(strsplit(x, " | ", fixed = TRUE)),
            unlist(strsplit(y, " | ", fixed = TRUE))
        ))))
        if (length(vals) == 0L) NA_character_ else paste(vals, collapse = " | ")
    }, a, b, SIMPLIFY = TRUE, USE.NAMES = FALSE)
}

# 2a. mimTitles: phenotype universe ----------------------------------------
# Asterisk (*) = gene only; NULL prefix = "predominantly phenotypes".
# Caret (^) = moved/removed -- included separately below.
phenotype_prefixes <- c("Number Sign", "Percent", "NULL")

mt_primary <- mt |>
    dplyr::filter(prefix %in% phenotype_prefixes) |>
    dplyr::transmute(
        omim      = paste0("MIM:", mim_number),
        omim_type = dplyr::case_when(
            prefix == "Number Sign" ~ "phenotype",
            prefix == "Percent"     ~ "phenotype_unknown_molecular_basis",
            TRUE                    ~ "predominantly_phenotypes"
        ),
        # Strip gene symbol after ";" to keep only the disease name portion
        label = stringr::str_extract(preferred_title_symbol, "^[^;]+"),
        flag  = NA_character_
    )

# Moved/removed entries: included so inventory_omim() can detect stale DO mappings.
mt_moved <- mt |>
    dplyr::filter(prefix == "Caret") |>
    dplyr::transmute(
        omim      = paste0("MIM:", mim_number),
        omim_type = "moved_removed",
        label     = stringr::str_extract(preferred_title_symbol, "^[^;]+"),
        flag      = NA_character_
    )

# Keep "INCLUDED" entities (share the parent MIM#) for reference
mt_included <- mt |>
    dplyr::filter(prefix %in% phenotype_prefixes, !is.na(included_title_s_symbols)) |>
    dplyr::select(mim_number, included_title_s_symbols) |>
    tidyr::separate_rows(included_title_s_symbols, sep = ";;") |>
    dplyr::mutate(
        label = stringr::str_trim(included_title_s_symbols),
        label = stringr::str_remove(label, ",?\\s*INCLUDED$"),
        label = stringr::str_trim(label),
        label = stringr::str_extract(label, "^[^;]+"),
        omim      = paste0("MIM:", mim_number),
        omim_type = "included_entity",
        flag      = NA_character_
    ) |>
    dplyr::select(omim, omim_type, label, flag)

# 2b. phenotypicSeries: PS records + membership lookup ---------------------
# Header rows (mim_number is NA) become standalone PS inventory records.
ps_records <- ps_raw |>
    dplyr::filter(is.na(mim_number)) |>
    dplyr::transmute(
        omim      = paste0("MIM:", phenotypic_series_number),
        omim_type = "phenotypic_series",
        label     = phenotype
    )

# Non-header rows map each individual phenotype MIM# to its PS(es).
ps_lookup <- ps_raw |>
    dplyr::filter(!is.na(mim_number)) |>
    dplyr::transmute(
        omim = paste0("MIM:", mim_number),
        ps   = paste0("MIM:", phenotypic_series_number)
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
        flag    = dplyr::case_when(
            bracket == "{"  ~ "susceptibility",
            bracket == "["  ~ "non_disease",
            bracket == "?"  ~ "provisional",
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
            is.na(pheno_mim_number), merge_flags(flag, "locus_mim"), flag
        ),
        pheno_mim_number = dplyr::if_else(
            is.na(pheno_mim_number), as.integer(mim_number), pheno_mim_number
        ),
        omim = paste0("MIM:", pheno_mim_number)
    ) |>
    dplyr::filter(is.na(flag) | !grepl("non_disease", flag)) |>
    # Join HGNC approved symbol by gene MIM#; fall back to first listed symbol.
    # Flag rows where fallback was used -- gene symbol may be non-standard.
    dplyr::left_join(hgnc_lookup, by = "mim_number") |>
    dplyr::mutate(
        first_locus_symbol = stringr::str_extract(
            gene_locus_and_other_related_symbols, "^[^,]+"
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
        delim = " | ", method = "unique", na.rm = TRUE
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
        phenotypes   = stringr::str_trim(phenotypes),
        pheno_mim    = as.integer(
            stringr::str_match(phenotypes, "(\\d{6}) \\([1-4]\\)")[, 2]
        ),
        inheritance = stringr::str_match(
            phenotypes, "\\d{6} \\([1-4]\\), (.+)$"
        )[, 2]
    ) |>
    dplyr::filter(!is.na(pheno_mim)) |>
    dplyr::select(pheno_mim, inheritance) |>
    DO.utils::collapse_col(inheritance, delim = " | ", method = "unique", na.rm = TRUE) |>
    dplyr::mutate(omim = paste0("MIM:", pheno_mim)) |>
    dplyr::select(omim, inheritance)


# 3. Assemble inventory table ---------------------------------------------

omim_pheno <- dplyr::bind_rows(mt_primary, mt_included, mt_moved) |>
    dplyr::left_join(ps_lookup,      by = "omim") |>
    dplyr::left_join(mm_summary,     by = "omim") |>
    dplyr::left_join(gm_inheritance, by = "omim") |>
    dplyr::mutate(
        flag = merge_flags(flag, mm_flag)
    ) |>
    dplyr::select(-mm_flag) |>
    # Append PS records as their own rows (no gene/inheritance/mapping_key data)
    dplyr::bind_rows(
        ps_records |>
            dplyr::mutate(
                ps          = omim,
                flag        = NA_character_,
                gene_symbol   = NA_character_,
                cyto_location = NA_character_,
                mapping_key   = NA_character_,
                inheritance   = NA_character_
            )
    ) |>
    dplyr::relocate(ps, .after = label)


# 4. DO inventory ---------------------------------------------------------

# inventory_omim() requires an omim_tbl; add the class to our assembled table
class(omim_pheno) <- c("omim_tbl", class(omim_pheno))

oi <- DO.utils::inventory_omim(doid_edit_path, omim_pheno)


# 5. Output ---------------------------------------------------------------

# (deferred -- will split by sheet as needed)
message(
    "Inventory complete: ", nrow(oi), " records\n",
    "  exists in DO : ", sum(oi$exists, na.rm = TRUE), "\n",
    "  missing      : ", sum(!oi$exists, na.rm = TRUE)
)
