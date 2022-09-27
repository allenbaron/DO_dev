# Comparing mesh_xrefs from PR #1074
#   https://github.com/DiseaseOntology/HumanDiseaseOntology/pull/1074
# By: J. Allen Baron
# 2022-09-23


# Setup + File paths ------------------------------------------------------
library(tidyverse)
library(googlesheets4)
library(DO.utils)
library(janitor)

repo_path <- "../Ontologies/HumanDiseaseOntology/"
mesh_biomappings_gs <- googlesheets4::as_sheets_id(
    "1KIMf3E5doIgTiOz8ogvennch23LUQ5yEMe5eQXry6zI"
)



# Custom Functions --------------------------------------------------------

##### MESH XREF MANIPULATORS #####

# Add/rm xref prefixes
format_xref <- function(x, how = "rm", prefix = NULL, ensure_prefix = TRUE) {
    how <- match.arg(how, c("add", "rm"))
    if (!is.character(x)) {
        stop("`x` must be a character vector.")
    }
    if (how == "add") {
        if (is.null(prefix)) {
            if (!is.null(attr(x, "prefix"))) {
                prefix <- attr(x, "prefix")
            } else {
                stop("`prefix` or `attr(x, \"prefix\")` must not be NULL.")
            }
        }
        if (!is.null(attr(x, "prefix"))) {
            prev_prefix <- attr(x, "prefix")
            if (prefix != prev_prefix) {
                warning(
                    "The provided `prefix` ", prefix,
                    " was used, instead of the prefix attribute on `x`: ", prev_prefix,
                    ".",
                    call. = FALSE
                )
            }
        }

        formatted <- dplyr::if_else(
            stringr::str_detect(x, ":"),
            as.character(x),
            paste0(prefix, ":", x)
        )
        class(formatted) <- c("curie", class(formatted))
    }

    if (how == "rm") {
        all_prefix <- all(stringr::str_detect(x, ":"))

        if (!all_prefix) {
            if (isTRUE(ensure_prefix)) {
                stop("Can't find prefix to remove from `x`. Expected pattern {prefix}:{id}.")
            } else {
                return(x)
            }
        }

        prefix <- stringr::str_extract(x, ".*:") %>%
            unique() %>%
            stringr::str_remove(":")
        if (isFALSE(ensure_prefix)) {
            prefix <- na.omit(prefix)
        }
        if (length(prefix) != 1) {
            stop(
                "All `x` must have the same prefix",
                if (isFALSE(ensure_prefix)) { "or none." } else { "." },
                "Found: ",
                paste0(prefix, collapse = ", ")
            )
        }

        formatted <- stringr::str_remove(x, paste0(prefix, ":", collapse = ""))
        attr(formatted, "prefix") <- prefix
        class(formatted) <- c("bare_id", class(formatted))
    }
    formatted
}


# Gets mesh UIDs from codes (needed to request summaries)
request_mesh_uid <- function(mesh_codes, max_per_iter = 100,
                             ensure_prefix = FALSE) {
    bare_codes <- format_xref(mesh_codes, how = "rm", ensure_prefix = FALSE)
    mc_list <- DO.utils::partition(bare_codes, max_per_iter)
    if (!is_whole_number(max_per_iter)) {
        stop("`max_per_iter` must be a whole number scalar.")
    }
    if (max_per_iter > 1000) {
        stop("`max_per_iter` must be <= 1000.")
    }
    max_per_iter <- as.integer(max_per_iter)

    pb <- utils::txtProgressBar(min = 0, max = length(mc_list), style = 3)
    iter <- 0

    res <- purrr::map(
        mc_list,
        function(mc) {
            res1 <- rentrez::entrez_search(
                db = "mesh",
                term = paste0(paste0(mc, "[MHUI]"), collapse = " OR "),
                retmax = max_per_iter,
                retmode = "xml"
            )

            iter <<- iter + 1
            utils::setTxtProgressBar(pb, value = iter)
            res1
        }
    )
    mesh_uid <- purrr::map(res, ~.x$ids) %>%
        unlist() %>%
        unique()

    n_diff <- length(mesh_codes) - length(mesh_uid)
    if (n_diff > 0) {
        warning("MeSH UIDs could not be found for ", n_diff, " `mesh_codes`.")
        missing_df <- purrr::map2(
            mc_list,
            res,
            ~ tibble::tibble(
                input = length(.x),
                output = length(.y$ids),

            )
        ) %>%
            dplyr::bind_rows() %>%
            dplyr::mutate(
                missing = input != output,
                start = cumsum(dplyr::lag(input, default = 1)),
                end = cumsum(input),
                range = dplyr::if_else(
                    start == end,
                    as.character(start),
                    paste0(start, ":", end)
                )
            ) %>%
            dplyr::filter(missing)

        missing_range <- paste0(missing_df$range, collapse = ",")

        attr(mesh_uid, "missing_range") <- missing_range
    }

    mesh_uid
}


# Gets mesh summary from UIDs
request_mesh_summary <- function(mesh_uids) {
    mesh_uids <- unique(mesh_uids)
    mesh_wh <- rentrez::entrez_post(db = "mesh", id = mesh_uid)
    mesh_summary <- rentrez::entrez_summary(
        db = "mesh",
        web_history = mesh_wh,
        always_return_list = TRUE,
        retmode = "xml"
    )

    mesh_tbl <- mesh_summary %>%
        purrr::map(~`class<-`(.x, "list")) %>%
        tibble::enframe(name = "esummary_id", value = "tmp") %>%
        tidyr::unnest_wider(col = "tmp")

    mesh_tbl
}


##### DATA FILTERS #####

# Warn + save if problems/meaningless data are identified
filter_problematic <- function(df, do_data = NULL, what, gs, ...) {
    ignore <- c("deprecated", "xref_exists")
    err_review <- c("xref_to_multiple", "not_label")
    what <- match.arg(
        what,
        c("all", "ignore", "err_review", ignore, err_review),
        several.ok = TRUE
    )
    if ("all" %in% what) what <- union(what, c(ignore, err_review))
    if ("ignore" %in% what) what <- union(what, ignore)
    if ("err_review" %in% what) what <- union(what, err_review)
    what <- what[!what %in% c("all", "ignore", "err_review")]

    retain <- df
    exclude <- list()
    if ("deprecated" %in% what) {
        if (!"doid" %in% names(retain)) {
            stop("`df` must have `doid` col.")
        }

        do_data_err <- "`do_data` must be provided with `doid` and `deprecated` cols."
        if (missing(do_data)) stop(do_data_err)
        if (!all(c("doid", "deprecated") %in% names(do_data))) stop(do_data_err)

        dep <- dplyr::filter(do_data, deprecated)
        exclude$dep <- dplyr::semi_join(retain, dep, by = "doid") %>%
            dplyr::mutate(reason_removed = "deprecated")
        retain <- dplyr::anti_join(retain, dep, by = "doid")
    }

    if ("xref_exists" %in% what) {
        expect_cols <- c("doid", "xref")
        err_msg <- "`df` and `do_data` must both be provided with `doid` and `xref` cols."
        if (missing(do_data)) stop(err_msg)
        if (!all(expect_cols %in% names(do_data))) stop(err_msg)
        if (!all(expect_cols %in% names(retain))) stop(err_msg)

        exclude$xref_exist <- dplyr::semi_join(retain, do_data, by = expect_cols) %>%
            dplyr::mutate(reason_removed = "xref_exists")
        retain <- dplyr::anti_join(retain, do_data, by = expect_cols)
    }

    if ("xref_to_multiple" %in% what) {
        expect_cols <- c("doid", "xref")
        err_msg <- "`df` must have `doid` and `xref` cols."
        if (!all(expect_cols %in% names(do_data))) stop(err_msg)
        if (!all(expect_cols %in% names(retain))) stop(err_msg)

        exclude$xref_mult <- retain %>%
            dplyr::filter(DO.utils::all_duplicated(xref)) %>%
            dplyr::mutate(reason_removed = "xref_to_multiple")
        retain <- dplyr::anti_join(
            retain,
            exclude$xref_mult,
            by = names(retain)
        )
    }

    if ("not_label" %in% what) {
        expect_cols <- c("doid", "doid_label")
        df_err_msg <- "`df` must be provided with `doid` and `doid_label` cols."
        do_data_err <- "`do_data` must be provided with `doid`, `doid_label`, and `label_type` cols."
        if (missing(do_data)) stop(err_msg)
        if (!all(expect_cols %in% names(do_data))) stop(err_msg)
        if (!all(expect_cols %in% names(retain))) stop(err_msg)

        exclude$not_label <- dplyr::anti_join(retain, do_data, by = expect_cols) %>%
            dplyr::mutate(reason_removed = "not_label")
        retain <- dplyr::semi_join(retain, do_data, by = expect_cols)

        # identify synonyms where labels don't match
        if ("synonym" %in% names(do_data)) {
            extra_col <- "synonym"
            if ("syn_type" %in% names(do_data)) {
                add_type_df <- do_data %>%
                    dplyr::select(doid, synonym, exists_as = syn_type) %>%
                    unique()
            } else {
                add_type_df <- do_data %>%
                    dplyr::select(doid, synonym) %>%
                    dplyr::mutate(exists_as = "synonym-type_unknown") %>%
                    unique()
            }

            exclude$not_label <- dplyr::left_join(
                exclude$not_label,
                add_type_df,
                by = c("doid", "doid_label" = "synonym")
            )
        }
    }

    exclude_df <- dplyr::bind_rows(exclude) %>%
        dplyr::select(reason_removed, dplyr::everything())
    if (nrow(exclude_df) == 0) {
        return(df)
    }

    warning(
        nrow(exclude_df),
        " records with potential errors or duplicate data were found.",
        call. = FALSE
    )

    ignore <- dplyr::filter(exclude_df, reason_removed %in% ignore)
    review <- dplyr::filter(exclude_df, reason_removed %in% err_review)
    if ("exists_as" %in% names(review)) {
        review <- review %>%
            dplyr::select(1:doid_label, exists_as, dplyr::everything())
    }

    if (missing(gs)) {
        out <- list()
        if (nrow(ignore) > 0) out$ignore <- ignore
        if (nrow(review) > 0) out$err_review <- review
        out$other <- retain
    } else {
        out <- retain
        if (nrow(ignore) > 0) googlesheets4::write_sheet(ignore, gs, "ignore")
        if (nrow(review) > 0) googlesheets4::write_sheet(review, gs, "err_review")
    }

    if (nrow(retain) + nrow(ignore) + nrow(review) != nrow(df)) {
        warning("PROBLEMS HAVE OCCURRED!!")
    }

    out
}


# retains rows that are not in another data.frame
retain_rows <- function(df, not_in) {
    dplyr::anti_join(df, not_in, by = names(df))
}


# convert all numbers to lowercase roman numerals, in place
to_roman_lc <- function(x) {
    has_number <- stringr::str_detect(x, "[0-9]+")
    numbers <- stringr::str_extract_all(x, "[0-9]+")
    rn <- purrr::map(
        numbers,
        ~ stringr::str_to_lower(utils::as.roman(.x))
    )
    replace_list <- purrr::map2(
        numbers,
        rn,
        function(num, rn) {
            if (length(num) == 0) {
                NA
            } else {
                pattern <- paste0("(^|[^0-9])", num, "([^0-9]|$)")
                purrr::set_names(rn, num)
            }
        }
    )

    purrr::map2_chr(
        x,
        replace_list,
        function(input, patt_repl) {
            if (all(is.na(patt_repl))) {
                input
            } else {
                stringr::str_replace_all(input, patt_repl)
            }
        }
    )
}



# Get & tidy data ---------------------------------------------------------

r <- DO.utils::DOrepo(repo_path)

# DO data - mesh & omim xrefs, labels
existing_xrefs <- r$doid$query("sparql/DO-xref-w_obs.rq") %>%
    tibble::as_tibble() %>%
    DO.utils::unnest_cross(where(is.list), keep_empty = TRUE) %>%
    dplyr::rename(doid = id, doid_label = label) %>%
    dplyr::mutate(
        ns = stringr::str_remove(xref, ":.*")
    )

existing_omim <- existing_xrefs %>%
    dplyr::filter(ns == "OMIM") %>%
    dplyr::select(doid, doid_omim = xref)

do_labels <- r$doid$query("sparql/DO-synonym-w_dep.rq") %>%
    tibble::as_tibble() %>%
    DO.utils::unnest_cross(where(is.list), keep_empty = TRUE) %>%
    dplyr::rename(doid = id, doid_label = label)


# suggested mesh xrefs by bgyori
suggested_xref <- googlesheets4::read_sheet(
    mesh_biomappings_gs,
    sheet = "suggested_xrefs_bgyori",
    range = "A:J"
) %>%
    janitor::clean_names() %>%
    dplyr::mutate(
        doid = dplyr::if_else(
            source_prefix == "doid",
            source_identifier,
            target_identifier
        ),
        doid_label = dplyr::if_else(
            source_prefix == "doid",
            source_name,
            target_name
        ),
        xref = dplyr::if_else(
            source_prefix == "mesh",
            paste0(stringr::str_to_upper(source_prefix), ":", source_identifier),
            paste0(stringr::str_to_upper(target_prefix), ":", target_identifier)
        ),
        xref_label = dplyr::if_else(
            source_prefix == "mesh",
            source_name,
            target_name
        )
    ) %>%
    dplyr::select(
        dplyr::contains("doid"),
        relation,
        dplyr::contains("xref"),
        dplyr::everything()
    )


# omim xrefs in mesh (using mesh terms from bgyori only via Entrez API)
rentrez::set_entrez_key(keyring::key_get("ENTREZ_KEY"))
mesh_uid <- unique(suggested_xref$xref) %>%
    request_mesh_uid()
mesh_summary <- request_mesh_summary(mesh_uid)
mesh_omim <- mesh_summary %>%
    dplyr::select(xref = DS_MeSHUI, def = DS_ScopeNote) %>%
    dplyr::mutate(
        xref = format_xref(xref, "add", "MESH") %>%
            as.character(),
        xref_omim = stringr::str_extract_all(def, "OMIM:[ 0-9_]+") %>%
            purrr::map_chr(
                ~ stringr::str_remove_all(.x, " ") %>%
                    unique_to_string()
            ) %>%
            dplyr::na_if("") %>%
            dplyr::na_if("NA")
    ) %>%
    dplyr::filter(!is.na(xref_omim)) %>%
    dplyr::select(-def)



# Separate out problematic suggested xrefs ---------------------------------

all_do <- dplyr::full_join(existing_xrefs, do_labels) %>%
    unique()

# Identify general problems
# ignore:
#   - xrefs for deprecated terms
#   - xrefs suggestions that already exist
# set aside for review:
#   - xrefs suggested for multiple DOIDs
#   - xrefs that are not matched on a diseases main label
suggested_other <- filter_problematic(
    suggested_xref,
    all_do,
    'all',
    gs = mesh_biomappings_gs
)

# Identify OMIM mismatches
suggested_omim_compare <- suggested_other %>%
    dplyr::left_join(mesh_omim, by = "xref") %>%
    dplyr::left_join(existing_omim, by = "doid")

omim_mismatch <- suggested_omim_compare %>%
    dplyr::filter(
        !is.na(xref_omim),
        !is.na(doid_omim),
        xref_omim != doid_omim
    ) %>%
    dplyr::select(doid:xref_label, dplyr::ends_with("omim"), review) %>%
    DO.utils::collapse_col(dplyr::ends_with("omim"))

googlesheets4::write_sheet(
    omim_mismatch,
    mesh_biomappings_gs,
    "omim_mismatch"
)

# Retain for further comparison
suggested_other <- dplyr::anti_join(suggested_other, omim_mismatch)


# Identify xrefs that are likely to be correct  ----------------------------

likely <- list()
# differ only by capitalization
suggested_other <- suggested_other %>%
    dplyr::mutate(
        dplyr::across(
            dplyr::ends_with("_label"),
            ~ stringr::str_to_lower(.x),
            .names = "{.col}_lc"
        )
    )

likely$diff_case <- suggested_other %>%
    dplyr::filter(doid_label_lc == xref_label_lc)

suggested_other <- dplyr::anti_join(
    suggested_other,
    likely$diff_case,
    by = names(suggested_other)
)


# differ by spacing (+ case?)
suggested_other <- suggested_other %>%
    dplyr::mutate(
        dplyr::across(
            dplyr::ends_with("_lc"),
            ~ stringr::str_remove_all(.x, "[[:space:]]+"),
            .names = "{.col}_sp"
        )
    )

likely$diff_sp <- suggested_other %>%
    dplyr::filter(doid_label_lc_sp == xref_label_lc_sp) %>%
    dplyr::select(-dplyr::matches("_(lc)$"))

suggested_other <- dplyr::anti_join(suggested_other, likely$diff_sp)


# differ by punctuation (+ case? + spacing?)
suggested_other <- suggested_other %>%
    dplyr::mutate(
        dplyr::across(
            dplyr::ends_with("_sp"),
            ~ stringr::str_remove_all(.x, "[[:punct:]]+"),
            .names = "{.col}_punc"
        )
    )

likely$diff_punc <-  suggested_other %>%
    dplyr::filter(doid_label_lc_sp_punc == xref_label_lc_sp_punc) %>%
    dplyr::select(-dplyr::matches("_(lc|sp)$"))

suggested_other <- dplyr::anti_join(suggested_other, likely$diff_punc)


# differ because of roman numerals (+ case? + spacing? + punc?)
suggested_other <- suggested_other %>%
    dplyr::mutate(
        dplyr::across(
            dplyr::ends_with("_lc"),
            function(.x) {
                to_roman_lc(.x) %>%
                    stringr::str_remove_all("[[:space:]]+") %>%
                    stringr::str_remove_all("[[:punct:]]+")
            },
            .names = "{.col}_rn"
        )
    )

likely$diff_rn <- suggested_other %>%
    dplyr::filter(doid_label_lc_rn == xref_label_lc_rn) %>%
    dplyr::select(-dplyr::matches("_(lc|sp|punc)$"))

suggested_other <- dplyr::anti_join(suggested_other, likely$diff_rn)



# Save data to google sheets ----------------------------------------------

retain_vars <- c("doid", "doid_label", "relation", "xref", "xref_label",
                 "review")

# likely just add
suggested_likely <- dplyr::bind_rows(likely) %>%
    dplyr::select(
        dplyr::one_of(retain_vars),
        dplyr::contains("_label_")
    ) %>%
    tidyr::pivot_longer(
        cols = dplyr::contains("_label_"),
        names_to = c("TMP", "differ_by"),
        names_pattern = "(^[^_]+)_label_(.+)",
        values_to = "val",
        values_drop_na = TRUE
    ) %>%
    tidyr::pivot_wider(
        names_from = TMP,
        values_from = val,
        names_glue = "{TMP}_{.value}"
    ) %>%
    dplyr::mutate(
        differ_by = stringr::str_replace_all(
            differ_by,
            c("lc" = "case", "sp" = "space", "_" = " +/- ")
        ),
        differ_by = dplyr::if_else(
            stringr::str_detect(differ_by, "rn"),
            "case +/- space +/- punc +/- number format",
            differ_by
        )
    )

googlesheets4::write_sheet(
    suggested_likely,
    mesh_biomappings_gs,
    sheet = "differ_by_format"
)

# need to review manually
suggested_other <- dplyr::select(
    suggested_other,
    dplyr::one_of(retain_vars)
)

googlesheets4::write_sheet(
    suggested_other,
    mesh_biomappings_gs,
    sheet = "suggested-review_manually"
)


message("DONE!!!")
