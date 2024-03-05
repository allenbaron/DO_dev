# add exact xrefs identified by SciBite via robot template

library(googlesheets4)
library(DO.utils)
library(tidyverse)
library(here)

sb_review_gs <- "1DFEqe1_jDgQNuJjgx-uIZUicNxHXI0br6DR669-F8Z8"
simple_add_gs <- "1mx9TtRndGDb8dqUGIKJfJJcbfVzjqb7TADJzTRWS7rc"
review_gs <- "1bFuVPc4DuZcpUq1az_8r7Lk0OfiK0uZj2M-0HPOAkls"
de_path <- "~/Documents/Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl"


# Supporting function(s) --------------------------------------------------

# merges a template into an input ontology file
rt_merge <- function(input, template, format = "ofn") {
    if (is.data.frame(template)) {
        temp <- tempfile(fileext = ".csv")
        on.exit(unlink(temp))
        readr::write_csv(template, temp)
    } else {
        if (!file.exists(template)) {
            rlang::abort("template must be the path to a ROBOT template or a properly formatted data.frame")
        }
        temp <- template
    }

    temp_out <- tempfile(fileext = ".owl")
    on.exit(unlink(temp_out), add = TRUE)
    DO.utils::robot(
        "template",
        "--merge-before",
        i = input,
        template = temp,
        o = temp_out
    )

    DO.utils::robot(
        "convert",
        i = temp_out,
        o = input,
        format = format
    )

    invisible(input)
}

# get A1 position for google sheets formula
gs_rc <- function(df, col_nm) {
    col_n <- which(names(df) == col_nm)
    col_id <- LETTERS[col_n]
    paste0(col_id, 1:nrow(df))
}

def_fmla <- function(df) {
    df <- dplyr::pick(dplyr::everything())
    nm <- names(df)

    plabel_id <- LETTERS[which(nm == "parent_label")]
    chr_by_id <- LETTERS[which(nm == "characterized_by")]
    gen_id <- LETTERS[which(nm == "genetic_basis")]

    row_n <- 1:nrow(df) + 1

    plabel_rc <- paste0(plabel_id, row_n)
    chr_by_rc <- paste0(chr_by_id, row_n)
    gen_rc <- paste0(gen_id, row_n)

    fmla <-  paste0(
        '=concatenate("An ", ', plabel_rc, ', ',
        '" characterized by ", ', chr_by_rc, ', ',
        '" that has_material_basis_in ", ', gen_rc, ')'
    )

    googlesheets4::gs4_formula(fmla)
}

# appends the Google sheets filename & sheet name (put in together as sheet_nm)
# to a column so all sheets where curation is taking place are noted
note_curate_in <- function(x, test, sheet_nm) {
    dplyr::case_when(
        test & is.na(x) ~ sheet_nm,
        test ~ paste(x, sheet_nm, sep = " | "),
        TRUE ~ NA_character_
    )
}

# Read in review of Scibite's OMIM-DO exact mappings ----------------------

sb_exact <- googlesheets4::read_sheet(
    sb_review_gs,
    "omim_inventory-exact",
    col_types = "cccclcclcccccccccc"
) %>%
    dplyr::mutate(
        # convert xref col to chr from list (lgl + chr col read in as list)
        add_xref = as.character(add_xref),
        # change TRUE to doid > omim
        add_xref = dplyr::case_when(
            add_xref == "NULL" ~ NA_character_,
            add_xref == "TRUE" ~ paste(doid_scibite, omim, sep = " > "),
            TRUE ~ add_xref
        )
    )


# XREFS -------------------------------------------------------------------

xref_curate_note <- "xref: Scibite_template-simple_add > omim-exact-xref"
sb_exact_out <- sb_exact %>%
    dplyr::mutate(
        curate_in = dplyr::if_else(
            !is.na(add_xref),
            xref_curate_note,
            NA_character_
        )
    )

xref_add <- sb_exact_out %>%
    dplyr::filter(stringr::str_detect(curate_in, xref_add_curate_sheet)) %>%
    dplyr::select(add_xref, DO_review)


xref_rt <- xref_add %>%
    DO.utils::lengthen_col(add_xref, delim = "|") %>%
    tidyr::separate(add_xref, c("iri", "xrefs"), sep = " > ", remove = TRUE) %>%
    dplyr::mutate(
        xrefs = stringr::str_replace_all(xrefs, " ?, ?", "|"),
        skos_exact = xrefs
    ) %>%
    dplyr::relocate(skos_exact, .before = DO_review) %>%
    tibble::add_row(
        iri = "ID",
        xrefs = "A oboInOwl:hasDbXref SPLIT=|",
        skos_exact = "A skos:exactMatch SPLIT=|",
        DO_review = NA_character_,
        .before = 1
    )

# write to Scibite_template-simple_add google sheet
googlesheets4::write_sheet(
    data = xref_rt,
    ss = simple_add_gs,
    sheet = "omim-exact-xref"
)

# add to doid-edit.owl via robot template
rt_merge(de_path, dplyr::select(xref_rt, iri:skos_exact))


# SUBTYPES/DISEASES -------------------------------------------------------

rt_curation_headers <- c(
    status = NA,
    curation_notes = NA,
    link = NA,
    genetic_basis = NA,
    characterized_by = NA,
    iri = "ID",
    obo_id = "A oboInOwl:id",
    parent_label = NA,
    parent_curie = "SC %",
    label = "AL rdfs:label@en",
    def = "AL obo:IAO_0000115@en",
    def_src = ">A oboInOwl:hasDbXref SPLIT=|",
    def_type = ">AI dc11:type SPLIT=|",
    syn_exact = "AL oboInOwl:hasExactSynonym@en SPLIT=|",
    xrefs = "A oboInOwl:hasDbXref SPLIT=|",
    skos_exact = "A skos:exactMatch SPLIT=|",
    namespace = "A oboInOwl:hasOBONamespace",
    sc_axiom_INHERITANCE = "SC 'has material basis in' some %",
    sc_axiom_ANATOMY = "SC 'disease has location' some %",
    sc_axiom_ONSET = "SC 'has material basis in' some %"
)

disease_add <- sb_exact %>%
    dplyr::filter(!is.na(add_subtype) | !is.na(add_disease))

disease_rt <- disease_add %>%
    tidyr::separate(
        omim_title,
        into = c("label", "syn_exact"),
        sep = "; ",
        fill = "right"
    ) %>%
    dplyr::mutate(
        label = stringr::str_to_lower(
            stringr::str_replace(label, "([^,]+), (.+)", "\\2 \\1")
        ),
        parent_curie = dplyr::if_else(add_subtype, doid_scibite, NA_character_),
        parent_label = dplyr::if_else(
            !is.na(parent_curie),
            do_label_scibite,
            NA_character_
        )
    ) %>%
    dplyr::select(
        scibite_review_notes = DO_review, label, parent_curie, parent_label,
        syn_exact, xrefs = omim
    ) %>%
    DO.utils::append_empty_col(
        append(
            names(rt_curation_headers),
            "scibite_review_notes",
            after = 1
        ),
        order = TRUE
    ) %>%
    dplyr::mutate(
        link = DO.utils::build_hyperlink(
            x = stringr::str_remove(xrefs, "OMIM:"),
            url = "OMIM",
            text = xrefs,
            as = "gs"
        ),
        def = def_fmla()
    ) %>%
    tibble::add_row(!!!rt_curation_headers, .before = 1) %>%
    dplyr::mutate(dplyr::across(c(link, def), ~ googlesheets4::gs4_formula(.x)))

# write to Scibite_template-simple_add google sheet
googlesheets4::write_sheet(
    data = disease_rt,
    ss = simple_add_gs,
    sheet = "omim-exact-disease"
)


# CHANGES NEEDING REVIEW --------------------------------------------------

sb_nr <- sb_exact %>%
    dplyr::filter(
        stringr::str_detect(
            DO_review,
            stringr::coll("needs review", ignore_case = TRUE)
        )
    ) %>%
    dplyr::select(dplyr::where(~ !DO.utils::is_invariant(.x)))


# EXTRA -------------------------------------------------------------------

rt_header <- c(
    iri = "ID",
    obo_id = "A oboInOwl:id",
    parent_curie = "SC %",
    label = "AL rdfs:label@en",
    def = "AL obo:IAO_0000115@en",
    def_src = ">A oboInOwl:hasDbXref SPLIT=|",
    def_type = ">AI dc11:type SPLIT=|",
    syn_exact = "AL oboInOwl:hasExactSynonym@en SPLIT=|",
    xrefs = "A oboInOwl:hasDbXref SPLIT=|",
    skos_exact = "A skos:exactMatch SPLIT=|",
    namespace = "A oboInOwl:hasOBONamespace",
    sc_axiom_INHERITANCE = "SC 'has material basis in' some %",
    sc_axiom_ANATOMY = "SC 'disease has location' some %",
    sc_axiom_ONSET = "SC 'has material basis in' some %"
)

curation_header <- c(
    status = NA, curation_notes = NA, link = NA,
    genetic_basis = NA, characterized_by = NA, parent_label = NA
)
