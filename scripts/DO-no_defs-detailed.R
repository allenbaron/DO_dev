# get DO terms missing definitions organized into groups (highest ancestor ALSO
#   is missing definition) --> to speed definition creation by organizing
#   related terms together

library(tidyverse)
library(DO.utils)
library(googlesheets4)

# function to format id/labels as "label (id)"
concat_id_label <- function(.df, .col, .id, .label) {
    dplyr::mutate(
        .df,
        {{ .col }} := dplyr::if_else(
            is.na({{ .label }}) & is.na({{ .id }}),
            NA_character_,
            paste0({{ .label }}, " (", {{ .id }}, ")")
        )
    ) %>%
        dplyr::select(-{{ .label }}, -{{ .id }})
}


# Identify terms without defs + metadata ----------------------------------

r <- DO.utils::DOrepo("~/Documents/Ontologies/HumanDiseaseOntology")

no_def <- r$doid$query("sparql/DO-no_def-common_ancestor.rq") %>%
    tibble::as_tibble() %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
        ancestor_id = dplyr::if_else(
            length(id) > 1 & ancestor_id == id,
            NA_character_,
            ancestor_id
        ),
        ancestor_label = dplyr::if_else(
            length(id) > 1 & ancestor_label == label,
            NA_character_,
            ancestor_label
        )
    ) %>%
    dplyr::ungroup() %>%
    # remove records for terms without parents when parents exist
    dplyr::filter(!(all_duplicated(id) & is.na(ancestor_id))) %>%
    dplyr::add_count(ancestor_id, name = "ancestor_count") %>%
    dplyr::arrange(dplyr::desc(ancestor_count), ancestor_id, id) %>%
    concat_id_label(ancestor, ancestor_id, ancestor_label) %>%
    DO.utils::collapse_col_flex(
        ancestor = "unique",
        ancestor_count = "first",
        delim = " | "
    ) %>%
    dplyr::mutate(ancestor = dplyr::na_if(ancestor, "NA")) %>%
    dplyr::arrange(dplyr::desc(ancestor_count), ancestor, id) %>%
    dplyr::select(-ancestor_count)

parent <- r$doid$query("sparql/DO-id_parent.rq") %>%
    tibble::as_tibble() %>%
    dplyr::rename(doid = id) %>%
    concat_id_label(parent, parent_id, parent_label) %>%
    DO.utils::collapse_col(.cols = parent, delim = " | ")

auth_sources <- c("OMIM", "GARD", "ORDO", "NCI", "MESH", "MEDDRA", "UMLS_CUI")
xref <- r$doid$query("sparql/DO-xref.rq") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(source = stringr::str_remove(xref, ":.*")) %>%
    dplyr::filter(source %in% auth_sources) %>%
    dplyr::mutate(src_fct = factor(source, levels = auth_sources)) %>%
    dplyr::arrange(src_fct) %>%
    dplyr::select(doid = id, xref) %>%
    DO.utils::collapse_col(.cols = xref, delim = " | ")

syn <- r$doid$query("sparql/DO-synonym.rq") %>%
    tibble::as_tibble() %>%
    dplyr::filter(syn_type == "hasExactSynonym") %>%
    dplyr::select(doid = id, synonym) %>%
    DO.utils::collapse_col(.cols = synonym, delim = " | ")



# Read data from google sheet ---------------------------------------------

gs <- "1aHfmvsO5BBPTIWL_mUcxZyEAEJDuoDVrWmbAITzH2MQ"

read_def_gs <- function(sheet) {
    googlesheets4::read_sheet(gs, sheet = sheet, col_types = "c")
}

# 1. NCI/MeSH definitions
defs <- read_def_gs(sheet = "definitions") %>%
    dplyr::select(-`Reviewed, Updated in DO`) %>%
    tidyr::separate(
        col = xref,
        into = c("source", "uid"),
        sep = ":"
    ) %>%
    tidyr::unite(
        col = "xref_def",
        uid, def,
        sep = "_"
    ) %>%
    tidyr::pivot_wider(names_from = source, values_from = xref_def)

def_meta <- defs %>%
    dplyr::select(index, doid) %>%
    DO.utils::collapse_col(.cols = index, delim = " | ")

def_nci <- defs %>%
    dplyr::select(doid, NCI) %>%
    dplyr::filter(!is.na(NCI)) %>%
    DO.utils::collapse_col(.cols = NCI, delim = " | ")

def_mesh <- defs %>%
    dplyr::select(doid, MESH) %>%
    dplyr::filter(!is.na(MESH)) %>%
    DO.utils::collapse_col(.cols = MESH, delim = " | ")

def_tidy <- Reduce(dplyr::left_join, list(def_meta, def_nci, def_mesh))


# 2. already reviewed defs
rev1 <- read_def_gs("reviewed definitions")
rev2 <- read_def_gs("syndromes_allen") %>%
    # remove un-reviewed definitions from list
    dplyr::filter(
        stringr::str_detect(`Reviewed, Updated in DO`, "Lynn|reviewed")
    )

rev_id <- union(rev1$doid, rev2$doid) %>%
    na.omit()




# Drop defs already reviewed & combine all data ---------------------------

def_no_rev <- no_def %>%
    dplyr::filter(!id %in% rev_id) %>%
    dplyr::rename(doid = id, do_name = label)

def_final <- Reduce(
    dplyr::left_join,
    list(def_no_rev, def_tidy, parent, xref, syn)
) %>%
    dplyr::select(
        index, doid, do_name, synonym, xref, parent, ancestor, NCI, MESH
    )



# Write to new "DO-no_defs-detailed" google sheet -------------------------

googlesheets4::write_sheet(
    data = def_final,
    ss = "1mqk-awxnwfuPGiG_c0trwDD34i3Vj1A-Dm0pKB7TrgI",
    sheet = "full_details"
)
