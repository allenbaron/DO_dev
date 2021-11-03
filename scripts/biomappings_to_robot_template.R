# script to convert predicted & reviewed biomappings results to a ROBOT template
#   for curator review (https://github.com/biopragmatics/biomappings)
# J. Allen Baron, 2021-10-20


# SPECIFY paths -----------------------------------------------------------

# 1. folder with biomappings resources
bm_resources_dir <- "/Users/allenbaron/Documents/FORKS/biomappings/src/biomappings/resources/"

# 2. HumanDiseaseOntology repo
do_repo <- "/Users/allenbaron/Documents/Ontologies/HumanDiseaseOntology"


# NOTES TO SELF -----------------------------------------------------------
# To run the DOID python script in biomappings the following modules are needed:
#   pip install -e .
#   pip install indra gilda pyobo pronto
library(tidyverse)
library(janitor)

read_clean_names <- function(.path) {
    readr::read_tsv(.path) %>%
        janitor::clean_names()
}


# Merge & tidy data -------------------------------------------------------

predicted <- read_clean_names(file.path(bm_resources_dir, "predictions.tsv"))
reviewed <- read_clean_names(file.path(bm_resources_dir, "mappings.tsv")) %>%
    dplyr::mutate(type = "reviewed-CONFIRMED")
unsure <- read_clean_names(file.path(bm_resources_dir, "unsure.tsv")) %>%
    dplyr::mutate(type = "reviewed-UNSURE")

merged <- dplyr::bind_rows(predicted2, reviewed, unsure)

# all DO xref/skos prefixes (placeholder; some are not accessed by biomappings)
all_do_xref_prefix <- c("EFO", "GARD", "ICD10CM", "ICD9CM", "ICDO", "KEGG",
                        "MEDDRA", "MESH", "NCI", "OMIM", "ORDO", "url",
                        "UMLS_CUI", "SNOMEDCT_US_2018_03_01",
                        "SNOMEDCT_US_2019_09_01", "SNOMEDCT_US_2020_03_01",
                        "SNOMEDCT_US_2020_09_01", "SNOMEDCT_US_2021_03_01")
bm_do_xref_prefix <- c(efo = "EFO", mesh = "MESH", ncit = "NCI",
                       umls = "UMLS_CUI",
                       # new to DO (HP & MONDO will not be added)
                       ido = "IDO", hp = "HP", mondo = "MONDO")

tidy_doid_mappings <- dplyr::bind_rows(
    dplyr::filter(merged, source_prefix == "doid"),
    dplyr::filter(merged, target_prefix == "doid") %>%
        dplyr::rename_with(
            .fn = ~dplyr::if_else(
                stringr::str_detect(.x, "source"),
                stringr::str_replace(.x, "source", "target"),
                stringr::str_replace(.x, "target", "source")
            ),
            .cols = dplyr::matches("(source|target).+")
        )
) %>%
    dplyr::filter(source_prefix == "doid") %>%
    dplyr::mutate(
        source_identifier = dplyr::if_else(
            stringr::str_detect(source_identifier, "^DOID:"),
            source_identifier,
            paste0("DOID:", source_identifier)
        ),
        target_identifier = dplyr::if_else(
            stringr::str_detect(target_identifier, ":"),
            paste0(
                dplyr::recode(target_prefix, !!!bm_do_xref_prefix), ":",
                stringr::str_remove(target_identifier, ".*:")
            ),
            paste0(
                dplyr::recode(target_prefix, !!!bm_do_xref_prefix), ":",
                target_identifier
            )
        )
    )


# Save HP data ------------------------------------------------------------

# get exclude-hp.txt & not if HP ID in that file
exclude_hp <- readr::read_lines(
    file.path(do_repo, "src/ontology/imports/exclude-hp.txt")
)

hp_only <- dplyr::filter(tidy_doid_mappings, target_prefix == "hp") %>%
    dplyr::select(-source_prefix, -target_prefix) %>%
    dplyr::mutate(in_exclude_file = target_identifier %in% exclude_hp) %>%
    dplyr::arrange(!in_exclude_file, source_identifier, desc(confidence),
                   target_identifier) %>%
    tidyr::pivot_wider(names_from = relation, values_from = target_identifier) %>%
    dplyr::relocate(target_name:source, .after = dplyr::last_col())

write_csv(hp_only, "biomappings-DOID_HP-20211020.csv")


# Save MONDO data ---------------------------------------------------------

mondo_only <- dplyr::filter(tidy_doid_mappings, target_prefix == "mondo") %>%
    dplyr::select(-source_prefix, -target_prefix) %>%
    dplyr::arrange(desc(confidence), source_identifier, target_identifier) %>%
    tidyr::pivot_wider(names_from = relation, values_from = target_identifier) %>%
    dplyr::relocate(target_name:source, .after = dplyr::last_col())

write_csv(mondo_only, "biomappings-DOID_MONDO-20211020.csv")


# Remainder as ROBOT template ---------------------------------------------

biomappings_robot <- tidy_doid_mappings %>%
    dplyr::filter(!target_prefix %in% c("hp", "mondo")) %>%
    dplyr::select(-source_prefix, -target_prefix)

# identify types of curation & roughly order
types <- unique(biomappings_robot$type)
types_reviewed <- stringr::str_detect(types, "reviewed")
type_lvls <- c(sort(types[types_reviewed]), sort(types[!types_reviewed]))

biomappings_robot <- biomappings_robot %>%
    # drop prediction duplicates (see biomappings issue #73)
    dplyr::mutate(
        dup = all_duplicated(dplyr::select(biomappings_robot, !(type:source)))
    ) %>%
    dplyr::filter( !(dup & !is.na(confidence)) ) %>%
    dplyr::select(-dup) %>%
    # order type by most likely to be correct
    dplyr::mutate(type = factor(type, levels = type_lvls)) %>%
    dplyr::arrange(type, desc(confidence), source_identifier,
                   target_identifier) %>%
    # add all skos matches to xref column
    dplyr::mutate("A oboInOwl:hasDbXref" = target_identifier) %>%
    # split skos matches by type (if any)
    tidyr::pivot_wider(
        names_from = relation,
        names_prefix = "A ",
        values_from = target_identifier
    ) %>%
    dplyr::relocate(target_name:source, .after = dplyr::last_col()) %>%
    dplyr::rename(ID = source_identifier, "DO_label*" = source_name,
                  "xref_label*" = target_name, "type*" = type,
                  "confidence*" = confidence, "source*" = source)

# create informative headers for ROBOT
bm_robot_file <- "biomappings-DOID_xrefs-20211020.csv"

names(biomappings_robot) %>%
    stringr::str_remove(".*:(has)?") %>%
    c(., "* columns for convenience -> remove before running ROBOT template'") %>%
    vctr_to_string(delim = ",") %>%
    readr::write_lines(bm_robot_file)

# save data
readr::write_csv(
    biomappings_robot,
    bm_robot_file,
    append = TRUE,
    col_names = TRUE
)
