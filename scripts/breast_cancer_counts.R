# collect DO stats related to breast cancer for tweet
# J. Allen Baron
# 2021-10-22

library(here)
library(DO.utils)
library(tidyverse)
library(ontologyIndex)

# Load doid.obo data (hard-coded, relative path) --------------------------
do_obo_path <- here::here(
    "../Ontologies/HumanDiseaseOntology/src/ontology/doid.obo"
)

do_obo <- ontologyIndex::get_ontology(
    do_obo_path, # checked out release: v2021-10-11
    propagate_relationships = "is_a",
    extract_tags = "minimal"
)


# Get Alliance Disease Data -----------------------------------------------
alliance_disease_tsv <- DO.utils::download_alliance_tsv(here::here())
alliance_dis <- DO.utils::read_alliance(alliance_disease_tsv)


# Get MNDR data -----------------------------------------------------------
mndr_disease_url <- "https://www.rna-society.org/mndr/download/All%20ncRNA-disease%20information.zip"
mndr_disease_file <- here::here("data-raw/MNDR_disease_all_ncRNA-RAW.tsv.zip")
if (!file.exists(mndr_disease_file)) {
    download.file(mndr_disease_url, mndr_disease_file)
}

# the downloaded file contains extra tabs in it in varying locations -->
#   read in, strip leading/trailing tabs, write to tsv file
mndr_file_fixed <- here::here("data-raw/MNDR_disease_all_ncRNA.tsv")
readr::read_lines(mndr_disease_file) %>%
    stringr::str_replace_all(c("^\t+" = "", "\t+$" = "")) %>%
    readr::write_lines(mndr_file_fixed)

mndr_dis <- readr::read_tsv(mndr_file_fixed) %>%
    dplyr::rename(DOID = "DO ID") %>%
    dplyr::rename_with(.fn = ~stringr::str_replace_all(.x, " ", "_"))


# Count alliance data related to breast cancer ----------------------------
bc_doid <- ontologyIndex::get_descendants(
    do_obo,
    roots = "DOID:1612"
)

alliance_bc_count <- DO.utils::count_alliance_records(
    alliance_dis,
    term_subset = bc_doid,
    record_lvl = "object"
)

write_csv(alliance_bc_count, "Alliance-breast_cancer_object_count.csv")

# Count MNDR data related to breast cancer --------------------------------
mndr_bc <- dplyr::filter(mndr_dis, DOID %in% bc_doid)

mndr_bc_count <- mndr_bc %>%
    dplyr::select(ncRNA_symbol, ncRNA_Category, Species) %>%
    unique() %>%
    dplyr::count(Species, ncRNA = ncRNA_Category) %>%
    tidyr::pivot_wider(
        names_from = ncRNA,
        names_glue = "{ncRNA}_n",
        values_from = n
    )

write_csv(mndr_bc_count, "MNDR-breast_cancer_object_count.csv")
