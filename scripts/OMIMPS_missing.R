# identify missing OMIM PS

library(here)
library(DO.utils)

# file locations
ps_titles_file <- here::here("data/mapping/src/OMIM-Phenotypic-Series-Titles-all.tsv")
de_file <- here::here("../Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl")

# load official PS file download
ps <- DO.utils::read_omim(ps_titles_file)

# inventory OMIM PS
ps_inventory <- DO.utils::inventory_omim(de_file, ps)

# check if OMIMPS on deprecated DOID
ps_report <- DO.utils::inventory_report(ps_inventory)

# add inventory_report() issue identifiers to inventory as "status"
with_status <- dplyr::bind_rows(
    ps_report[names(ps_report) != "stats"],
    .id = "status"
) %>%
    dplyr::select(omim, doid, status) %>%
    DO.utils::collapse_col(status, delim = " | ", na.rm = TRUE)

if (nrow(with_status) > 0) {
    ps_inventory <- dplyr::full_join(
        ps_inventory,
        with_status,
        by = c("omim", "doid")
    )
}

# write inventory results to google sheets?
continue <- readline("Write to file? yes/no ")

if (continue == "yes") {
    # current OMIM PS inventory: https://docs.google.com/spreadsheets/d/1xwvVtnaNlLLuLregkOUNbNpu5maa0AYmFJsGSCfM3mY/edit#gid=0
    gs_output <- readline("Provide a Google Sheet identifier: ")

    DO.utils::write_gs(
        ps_inventory,
        gs_output
    )
}
