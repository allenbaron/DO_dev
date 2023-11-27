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
if (nrow(ps_dep) != 0) {
    rlang::warn("OMIM PS on deprecated DOID: check `ps_dep`")
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
