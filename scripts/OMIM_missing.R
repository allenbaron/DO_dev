# identify missing OMIM xrefs

library(here)
library(DO.utils)
library(tidyverse)

# paths -- UPDATE AS NECESSARY (always for Google Sheet, gs_output)
omim_tsv <- here::here("DEL_omim.tsv")
de_path <- here::here(
    "../Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl"
)

omim_res <- DO.utils::inventory_omim(de_path, omim_tsv)

# review results
report <- DO.utils::elucidate(omim_res)

# write to google sheets?
continue <- readline("Write to file? yes/no ")

if (continue == "yes") {
    gs_output <- readline("Provide a Google Sheet identifier: ")
    x <- DO.utils::write_gs(data = omim_res, ss = gs_output)
}
