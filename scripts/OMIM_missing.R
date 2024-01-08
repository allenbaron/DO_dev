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

# drop genes when using search results
if ("omim_search" %in% class(omim_res)) {
    omim_res <- dplyr::filter(omim_res, mim_type != "gene")
}

# review results
omim_report <- DO.utils::inventory_report(omim_res)

# add inventory_report() issue identifiers to inventory as "status"
with_status <- dplyr::bind_rows(
    omim_report[names(omim_report) != "stats"],
    .id = "status"
) %>%
    dplyr::select(omim, doid, status) %>%
    DO.utils::collapse_col(status, delim = " | ", na.rm = TRUE)

if (nrow(with_status) > 0) {
    omim_res <- dplyr::full_join(
        omim_res,
        with_status,
        by = c("omim", "doid")
    )
}

# write to google sheets?
continue <- readline("Write to file? yes/no ")

if (continue == "yes") {
    gs_output <- readline("Provide a Google Sheet identifier: ")
    x <- DO.utils::write_gs(data = omim_res, ss = gs_output)
}
