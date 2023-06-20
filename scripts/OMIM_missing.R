# identify missing OMIM xrefs

library(here)
library(DO.utils)
library(tidyverse)

# paths -- UPDATE AS NECESSARY (always for Google Sheet, gs_output)
omim_csv <- here::here("DEL_omim.csv")
de_path <- here::here(
    "../Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl"
)
gs_output <- "1eVPhphVZE5cQG4i2bTBjRlnKwAepuPP_9o8Xu53YtUU"
timestamp <- format(Sys.Date(), "%Y%m%d") # appended to end of sheet names

omim_res <- DO.utils::onto_missing(de_path, omim_csv)

# check for obsolete DO terms with OMIM xrefs
inDO_dep <- omim_res$in_onto %>% dplyr::filter(dep)

if (nrow(inDO_dep) > 0) {
    rlang::abort(
        c(
            paste0(
                "Obsolete diseases with OMIM xrefs: ",
                dplyr::n_distinct(inDO_dep$id)
            ),
            "Pause to examine and possibly fix with dep_fix!!!"
        )
    )
    dep_fix <- dplyr::filter(do_omim, omim %in% inDO_dep$omim)
    scan()
}

# save
if (nrow(omim_res$missing) > 0) {
    missing <- omim_res$missing %>%
        dplyr::mutate(
            omim = DO.utils::build_hyperlink(
                x = stringr::str_remove(omim, ".*:"),
                url = "OMIM",
                as = "gs",
                txt = omim,
                preserve = "txt"
            )
        )
    googlesheets4::write_sheet(
        data = missing,
        ss = gs_output,
        sheet = paste0("missing-", timestamp)
    )
} else{
    message("No OMIM from this PS are new.")
}

if (nrow(omim_res$in_onto) > 0) {
    in_onto <- omim_res$in_onto %>%
        dplyr::mutate(
            id = DO.utils::build_hyperlink(
                x = id,
                url = "DO_website",
                as = "gs",
                txt = id,
                preserve = "txt"
            ),
            mapping = DO.utils::build_hyperlink(
                x = stringr::str_remove(mapping, ".*:"),
                url = stringr::str_remove(mapping, ":.*"),
                as = "gs",
                txt = mapping,
                preserve = "txt"
            ),
        )
    googlesheets4::write_sheet(
        data = in_onto,
        ss = gs_output,
        sheet = paste0("in_onto-", timestamp)
    )
} else {
    message("All OMIM in this PS are new!!")
}
