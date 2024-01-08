library(here)
library(tidyverse)
library(DO.utils)
library(googlesheets4)

res_file <- tempfile(".csv")
robot(
    "query",
    i = here::here("../Ontologies/HumanDiseaseOntology/src/ontology/doid.owl"),
    query = here::here("sparql/limited/lipoma_tosis.rq"),
    res_file
)
res <- readr::read_csv(res_file)

lipoma <- dplyr::mutate(
    res,
    dplyr::across(
        dplyr::everything(),
        DO.utils::to_curie
    ),
    pred = stringr::str_replace_all(pred, c("IAO:0000115" = "def", ".*:" = "")),
    pred = dplyr::if_else(
        !stringr::str_starts(obj, "DOID") & pred == "subClassOf",
        "sc_anon",
        pred
    )
) %>%
    dplyr::filter(
        !pred %in% c("type", "hasOBONamespace", "comment", names(res))
    ) %>%
    tidyr::pivot_wider(
        names_from = pred,
        values_from = obj,
        values_fn = ~ unique_to_string(.x, na.rm = TRUE, sort = TRUE)
    ) %>%
    dplyr::select(
        id, label, def, hasAlternativeId, top, subClassOf, inSubset,
        sc_anon, equivalentClass, dplyr::everything()
    ) %>%
    dplyr::select(!class)

# replace top ID with label for easier identification
top_recode <- dplyr::filter(lipoma, top == id) %>%
    { setNames(.$label, .$id) }

lipoma <- dplyr::mutate(lipoma, top = dplyr::recode(top, !!!top_recode)) %>%
    dplyr::arrange(top, id)


# merge xref + exactMatch into mapping (don't split xref & synonym)
lipoma <- lipoma %>%
    DO.utils::lengthen_col(
        cols = c("hasDbXref", "exactMatch"),
        trim = TRUE,
        convert = FALSE
    ) %>%
    tidyr::pivot_longer(
        cols = c("hasDbXref", "exactMatch"),
        names_to = "mapping_type",
        values_to = "mapping"
    ) %>%
    dplyr::select(!mapping_type) %>%
    dplyr::arrange(top, id, mapping) %>%
    DO.utils::collapse_col(mapping, na.rm = TRUE) %>%
    dplyr::arrange(top, id)

# write to google sheet
gs <- "https://docs.google.com/spreadsheets/d/17fK2ZXXxbjip6kLOB3MzlcDpegwvN8CGbDVMdG_318s/edit#gid=0"

googlesheets4::write_sheet(lipoma, ss = gs, sheet = "DO_data_w_manual_changes")
