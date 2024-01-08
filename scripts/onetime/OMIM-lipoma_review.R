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
        values_fn = unique_to_string
    ) %>%
    dplyr::select(
        id, label, def, hasAlternativeId, top, subClassOf, inSubset,
        sc_anon, equivalentClass, dplyr::everything()
    ) %>%
    dplyr::select(!class) %>%
    dplyr::arrange(top, id)

# replace top ID with label for easier identification
top_recode <- dplyr::filter(lipoma, top == id) %>%
    { setNames(.$label, .$id) }

lipoma <- dplyr::mutate(lipoma, top = dplyr::recode(top, !!!top_recode))


# split dataset to enhance info and reduce repetition
lipoma_list <- list(
    details = dplyr::select(lipoma, id:equivalentClass),
    xrefs = dplyr::select(lipoma, id, label, top, hasDbXref, exactMatch) %>%
        DO.utils::lengthen_col(c(hasDbXref, exactMatch), convert = FALSE) %>%
        tidyr::pivot_longer(
            cols = c(hasDbXref, exactMatch),
            names_to = "mapping_type",
            values_to = "mapping"
        ) %>%
        DO.utils::collapse_col(mapping_type, na.rm = TRUE) %>%
        dplyr::filter(!is.na(mapping)) %>%
        dplyr::mutate(
            mapping2 = dplyr::if_else(
                stringr::str_detect(mapping, "^(GARD|ICDO|SNOMEDCT)"),
                mapping,
                NA_character_
            ),
            mapping = dplyr::if_else(
                stringr::str_detect(mapping, "^(GARD|ICDO|SNOMEDCT)"),
                NA,
                DO.utils::build_hyperlink(
                    x = stringr::str_remove(mapping, ".*:"),
                    url = stringr::str_remove(mapping, ":.*"),
                    text = mapping,
                    as = "gs"
                )
            )
        ) %>%
        dplyr::arrange(top, id, mapping, mapping2),
    synonyms = dplyr::select(lipoma, id, label, top, hasExactSynonym) %>%
        DO.utils::lengthen_col(hasExactSynonym, convert = FALSE) %>%
        dplyr::filter(!is.na(hasExactSynonym)) %>%
        dplyr::arrange(top, id, hasExactSynonym)
)

# write to google sheet
gs <- "https://docs.google.com/spreadsheets/d/17fK2ZXXxbjip6kLOB3MzlcDpegwvN8CGbDVMdG_318s/edit#gid=0"

purrr::walk2(
    lipoma_list,
    names(lipoma_list),
    ~ googlesheets4::write_sheet(.x, ss = gs, sheet = .y)
)
