# overlapping numeric local unique identifiers in DO

library(here)
library(DO.utils)
library(tidyverse)

# list all DOIDs (using doid-edit.owl to ensure up-to-date)
de <- DO.utils:::read_doid_edit(
    here::here("../Ontologies/HumanDiseaseOntology/")
)
doid <- tibble::tibble(
    curie = stringr::str_extract_all(de, "DOID[:_][0-9]+") %>%
        unlist() %>%
        stringr::str_replace_all("_", ":") %>%
        unique(),
    n = as.integer(stringr::str_remove(curie, ".*:")),
    curator = dplyr::case_when(
        n < 50000 ~ "pre-2015",
        dplyr::between(n, 50000, 50999) ~ "Lynn",
        dplyr::between(n, 60000, 60999) ~ "Claudia",
        dplyr::between(n, 70000, 70999) ~ "Allen",
        dplyr::between(n, 80000, 85999) ~ "Lynn",
        n >= 100000 ~ "MODs/reserve",
        n > 110000 ~ "Sue",
        TRUE ~ "unassigned"
    )
)


# get do classes and names
do_owl <- owl_xml("../Ontologies/HumanDiseaseOntology/src/ontology/doid.owl")
do_info <- do_owl$query("sparql/DO-id_label-w_dep.rq") %>%
    tidy_sparql()


# evaluate numeric overlap

## current
overlap <- doid %>%
    dplyr::filter(DO.utils::all_duplicated(n)) %>%
    arrange(n, curie)

overlap

## future
future_overlap <- doid %>%
    dplyr::filter(
        !curie %in% overlap$curie,
        stringr::str_detect(curie, "DOID:(0[2-9]|[^0][0-9]{4,})")
    ) %>%
    dplyr::mutate(
        range = n %/% 1000 * 1000,
        w_zeros = stringr::str_pad(
            range,
            width = 7,
            pad = "0",
            side = "left",
            use_width = FALSE
        )
    )

### by current curator
dplyr::count(future_overlap, curator)

### by thousand range (shown +/- leading zeros)
dplyr::count(future_overlap, range, w_zeros)


