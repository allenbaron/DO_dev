# compare before and after counts of DO_rare_slim

library(here)
library(DO.utils)
library(tidyverse)

# before and after updating slims be sure to run:
#   robot reason -i src/ontology/doid-edit.owl -o "DEL-{before|after}.owl"
repo_path <- here::here("../Ontologies/HumanDiseaseOntology")
file_id <- c("DEL_before.owl", "DEL_after.owl") # DEL to remind you to delete them

slim_id <- c("DO_infectious_disease_slim", "DO_rare_slim")
slim_col <- c("infect_branch", "xref_rare")


owl_paths <- file.path(repo_path, file_id)

owl <- purrr::map(owl_paths, owl_xml)

slim_query <- DO.utils:::glueV("
    SELECT ?id ?label
        (GROUP_CONCAT(?slim; SEPARATOR='|') AS ?slims)
        (GROUP_CONCAT(?branch; SEPARATOR='|') AS ?branches)
        (GROUP_CONCAT(?xref; SEPARATOR='|') AS ?xrefs)
    WHERE {
        ?class a owl:Class ;
            oboInOwl:id ?id ;
            rdfs:label ?label ;
            rdfs:subClassOf* ?branch_iri .
        ?branch_iri rdfs:subClassOf obo:DOID_4 ;
            rdfs:label ?branch .

        FILTER NOT EXISTS { ?class owl:deprecated ?any . }
        OPTIONAL { ?class oboInOwl:hasDbXref ?xref . }
        OPTIONAL {
            ?class oboInOwl:inSubset ?slim_iri .
            FILTER(REGEX(str(?slim), '!<<slim_regex>>!'))
            BIND(CONCAT('doid:', STRAFTER(str(?slim_iri), '#')) AS ?slim)
        }
    } GROUP BY ?id ?label",
    slim_regex = DO.utils::vctr_to_string(slim_id, "|")
)

res <- purrr::map(
    owl,
    ~ .x$query(slim_query) %>%
        tidy_sparql() %>%
        dplyr::mutate(
            infect_branch = stringr::str_detect(branches, "infectious"),
            xref_rare = stringr::str_detect(xrefs, "ORDO|GARD")
        )
) %>%
    purrr::set_names(nm = tools::file_path_sans_ext(basename(owl_paths))) %>%
    dplyr::bind_rows(.id = "file_id")

slim_tidy <- purrr::map2(
    slim_id,
    slim_col,
    ~ res %>%
        dplyr::mutate(
            slim = dplyr::if_else(
                stringr::str_detect(slims, .x),
                .x,
                NA_character_
            )
        ) %>%
        dplyr::select(file_id, id, label, slim, dplyr::all_of(.y))
) %>%
    purrr::set_names(slim_id)

slim_added <- purrr::map2(
    slim_tidy,
    slim_col,
    ~ .x %>%
        tidyr::pivot_wider(names_from = file_id, values_from = slim) %>%
        dplyr::filter(is.na(DEL_before), !is.na(DEL_after)) %>%
        dplyr::arrange(id)
)

slim_counts <- purrr::map2(
    slim_tidy,
    slim_col,
    ~ .x %>%
        dplyr::count(file_id, slim, .data[[.y]]) %>%
        dplyr::arrange(dplyr::desc(file_id), slim, dplyr::desc(.data[[.y]])) %>%
        tidyr::pivot_wider(names_from = file_id, values_from = n)
)

slim_counts
