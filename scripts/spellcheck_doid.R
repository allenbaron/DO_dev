# spell check DO definitions

library(here)
library(tidyverse)
library(DO.utils)
library(spelling)


# File paths --------------------------------------------------------------

de_path <- here::here(
    "../Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl"
)


# Custom functions --------------------------------------------------------

# convert numeric df index to ID-{str_type}
index_to_info <- function(index) {
    do_str[index, ] %>%
        dplyr::select(id, str_type) %>%
        DO.utils::collapse_col(str_type, delim = ",") %>%
        with(paste(id, str_type, sep = "-")) %>%
        DO.utils::collapse_to_string(delim = " | ")
}


# Dictionaries ------------------------------------------------------------

dict_paths <- list.files(
    here::here("data/spelling"),
    pattern = ".*\\.txt",
    full.names = TRUE
)

dicts <- purrr::map(dict_paths, readr::read_lines) %>%
    purrr::set_names(tools::file_path_sans_ext(basename(dict_paths)))
dict <- dicts %>%
    unlist() %>%
    unique()


# doid.owl data -----------------------------------------------------------

# convert & load doid-edit.owl --> better for spelling corrections
de_owl_path <- tempfile(fileext = ".owl")
DO.utils::robot(
    "convert",
    i = de_path,
    o = de_owl_path
)

de_owl <- DO.utils::owl_xml(de_owl_path)

do_str <- de_owl$query(
    "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>

    SELECT ?id ?str_type ?str
    WHERE {
        ?class oboInOwl:id ?id ;
            ?str_type ?str .
        FILTER ( datatype(?str) = xsd:string )
        FILTER (
            !REGEX (
                str( ?str_type ),
                'oboInOwl#(id|hasAlternativeId|hasDbXref|creation_date|hasOBONamespace)|skos'
            )
        )
    }"
) %>%
    DO.utils::tidy_sparql() %>%
    dplyr::mutate(
        str_type = stringr::str_replace_all(
            str_type,
            c("http://www.geneontology.org/formats/oboInOwl#" = "oboInOwl:",
              "http://purl.obolibrary.org/obo/" = "obo:",
              "http://www.w3.org/2000/01/rdf-schema#" = "rdfs:"
            )
        ),
        info = paste(id, str_type, sep = "-")
    )



# Spell check results -----------------------------------------------------

spell_res <- spelling::spell_check_text(do_str$str, ignore = dict) %>%
    tibble::as_tibble()

abbrev_review <- spell_res %>%
    dplyr::filter(stringr::str_detect(word, "[A-Z0-9]{3,}"))

word_review <- spell_res %>%
    dplyr::filter(!word %in% abbrev_review$word) %>%
    dplyr::mutate(found = purrr::map(found, index_to_info))
