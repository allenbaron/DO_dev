# Summarize amount of data in DO each year from during 2019-2021,
# 2023-09-27

library(here)
library(tidyverse)
library(DO.utils)

do_repo_path <- here::here("../Ontologies/HumanDiseaseOntology")
do_repo <- DO.utils::DOrepo(do_repo_path)

# tag corresponding to last release of each year
yr_tags <- c("v2019-12-12", "v2020-12-22", "v2021-12-15")

# capture tags for iteration & counting
tags <- do_repo$tags
release <- list()
tag_nm <- reticulate::iterate(
    tags,
    function(t) {
        if(t$name %in% yr_tags) {
            release[[paste0("v", stringr::str_extract(t$name, "[0-9]{4}"))]] <<- t
        }
        t$name
    }
)

# define queries
q <- list (
    triples = "
        SELECT (COUNT( DISTINCT * ) AS ?triples)
        WHERE { ?s ?p ?o }
    ",
    classes = "
        SELECT (COUNT( DISTINCT ?class ) AS ?classes)
        WHERE { ?class a owl:Class . }
    ",
    active_classes <- "
        SELECT (COUNT( DISTINCT ?class ) AS ?active_classes)
        WHERE {
            ?class a owl:Class .
            FILTER( !isBlank( ?class ) )
            FILTER NOT EXISTS { ?class owl:deprecated ?any . }
        }
    "
)

# define iterator function
tq_iterate <- function(repo, tags, queries) {
    git_head <- repo$capture_head()
    on.exit(repo$git$checkout(git_head))

    if (class(tags) == "character") {
        tags_chr <- tags
        tags <- list()
        reticulate::iterate(
            repo$tags,
            function(t) {
                if (t$name %in% tags_chr) {
                    tags <<- append(tags, t)
                }
            }
        )
    }

    purrr::map(
        tags,
        function(.t) {
            repo$git$checkout(.t)
            repo$doid$load()
            .res <- purrr::map(
                queries,
                ~ DO.utils::tidy_sparql(repo$doid$query(.x))
            )
            dplyr::bind_cols(.res) %>%
            dplyr::mutate(yr = stringr::str_extract(.t$name, "[0-9]{4}"))
        }
    ) %>%
        dplyr::bind_rows()
}


# iterate over tags & execute queries
data_count <- tq_iterate(do_repo, release, q) %>%
    dplyr::mutate(
        releases = purrr::map_int(
            as.character(2019:2021),
            ~ sum(stringr::str_detect(tag_nm, .x))
        ),
        last_release = purrr::map_chr(release, ~ .x$name)
    ) %>%
    dplyr::select(yr, releases, last_release, dplyr::everything())


# Confirm def & def_src count from latest release v2023-08-08
q_def <- "
    SELECT (COUNT(DISTINCT ?def) AS ?n_def) (COUNT( ?src ) AS ?n_src)
    WHERE {
        ?class obo:IAO_0000115 ?def .
        ?anon owl:annotatedSource ?class ;
            owl:annotatedProperty obo:IAO_0000115 ;
            owl:annotatedTarget ?def ;
            oboInOwl:hasDbXref ?src .
        FILTER( CONTAINS( str(?class), 'DOID' ) )
        FILTER NOT EXISTS { ?class owl:deprecated ?any }
    }"

def_count <- tq_iterate(do_repo, "v2023-08-08", q_def)
