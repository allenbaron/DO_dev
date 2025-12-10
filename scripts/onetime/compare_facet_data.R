# Comparison of data in elasticsearch_input.json (used as input for DO-KB Facet
# Search with output from HumanDiseaseOntology/src/sparql/build/facet-input.rq
# 2025-12-05
library(tidyverse)
library(jsonlite)
library(DO.utils)

# manual inputs -----------------------------------------------------------

elastic_path <- "elasticsearch_input.json"
do_repo_path <- "../Ontologies/HumanDiseaseOntology"



# parsers -----------------------------------------------------------------

# elasticsearch_input.json
elastic_raw <- readr::read_lines(elastic_path)
elastic_out <- elastic_raw[!stringr::str_detect(elastic_raw, stringr::coll('{ "create":{ } }'))]
elastic <- purrr::map_dfr(
    elastic_out,
    function(.x) {
        z <- jsonlite::fromJSON(.x)
        out <- tibble::tibble(
            id = z$id,
            name = z$name,
            definition = z$definition,
            import = dplyr::bind_rows(
                z[!names(z) %in% c("id", "name", "definition")],
                .id = "facet"
            )
        )
    }
) |>
    tidyr::unnest("import", names_sep = "_") |>
    dplyr::select(-"import_id_and_keyword") |>
    dplyr::rename(facet = "import_facet") |>
    dplyr::mutate(
        # dplyr::across(
        #     c("name", "facet". "import_keyword"),
        #     ~ stringr::str_replace_all(.x, c("_" = " ", "@.*$" = ""))
        # ),
        dplyr::across(c("id", "import_id"), ~ str_replace(.x, "_", ":")),
        definition = stringr::str_remove_all(.data$definition, '^"|"$')
    ) |>
    unique()


# facet-input.rq (using doid-merged.owl)
dm_path <- file.path(do_repo_path, "src", "ontology", "doid-merged.owl")
sparql_path <- file.path(do_repo_path, "src", "sparql", "build", "facet-input.rq")

sparql <- DO.utils::robot_query(
    i = dm_path,
    query = sparql_path,
    tidy_what = "everything"
) |>
    unique() #|>
    # dplyr::mutate(
    #     dplyr::across(
    #         c("name", "facet". "import_keyword"),
    #         ~ str_replace_all(.x, "_", " "))
    # )

.df <- list(
    elastic = elastic,
    sparql = sparql
) |>
    dplyr::bind_rows(.id = "src") |>
    dplyr::select(-"definition") |>
    DO.utils::collapse_col(c(src, facet))

dplyr::count(.df, .data$src)
dplyr::count(.df, .data$facet, .data$src) |> print(n = 60)

dplyr::filter(.df, .data$facet != "evidence") |>
    dplyr::count(.data$src)
