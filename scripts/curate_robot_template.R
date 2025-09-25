# INITIAL TEST: create true ROBOT template from curation long-form template

library(here)
library(googlesheets4)
library(tidyverse)
library(DO.utils)

de <- here::here("../Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl")

gs_rt_recode <- "https://docs.google.com/spreadsheets/d/1Zn6p5xkVHUwbWe1N8FUa3fNcEkAOoE9P4ADb12f69hQ/edit"

gs <- "https://docs.google.com/spreadsheets/d/1VYddDuDEQ_EpvuNE4O2QvdgcCq1WBcp-yI85DW5vnRI/edit?gid=874791186#gid=874791186"
sheet_ct <- "curation-20251121"
sheet_rt <- paste0("robot_template-", stringr::str_extract(sheet_ct, "20[0-9]+$"))

# INCOMPLETE - SPARQL remove procedure... may not want to source it at this point in script
#source(here::here("scripts/curate_remove_template.R"))


# Identify expected values and template codes -----------------------------

rt_main <- googlesheets4::read_sheet(
    gs_rt_recode,
    range = "template_options!A:E",
    col_types = "c"
) |>
    dplyr::filter(
        !is.na(.data$template),
        is.na(.data$inclusion) | .data$inclusion != "deprecated"
    )


rt_auto <- dplyr::filter(
    rt_main,
    stringr::str_detect(.data$inclusion, "auto")
)$data_type


# Prep & check curated data -----------------------------------------------

# Load and pre-process curated data
cur <- googlesheets4::read_sheet(gs, sheet_ct, range = "A:E", col_types = "c")

# 1. drop curation columns
prep <- cur %>%
# 2. propagate id
    tidyr::fill("id", .direction = "down") %>%
# 3. drop rows with only curation info (id, data_type, or value are empty)
    dplyr::filter(!dplyr::if_any("data_type":"value", is.na)) %>%
# 4. keep only rows with headers of defined templates
    dplyr::filter(.data$data_type %in% rt_main$data_type) %>%
# 6. ensure def sources start with 'url:'
    dplyr::mutate(
        value = dplyr::if_else(
            .data$data_type == "definition source(s)",
            stringr::str_replace_all(value, "(url:)?(https?://)", "url:\\2"),
            .data$value
        )
    ) %>%
# 7. unsplit multiple values in one row
    DO.utils::lengthen_col(value, delim = "|") %>%
# **. [TEMPORARY] drop automated columns that may have been entered manually
    dplyr::filter(!.data$data_type %in% rt_auto)

# ADDITIONAL PROCESSING IDEAS
# 1. drop 'acronym annotation' header if it's been added; may be incorrect, more
#   confident correct if adding programmatically


# CHECK data

# 1. determine if either of annotation or value is missing --> error ==> THIS IS PRECLUDED BY PREP STEP #3
# err <- prep %>%
#     dplyr::filter(
#         dplyr::if_any(
#             c("annotation", "value"),
#             ~ DO.utils::is_blank(.x) | is.na(.x)
#         )
#     )
#
# if (nrow(err) > 0) {
#     rlang::abort(
#         c(
#             "Annotations and values must be specified in pairs.",
#             purrr::set_names(
#                 paste(err[["id"]], err$annotation, err$value, sep = " - "),
#                 nm = rep("x", nrow(err))
#             )
#         )
#     )
# }

# 3. determine if data_type headers are properly limited to single values
ann1 <- dplyr::filter(
    rt_main,
    !stringr::str_detect(template, stringr::coll("SPLIT=", ignore_case = TRUE))
)$data_type
err <- prep %>%
    dplyr::filter(
        .data$data_type %in% ann1,
        !.data$status %in% c("remove", "exclude", "ignore")
    ) %>%
    DO.utils::collapse_col(.cols = "value", delim = " | ") %>%
    dplyr::filter(stringr::str_detect(.data$value, "\\|"))

if (nrow(err) > 0) {
    rlang::abort(
        c(
            "Disease(s) must include no more than 1 value for specified data_types.",
            purrr::set_names(
                paste(err[["id"]], err$data_type, err$value, sep = " - "),
                rep("x", nrow(err))
            )
        )
    )
}

# 4. determine if there are unexpected data headers (besides drop & keep) --> error ==> THIS IS PRECLUDED BY PREP STEP #3
# err <- prep %>%
#     dplyr::filter(!.data$annotation %in% rt_main$header) %>%
#     dplyr::select("id", "annotation") %>%
#     DO.utils::collapse_col(.cols = "annotation", delim = ", ")
#
# if (nrow(err) > 0) {
#     rlang::abort(
#         c(
#             "Unrecognized annotations exist in curated data.",
#             purrr::set_names(
#                 paste(err[["id"]], err$annotation, sep = " - "),
#                 rep("x", nrow(err))
#             )
#         )
#     )
# }

# 4. make sure acronym/synonym annotations are correct
err <- prep %>%
    dplyr::mutate(
        is_acronym = stringr::str_detect(value, "^[A-Za-z][A-Z0-9]{1,7}$")
    ) %>%
    dplyr::filter(
        (!.data$is_acronym & stringr::str_detect(.data$data_type, "acronym")) |
            (.data$is_acronym & stringr::str_detect(.data$data_type, "synonym"))
    )

if (nrow(err) > 0) {
    rlang::warn(
        c(
            "Are acronyms/synonyms annotations mixed up?",
            purrr::set_names(
                paste(err[["id"]], err$data_type, err$value, sep = " - "),
                rep("i", nrow(err))
            )
        )
    )
}

# 5. confirm existing / new diseases being added
all_id <- unique(prep[["id"]])

# check if classes exist --> safe, can also return term info, takes > 6 s
query <- DO.utils:::glueV('
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX obo: <http://purl.obolibrary.org/obo/>
PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>

!<<prefix_stmt>>!

SELECT DISTINCT ?iri ?ns ?label ?parent_iri ?definition ?comment ?deprecated
WHERE {
    VALUES ?iri { !<<iri>>! }

    ?iri a owl:Class .

    OPTIONAL { ?iri oboInOwl:id ?id }
    OPTIONAL { ?iri oboInOwl:hasOBONamespace ?ns }
    OPTIONAL {
        ?iri rdfs:label ?label_en .
        FILTER(lang(?label_en) IN ("", "en"))
        BIND(str(?label_en) AS ?label)
    }
    OPTIONAL {
        ?iri rdfs:subClassOf ?parent_iri .
        FILTER(!isBlank(?parent_iri))
    }
    OPTIONAL {
        ?iri obo:IAO_0000115 ?def_en .
        FILTER(lang(?def_en) IN ("", "en"))
        BIND(str(?def_en) AS ?definition)
    }
    OPTIONAL {
        ?iri rdfs:comment ?comment_en .
        FILTER(lang(?comment_en) IN ("", "en"))
        BIND(str(?comment_en) AS ?comment)
    }
    OPTIONAL { ?iri owl:deprecated ?any }
}',
    prefix_stmt = "PREFIX DOID: <http://purl.obolibrary.org/obo/DOID_>",
    iri = DO.utils::vctr_to_string(all_id, delim = " ")
)

in_DO <- DO.utils::robot_query(
        de,
        query,
        tidy_what = c("header", "uri_to_curie")
    )

# ensure cols exist when all diseases are new
if (nrow(in_DO) == 0) {
    in_DO <- tibble::tibble(
        iri = character(0),
        ns = character(0),
        label = character(0),
        parent_iri = character(0),
        definition = character(0),
        comment = character(0),
        deprecated = logical(0)
    )
} else {
    # otherwise, ensure columns are correctly formatted, even when empty
    in_DO <- in_DO %>%
        dplyr::mutate(
            dplyr::across(c("definition", "comment"), as.character)
        ) %>%
        tidy_sparql(tidy_what = "lgl_NA_FALSE")
}

new_id <- all_id[!all_id %in% in_DO$iri]

dep_n <- sum(in_DO$deprecated)
id_n <- c(nrow(in_DO), length(new_id), length(all_id))
print(
    array(
        data = id_n,
        dim = c(1, 3),
        dimnames = list("Curated diseases:", c("in_DO", "new", "total"))
    )
)
if (dep_n > 0) {
    print(paste0("Existing but deprecated: ", dep_n))
}

## ALTERNATE APPROACH: grep edit file -- less safe / info, takes ~ 0.2 s
# all_regex <- all_id %>%
#     # DO.utils::to_uri() %>%
#     # DO.utils::
#     DO.utils::vctr_to_string(delim = "|")
#
# in_DO <- readr::read_file(de) %>%
#     stringr::str_extract_all(all_regex) %>%
#     unlist() %>%
#     unique()
# new_id <- all_id[!all_id %in% in_DO]
#
# id_n <- c(length(in_DO), length(new_id), length(all_id))
# print(
#     array(
#         data = id_n,
#         dim = c(1, 3),
#         dimnames = list("Curated diseases:", c("in_DO", "new", "total"))
#     )
# )

# maybe change this to ask if the user wants to check the list first, then ask if continue?
user_check <- NA
while (!user_check %in% c("y", "n")) {
    user_check <- readline("Does existing/new disease counts appear correct?  y/n")
}
if (user_check != "y") {
    rlang::warn("Exiting with existing/new disease info for user review.")

    # custom function to return curated labels when they exist
    get_cur_label <- function(.df, new) {
        new_w_label <- dplyr::filter(
            prep,
            .data$id %in% new_id & .data$data_type == "label"
        ) %>%
            dplyr::select("id", label = "value")

        new_no_label <- tibble::tibble(
            id = new[!new %in% new_w_label[["id"]]],
            label = NA_character_
        )
        out <- dplyr::bind_rows(new_w_label, new_no_label) %>%
            dplyr::arrange(.data$id)

        out
    }

    # return(
        list(
            in_DO = dplyr::rename(in_DO, "id" = "iri"),
            new = get_cur_label(new = new_id)
        )
    # )
}

# 6. determine if any required manual data_types are missing for new diseases --> error
#   --> will report which requirements are missing for all given id
new_req <- dplyr::filter(
    rt_main,
    stringr::str_detect(
        .data$inclusion,
        stringr::coll("required manual", ignore_case = TRUE)
    )
)$data_type
new_req <- new_req[new_req != "id"]
err <- prep %>%
    dplyr::filter(.data$id %in% new_id) %>%
    dplyr::summarize(
        missing = DO.utils::vctr_to_string(
            new_req[!new_req %in% .data$data_type],
            delim = ", "
        ),
        .by = "id"
    ) %>%
    dplyr::filter(!is.na(.data$missing))

if (nrow(err) > 0) {
    rlang::abort(
        c(
            "New disease(s) must have all required data_types.",
            purrr::set_names(
                paste(err[["id"]], err$missing, sep = " - "),
                rep("x", nrow(err))
            )
        )
    )
}

# 7. determine if any single value data_types would be added to existing terms
#.  resulting in multiples --> error
#   --> will report which requirements are missing for all given id
exist_data <- in_DO %>%
    dplyr::rename(
        "id" = "iri", "obo namespace" = "ns" ,
        "parent id" = "parent_iri"
    ) %>%
    dplyr::select(dplyr::any_of(ann1)) %>%
    dplyr::mutate(dplyr::across(-"id", as.character)) |>
    tidyr::pivot_longer(
        -"id",
        names_to = "data_type",
        values_to = "value",
        values_drop_na = TRUE
    )

err <- prep %>%
    dplyr::filter(
        .data$id %in% exist_data[["id"]],
        .data$data_type %in% ann1,
        !.data$status %in% c("remove", "exclude", "ignore")
    ) %>%
    dplyr::bind_rows(
        dplyr::filter(exist_data, .data$data_type %in% ann1),
        .id = "src"
    ) %>%
    dplyr::group_by(.data[["id"]], .data$data_type) %>%
    dplyr::mutate(
        both = all(c("1", "2") %in% .data$src),
        differ = dplyr::n_distinct(.data$value) > 1
    ) %>%
    dplyr::filter(.data$both, .data$differ) %>%
    dplyr::select("id":"value") %>%
    DO.utils::collapse_col("value")

# can't manage to show the two values without some serious processing... not worth it at this point
    # dplyr::left_join(
    #     dplyr::rename(exist_data, existing_value = "value"),
    #     by = c("id", "annotation")
    # )

if (nrow(err) > 0) {
    rlang::abort(
        c(
            "Curated disease info must not duplicate existing, singular data.",
            purrr::set_names(
                paste(
                    err[["id"]], err$data_type,
                    # paste0(
                    #     "curated: ", err$value,
                    #     "vs existing: ", err$existing_value
                    # ),
                    sep = " - "
                ),
                rep("x", nrow(err))
            )
        )
    )
}



# Generate ROBOT template -------------------------------------------------

# retain ONLY data to be added
add <- prep %>%
    dplyr::filter(.data$status %in% c("add", "restore")) %>%
    dplyr::select("id", "data_type", "value")

# add obo id & namespace to new diseases
obo_req <- tibble::tibble(
    'id' = rep(new_id, times = 2),
    data_type = rep(c("obo id", "obo namespace"), each = length(new_id)),
    value = c(
        DO.utils::to_curie(new_id),
        rep("disease_ontology", length(new_id))
    )
)


# if def src or src type & no def for existing in curated sheet, add existing def
### THIS DOES NOT WORK AS DESIRED... IT ADDS ANOTHER COPY OF THE DEFINITION
needed_def <- add %>%
    dplyr::summarize(
        .by = "id",
        temp = any(.data$data_type %in% c("definition source(s)", "definition source type(s)")) &
            !any(.data$data_type == "definition")
    ) %>%
    dplyr::filter(temp) %>%
    dplyr::select("id") %>%
    dplyr::left_join(
        dplyr::filter(exist_data, .data$data_type == "definition"),
        by = "id"
    )

# add oboInOwl:hasDbXref from skos:exactMatch annotations (not reciprocal because of exceptions)
xref_skos <- add %>%
    dplyr::filter(.data$data_type %in% "skos mapping(s): exact") %>%
    dplyr::mutate(data_type = "xref(s)")

# add & format data
wide <- add %>%
    dplyr::bind_rows(obo_req, needed_def, xref_skos) %>%
    DO.utils::collapse_col("value", delim = "|") %>%
    dplyr::mutate(
        data_type = factor(.data$data_type, levels = rt_main$data_type)
    ) %>%
    dplyr::arrange(.data$data_type, .data$id) %>%
    tidyr::pivot_wider(
        names_from = "data_type",
        values_from = "value"
    )

# add templates to existing columns
rt_template <- rt_main %>%
    dplyr::select("data_type", "template") %>%
    dplyr::filter(.data$data_type %in% names(wide)) %>%
    tidyr::pivot_wider(
        names_from = "data_type",
        values_from = "template"
    )
rt <- tibble::add_row(wide, rt_template, .before = 1)

# add acronym annotations
#   multiple acronym columns with varying synonym types are supported
#   data_type column name will match acronym col being annotated appended with " - annotation"
acr_dt <- names(rt)[stringr::str_detect(names(rt), "^acronym")]
acr_template <- dplyr::filter(rt_main, .data$data_type == "acronym annotation")$template
purrr::walk2(
    acr_dt,
    seq_along(acr_dt),
    function(.x, .y) {
        acr_ann <- dplyr::if_else(!is.na(rt[[.x]]), "acronym", NA_character_)
        acr_ann[1] <- acr_template
        acr_ann_col <- list(acr_ann)
        names(acr_ann_col) <- paste0(.x, " - annotation")
        rt <<- tibble::add_column(
            rt,
            !!!acr_ann_col,
            .after = .x
        )
    }
)

# write final robot template
googlesheets4::write_sheet(rt, gs, sheet_rt)
