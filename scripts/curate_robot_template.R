# INITIAL TEST: create true ROBOT template from curation long-form template

library(here)
library(googlesheets4)
library(tidyverse)
library(DO.utils)

de <- here::here("../Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl")

gs_rt_recode <- "https://docs.google.com/spreadsheets/d/1Zn6p5xkVHUwbWe1N8FUa3fNcEkAOoE9P4ADb12f69hQ/edit"

gs_camrq <- "https://docs.google.com/spreadsheets/d/1bWW6bcOm9tisTb0DroQrhD2oqe2XDEbbm8xkqPKiFkM/edit"
sheet_ct <- "curation_20240718"
sheet_rt <- "robot_template_20240718"


# Identify expected values and template codes -----------------------------

rt_main <- googlesheets4::read_sheet(
    gs_rt_recode,
    range = "main!A:C",
    col_types = "c"
) %>%
    dplyr::filter(!is.na(template))

rt_auto <- dplyr::filter(rt_main, stringr::str_detect(type, "auto"))$header


# Prep & check curated data -----------------------------------------------

camrq_cur <- googlesheets4::read_sheet(gs_camrq, sheet_ct, col_types = "c")

# 1. drop curation columns
camrq_prep <- camrq_cur %>%
    dplyr::select("iri/curie", "annotation", "value", "remove") %>%
# 2. drop rows with only curation info (iri/curie, annotation, & value are empty)
    dplyr::filter(!dplyr::if_all("iri/curie":"value", is.na)) %>%
# 3. keep only rows with headers of defined templates
    dplyr::filter(.data$annotation %in% rt_main$header) %>%
# 4. propagate iri/curie
    tidyr::fill("iri/curie", .direction = "down") %>%
# 5. standardize remove column values
    dplyr::mutate(
        remove = dplyr::if_else(
            DO.utils::is_blank(.data$remove) | is.na(.data$remove),
            FALSE,
            TRUE
        )
    ) %>%
# 6. ensure def sources start with 'url:'
    dplyr::mutate(
        value = dplyr::if_else(
            .data$annotation == "definition source(s)",
            stringr::str_replace_all(value, "(url:)?(https?://)", "url:\\2"),
            .data$value
        )
    ) %>%
# 7. unsplit multiple values in one row
    DO.utils::lengthen_col(value, delim = "|") %>%
# **. [TEMPORARY] drop automated columns that may have been entered manually
    dplyr::filter(!.data$annotation %in% rt_auto)


##### ADDITIONAL PROCESSING IDEAS ####
# 1. drop 'acronym annotation' header if it's been added; may be incorrect, more
#   confident correct if adding programmatically

# CHECK:
# 1. determine if either of annotation or value is missing --> error
camrq_err <- camrq_prep %>%
    dplyr::filter(
        dplyr::if_any(
            c("annotation", "value"),
            ~ DO.utils::is_blank(.x) | is.na(.x)
        )
    )

if (nrow(camrq_err) > 0) {
    rlang::abort(
        c(
            "Annotations and values must be specified in pairs.",
            purrr::set_names(
                paste(camrq_err[["iri/curie"]], camrq_err$annotation, camrq_err$value, sep = " - "),
                nm = rep("x", nrow(camrq_err))
            )
        )
    )
}

# 3. determine if headers are properly limited to single values
ann1 <- dplyr::filter(
    rt_main,
    !stringr::str_detect(template, stringr::coll("SPLIT=", ignore_case = TRUE))
)$header
camrq_err <- camrq_prep %>%
    dplyr::filter(.data$annotation %in% ann1 & !.data$remove) %>%
    DO.utils::collapse_col(.cols = "value", delim = " | ") %>%
    dplyr::filter(stringr::str_detect(.data$value, "\\|"))

if (nrow(camrq_err) > 0) {
    rlang::abort(
        c(
            "Disease(s) must include no more than 1 value for specified annotations.",
            purrr::set_names(
                paste(camrq_err[["iri/curie"]], camrq_err$annotation, camrq_err$value, sep = " - "),
                rep("x", nrow(camrq_err))
            )
        )
    )
}

# 4. determine if there are unexpected data headers (besides drop & keep) --> error
camrq_err <- camrq_prep %>%
    dplyr::filter(!.data$annotation %in% rt_main$header) %>%
    dplyr::select("iri/curie", "annotation") %>%
    DO.utils::collapse_col(.cols = "annotation", delim = ", ")

if (nrow(camrq_err) > 0) {
    rlang::abort(
        c(
            "Unrecognized annotations exist in curated data.",
            purrr::set_names(
                paste(camrq_err[["iri/curie"]], camrq_err$annotation, sep = " - "),
                rep("x", nrow(camrq_err))
            )
        )
    )
}

# 5. confirm existing / new diseases being added
all_id <- unique(camrq_prep[["iri/curie"]])

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

    ?iri a owl:Class ;
        oboInOwl:hasOBONamespace ?ns ;
        # oboInOwl:id ?id ; #### temporarily not checking, auto-dropped ####
        rdfs:label ?label_en ;
        rdfs:subClassOf ?parent_iri .

    FILTER(!isBlank(?parent_iri))
    FILTER(lang(?label_en) IN ("", "en"))
    BIND(str(?label_en) AS ?label)

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
    OPTIONAL { ?iri owl:deprecated true }
}',
    prefix_stmt = "PREFIX DOID: <http://purl.obolibrary.org/obo/DOID_>",
    iri = DO.utils::vctr_to_string(all_id, delim = " ")
)

in_DO <- DO.utils::robot_query(
    de,
    query,
    tidy_what = c("header", "uri_to_curie")
) %>%
    dplyr::mutate(
        dplyr::across(c("definition", "comment"), as.character)
    ) %>%
    tidy_sparql(tidy_what = "lgl_NA_FALSE")

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
user_check <- readline("Does existing/new disease counts appear correct?  y/n")

if (user_check != "y") {
    rlang::warn("Exiting with existing/new disease info for user review.")

    # custom function to return curated labels when they exist
    get_cur_label <- function(.df, new) {
        new_w_label <- dplyr::filter(
            camrq_prep,
            .data$`iri/curie` %in% new_id & .data$annotation == "label"
        ) %>%
            dplyr::select("iri/curie", label = "value")

        new_no_label <- tibble::tibble(
            `iri/curie` = new[!new %in% new_w_label[["iri/curie"]]],
            label = NA_character_
        )
        out <- dplyr::bind_rows(new_w_label, new_no_label) %>%
            dplyr::arrange(.data$`iri/curie`)

        out
    }

    # return(
        list(
            in_DO = dplyr::rename(in_DO, "iri/curie" = "iri"),
            new = get_cur_label(new = new_id)
        )
    # )
}

# 6. determine if any required manual headers are missing for new diseases --> error
#   --> will report which requirements are missing for all given iri/curie
new_req <- dplyr::filter(
    rt_main,
    stringr::str_detect(type, stringr::coll("required manual", ignore_case = TRUE))
)$header
new_req <- new_req[!new_req == "iri/curie"]
camrq_err <- camrq_prep %>%
    dplyr::filter(.data$`iri/curie` %in% new_id) %>%
    dplyr::summarize(
        missing = DO.utils::vctr_to_string(
            new_req[!new_req %in% .data$annotation],
            delim = ", "
        ),
        .by = "iri/curie"
    ) %>%
    dplyr::filter(!is.na(.data$missing))

if (nrow(camrq_err) > 0) {
    rlang::abort(
        c(
            "New disease(s) must have all required annotations.",
            purrr::set_names(
                paste(camrq_err[["iri/curie"]], camrq_err$missing, sep = " - "),
                rep("x", nrow(camrq_err))
            )
        )
    )
}

# 7. determine if any single value headers would be added to existing terms --> error
#   --> will report which requirements are missing for all given iri/curie
exist_data <- in_DO %>%
    dplyr::rename(
        "iri/curie" = "iri", "obo namespace" = "ns" ,
        "parent iri/curie" = "parent_iri"
    ) %>%
    dplyr::select(dplyr::any_of(ann1)) %>%
    tidyr::pivot_longer(
        -"iri/curie",
        names_to = "annotation",
        values_to = "value",
        values_drop_na = TRUE
    )

camrq_err <- camrq_prep %>%
    dplyr::filter(
        .data$`iri/curie` %in% exist_data[["iri/curie"]],
        .data$annotation %in% ann1
    ) %>%
    dplyr::anti_join(exist_data, by = names(exist_data)) #%>%
# can't manage to show the two values without some serious processing... not worth it at this point
    # dplyr::left_join(
    #     dplyr::rename(exist_data, existing_value = "value"),
    #     by = c("iri/curie", "annotation")
    # )

if (nrow(camrq_err) > 0) {
    rlang::abort(
        c(
            "Curated disease info must not duplicate existing, singular data.",
            purrr::set_names(
                paste(
                    camrq_err[["iri/curie"]], camrq_err$annotation,
                    # paste0(
                    #     "curated: ", camrq_err$value,
                    #     "vs existing: ", camrq_err$existing_value
                    # ),
                    sep = " - "
                ),
                rep("x", nrow(camrq_err))
            )
        )
    )
}



# Generate ROBOT template -------------------------------------------------

# retain ONLY data to be added (exclude data to remove)
camrq_add <- camrq_prep %>%
    dplyr::filter(!.data$remove) %>%
    dplyr::select("iri/curie", "annotation", "value")

# add obo id & namespace to new diseases
obo_req <- tibble::tibble(
    'iri/curie' = rep(new_id, times = 2),
    annotation = rep(c("obo id", "obo namespace"), each = length(new_id)),
    value = c(
        DO.utils::to_curie(new_id),
        rep("disease_ontology", length(new_id))
    )
)


# if def src or src type & no def for existing in curated sheet, add existing def
### THIS DOES NOT WORK AS DESIRED... IT ADDS ANOTHER COPY OF THE DEFINITION
needed_def <- camrq_add %>%
    dplyr::summarize(
        .by = "iri/curie",
        temp = any(annotation %in% c("definition source(s)", "definition source type(s)")) &
            !any(annotation == "definition")
    ) %>%
    dplyr::filter(temp) %>%
    dplyr::select("iri/curie") %>%
    dplyr::left_join(
        dplyr::filter(exist_data, .data$annotation == "definition"),
        by = "iri/curie"
    )

# reciprocally add oboInOwl:hasDbXref / skos:exactMatch annotations
xref_skos <- camrq_add %>%
    dplyr::filter(.data$annotation %in% c("xref(s)", "skos mapping(s): exact")) %>%
    dplyr::mutate(
        annotation = dplyr::recode(
            annotation,
            "xref(s)" = "skos mapping(s): exact",
            "skos mapping(s): exact" = "xref(s)"
        )
    )

# add & format data
camrq_wide <- camrq_add %>%
    dplyr::bind_rows(obo_req, needed_def, xref_skos) %>%
    DO.utils::collapse_col("value", delim = "|") %>%
    dplyr::mutate(
        annotation = factor(.data$annotation, levels = rt_main$header)
    ) %>%
    dplyr::arrange(.data$annotation, .data$`iri/curie`) %>%
    tidyr::pivot_wider(
        names_from = "annotation",
        values_from = "value"
    )

# add templates to existing columns
rt_template <- rt_main %>%
    dplyr::select("header", "template") %>%
    dplyr::filter(.data$header %in% names(camrq_wide)) %>%
    tidyr::pivot_wider(
        names_from = "header",
        values_from = "template"
    )
camrq_rt <- tibble::add_row(camrq_wide, rt_template, .before = 1)

# add acronym annotations
#   multiple acronym columns with varying synonym types are supported
#   annotation column name will match acronym col being annotated appended with " - annotation"
acr_header <- names(camrq_rt)[stringr::str_detect(names(camrq_rt), "^acronym")]
acr_template <- dplyr::filter(rt_main, header == "acronym annotation")$template
purrr::walk2(
    acr_header,
    1:length(acr_header),
    function(.x, .y) {
        acr_ann <- dplyr::if_else(!is.na(camrq_rt[[.x]]), "acronym", NA_character_)
        acr_ann[1] <- acr_template
        acr_ann_col <- list(acr_ann)
        names(acr_ann_col) <- paste0(.x, " - annotation")
        camrq_rt <<- tibble::add_column(
            camrq_rt,
            !!!acr_ann_col,
            .after = .x
        )
    }
)

# write final robot template
googlesheets4::write_sheet(camrq_rt, gs_camrq, sheet_rt)

# Generate SPARQL remove query --------------------------------------------

### INCOMPLETE - SPARQL remove query ####
source(here::here("scripts/curate_remove_template.R"))

# SPARQL remove query
camrq_rm <- camrq_prep %>%
    dplyr::filter(remove) %>%
    # format values for remove
    dplyr::mutate(
        value = dplyr::case_when(
            annotation %in% sparql_string ~
                stringr::str_replace_all(value, c('^"?'
    dplyr::transmute(
        annotation,
        values_linked = paste0("(", `iri/curie`, " ", value, ")"),
    ) %>%
    DO.utils::collapse_col("values_linked", delim = " ", na.rm = TRUE)

values_stmt <- paste0(
    "VALUES ",
    dplyr::recode(camrq_rm$annotation, !!!sparql_values),
    " { ",
    camrq_rm$values_linked,
    " }"
)
# where_stmt = dplyr::if_else(
#     annotation %in% c("definition source(s)", "definition source type(s)"),
#     "definition",
#     annotation
# ),
# where_stmt = dplyr::recode(where_stmt, !!!sparql_where),
delete_stmt <- dplyr::recode(camrq_rm$annotation, !!!sparql_delete)




# EXTRA -------------------------------------------------------------------


# header order ensure consistent output (mostly for human review but also
#   ensures definition annotation columns are in correct position)
header_order <- c(
    "iri/curie", "obo id", "obo namespace", "label", "comment",
    "deprecate", "alternate id(s)", "term replaced by",
    "parent iri/curie",
    "definition", "definition source(s)", "definition source type(s)",
    "acronym(s): exact", "acronym(s): related",
    "synonym(s): exact", "synonym(s): broad", "synonym(s): narrow",
    "synonym(s): related",
    "xref(s)",
    "skos mapping(s): exact", "skos mapping(s): broad",
    "skos mapping(s): narrow", "skos mapping(s): related",
    "subset(s)",
    "equivalent class",
    "sc axiom: inheritance", "sc axiom: anatomical location",
    "sc axiom: onset", "sc axiom: has_material_basis_in", "sc axiom: located_in",
    "disjoint class"
)

# PREP:
header_drop <- c(
    "parent label", "def phrase: genetic basis", "def phrase: characterized by"
)
