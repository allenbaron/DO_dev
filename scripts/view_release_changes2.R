library(here)
library(tidyverse)
library(DO.utils)
library(git2r)  # until giterateR is active

# choose releases to compare & set path to repo
repo_path <- here::here("../Ontologies/SymptomOntology")
file_path <- file.path(repo_path, "src/ontology/symp.owl")
ns <- DO.utils::ns_prefix['SYMP']
releases <- list(latest = "v2022-09-06", prior = "v2022-07-26")


# custom functions
compare_releases <- function(repo_path, file_path, namespace, releases) {
    initial_head <- git2r::repository_head(repo = repo_path)
    on.exit(git2r::checkout(initial_head))

    if (!namespace %in% DO.utils::ns_prefix) {
        if (!namespace %in% names(DO.utils::ns_prefix)) {
            rlang::abort(
                c(
                    "`namespace` can't be found in DO.utils::ns_prefix",
                    "x" = namespace
                )
            )
        }
        namespace <- DO.utils::ns_prefix[namespace]
    }

    query <- glue::glue(
        'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX owl: <http://www.w3.org/2002/07/owl#>
        PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>

        SELECT ?class ?id ?label ?deprecated ?def ?xref
        WHERE {
            ?class a owl:Class .
            FILTER(!isBlank(?class) && CONTAINS(str(?class), "!<<namespace>>!"))

            OPTIONAL { ?class oboInOwl:id ?id . }
            OPTIONAL { ?class rdfs:label ?label . }
            OPTIONAL { ?class owl:deprecated ?deprecated . }
            OPTIONAL { ?class obo:IAO_0000115 ?def . }
            OPTIONAL { ?class oboInOwl:hasDbXref ?xref . }
        }',
        namespace = namespace,
        .open = "!<<",
        .close = ">>!"
    )

    git2r::checkout(repo_path, releases$latest)
    latest <- DO.utils::owl_xml(file_path)$query(query) %>%
        dplyr::as_tibble() %>%
        DO.utils::unnest_cross(where(is.list), keep_empty = TRUE) %>%
        tidyr::replace_na(list(deprecated = FALSE))
    latest_stats <- calc_stats(latest) %>%
        dplyr::mutate(release = releases$latest)

    git2r::checkout(repo_path, releases$prior)
    prior <- DO.utils::owl_xml(file_path)$query(query) %>%
        dplyr::as_tibble() %>%
        DO.utils::unnest_cross(where(is.list), keep_empty = TRUE) %>%
        tidyr::replace_na(list(deprecated = FALSE))
    prior_stats <- calc_stats(prior) %>%
        dplyr::mutate(release = releases$prior)

    stats <- dplyr::bind_rows(prior_stats, latest_stats) %>%
        tidyr::pivot_wider(
            names_from = release,
            values_from = count
        ) %>%
        dplyr::mutate(diff = .data[[releases$latest]] - .data[[releases$prior]])

    changes <- gather_changes(latest, prior)
    details <- dplyr::bind_rows(
        dplyr::mutate(latest, release = releases$latest),
        dplyr::mutate(prior, release = releases$prior)
    )

    list(diff = stats, changes = changes, all = details)
}

calc_stats <- function(df) {
    xref_stat <- df %>%
        dplyr::group_by(deprecated, class) %>%
        dplyr::summarize(xref = n_uniq(xref), .groups = "drop_last") %>%
        dplyr::summarize(xref = sum(xref), .groups = "drop") %>%
        tidyr::pivot_longer(
            cols = !deprecated,
            names_to = "type",
            values_to = "count"
        )

    stats <- df %>%
        dplyr::group_by(deprecated) %>%
        dplyr::summarize(
            dplyr::across(c(class, def), n_uniq),
            .groups = "drop"
        ) %>%
        tidyr::pivot_longer(
            cols = !deprecated,
            names_to = "type",
            values_to = "count"
        ) %>%
        dplyr::bind_rows(xref_stat) %>%
        dplyr::select(type, count, deprecated) %>%
        dplyr::arrange(type, deprecated)

    stats
}

n_uniq <- function(x) {
    dplyr::n_distinct(x, na.rm = TRUE)
}

gather_changes <- function(latest, prior) {
    new <- gather_new(latest, prior)
    dep <- gather_dep_change(latest, prior)

    list(
        class_new = new$class,
        def_new = new$def,
        xref_new = new$xref,
        dep_change = dep
    )
}


gather_new <- function(latest, prior) {
    class <- dplyr::anti_join(latest, prior, by = "class") %>%
        dplyr::filter(!deprecated) %>%
        DO.utils::collapse_col(xref, delim = " | ")
    persist_class <- dplyr::semi_join(latest, prior, by = "class")
    def <- dplyr::anti_join(persist_class, prior, by = "def") %>%
        dplyr::filter(!deprecated) %>%
        DO.utils::collapse_col(xref, delim = " | ")
    xref <- dplyr::anti_join(persist_class, prior, by = "xref") %>%
        dplyr::filter(!deprecated)

    list(class = class, def = def, xref = xref)
}

gather_dep_change <- function(latest, prior) {
    .late <- with(latest, paste(class, deprecated))
    .prior <- with(latest, paste(class, deprecated))
    change_idx <- which(!.late %in% .prior)

    latest[change_idx, ]
}

# calculation
res <- compare_releases(repo_path, file_path, ns, releases)

