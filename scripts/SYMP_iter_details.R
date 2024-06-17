# get stats of an OWL file across releases (tags) -- SYMP example

library(here)
library(tidyverse)
library(git2r)
library(DO.utils)
library(lubridate)

repo_path <- here::here("../Ontologies/SymptomOntology")
repo_tags <- git2r::tags(repo_path)



# sparql query
sparql_q <- '
    PREFIX owl: <http://www.w3.org/2002/07/owl#>

    SELECT (COUNT(?s) AS ?terms) (COUNT(?d) AS ?defs)
    WHERE {
        ?s a owl:Class .
        OPTIONAL { ?s obo:IAO_0000115 ?d . }
        FILTER CONTAINS(str(?s), "SYMP_")
        FILTER NOT EXISTS {?s owl:deprecated ?any}
    }'


# Count at TAGS -----------------------------------------------------------
# custom function
tag_exec_query <- function(repo_path, owl_relpath, query, tags = NULL) {
    repo <- git2r::repository(repo_path)
    owl_path <- file.path(repo_path, owl_relpath)
    if (!file.exists(owl_path)) {
        rlang::abort(
            "`owl_relpath` must point to a file that exists.",
            x = owl_path
        )
    }

    all_tags <- git2r::tags(repo)
    if (is.null(tags)) {
        iter_tags <- all_tags
    } else {
        tag_nm <- names(all_tags)
        tag_err <- !tags %in% tag_nm
        if (any(tag_err)) {
            rlang::abort(
                c("`tags` must all match existing repo tags.",
                  setNames(tag_nm[tag_err], rep("x", sum(tag_err)))
                )
            )
        }
        iter_tags <- all_tags[tags]
    }

    head <- git2r::repository_head(repo)
    on.exit(git2r::checkout(head))

    progress <- 0
    pb <- utils::txtProgressBar(max = length(iter_tags), style = 3)
    on.exit(close(pb), add = TRUE, after = FALSE)

    res <- purrr::map2(
        .x = iter_tags,
        .y = names(iter_tags),
        function(.x, .y) {
            git2r::checkout(.x)
            owl <- DO.utils::owl_xml(owl_path)
            res <- owl$query(query) %>%
                DO.utils::tidy_sparql() %>%
                dplyr::mutate(tag = .y)

            progress <<- progress + 1
            setTxtProgressBar(pb, progress)

            res
        }
    )

    dplyr::bind_rows(res) %>%
        dplyr::select(tag, dplyr::everything())
}

# execute
tag_counts <- tag_exec_query(
    repo_path,
    "src/ontology/symp.owl",
    sparql_q
) %>%
    dplyr::mutate(
        date = stringr::str_remove(tag, "^v") %>%
            lubridate::ymd()
    )



# Count at COMMITS --------------------------------------------------------
# query at various commits (1/month at least 1 month before releases used)

earliest_tag_date <- min(tag_counts$date)
before <- earliest_tag_date %m-% months(1)

commit_df <- git2r::repository(repo_path) %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
        date = lubridate::as_date(when, tz = "UTC"),
        month = lubridate::month(date),
        yr = lubridate::year(date)
    )
pretag_commits <- commit_df %>%
    dplyr::filter(date <= before) %>%
    dplyr::group_by(month, yr) %>%
    dplyr::filter(
        dplyr::row_number(when) == max(dplyr::row_number(when))
    ) %>%
    dplyr::ungroup()

pb <- utils::txtProgressBar(max = nrow(pretag_commits), style = 3)
progress <- 0

repo_head <- git2r::repository_head(repo_path)
symp_filepath <- file.path(repo_path, "src/ontology/symp.owl")
symp_filepath_old <- file.path(repo_path, "symp.owl")

commit_count <- purrr::map2(
    .x = pretag_commits$sha,
    .y = pretag_commits$date,
    function(.x, .y) {
        git2r::checkout(repo_path, .x)
        if (file.exists(symp_filepath)) {
            owl <- DO.utils::owl_xml(symp_filepath)
        } else if (file.exists(symp_filepath_old)) {
            owl <- DO.utils::owl_xml(symp_filepath_old)
        } else {
            return(NULL)
        }
        res <- owl$query(sparql_q) %>%
            DO.utils::tidy_sparql() %>%
            dplyr::mutate(sha = .x, date = .y)

        progress <<- progress + 1
        utils::setTxtProgressBar(pb, progress)

        res
    }
) %>%
    dplyr::bind_rows() %>%
    dplyr::select(commit = sha, date, dplyr::everything())

# cleanup
close(pb)
git2r::checkout(repo_head)



# Combine all counts & SAVE -----------------------------------------------

symp_counts <- dplyr::bind_rows(
    tag_counts %>%
        dplyr::mutate(id_type = "tag") %>%
        dplyr::select(id = tag, id_type, dplyr::everything()),
    commit_count %>%
        dplyr::mutate(id_type = "commit") %>%
        dplyr::select(id = commit, id_type, dplyr::everything())
) %>%
    dplyr::arrange(date) %>%
    dplyr::relocate(id, id_type, date, terms, defs)

readr::write_csv(
    symp_counts,
    here::here("data/ST_release/SYMP_term_def_counts.csv")
)



# Create plot & SAVE ------------------------------------------------------
symp_plot_df <- symp_counts %>%
    dplyr::mutate(
        n_terms = .data$terms - .data$defs,
        n_defs = .data$defs
    ) %>%
    dplyr::select(-"terms", -"defs") %>%
    tidyr::pivot_longer(
        cols = c(n_terms, n_defs),
        names_to = "variable",
        values_to = "value"
    ) %>%
    dplyr::mutate(
        variable = factor(.data$variable, levels = c("n_terms", "n_defs"))
    )

g_symp <- ggplot2::ggplot(symp_plot_df) +
    ggplot2::geom_area(
        ggplot2::aes(x = .data$date, y = .data$value, fill = .data$variable),
        linewidth = 1
    ) +
    ggplot2::scale_fill_manual(
        name = "Total",
        values = unname(DO.utils::DO_colors[c("sat_light", "sat")]),
        labels = c("Terms", "Terms Defined")
    ) +
    ggplot2::scale_y_continuous(
        name = NULL#,
        # breaks = seq(0, 12000, by = 2000)
    ) +
    ggplot2::scale_x_date(
        name = "Release Date",
        date_breaks = "1 year",
        date_labels = "%Y"
    ) +
    DO.utils::theme_DO(base_size = 13)

ggplot2::ggsave(
    filename = "graphics/website/SYMP_term_def_count.png",
    plot = g_symp,
    width = 8,
    height = 5.6,
    units = "in",
    dpi = 600
)
