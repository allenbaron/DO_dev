library(here)
library(tidyverse)
library(DO.utils)


# Define Resources --------------------------------------------------------

repo_path <- here::here("../Ontologies/HumanDiseaseOntology/")
reports <- list.files(
    here::here("sparql/current_reports"),
    full.names = TRUE
)
class_q <- here::here("sparql/DO-id_label.rq")


# Custom Functions --------------------------------------------------------

progress_bar <- methods::setRefClass(
    "progress_bar",
    fields = c("bar", "tick"),
    methods = list(
        initialize = function(n, ...) {
            args <- list(...)
            bar <<- utils::txtProgressBar(
                min = 0,
                max = n,
                style = if ("style" %in% names(args)) { args$style } else { 3 },
                width = if ("width" %in% names(args)) { args$width } else { 50 },
                char = if ("char" %in% names(args)) { args$char } else { "=" }
            )
            tick <<- 1L
        },
        advance = function(...) {
            utils::setTxtProgressBar(bar, tick, ...)
            tick <<- tick + 1
        }
    )
)


exec_queries <- function(input, queries, load = TRUE, ..., progress = NULL) {
    if (isTRUE(progress)) { progress <- progress_bar$new(length(queries)) }
    if (isTRUE(load)) { input$load() }

    purrr::map2(
        queries,
        1:length(queries),
        function(.x, .y) {
            report_res <- input$query(query = .x)
            if (!is.null(progress)) { progress$advance() }
            tibble::as_tibble(report_res)
        }
    )
}

tag_exec_queries <- function(.repo, input = "doid_merged", queries, tags,
                             nm,  ...) {
    input <- match.arg(
        input,
        c("doid", "doid_merged", "doid_base", "doid_non_classified")
    )

    head <- .repo$capture_head()
    on.exit(.repo$restore_head())
    progress <- progress_bar$new(length(tags) * length(queries))

    tag_res <- purrr::map(
        tags,
        function(.tag) {
            repo$checkout_tag(.tag)
            q_res <- exec_queries(.repo[input], queries, progress = progress)
            names(q_res) <- nm
            q_res
        }
    ) %>%
        purrr::set_names(tags)

    tag_res
}


# Run Reports & Compare ---------------------------------------------------

repo <- DO.utils::DOrepo(repo_path)
report_res <- tag_exec_queries(
    repo,
    queries = reports,
    tags = c("v2022-07-27", "v2022-08-29"),
    nm = stringr::str_replace(
        reports,
        ".*/(.+)-report.rq",
        "\\1"
    )
)

report_merge <- purrr::map(
    DO.utils::invert_sublists(res, use_sublist_names = TRUE),
    dplyr::bind_rows,
    .id = "tag"
) %>%
    # standardize data.frame columns (tag, count)
    purrr::map_at(
        .at = c("doid", "logical-definitions"),
        .f = ~ tidyr::pivot_longer(
            .x,
            cols = -tag,
            names_to = "type",
            values_to = "count"
        )
    ) %>%
    purrr::map(
        ~ tidyr::pivot_wider(.x, names_from = tag, values_from = count) %>%
            dplyr::mutate(
                diff = `v2022-08-29` - `v2022-07-27`
            )
    )

View(report_merge)


# Identify New Classes ----------------------------------------------------

class_res <- tag_exec_queries(
    repo,
    queries = class_q,
    tags = c("v2022-07-27", "v2022-08-29"),
    nm = classes
)

new_class <- dplyr::filter(
    class_res[[2]][[1]],
    !id %in% class_res[[1]][[1]]$id
) %>%
    dplyr::arrange(label)

View(new_class)

# EXTRA: not in use but works ---------------------------------------------

# R6progress_bar <- R6::R6Class(
#     "progress_bar",
#     public = list(
#         initialize = function(n, ...) {
#             args <- list(...)
#             self$bar <- txtProgressBar(
#                 min = 0,
#                 max = n,
#                 style = if ("style" %in% names(args)) { args$style } else { 3 },
#                 width = if ("width" %in% names(args)) { args$width } else { 50 },
#                 char = if ("char" %in% names(args)) { args$char } else { "=" }
#             )
#         },
#         advance = function(...) {
#             setTxtProgressBar(self$bar, self$tick, ...)
#             self$tick <- self$tick + 1
#         },
#         bar = NA,
#         tick = 1L
#     )
# )
