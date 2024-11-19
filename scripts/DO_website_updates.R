# Create WEBSITE statistics plots
# J. Allen Baron
# Created: 2022-02-28
# Last Updated: 2023-04-05

library(here)
library(DO.utils) # >= 0.3.3

# MANUALLY Update Data!!! -------------------------------------------------

# 1. For updated cited by counts (data/citedby/DO_citedby.csv), execute
#   source(scripts/citedby_full_procedure.R)
# 2. For update Use Cases HTML, curate new resources from cited by info in
#   DO_uses (Google Sheet) "cited_by" sheet to "DO_website_user_list" sheet.
#   NOTE: Resources are only included if they have a checkbox (doesn't matter
#       if it's filled in or not).

# Custom functions --------------------------------------------------------

# INTERIM USE has ended... only keeping here for reference work on giterateR
# git_tag_query <- function(repo_path, tag, input, query, output, ...) {
#     repo_head <- git2r::repository_head(repo_path) # nolint
#     on.exit(git2r::checkout(repo_head)

#     git2r::checkout(repo_path, tag)
#     owl <- DO.utils::owl_xml(file.path(repo_path, input))

#     n_query <- length(query)
#     if (n_query == 0) {
#         stop("At least one `query` must be provided.")
#     }

#     if (n_query > 1) {
#         if (length(output) != length(query)) {
#             stop("`output` must be same length as `query`.")
#         }

#         res <- purrr::map(
#             query,
#             ~ owl$query
#         )

#         purrr::walk2(res, output, ~ readr::write_csv(.x, .y, ...))

#     } else {
#         if (missing(output)) {
#             stop("`output` must be specified.")
#         }

#         res <- owl$query(query)
#         readr::read_csv(res, output, ...)
#     }

#     invisible(res)
# }


# Automatically Update Data -----------------------------------------------

data_dir <- here::here("data/DO_release")

repo_path <- here::here("../Ontologies/HumanDiseaseOntology")

svn_path <- here::here("../DO_website/do_trunk")
plot_outdir <- file.path(svn_path, "static/disease_ontology/media/images/statistics")

# for data/DO_release/DO_release_details.csv
source(here::here("scripts/DO_release_details.R"))
release_details <- readr::read_csv(
    file.path(data_dir, "DO_release_details.csv")
)
latest_release <- dplyr::filter(
    release_details,
    created_at == max(created_at)
)$tag_name

# for data/DO_release/DO_term_def_counts.csv
source(here::here("scripts/DO_term_def_counts.R"))


# Generate Plots ----------------------------------------------------------

g_cb <- DO.utils::plot_citedby(out_dir = plot_outdir)
g_term <- DO.utils::plot_term_def_counts(out_dir = plot_outdir)
g_branch <- DO.utils::plot_branch_counts(repo_path, out_dir = plot_outdir)
g_xref <- DO.utils::plot_xref_counts(repo_path, out_dir = plot_outdir)
g_src <- DO.utils::plot_def_src(repo_path, out_dir = plot_outdir)


# Generate html page updates ----------------------------------------------

use_cases <- DO.utils::make_use_case_html()
web_counts <- DO.utils::update_website_count_tables(
    repo,
    latest_release,
    svn_path
)
