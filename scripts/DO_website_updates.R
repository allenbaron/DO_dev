# Create WEBSITE statistics plots
# J. Allen Baron
# Created: 2022-02-28
# Last Updated: 2022-09-14

library(here)
library(DO.utils)


# MANUALLY Update Data!!! -------------------------------------------------

# 1. data/citedby/DO_citedby.csv - source(scripts/citedby_full_procedure.R)
# 2. data/DO_release/DO_term_def_counts.csv - EXECUTE
#   scripts/DO_term_def_counts-from_git.py (in PyCharm) --> convert to R script via DO.utils!!!
#
# Manually copy & paste stats from Google Sheet - DO_github_release_log
#   (https://docs.google.com/spreadsheets/d/1-ZSUH43MJloR2EsBqHpGeY6IfKG7Gt8KBcU5remnoGI/edit#gid=269344614)
# 2. data/DO_release/branch_counts.csv
# 3. data/DO_release/cross_references.csv

latest_release <- "v2022-08-29"
repo_path <- here::here("../Ontologies/HumanDiseaseOntology")
svn_path <- here::here("../DO_website")
plot_outdir <- file.path(svn_path, "media/images/statistics")

# Automatically Update Data -----------------------------------------------

source("scripts/DO_release_details.R") # data/DO_release/DO_release_details.csv

repo <- DO.utils::DOrepo(repo_path)

# Generate Plots ----------------------------------------------------------

DO.utils::plot_citedby(out_dir = plot_outdir)
DO.utils::plot_term_def_counts(out_dir = plot_outdir)
DO.utils::plot_branch_counts(out_dir = plot_outdir)
DO.utils::plot_xref_counts(out_dir = plot_outdir)
DO.utils::plot_def_src(repo_path)


# Generate html page updates ----------------------------------------------

DO.utils::make_use_case_html()
DO.utils::update_website_count_tables(repo, latest_release, svn_path)
