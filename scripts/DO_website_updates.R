# Create WEBSITE statistics plots
# J. Allen Baron
# Created: 2022-02-28

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


# Automatically Update Data -----------------------------------------------

source("scripts/DO_release_details.R") # data/DO_release/DO_release_details.csv


# Generate Plots ----------------------------------------------------------

plot_citedby()
plot_term_def_counts()
plot_branch_counts()
plot_xref_counts()
plot_def_src("../Ontologies/HumanDiseaseOntology")


# Generate html page updates ----------------------------------------------

user_list_file <- file.path(
    "graphics",
    "website",
    paste0(
        stringr::str_remove_all(Sys.Date(), "-"),
        "-",
        "website_user_list.html"
    )
)
make_user_list_html(user_list_file)
