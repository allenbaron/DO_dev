# Create WEBSITE statistics plots
# J. Allen Baron
# Created: 2022-02-28

library(DO.utils)


# UPDATE CSV FILES BEFORE RUNNING!!! --------------------------------------

# 1. data/citedby/DO_citedby.csv - EXECUTE scripts/citedby_full_procedure.R
# 2. data/DO_release/DO_release_details.csv - EXECUTE scripts/DO_release_details.R
# 3. data/DO_release/DO_term_def_counts.csv - EXECUTE
#   scripts/DO_term_def_counts-from_git.py (in PyCharm).
#
# Manually copy & paste stats from Google Sheet - DO_github_release_log
#   (https://docs.google.com/spreadsheets/d/1-ZSUH43MJloR2EsBqHpGeY6IfKG7Gt8KBcU5remnoGI/edit#gid=269344614)
# 4. data/DO_release/branch_counts.csv
# 5. data/DO_release/cross_references.csv


# Generate Plots ----------------------------------------------------------

plot_citedby()
plot_term_def_counts()
plot_branch_counts()
plot_xref_counts()
plot_def_src("../Ontologies/HumanDiseaseOntology")



