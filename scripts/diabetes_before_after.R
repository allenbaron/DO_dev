# R SCRIPT to access local version of HumanDiseaseOntology git repo,
#   extract all terms to show changes before & after updating diabetes
#   classification (early 2022, for Complex disease paper), and to format as
#   csv for copy-paste into Illustrator as text tree.
#
# J. Allen Baron
# 2022-03-24


# Specify releases & roots -------------------------------------------

release <- list(
    before = "v2022-01-31",
    after = "v2022-03-02"
)

root <- list(
    before = "DOID:9351",
    after = "DOID:0081062"
)


# Setup -------------------------------------------------------------------

library(DO.utils) # v0.2.1
library(googlesheets4) #v1.0.0

# Access DO repository
repo <- DO.utils::DOrepo("~/Documents/Ontologies/HumanDiseaseOntology")


# Extract subtree at specified releases ------------------------------------

# capture initial repo position (for restoration)
repo$capture_head()

repo$checkout_tag(release$before)
before <- DO.utils::extract_subtree(repo$doid, root$before, reload = TRUE)

repo$checkout_tag(release$after)
after <- DO.utils::extract_subtree(repo$doid, root$after, reload = TRUE)

# return head to main (only to avoid problems with git repo later)
repo$restore_head()


# Format & save text-based tree -------------------------------------------

before_tree <- DO.utils::format_subtree(before, root$before)
after_tree <- DO.utils::format_subtree(after, root$after)

# write to diabetes_before_after google sheet
gs <- "1Z5qH9AXHy_WbzYipRw5mIU7gBxx0ea8ZsSSuV_P67aA"
googlesheets4::write_sheet(before_tree, gs, sheet = "before")
googlesheets4::write_sheet(after_tree, gs, sheet = "after")
