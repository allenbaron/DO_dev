# R SCRIPT to access local version of HumanDiseaseOntology git repo,
#   extract all terms to show changes before & after updating diabetes
#   classification (early 2022, for Complex disease paper), and to format as
#   csv for copy-paste into Illustrator as text tree.
#
# J. Allen Baron
# Created: 2022-03-24
# Last Updated: 2022-08-19


# Specify releases & roots -------------------------------------------

release <- list(
    before = "v2022-01-31",
    before_v2 = "v2016-12-16",
    after = "v2022-03-02",
    after_v2 = "v2022-06-07",
    after_v3 = "v2022-07-27"
)

root <- list(
    before = "DOID:9351", # same vor v2a tree
    before2 = "DOID:0050524", # MODY
    after = "DOID:0081062", # same for v2 & v3 trees
    after2 = "DOID:1837", # diabetic ketoacidosis
    after3 = "DOID:11716" # prediabetes syndrome
    # diabetic peripheral angiopathy --> obsoleted, added manually
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


# Create version 2 of "after" tree ----------------------------------------
#   - minor update to "after" tree in v2022-06-07

repo$capture_head()
repo$checkout_tag(release$after_v2)

after_v2 <- DO.utils::extract_subtree(repo$doid, root$after, reload = TRUE)
after_v2_tree <- DO.utils::format_subtree(after_v2, root$after)
googlesheets4::write_sheet(after_v2_tree, gs, sheet = "after_v2")

repo$restore_head()

# Create version 3 of "after" tree ----------------------------------------
#   - update to "after" tree in v2022-07-27

repo$capture_head()
repo$checkout_tag(release$after_v3)

after_v3 <- DO.utils::extract_subtree(repo$doid, root$after, reload = TRUE)
after_v3_tree <- DO.utils::format_subtree(after_v3, root$after)
googlesheets4::write_sheet(after_v3_tree, gs, sheet = "after_v3")

repo$restore_head()


# Create version 2 of "before" tree ----------------------------------------
#   - update to "before" tree in v2016-12-06 (requires two trees)

repo$capture_head()
repo$checkout_tag(release$before_v2)

before_v2a <- DO.utils::extract_subtree(repo$doid, root$before, reload = TRUE)
before_v2a_tree <- DO.utils::format_subtree(before_v2a, root$before)

# MODY, only disease + parent; no need to format tree (fails anyway)
before_v2b <- DO.utils::extract_subtree(repo$doid, root$before2)

googlesheets4::write_sheet(before_v2a_tree, gs, sheet = "before_v2a")
googlesheets4::write_sheet(before_v2b, gs, sheet = "before_v2b")

repo$restore_head()


# Add "after_v3" diseases to match "before_v2" ----------------------------

repo$capture_head()
repo$checkout_tag(release$after_v3)

after_v3b <- DO.utils::extract_subtree(repo$doid, root$after2, reload = TRUE)
after_v3c <- DO.utils::extract_subtree(repo$doid, root$after3)

googlesheets4::write_sheet(after_v3b, gs, sheet = "after_v3b")
googlesheets4::write_sheet(after_v3c, gs, sheet = "after_v3c")

repo$restore_head()
