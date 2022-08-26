# R SCRIPT to access local version of HumanDiseaseOntology git repo,
#   extract all terms to show changes before & after updating asthma
#   classification (mid-2020, for Complex disease paper), and to format as
#   csv for copy-paste into Illustrator as text tree.
#
# J. Allen Baron
# Created: 2022-08-22

# Specify releases & roots -------------------------------------------

release <- list(
    before = "v2020-04-01",
    after = "v2020-11-11"
)

root <- "DOID:2841" # asthma


# Setup -------------------------------------------------------------------

library(DO.utils) # v0.2.1
library(googlesheets4) #v1.0.0

# Access DO repository
repo <- DO.utils::DOrepo("~/Documents/Ontologies/HumanDiseaseOntology")


# Extract subtree at specified releases ------------------------------------

# capture initial repo position (for restoration)
repo$capture_head()

repo$checkout_tag(release$before)
before <- DO.utils::extract_subtree(repo$doid, root, reload = TRUE)

repo$checkout_tag(release$after)
after <- DO.utils::extract_subtree(repo$doid, root, reload = TRUE)

# return head to main (only to avoid problems with git repo later)
repo$restore_head()


# Format & save text-based tree -------------------------------------------

before_tree <- DO.utils::format_subtree(before, root)
after_tree <- DO.utils::format_subtree(after, root)

# write to diabetes_before_after google sheet
gs <- "1rZap2ECagq9VTc0W8cOYRL6J6DKee0l6WUj09QP1EjM"
googlesheets4::write_sheet(before_tree, gs, sheet = "before")
googlesheets4::write_sheet(after_tree, gs, sheet = "after")


# Ensure diseases are NEW (& not just moved) ------------------------------

# capture initial repo position (for restoration)
repo$capture_head()

in_after <- DO.utils::format_doid(after$id, "obo_CURIE")

repo$checkout_tag(release$before)
in_before <- repo$doid$query(
    paste0(
        "SELECT ?id ?label ?parent
            WHERE {
                ?class a owl:Class ;
                    rdfs:label ?label ;
                    oboInOwl:id ?id ;
                    rdfs:subClassOf ?superclass  .
                ?superclass rdfs:label ?parent_label ;
                    oboInOwl:id ?parent_id .
                BIND(
                    CONCAT(str(?parent_label), ' (', str(?parent_id), ')')
                    AS ?parent
                )
                VALUES ?class { ",
        DO.utils::vctr_to_string(in_after, delim = " "),
        " } }"
    ),
    reload = TRUE
) %>%
    tibble::as_tibble() %>%
    DO.utils::collapse_col(.cols = parent, delim = " | ")

# Answering: Were any of the 'new' after asthma diseases just moved?
#   NO, they are all truly new.
in_before_not_asthma <- in_before %>%
    dplyr::filter(!id %in% before$id)

not_in_before <- after[!after$id %in% in_before$id, ]

repo$restore_head()
