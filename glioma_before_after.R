# R SCRIPT to access local version of HumanDiseaseOntology git repo,
#   extract all terms below malignant/high grade glioma (DOID:3070) updated
#   in conjunction with CIViC in early 2021, and to format as csv for copy-paste
#   into Illustrator as text tree.
#
# J. Allen Baron
# 2022-03-03


# Setup -------------------------------------------------------------------

library(tidyverse) # v1.3.1
library(reticulate) # v1.24
p <- reticulate::import("pyDOID") # pyDOID v0.1.0
library(tidygraph) # v1.2.0
library(DO.utils) # v0.1.7.9000
library(googlesheets4) #v1.0.0

# Functions for creating the cell-based tree (csv/Excel/Google Sheets)
create_tidygraph <- function(df) {
    tg <- df %>%
        dplyr::select(id, parent_id) %>%
        tidygraph::as_tbl_graph() %>%
        tidygraph::activate("nodes") %>%
        dplyr::filter(!tidygraph::node_is_sink())
    in_tg <- tg %>%
        tibble::as_tibble() %>%
        .$name

    label_df <- df %>%
        dplyr::filter(
            id == "DOID:3070" |
                (id %in% in_tg & parent_id %in% in_tg)
        ) %>%
        DO.utils::collapse_col_flex(parent_id, parent_label)

    tg %>%
        # add back labels
        tidygraph::left_join(
            label_df,
            by = c("name" = "id")
        )
}

create_tree_df <- function(tg) {
    tg %>%
        tidygraph::activate("nodes") %>%
        dplyr::mutate(
            order = tidygraph::dfs_rank(
                root = which(tg$id == "DOID:3070"),
                mode = "in"
            ),
            dist = tidygraph::dfs_dist(
                root = which(tg$id == "DOID:3070"),
                mode = "in"
            ),
            insert = paste0("V", dist)
        ) %>%
        tidygraph::arrange(order) %>%
        tidygraph::as_tibble() %>%
        dplyr::select(
            parent_id, parent_label, name,
            tidyselect::everything()
        ) %>%
        tidyr::pivot_wider(
            names_from = insert,
            values_from = label
        )
}

# Path to DO repo on my local machine
repo <- p$DOrepo("~/Documents/Ontologies/HumanDiseaseOntology")


# Specify releases for checkout ------------------------------------------

tags <- repo$tags
release <- list()
reticulate::iterate(
    tags,
    function(t) {
        if(t$name == "v2020-12-22") {
            release$before <<- t
        }
        if(t$name == "v2022-03-02") {
            release$after <<- t
        }
    }
)


# Define SPARQL query -----------------------------------------------------

q <- '
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX obo: <http://purl.obolibrary.org/obo/>
PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>

SELECT ?id ?label ?parent_id ?parent_label
WHERE {
    ?class a owl:Class ;
        rdfs:subClassOf* obo:DOID_3070 ;
        rdfs:subClassOf ?parent ;
        oboInOwl:id ?id ;
        rdfs:label ?label .
    ?parent oboInOwl:id ?parent_id ;
        rdfs:label ?parent_label .
    FILTER(!isblank(?parent))
}'


# Execute query for specified releases ------------------------------------

repo$git$checkout(release$before)
before <- repo$doid$query(q, load = TRUE) %>%
    tibble::as_tibble()

repo$git$checkout(release$after)
after <- repo$doid$query(q, load = TRUE) %>%
    tibble::as_tibble()

# return head to main (only to avoid problems with git repo later)
repo$git$checkout("main")


# Format for cell-based output ----------------------------------------------

before_tg <- create_tidygraph(before)
before_tree <- create_tree_df(before_tg)

after_tg <- create_tidygraph(after)
after_tree <- create_tree_df(after_tg)

# write to google sheet
gs <- "1rwbRy-WmfBPHw4XmPvwoswlHskUR90ilur174PhXN8c"
googlesheets4::write_sheet(before_tree, gs, sheet = "before")
googlesheets4::write_sheet(after_tree, gs, sheet = "after")

