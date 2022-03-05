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
create_tidygraph <- function(df, root = "DOID:3070") {
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
            id == root |
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

create_tree_df <- function(tg, root = "DOID:3070") {
    tg %>%
        tidygraph::activate("nodes") %>%
        dplyr::mutate(
            order = tidygraph::dfs_rank(
                root = which(tg$id == root),
                mode = "in"
            ),
            dist = tidygraph::dfs_dist(
                root = which(tg$id == root),
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


# Capture "after" version of low grade glioma ------------------------------
#   Same execution, only top-level DOID in query differs
q_lg <- '
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX obo: <http://purl.obolibrary.org/obo/>
PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>

SELECT ?id ?label ?parent_id ?parent_label
WHERE {
    ?class a owl:Class ;
        rdfs:subClassOf* obo:DOID_0080829 ;
        rdfs:subClassOf ?parent ;
        oboInOwl:id ?id ;
        rdfs:label ?label .
    ?parent oboInOwl:id ?parent_id ;
        rdfs:label ?parent_label .
    FILTER(!isblank(?parent))
}'

repo$git$checkout(release$after)
after_lg <- repo$doid$query(q_lg, load = TRUE) %>%
    tibble::as_tibble()

# return head to main (to avoid problems with git repo later)
repo$git$checkout("main")

after_lg_tg <- create_tidygraph(after_lg, root = "DOID:0080829")
after_lg_tree <- create_tree_df(after_lg_tg, root = "DOID:0080829")

# write to google sheet
googlesheets4::write_sheet(after_lg_tree, gs, sheet = "after_low_grade")


# Identify terms in "after" missing from "before" -------------------------

q_missing_before <- '
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX obo: <http://purl.obolibrary.org/obo/>
PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>

SELECT ?id ?label ?parent_id ?parent_label
WHERE {
    ?class a owl:Class .

    VALUES ?class {
        obo:DOID_5074 obo:DOID_5889 obo:DOID_5508 obo:DOID_5509
        obo:DOID_5890 obo:DOID_0080889 obo:DOID_5503 obo:DOID_0080890
        obo:DOID_0080892 obo:DOID_0080891 obo:DOID_0080879 obo:DOID_0080880
        obo:DOID_0080875 obo:DOID_0080876 obo:DOID_0080854 obo:DOID_0080881
        obo:DOID_0080904 obo:DOID_0080877 obo:DOID_0080878 obo:DOID_7154
        obo:DOID_0080882 obo:DOID_0080888 obo:DOID_4844 obo:DOID_5500
        obo:DOID_5507 obo:DOID_5075 obo:DOID_5505 obo:DOID_4843
        obo:DOID_5077 obo:DOID_5504 obo:DOID_4857
    }

    OPTIONAL { ?class oboInOwl:id ?id . }
    OPTIONAL { ?class rdfs:label ?label . }
    OPTIONAL {
        ?class rdfs:subClassOf ?parent .
        ?parent oboInOwl:id ?parent_id ;
            rdfs:label ?parent_label .
    }
}'

repo$git$checkout(release$before)
before_miss <- repo$doid$query(q_missing_before, load = TRUE) %>%
    tibble::as_tibble() %>%
    tidyr::unnest(parent_id, keep_empty = TRUE) %>%
    tidyr::unnest(parent_label, keep_empty = TRUE)

before_miss
