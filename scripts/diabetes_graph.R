# Generate diabetes graph for November Diabetes Awareness Tweet
# J. Allen Baron
# 2021-11-19


# Load libraries and custom functions -------------------------------------

library(here)
library(tidyverse)
library(ontologyIndex)
library(tidygraph)
library(ggraph)
library(ggtext)
library(cowplot)
library(svglite)

is_descendant <- function(id) {
    tidygraph:::expect_nodes()
    .graph <- tidygraph:::.G()
    descendants <- igraph::subcomponent(
        .graph,
        v = id,
        mode = "in"
    ) %>%
        attr("names")

    .N()$name %in% descendants
}


# Load doid.obo data (hard-coded, relative path) --------------------------
do_obo_path <- here::here(
    "../Ontologies/HumanDiseaseOntology/src/ontology/doid.obo"
)

do_obo <- ontologyIndex::get_ontology(
    do_obo_path, # checked out release: v2021-11-17
    propagate_relationships = "is_a",
    extract_tags = "minimal"
)


# Extract diabetes mellitus, descendants, parents, and top-level n --------

diabetes_children <- ontologyIndex::get_descendants(do_obo,roots = "DOID:9351")
diabetes_ancestors <- ontologyIndex::get_ancestors(do_obo, "DOID:9351")
diabetes_relations <- c(diabetes_children, diabetes_ancestors)

do_diabetes <- purrr::map(do_obo, ~ .x[names(.x) %in% diabetes_relations]) %>%
    "class<-"(class(do_obo))

# check how many are obsolete (NONE, that's good)
sum(do_diabetes$obsolete)


# Convert to tidygraph ----------------------------------------------------

do_diabetes_relations <- tibble::tibble(
    id = names(do_diabetes$parents),
    parent = do_diabetes$parents
) %>%
    tidyr::unnest_longer(id) %>%
    tidyr::unnest_longer(parent) %>%
    # remove non-existing relationships -> may cause problems for tidygraph
    dplyr::filter(!is.na(parent)) %>%
    # ignore parent terms that are part of alternate paths
    dplyr::filter(parent %in% diabetes_relations)

do_diabetes_tg <- do_diabetes_relations %>%
    tidygraph::as_tbl_graph() %>%
    tidygraph::activate("nodes") %>%
    dplyr::mutate(label = do_obo$name[name])

# check properties of nodes --> graph arranged as expected
tg_check <- do_diabetes_tg %>%
    dplyr::mutate(
        root = tidygraph::node_is_root(), # no root identified but works in
                    # dendrogram with direction = "in"
        in_only = tidygraph::node_is_sink(), # identifies root
        center = tidygraph::node_is_center(),
        leaf = tidygraph::node_is_leaf(), # leaf calculation also did not work
        out_only = tidygraph::node_is_source(), # expect: same as leaf
        unconnected = tidygraph::node_is_isolated(), # expect: none
        connected_to_all = tidygraph::node_is_universal(), # expect: none
        #keyplayer = tidygraph::node_is_keyplayer()
    ) %>%
    tibble::as_tibble()

tg_check %>%
    dplyr::summarize(
        #root_correct = label[root] == "disease",
        center_eq_in_only = all(center == in_only),
        #out_only_eq_leaf = all(out_only == leaf),
        unconnected = any(unconnected),
        connected_to_all = any(connected_to_all)
    ) %>%
    tidyr::pivot_longer(
        cols = everything(),
        names_to = "test",
        values_to = "result"
    )


# Add data to control graph properties ------------------------------------

# get DO logo (from Google Drive)
drive_id <- googledrive::as_id("https://drive.google.com/file/d/18vvBQGin8NAc0HBeafc-rTtEDpy3pT0-/view?usp=sharing")
do_logo_file <- here::here("graphics/DO_blue_logo.png")
googledrive::drive_download(drive_id, path = do_logo_file)

# use DO colors for graphing
chosen_colors <- c(std = "#3a9596", # blue_dark
          light = "#afd2d2", # blue_light
          default = "#FFFFFF"
)

# set graph properties
do_diabetes_tg <- do_diabetes_tg %>%
    tidygraph::activate("nodes") %>%
    dplyr::mutate(
        root = tidygraph::node_is_sink(),
        leaf = tidygraph::node_is_source(),
        dist_to_root = tidygraph::node_distance_to(root),
        # CHOOSE:
            # which is CENTRAL FOCUS: Diabetes mellitus
            central_focus = label == "diabetes mellitus",
            # which are OF INTEREST: non-series descendants of Dm
            of_interest = is_descendant("DOID:9351"),
            # which to LABEL FULLY: non-series nodes
            make_full_label = !stringr::str_detect(label, "[0-9]$"),
        # create labels
            # create full labels (term name & DOID)
            full_label = dplyr::if_else(
                make_full_label,
                paste0(label, "\n[", name, "]"),
                NA_character_
            ),
            # create DOID only labels
            doid_label = dplyr::if_else(!make_full_label, name, NA_character_),
        # set vertical arrangement
            # root at top, farthest term at bottom
            dend_height = max(dist_to_root) - dist_to_root,
            # shorten nodes w/o full labels nodes slightly & equally
            dend_height = dplyr::if_else(
                !make_full_label,
                dend_height + 0.6,
                dend_height
            ),
            # EXTRA: shorten non-focus distances
            dend_height = dend_height - dplyr::if_else(
                dist_to_root < 5, (dend_height - 2) * 0.7, 0
            ),
        # set label formatting
            # set colors
            label_color = dplyr::case_when(
                central_focus ~ chosen_colors["std"],
                of_interest ~ chosen_colors["light"],
                TRUE ~ chosen_colors["default"]
            ),
            # set typeface
            ff = dplyr::if_else(central_focus, "bold", "plain"),
    )


# Create plot -------------------------------------------------------------

g_plot <- ggraph::ggraph(
    graph = do_diabetes_tg,
    layout = "dendrogram", direction = "in",
    #circular = TRUE,
    height = dend_height
) +
    ggraph::geom_edge_bend(strength = 0.4) +
    ggraph::geom_node_label(
        aes(label = full_label, fontface = ff, fill = label_color),
        family = "mono", size = 3, hjust = 0.5,
        repel = TRUE, point.size = NA,
    ) +
    scale_fill_identity() +
    ggraph::geom_node_label(
        aes(label = doid_label, family = "mono"),
        angle = 270, size = 2,
    ) +
    theme_void() +
    # add background (void makes it empty.. probably not look good on twitter)
    theme(panel.background = element_rect(fill = "white"))


# add DO logo to top-left
g_complete <- cowplot::ggdraw() +
    cowplot::draw_plot(g_plot) +
    # ADJUST position of DO logo!!!
    cowplot::draw_image(do_logo_file,  x = -0.37, y = 0.37, scale = .2)

# View
g_complete

ggsave(g_complete,
       # CHANGE filename and ADJUST plot size
       filename = here::here("graphics/DO_diabetes_graph-20211119.png"),
       width = 12, height = 6, units = "in")
