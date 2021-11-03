# Generate breast cancer graph for October Breast Cancer Awareness Tweet
# J. Allen Baron
# 2021-10-20

library(here)
library(tidyverse)
library(ontologyIndex)
library(tidygraph)
library(ggraph)
library(cowplot)
library(svglite)

# unused packages; UNCOMMENT TO USE!!!
# # for addition of pink DO logo
#     library(googledrive)
#     library(ggimage)
# library(ggtext) # for attempt to rotate labels, abandoned


# Load doid.obo data (hard-coded, relative path) --------------------------
do_obo_path <- here::here(
    "../Ontologies/HumanDiseaseOntology/src/ontology/doid.obo"
)

do_obo <- ontologyIndex::get_ontology(
    do_obo_path, # checked out release: v2021-10-11
    propagate_relationships = "is_a",
    extract_tags = "minimal"
)


# Extract breast cancer & descendants -------------------------------------
bc_doid <- ontologyIndex::get_descendants(
    do_obo,
    roots = "DOID:1612"
)

do_bc <- purrr::map(do_obo, ~ .x[names(.x) %in% bc_doid]) %>%
    "class<-"(class(do_obo))
# set breast cancer as root node
do_bc$parents[["DOID:1612"]] <- character(0)

# check how many are obsolete (NONE, that's good)
sum(do_bc$obsolete)


# Convert to tidygraph ----------------------------------------------------

do_bc_relations <- tibble::tibble(
    id = names(do_bc$parents),
    parent = do_bc$parents,
) %>%
    tidyr::unnest_longer(parent) %>%
    # ignore parent terms that are outside of breast cancer
    dplyr::filter(parent %in% bc_doid) #| id == "DOID:1612")

do_bc_tg <- do_bc_relations %>%
    tidygraph::as_tbl_graph() %>%
    tidygraph::activate("nodes") %>%
    dplyr::mutate(
        label = do_bc$name[name],
        # label only root/leaf nodes
        rl_label = dplyr::if_else(
            purrr::map_int(do_bc$children[name], length) == 0 |
                label == "breast cancer",
            label,
            NA_character_
        )
    )

# check properties of nodes --> graph arranged as expected
tg_check <- do_bc_tg %>%
    dplyr::mutate(
        root = tidygraph::node_is_root(),
        in_only = tidygraph::node_is_sink(), # expect: same as root
        center = tidygraph::node_is_center(), # expect: same as root
        leaf = tidygraph::node_is_leaf(),
        out_only = tidygraph::node_is_source(), # expect: same as leaf
        unconnected = tidygraph::node_is_isolated(), # expect: none
        connected_to_all = tidygraph::node_is_universal(), # expect: none
        #keyplayer = tidygraph::node_is_keyplayer()
    ) %>%
    tibble::as_tibble()

tg_check %>%
    dplyr::summarize(
        root_correct = label[root] == "breast cancer",
        in_only_eq_root = all(in_only == root),
        center_eq_root = all(center == root),
        out_only_eq_leaf = all(out_only == leaf),
        unconnected = any(unconnected),
        connected_to_all = any(connected_to_all)
    ) %>%
    tidyr::pivot_longer(
        cols = everything(),
        names_to = "test",
        values_to = "result"
    )


# Add data to control graph properties ------------------------------------

# get DO logo in pink (from Google Drive)
drive_id <- googledrive::as_id("https://drive.google.com/file/d/1xGiSHE_H2u12Vzu5TtkD9Y44yzjl8WOx/view?usp=sharing")
do_logo_file <- here::here("do_logo_pink.png")
googledrive::drive_download(drive_id, path = do_logo_file)

# set breast cancer pink colors for graphing ("picked" from NBCF website)
pink <- c(std = "#ff6185", light = "#ecaab8")

# calculate graph properties
do_bc_tg <- do_bc_tg %>%
    tidygraph::activate("nodes") %>%
    dplyr::mutate(
        root = tidygraph::node_is_root(),
        leaf = tidygraph::node_is_leaf(),
        dist_to_root = tidygraph::node_distance_to(root),
        # specify vertical arrangement with "breast cancer" at top, farthest
        #   term at bottom
        dend_height = max(dist_to_root) - dist_to_root,
            # shorten all child only nodes slightly & equally
            dend_height = dplyr::if_else(leaf, dend_height + 0.6, dend_height),
        # parent labels include term name & DOID
        parent_label = dplyr::if_else(
            !leaf,
            paste0(label, "\n[", name, "]"),
            NA_character_
        ),
        # color parent labels pink, with "breast cancer" darker
        parent_pink = dplyr::if_else(root, pink["std"], pink["light"]),
        # child only labels include DOID only
        child_label = dplyr::if_else(leaf, name, NA_character_),
        # bold "breast cancer"
        ff = dplyr::if_else(root, "bold", "plain"),
    )


# Create plot -------------------------------------------------------------

g_bc <- ggraph::ggraph(
    graph = do_bc_tg,
    layout = "dendrogram", direction = "in", circular = TRUE,
    height = dend_height
) +
    ggraph::geom_edge_link() +
    # add parent labels
    ggraph::geom_node_label(
        aes(label = parent_label, fontface = ff, fill = parent_pink),
        family = "mono", size = 3, hjust = 0.5,
        repel = TRUE, point.size = NA
    ) +
    scale_fill_identity() +
    # add child only labels
    ggraph::geom_node_label(
        aes(label = child_label, family = "mono"),
        angle = 270, size = 2
    ) +
    theme_void() +
    # add background (void makes it empty.. probably not look good on twitter)
    theme(panel.background = element_rect(fill = "white"))

# add pink DO logo to top-left
g_bc_logo <- cowplot::ggdraw() +
    cowplot::draw_plot(g_bc) +
    cowplot::draw_image(do_logo_file,  x = -0.43, y = 0.4, scale = .2)

# View
g_bc_logo

ggsave(g_bc_logo, filename = "DO_breast_cancer_graph-20211022.png",
       width = 15, height = 4.25, units = "in")


# EXTRA (previous experiments) --------------------------------------------

# colors
colorscale_v3 <- viridisLite::viridis(3)
colorscale_light3 <- c("#cadc4d", "#f7a349", "#01d8a0")
colorscale_light2 <- c("#ffb85d", "#02e393")

tg_extra <- do_bc_tg %>%
    dplyr::mutate(
        r = row_number(),
        rl_color = dplyr::case_when(
            root ~ colorscale_v3[3],
            leaf ~ colorscale_v3[1],
            TRUE ~ colorscale_v3[2]
        ),
        rl_size = dplyr::case_when(
            root ~ 4,
            leaf ~ 1,
            TRUE ~ 2
        ),
        label_color = dplyr::case_when(
            name == "DOID:1612" ~ colorscale_light2[1],
            !is.na(rl_label) ~ colorscale_light2[2],
            TRUE ~ NA_character_
        ),
        rn_label = dplyr::if_else(
            !leaf,
            label,
            NA_character_
        ),
        rn_label_color = dplyr::case_when(
            root ~ label_color,
            !is.na(rn_label) ~ colorscale_light2[2]
        ),
        # label_angle = dplyr::if_else(root, 0, 90),
        text_nudge_y = dplyr::if_else(leaf, -0.2, 0)
    )

# variations on non-circular dendogram layout
ggraph(
    graph = do_bc_tg,
    layout = "dendrogram", direction = "in",
    height = dend_height
) +
    geom_edge_elbow(flipped = TRUE, strength = 0.95) +
    # geom_edge_bend(flipped = TRUE, strength = 0.5) +
    # geom_edge_diagonal(strength = 2) +
    # geom_edge_link() +
    # geom_node_point() +
    geom_node_label(
        aes(label = parent_label, family = "mono", fontface = ff, fill = parent_pink),
        size = 3, hjust = 0.5,
        repel = TRUE, point.size = NA
    ) +
    # PROBLEM: ggplot2 can't rotate labels out of the box!!!
    #   - Attempted fix: ggtext --> how to tell it where nodes are?
    # geom_richtext(aes(x = ..x.., y = ..y.., label = label))
    geom_node_label(
        aes(label = child_label, family = "mono"),
        angle = 270, size = 2, #nudge_y = -0.15,
        # repel = TRUE
    ) +
    # )
    scale_fill_identity()

ggraph(graph = do_bc_tg, layout = "dendrogram", direction = "in",
       height = dend_height) +
    # geom_edge_elbow(flipped = TRUE, strength = 1) +
    geom_edge_bend(flipped = TRUE, strength = 0.5) +
    geom_node_point() +
    #scale_color_identity() +
    scale_size_identity() +
    geom_node_label(
        # aes(label = rl_label, fill = rl_color, fontface = ff), repel = TRUE
        aes(label = r, fill = rn_label_color, fontface = ff),
    ) +
    scale_fill_identity()


# tested various layouts; kk, nicely, etc looked good but I realized a
#   dendrogram would be neatest
ggraph(graph = do_bc_tg, layout = "kk") +
    geom_edge_link() +
    geom_node_point() +
    #scale_color_identity() + # "identity" functions use data directly from data
    scale_size_identity() +
    geom_node_label(
        aes(label = rn_label, fill = rn_label_color, fontface = ff),
        repel = TRUE
    ) +
    scale_fill_identity()


# was going to add simplified pink DO logos instead of DOIDs but never tried it
#   because I liked the DOIDs; UNCOMMENT CODE BELOW TO DOWNLOAD THE FILES!!!

# # get DO logo in (light) pink from Google Drive
# drive_ids <- googledrive::as_id(
#     x = c(
#         pink = "https://drive.google.com/file/d/1wyC9-zrODaBmNwk4yU_ae1YbCndZ-pa1/view?usp=sharing",
#         light_pink = "https://drive.google.com/file/d/1wxDCjs8Auc6T90nNPTAptDHO4YU6CJ0F/view?usp=sharing"
#     )
# )
#
# do_logo_files <- c(
#     pink = here::here("do_pink.png"),
#     light_pink = here::here("do_light_pink.png")
# )
# googledrive::drive_download(drive_ids["pink"], path = do_logo_files["pink"])
# googledrive::drive_download(
#     drive_ids["light_pink"],
#     path = do_logo_files["light_pink"]
# )


# MORE IDEAS
# colors/text size:
#   1. by distance from breast cancer
#   2. by number of genes/alleles/models annotated with term
#   3. checkout other genomic databases for breast cancer - DO links
# make breast cancer larger than everything else
# try different layouts
# include info about number of breast cancer publications that have used DO

