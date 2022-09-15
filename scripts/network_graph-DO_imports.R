library(DO.utils)
library(tidyverse)
library(tidygraph)
library(ggraph)


# Get import-DOID relationships (roughly) ---------------------------------

axioms <- DO.utils::extract_class_axiom("../Ontologies/HumanDiseaseOntology")
axiom_links <- tibble::tibble(
    doid = stringr::str_extract(unlist(axioms), "obo:DOID_[0-9]+"),
    obo_id = stringr::str_extract_all(unlist(axioms), "obo:[A-Za-z_0-9]+")
) %>%
    DO.utils::unnest_cross(obo_id, keep_empty = TRUE) %>%
    dplyr::filter(!stringr::str_detect(obo_id, 'DOID|RO_|IDO_')) %>%
    dplyr::mutate(
        dplyr::across(
            dplyr::contains("id"),
            ~ stringr::str_replace_all(.x, c("obo:" = "", "_" = ":"))
        )
    ) %>%
    unique()# %>%
    # break multi-parentage preserving all relationships (required for
    #   tree-based plots, ignoring for drl)
    dplyr::group_by(doid) %>%
    dplyr::mutate(obo_id = paste(obo_id, doid, sep = ">>"))



# Get DOID-DOID relationships ---------------------------------------------

r <- DO.utils::DOrepo("../Ontologies/HumanDiseaseOntology")
parentage <- r$doid_merged$query(
    'SELECT ?id ?parent_id
    WHERE {
        ?class a owl:Class ;
            oboInOwl:id ?id ;
            rdfs:subClassOf ?parent .
        ?parent oboInOwl:id ?parent_id .
        FILTER(CONTAINS(str(?class), "DOID"))
        FILTER NOT EXISTS { ?class owl:deprecated ?dep . }
    }'
) %>%
    tibble::as_tibble() %>%
    DO.utils::unnest_cross(where(is.list), keep_empty = TRUE)# %>%
    # drop multi-parentage preserving only 1 relationship with parent with fewest
    #   children (required for tree-based plots, ignoring for 'drl')
    dplyr::add_count(parent_id, name = "parent_n") %>%
    dplyr::group_by(id) %>%
    dplyr::filter(dplyr::row_number(parent_n) == 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-parent_n)


# Combine data for full relationship graph --------------------------------

links <- dplyr::bind_rows(
    dplyr::select(parentage, child = id, parent = parent_id),
    dplyr::select(axiom_links, child = obo_id, parent = doid)
) %>%
    dplyr::mutate(
        relationship = dplyr::if_else(
            stringr::str_detect(child, ">>"),
            "import",
            "nosology"
        )
    )

tg <- tidygraph::as_tbl_graph(links) %>%
    tidygraph::activate("nodes") %>%
    dplyr::mutate(ns = stringr::str_remove(name, ":.*"))

leaves <- which(as_tibble(mutate(tg, leaf = node_is_leaf()))$leaf)

tg <- tg %>%
    tidygraph::activate("edges") %>%
    dplyr::mutate(strength = dplyr::case_when(
        from %in% leaves & relationship == "import" ~ 0.01, 0.5))



# Define point color scheme -----------------------------------------------

ns_color <- c(
    "DOID" = DO_colors[["sat"]],
    purrr::set_names(
        hues::iwanthue(dplyr::n_distinct(imports)),
        imports
    )
)


# Plot --------------------------------------------------------------------

graph <- ggraph::ggraph(
    tg,
    layout = "drl",
    options = igraph::drl_defaults$refine,
    weights = strength
) +
    geom_edge_link(aes(color = relationship), width = 0.1) +
    scale_edge_color_manual(
        values = c("import" = "grey70", "nosology" = "grey30")
    ) +
    geom_node_point(aes(color = ns), size = 0.2) +
    scale_color_manual(
        values = ns_color
    ) +
    theme_void() +
    theme(legend.position = "none")

ggplot2::ggsave(
    "graphics/DO_import-network-drl.png",
    plot = graph,
    height = 3,
    width = 3
)


# EXTRA - sunburst plot ---------------------------------------------------

sun_graph <- ggraph::ggraph(tg, layout = "partition", direction = "in", circular = TRUE) +
    geom_edge_link(aes(color = relationship), width = 0.1) +
    scale_edge_color_manual(
        values = c("import" = "grey70", "nosology" = "grey30")
    ) +
    geom_node_point(aes(color = ns), size = 0.2) +
    scale_color_manual(
        values = ns_color
    ) +
    theme(legend.position = "none")
