# Prepare Tweet of Rare Diseases in the DO
# By J. Allen Baron
# 2022-02-11

# Also used to identify diseases with ORDO/GARD xrefs that are NOT in the
#   DO_rare_slim (for review and potential addition to slim).


# Files -------------------------------------------------------------------

# input
doid_file <- here::here("../Ontologies/HumanDiseaseOntology/src/ontology/doid.owl")
sparql_q <- here::here("sparql/DO-rare-id_label_rareid.rq")

# output
vd_file <- here::here("graphics/DO-rare_vd-20220211.svg")


# Setup -------------------------------------------------------------------

library(reticulate)
library(here)
library(tidyverse)
library(DO.utils)
library(googlesheets4)
library(VennDiagram)
library(grid)

# work around
py_rdf <- reticulate::import_from_path(
    "py_rdf",
    system.file("python", package = "DO.utils", mustWork = TRUE)
)


# Custom Functions --------------------------------------------------------

# reformat data to list expected by venn.diagram
as_vd_data <- function(df, venn_col, groups, id) {
    vd_list <- purrr::map(
        groups,
        ~ dplyr::filter(df, {{ venn_col }} == .x) %>%
            .[[id]] %>%
            unique()
    )
    vd_lengths <- purrr::map_int(vd_list, length)
    names(vd_list) <- paste0(groups, " (", vd_lengths, ")")

    vd_list
}

# to create a venn diagram (allows plotting when printed)
plot_vd <- function(..., filename = NULL, device = "svg", width = NA,
                    height = NA, units = "in", dpi = 300) {
    # remove meaningless logs (results accessible with "$result")
    venn_diagram <- purrr::quietly(VennDiagram::venn.diagram)
    if (is.null(filename)) {
        vd <- venn_diagram(..., filename = NULL, disable.logging = TRUE)$result
    } else {
        # create version for printing
        vd <- venn_diagram(..., filename = NULL, disable.logging = TRUE)$result

        # better save method ("svg" method of venn.diagram fails)
        ggplot2::ggsave(
            filename = filename,
            plot = vd,
            device = device,
            width = width,
            height = height,
            units = units,
            dpi = dpi
        )
    }

    # set class for print method
    class(vd) <- c("venn_diagram", class(vd))
    invisible(vd)
}

# print method for venn diagram
print.venn_diagram <- function(x, ...) {
    grid::grid.newpage()
    grid::grid.draw(x)
}


# Extract rare diseases ---------------------------------------------------

do_owl <- py_rdf$read(doid_file)
do_rare <- py_rdf$sparql_query(do_owl, sparql_q) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(group = stringr::str_remove(rare_id, ":.*")) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
        in_slim = any(group == "DO_rare_slim"),
        has_xref = any(group %in% c("ORDO", "GARD"))
    ) %>%
    dplyr::ungroup()


# Identify xrefs +/- rare slim -----------------------------------------

xref_slim_overlap <- do_rare %>%
    dplyr::select(-group) %>%
    # to make order of xrefs consistent in output
    dplyr::arrange(id, rare_id) %>%
    dplyr::filter(rare_id != "DO_rare_slim" | !has_xref) %>%
    DO.utils::collapse_col(rare_id, delim = " | ")

# save to google sheets for review
googlesheets4::write_sheet(
    xref_slim_overlap,
    "https://docs.google.com/spreadsheets/d/1mxA_6IjLvlw5V98rz4OOUp3msxf25wNxYdAeJkOIGPE/edit?usp=sharing",
    "xref_slim_overlap_2022-02-11"
)


# Create & Save Venn Diagram ------------------------------------------

# reformat data
v_data <- as_vd_data(
    df = do_rare,
    venn_col = group,
    groups = unique(do_rare$group),
    id = "id"
)

v_data_xref_only <- as_vd_data(
    df = do_rare,
    venn_col = group,
    groups = c("ORDO", "GARD"),
    id = "id"
)

# define colors
rare_color <- list(
    # extract colors with https://imagecolorpicker.com/en
    # from https://www.orpha.net/consor/cgi-bin/index.php
    ordo = c(dark = "#064d8f", mid = "#5d99d8", light = "#c7e1fa"),
    # from https://beta.rarediseases.info.nih.gov/diseases/5740/addisons-disease
    gard = c(dark = "#7f2754", mid = "#a96d8c", light = "#d0afc0")
)

rare_color_xref <- c(rare_color$gard["dark"], rare_color$ordo["dark"])

plot_vd(
    x = v_data_xref_only,
    category.names = names(v_data_xref_only),
    filename = vd_file,
    device = "svg",
    height = 2,
    width = 2,
    unit = "in",
    resolution = 300,
    lwd = 1,
    fill = rare_color_xref,
    cex = 0.75,
    fontfamily = "sans",
    cat.default.pos = "outer",
    cat.pos = c(170, 190),
    cat.cex = 0.75,
    cat.dist = 0.05,
    cat.fontfamily = "sans",
    alpha = 0.75
)
