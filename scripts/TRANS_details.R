# Stats for TRANS (not over time because few terms/changes)

library(here)
library(tidyverse)
library(DO.utils)
library(httr)
library(googlesheets4)

data_dir <- here::here("data/ST_release")

repo_path <- here::here("../Ontologies/PathogenTransmissionOntology")
trans_owl <- file.path(repo_path, "src/ontology/trans.owl")

term_def_q <- '
    PREFIX owl: <http://www.w3.org/2002/07/owl#>

    SELECT (COUNT(?s) AS ?terms) (COUNT(?d) AS ?defs)
    WHERE {
        ?s a owl:Class .
        OPTIONAL { ?s obo:IAO_0000115 ?d . }
        FILTER CONTAINS(str(?s), "TRANS_")
        FILTER NOT EXISTS {?s owl:deprecated ?any}
    }'

def_src_q <- '
    SELECT ?id ?def ?def_src
    WHERE {
        ?class a owl:Class ;
            oboInOwl:id ?id ;
            obo:IAO_0000115 ?def .
        ?blank owl:annotatedTarget ?def ;
		    oboInOwl:hasDbXref ?def_src .

        FILTER(CONTAINS(str(?class), "TRANS_"))
        FILTER NOT EXISTS { ?class owl:deprecated ?any . }
    }'


owl <- DO.utils::owl_xml(trans_owl)


term_def_counts <- owl$query(term_def_q) %>%
    DO.utils::tidy_sparql()

readr::write_csv(
    term_def_counts,
    file.path(data_dir, "TRANS_term_def_counts.csv")
)


src_counts <- owl$query(def_src_q) %>%
    DO.utils::tidy_sparql() %>%
    dplyr::mutate(
        source = purrr::map_chr(
            def_src,
            ~ httr::parse_url(.x)$hostname
        )
    ) %>%
    dplyr::count(source, sort = TRUE) %>%
    dplyr::mutate(source = stringr::str_remove(source, "^www\\."))

readr::write_csv(
    src_counts,
    file.path(data_dir, "TRANS_def_src_counts.csv")
)

# src count table html
src_html <- src_counts %>%
    dplyr::mutate(
        dplyr::across(dplyr::everything(), ~ paste0("<td>", .x, "</td>")),
        html = paste0(
            "<tr>\n",
            "\t", source, "\n",
            "\t", n, "\n",
            "</tr>\n"
        )
    ) %>%
    .$html %>%
    unlist()

cat(src_html)

g <- src_counts %>%
    dplyr::arrange(dplyr::desc(n), source) %>%
    dplyr::mutate(source = factor(source, levels = rev(source))) %>%
    ggplot() +
    geom_col(aes(x = source, y = n), fill = DO.utils::DO_colors[["orange"]]) +
    geom_text(
        aes(x = source, label = source),
        y = min(src_counts$n) + 0.2,
        hjust = 0,
        # color = "white",
        # fontface = "bold"
    ) +
    scale_x_discrete(
        name = "Definition Source"
    ) +
    scale_y_continuous(
        name = "Total References",
        expand = expansion(mult = c(0, 0.1))
    ) +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.y = element_blank())


ggplot2::ggsave(
    filename = "graphics/website/TRANS_def_src.png",
    plot = g,
    width = 4,
    height = 3,
    units = "in",
    dpi = 600
)



# Set contributors --------------------------------------------------------

cont_gs <- "1kD7rgOWO2uVUwKYoKFSLBEpv1WZFf-GDhEusAq_H5sM"
trans_cont <- googlesheets4::read_sheet(
    ss = cont_gs,
    sheet = "TRANS",
    col_types = "c"
)

format_contrib_html <- function(cont_df) {
    paste0(
        name,
