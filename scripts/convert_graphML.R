library(here)
library(DO.utils)
library(tidyverse)
library(tidygraph)

do_owl <- DO.utils::owl_xml(
    here::here("../Ontologies/HumanDiseaseOntology/src/ontology/doid.owl")
)

do <- do_owl$query("
    SELECT ?id ?label ?parent ?plabel ?gard
    WHERE {
        ?class a owl:Class ;
            oboInOwl:id ?id ;
            rdfs:label ?label ;
            rdfs:subClassOf ?p_iri .

        ?p_iri oboInOwl:id ?parent ;
            rdfs:label ?plabel .

        OPTIONAL {
            ?class oboInOwl:hasDbXref ?gard .
            FILTER(CONTAINS(str(?gard), 'GARD'))
        }
    }"
) %>%
    tidy_sparql()

do_annotate <- dplyr::bind_rows(
    dplyr::select(do, name = id, label, gard),
    dplyr::select(do, name = parent, label = plabel)
) %>%
    DO.utils::collapse_col(gard)

do_tg <- tidygraph::as_tbl_graph(
    dplyr::select(do, id, parent)
) %>%
    tidygraph::activate(nodes) %>%
    dplyr::left_join(do_annotate, by = "name")

igraph::write_graph(
    do_tg,
    here::here("data/do_gard.graphml"),
    format = "graphml"
)
