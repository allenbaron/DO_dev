# Disease review of tuberculosis

library(tidyverse)
library(DO.utils)

de <- "~/Documents/Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl"

# Extract all tuberculosis-related diseases -------------------------------

tb_query <- '#description: All tuberculosis-related diseases with all info, except logical axioms
#input: doid.owl
#id: DOq001
#description: All diseases and their linked data in the DO
#input: doid.owl
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX obo: <http://purl.obolibrary.org/obo/>
PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>
PREFIX dc: <http://purl.org/dc/elements/1.1/>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX DOID: <http://purl.obolibrary.org/obo/DOID_>

SELECT ?id ?label ?definition ?deprecated ?replaced_by
  (GROUP_CONCAT(?parent_full;separator="|") AS ?parent)
  (GROUP_CONCAT(?src;separator="|") AS ?def_src)
  (GROUP_CONCAT(?evidence_code;separator="|") AS ?eco)
  (GROUP_CONCAT(?alt;separator="|") AS ?alt_id)
  (GROUP_CONCAT(?syn;separator="|") AS ?syn_typed)
  (GROUP_CONCAT(?subset;separator="|") AS ?slim)
WHERE {
  # limit to tuberculosis (+ children) and tuberculosis-related by label
  FILTER(CONTAINS(str(?label), "tubercul") || EXISTS { ?iri rdfs:subClassOf* DOID:399 } )

  # general disease information
  ?iri a owl:Class ;
    oboInOwl:id ?id ;
    rdfs:label ?label ;
    oboInOwl:hasOBONamespace "disease_ontology" .

  # deprecation status
  OPTIONAL { ?iri owl:deprecated ?deprecated . }

  # alternate ID(s)
  OPTIONAL { ?iri oboInOwl:hasAlternativeId ?alt . }

  # term replaced by -- only for deprecated classes
  OPTIONAL { ?iri obo:IAO_0100001 ?replaced_by . }

  # definition
  OPTIONAL {
    ?iri obo:IAO_0000115 ?definition .

    # definition source(s)
    OPTIONAL {
      [] owl:annotatedSource ?iri ;
        owl:annotatedProperty obo:IAO_0000115 ;
        owl:annotatedTarget ?definition ;
        oboInOwl:hasDbXref ?src .
    }

    # evidence code(s)
    OPTIONAL {
      [] owl:annotatedSource ?iri ;
        owl:annotatedProperty obo:IAO_0000115 ;
        owl:annotatedTarget ?definition ;
        dc:type ?evidence_code .
    }
  }

  # comment
  OPTIONAL { ?iri rdfs:comment ?comment . }

  # parent(s)
  OPTIONAL {
    ?iri rdfs:subClassOf ?parent_id .
    ?parent_id rdfs:label ?parent_label .
    BIND(CONCAT(str(?parent_id), "-", ?parent_label) AS ?parent_full)
    FILTER(!isBlank(?parent_id))
  }

  # synonym(s)
  OPTIONAL {
    VALUES ?syn_relation {
       oboInOwl:hasExactSynonym oboInOwl:hasBroadSynonym
       oboInOwl:hasNarrowSynonym oboInOwl:hasRelatedSynonym
    }
    ?iri ?syn_relation ?synonym .
    BIND(CONCAT(?synonym, "-" , str(?syn_relation)) AS ?syn)
  }

  # subset(s)
  OPTIONAL { ?iri oboInOwl:inSubset ?subset . }
}
GROUP BY ?id ?label ?definition ?deprecated ?replaced_by'

tb <- DO.utils::robot_query(de, query = tb_query, tidy_what = "everything")

# identify diseases in TB branch
temp_owl <- tempfile(fileext = ".owl")
DO.utils::robot("convert", i = de, o = temp_owl)
de_owl <- owl_xml(temp_owl)
tb_branch <- extract_subtree(de_owl, "DOID:399")
tb_tree <- format_subtree(tb_branch, "DOID:399")

tb1 <- DO.utils::robot_query(de, query = tb_query, tidy_what = "everything")

# identify diseases in TB branch
temp_owl1 <- tempfile(fileext = ".owl")
DO.utils::robot("convert", i = de, o = temp_owl1)
de_owl1 <- owl_xml(temp_owl1)
tb_branch1 <- extract_subtree(de_owl1, "DOID:399")
tb_tree1 <- format_subtree(tb_branch1, "DOID:399")


export_file <- tempfile(fileext = ".tsv")

robot(
    "export",
    i = de,
    e = export_file,
    header = '"ID|Equivalent Class|SubClassOf [ANON]"'
)

# tb_export <- readr::read_tsv(export_file, col_type = "c") %>%
#     DO.utils::tidy_sparql() %>%
#     dplyr::filter(id %in%
