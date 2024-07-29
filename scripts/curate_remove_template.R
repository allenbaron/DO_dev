sparql_query <- '
# remove specified data
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX obo: <http://purl.obolibrary.org/obo/>
PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>
PREFIX DOID: <http://purl.obolibrary.org/obo/DOID_>
PREFIX doid: <http://purl.obolibrary.org/obo/doid#>
PREFIX dc: <http://purl.org/dc/elements/1.1/>
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

DELETE {
  !<<delete_stmt>>!
}
WHERE {
  !<<values_stmt>>!

  ?iri a owl:Class .
  !<<where_stmt>>!
}'

sparql_values <- c(
    "obo id" = '(?iri ?id)',
    label = '(?iri ?label)',
    "parent iri/curie" = '(?iri ?parent_iri)',
    comment = '(?iri ?comment)',
    deprecate = '(?iri ?deprecated)',
    "alternate id(s)" = '(?iri ?alt_id)',
    "term replaced by" = '(?iri ?replaced_by)',
    "synonym(s): exact" = '(?iri ?ex_syn)',
    "synonym(s): broad" = '(?iri ?b_syn)',
    "synonym(s): narrow" = '(?iri ?n_syn)',
    "synonym(s): related" = '(?iri ?r_syn)',
    "xref(s)" = '(?iri ?xref)',
    "skos mapping(s): exact" = '(?iri ?mapping)',
    "skos mapping(s): broad" = '(?iri ?mapping)',
    "skos mapping(s): narrow" = '(?iri ?mapping)',
    "skos mapping(s): related" = '(?iri ?mapping)',
    "subset(s)" = '(?iri ?subset)',
    definition = '(?iri ?definition)',
    "definition source(s)" = '(?iri ?def_src)',
    "definition source type(s)" = '(?iri ?evidence_code)',
    "acronym(s): exact" = '(?iri ?ex_acr)',
    "acronym(s): related" = '(?iri ?r_acr)'
)


sparql_where <- c(
    "obo id" = '?iri oboInOwl:id ?id .',
    "obo namespace" = '?iri oboInOwl:hasOBONamespace "disease_ontology" .',
    label = '?iri rdfs:label ?label .',
    "parent iri/curie" = '?iri rdfs:subClassOf ?parent_iri .',
    comment = '?iri rdfs:comment ?comment .',
    deprecate = '?iri owl:deprecated ?deprecated .',
    "alternate id(s)" = '?iri oboInOwl:hasAlternativeId ?alt_id .',
    "term replaced by" = '?iri obo:IAO_0100001 ?replaced_by .',
    "synonym(s): exact" = '?iri oboInOwl:hasExactSynonym ?ex_syn .',
    "synonym(s): broad" = '?iri oboInOwl:hasBroadSynonym ?b_syn .',
    "synonym(s): narrow" = '?iri oboInOwl:hasNarrowSynonym ?n_syn .',
    "synonym(s): related" = '?iri oboInOwl:hasRelatedSynonym ?r_syn .',
    "xref(s)" = '?iri oboInOwl:hasDbXref ?xref .',
    "skos mapping(s): exact" = '?iri skos:exactMatch ?mapping .',
    "skos mapping(s): broad" = '?iri skos:broadMatch ?mapping .',
    "skos mapping(s): narrow" = '?iri skos:narrowMatch ?mapping .',
    "skos mapping(s): related" = '?iri skos:relatedMatch ?mapping .',
    "subset(s)" = '?iri oboInOwl:inSubset ?subset .',
    definition = '
        ?iri obo:IAO_0000115 ?definition .

        # definition source(s)
        OPTIONAL {
          ?def_src_ax owl:annotatedSource ?iri ;
            owl:annotatedProperty obo:IAO_0000115 ;
            owl:annotatedTarget ?definition ;
            oboInOwl:hasDbXref ?def_src .
        }

        # evidence code(s)
        OPTIONAL {
          ?def_eco owl:annotatedSource ?iri ;
            owl:annotatedProperty obo:IAO_0000115;
            owl:annotatedTarget ?definition ;
            dc:type ?evidence_code .
        }',
    "acronym(s): exact" = '
        ?iri oboInOwl:hasExactSynonym ?ex_acr .

        # acronym annotation
        OPTIONAL {
          ?ex_acr_ax owl:annotatedSource ?class ;
            owl:annotatedProperty oboInOwl:hasExactSynonym ;
            owl:annotatedTarget ?ex_acr ;
            oboInOwl:hasSynonymType obo:OMO_0003012 .
        }',
    "acronym(s): related" = '
        ?iri oboInOwl:hasRelatedSynonym ?r_acr .

        # acronym annotation
        OPTIONAL {
          ?r_acr_ax owl:annotatedSource ?class ;
            owl:annotatedProperty oboInOwl:hasRelatedSynonym ;
            owl:annotatedTarget ?r_acr ;
            oboInOwl:hasSynonymType obo:OMO_0003012 .
        }'
)

sparql_delete <- c(
    "obo id" = '?iri oboInOwl:id ?id .',
    "obo namespace" = '?iri oboInOwl:hasOBONamespace "disease_ontology" .',
    label = '?iri rdfs:label ?label .',
    "parent iri/curie" = '?iri rdfs:subClassOf ?parent_iri .',
    comment = '?iri rdfs:comment ?comment .',
    deprecate = '?iri owl:deprecated ?deprecated .',
    "alternate id(s)" = '?iri oboInOwl:hasAlternativeId ?alt_id .',
    "term replaced by" = '?iri obo:IAO_0100001 ?replaced_by .',
    "synonym(s): exact" = '?iri oboInOwl:hasExactSynonym ?ex_syn .',
    "synonym(s): broad" = '?iri oboInOwl:hasBroadSynonym ?b_syn .',
    "synonym(s): narrow" = '?iri oboInOwl:hasNarrowSynonym ?n_syn .',
    "synonym(s): related" = '?iri oboInOwl:hasRelatedSynonym ?r_syn .',
    "xref(s)" = '?iri oboInOwl:hasDbXref ?xref .',
    "skos mapping(s): exact" = '?iri skos:exactMatch ?mapping .',
    "skos mapping(s): broad" = '?iri skos:broadMatch ?mapping .',
    "skos mapping(s): narrow" = '?iri skos:narrowMatch ?mapping .',
    "skos mapping(s): related" = '?iri skos:relatedMatch ?mapping .',
    "subset(s)" = '?iri oboInOwl:inSubset ?subset .',
    definition = '
        ?iri obo:IAO_0000115 ?definition .
        ?def_src_ax oboInOwl:hasDbXref ?def_src .
        ?def_eco_ax dc:type ?evidence_code .
        ',
    "definition source(s)" = '?def_src_ax oboInOwl:hasDbXref ?def_src .',
    "definition source type(s)" = '?def_eco_ax dc:type ?evidence_code .',
    "acronym(s): exact" = '
        ?iri oboInOwl:hasExactSynonym ?ex_acr .
        ?ex_acr_ax oboInOwl:hasSynonymType obo:OMO_0003012 .
        ',
    "acronym(s): related" = '
        ?iri oboInOwl:hasRelatedSynonym ?r_acr .
        ?r_acr_ax oboInOwl:hasSynonymType obo:OMO_0003012 .
        '
)

sparql_delete <- c(
    "obo id" = '?iri oboInOwl:id ?id .',
    "obo namespace" = '?iri oboInOwl:hasOBONamespace "disease_ontology" .',
    label = '?iri rdfs:label ?label .',
    "parent iri/curie" = '?iri rdfs:subClassOf ?parent_iri .',
    comment = '?iri rdfs:comment ?comment .',
    deprecate = '?iri owl:deprecated ?deprecated .',
    "alternate id(s)" = '?iri oboInOwl:hasAlternativeId ?alt_id .',
    "term replaced by" = '?iri obo:IAO_0100001 ?replaced_by .',
    "synonym(s): exact" = '?iri oboInOwl:hasExactSynonym ?ex_syn .',
    "synonym(s): broad" = '?iri oboInOwl:hasBroadSynonym ?b_syn .',
    "synonym(s): narrow" = '?iri oboInOwl:hasNarrowSynonym ?n_syn .',
    "synonym(s): related" = '?iri oboInOwl:hasRelatedSynonym ?r_syn .',
    "xref(s)" = '?iri oboInOwl:hasDbXref ?xref .',
    "skos mapping(s): exact" = '?iri skos:exactMatch ?mapping .',
    "skos mapping(s): broad" = '?iri skos:broadMatch ?mapping .',
    "skos mapping(s): narrow" = '?iri skos:narrowMatch ?mapping .',
    "skos mapping(s): related" = '?iri skos:relatedMatch ?mapping .',
    "subset(s)" = '?iri oboInOwl:inSubset ?subset .',
    definition = '
        ?iri obo:IAO_0000115 ?definition .
        ?def_src_ax oboInOwl:hasDbXref ?def_src .
        ?def_eco_ax dc:type ?evidence_code .
        ',
    "definition source(s)" = '?def_src_ax oboInOwl:hasDbXref ?def_src .',
    "definition source type(s)" = '?def_eco_ax dc:type ?evidence_code .',
    "acronym(s): exact" = '
        ?iri oboInOwl:hasExactSynonym ?ex_acr .
        ?ex_acr_ax oboInOwl:hasSynonymType obo:OMO_0003012 .
        ',
    "acronym(s): related" = '
        ?iri oboInOwl:hasRelatedSynonym ?r_acr .
        ?r_acr_ax oboInOwl:hasSynonymType obo:OMO_0003012 .
        '
)

eco_recode <- tibble::tibble(
    iri = c(
        "ECO:0007636", "ECO:0007638", "ECO:0007645", "ECO:0007646",
        "ECO:0007637", "ECO:0007644", "ECO:0007640", "ECO:0007643",
        "ECO:0007641", "ECO:0007639"
    ),
    label = c(
        "curator inference from database",
        "curator inference from Wikipedia",
        "curator inference from journal publication",
        "curator inference from book",
        "curator inference from encyclopedia",
        "curator inference from MedlinePlus dictionary",
        "curator inference from MedlinePlus encyclopedia",
        "curator inference from Merriam-Webster Dictionary",
        "curator inference from dictionary",
        "curator inference from Britannica"
    )
)

sparql_string <- c(
    "obo namespace", "label", "definition", "comment", "xref(s)",
    "alternate id(s)",
    "acronym(s): exact", "acronym(s): related",
    "synonym(s): exact", "synonym(s): broad", "synonym(s): narrow",
    "synonym(s): related",
    "skos mapping(s): exact", "skos mapping(s): broad",
    "skos mapping(s): narrow", "skos mapping(s): related"
)

sparql_query <- '
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

DELETE {
  !<<delete_stmt>>!
}
WHERE {
  !<<values_stmt>>!

  # general disease information
  ?iri a owl:Class ;
    oboInOwl:id ?id ;
    rdfs:label ?label ;
    oboInOwl:hasOBONamespace "disease_ontology" .

  # deprecation status
  OPTIONAL { ?iri owl:deprecated ?deprecated . }

  # alternate ID(s)
  OPTIONAL { ?iri oboInOwl:hasAlternativeId ?alt_id . }

  # term replaced by -- only for deprecated classes
  OPTIONAL { ?iri obo:IAO_0100001 ?replaced_by . }

  # definition
  OPTIONAL {
    ?iri obo:IAO_0000115 ?definition .

    # definition source(s) -- must have at least one when defined
    OPTIONAL {
      [] owl:annotatedSource ?iri ;
        owl:annotatedProperty obo:IAO_0000115 ;
        owl:annotatedTarget ?definition ;
        oboInOwl:hasDbXref ?def_src .
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

  # parent(s) -- must have at least one
  OPTIONAL {
    ?iri rdfs:subClassOf ?parent_IRI .
    FILTER(!isBlank(?parent_IRI))
  }

  # synonym(s)
  OPTIONAL { ?iri oboInOwl:hasExactSynonym ?ex_syn }
  OPTIONAL { ?iri oboInOwl:hasBroadSynonym ?b_syn }
  OPTIONAL { ?iri oboInOwl:hasNarrowSynonym ?n_syn }
  OPTIONAL { ?iri oboInOwl:hasRelatedSynonym ?r_syn }

  # synonym(s)
######## INCOMPLETE FROM HERE -- NEED TO DETERMINE BETTER SYSTEM -> combine syn types & use values?
#   OPTIONAL {
#     ?iri oboInOwl:hasExactSynonym ?ex_acr
#     ?ex_acr_ax a owl:Axiom ;
# 		owl:annotatedSource ?class ;
# 		owl:annotatedProperty oboInOwl:hasExactSynonym ;
# 		owl:annotatedTarget ?ex_acr ;
# 		oboInOwl:hasSynonymType obo:OMO_0003012 .
#   }
  OPTIONAL {
    ?iri oboInOwl:hasBroadSynonym ?b_acr
    ?b_acr_ax a owl:Axiom ;
		owl:annotatedSource ?class ;
		owl:annotatedProperty oboInOwl:hasBroadSynonym ;
		owl:annotatedTarget ?b_acr ;
		oboInOwl:hasSynonymType obo:OMO_0003012 .
  }
  OPTIONAL { ?iri oboInOwl:hasNarrowSynonym ?n_acr }
  OPTIONAL { ?iri oboInOwl:hasRelatedSynonym ?r_acr }

  # xref(s)
  OPTIONAL { ?iri oboInOwl:hasDbXref ?xref . }

  # skos mapping(s)
  OPTIONAL { ?iri skos:exactMatch ?skos_mapping }
  OPTIONAL { ?iri skos:closeMatch ?skos_mapping }
  OPTIONAL { ?iri skos:broadMatch ?skos_mapping }
  OPTIONAL { ?iri skos:narrowMatch ?skos_mapping }
  OPTIONAL { ?iri skos:relatedMatch ?skos_mapping }

  # subset(s)
  OPTIONAL { ?iri oboInOwl:inSubset ?subset . }

  # OWL equivalent class logical axiom(s) -- likely to be incomplete
  OPTIONAL {
    ?iri owl:equivalentClass ?eq_placeholder .
    ?eq_placeholder owl:intersectionOf/(rdf:first|(rdf:rest/rdf:first)+) ?eq_element .
    ?eq_element a ?eq_type .
  }

  # OWL subclass of logical axiom(s) -- likely to be incomplete
  OPTIONAL {
    ?iri rdfs:subClassOf ?sc_placeholder .
    ?sc_placeholder owl:intersectionOf / (rdf:first | (rdf:rest/rdf:first)+) ?sc_element .
    ?sc_element a ?sc_type .
  }
}'
