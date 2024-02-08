# converts ORDO into a more DO-friendly format
# - ID: Convert skos:notation to oboInOwl:id
# - HIERARCHY:
#  - Convert part of hierarchy to rdfs:subClassOf
#  - Convert disease type from is_a hierarchy to annotation
# - SYNONYMS: Convert efo:alternative_term to oboInOwl:hasExactSynonym (https://github.com/EBISPOT/efo/issues/1166)
# - MAPPINGS:
#  - Retain only exact matches as xref
#  - Convert annotated xrefs to matching skos mappings (or special notMatch/uncertain preds)

PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX obo: <http://purl.obolibrary.org/obo/>
PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX efo: <http://www.ebi.ac.uk/efo/>
PREFIX ORDO: <http://www.orpha.net/ORDO/Orphanet_>
PREFIX doid: <http://purl.obolibrary.org/obo/doid#>

# Standardize ORDO xref mapping_type annotations
DELETE { ?xref_annot obo:ECO_0000218 ?xref_eco }
INSERT { ?xref_annot obo:ECO_0000218 ?xref_type }
WHERE {
    ?class oboInOwl:hasDbXref ?xref .
    ?xref_annot owl:annotatedSource ?class ;
        owl:annotatedProperty oboInOwl:hasDbXref ;
        owl:annotatedTarget ?xref ;
        obo:ECO_0000218 ?xref_eco .
    FILTER( lang(?xref_eco) IN ("", "en") )
    BIND(
        REPLACE(
            str(?xref_eco), # convert to string to avoid possible language tag mismatches
            ".*(E|NTBT|BTNT|W|ND)(.|\\n)*",
            "$1"
        ) AS ?xref_type
    )
};

# DOID-ify
DELETE {
    ?class skos:notation ?id ;
        rdfs:subClassOf ?part_of ;
        rdfs:subClassOf ?disease_type ;
        efo:alternative_term ?syn ;
        oboInOwl:hasDbXref ?xref .
    ?xref_annot ?p ?o .
    ?part_of owl:onProperty obo:BFO_0000050 ;
        owl:someValuesFrom ?parent .
}
INSERT {
    ?class oboInOwl:id ?id ;
        rdfs:subClassOf ?parent ;
        doid:disease_type ?disease_type_label ;
        oboInOwl:hasExactSynonym ?syn ;
        ?mapping_type ?xref ;
        oboInOwl:hasDbXref ?xref_exact .
}
WHERE {
    ?class a owl:Class ;
        skos:notation ?id .
    FILTER( STRSTARTS(?id, "ORPHA:") )

    OPTIONAL {
        ?class rdfs:subClassOf ?part_of .
        ?part_of owl:onProperty obo:BFO_0000050 ;
            owl:someValuesFrom ?parent .
    }

    OPTIONAL {
        ?disease_type rdfs:subClassOf ORDO:C001 ;
            rdfs:label ?disease_type_label .
        ?disease_child rdfs:subClassOf+ ORDO:377788 .
        ?class rdfs:subClassOf+ ?disease_type .
    }

    OPTIONAL {
        ?class efo:alternative_term ?syn .
    }

    OPTIONAL {
        VALUES (?xref_type ?mapping_type) {
            ("E" skos:exactMatch)
            ("BTNT" skos:narrowMatch)
            ("NTBT" skos:broadMatch)
            ("ND" doid:uncertain)
            ("W" doid:notMatch)
        }
        ?class oboInOwl:hasDbXref ?xref .
        ?xref_annot owl:annotatedSource ?class ;
            owl:annotatedProperty oboInOwl:hasDbXref ;
            owl:annotatedTarget ?xref ;
            obo:ECO_0000218 ?xref_type .
        ?xref_annot ?p ?o .
    }

    OPTIONAL {
        ?class oboInOwl:hasDbXref ?xref_exact .
        ?xref_annot owl:annotatedSource ?class ;
            owl:annotatedProperty oboInOwl:hasDbXref ;
            owl:annotatedTarget ?xref_exact ;
            obo:ECO_0000218 "E" .
    }
}
