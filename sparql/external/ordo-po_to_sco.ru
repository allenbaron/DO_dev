# converts ORDO's part of hierarchy to a subClassOf hierarchy

PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX obo: <http://purl.obolibrary.org/obo/>

INSERT { ?class rdfs:subClassOf ?parent . }
WHERE {
    ?class a owl:Class ;
        rdfs:subClassOf [
        owl:onProperty obo:BFO_0000050 ;
        owl:someValuesFrom ?parent
    ] .
}
