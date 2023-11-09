SELECT ?id ?label ?def_src
WHERE {
    ?class a owl:Class ;
        oboInOwl:id ?id ;
        rdfs:label ?label ;
        obo:IAO_0000115 ?def .
    FILTER(CONTAINS(str(?class), 'DOID'))

    ?blank owl:annotatedSource ?class ;
        owl:annotatedProperty obo:IAO_0000115 ;
        owl:annotatedTarget ?def ;
        oboInOwl:hasDbXref ?def_src .
} LIMIT 1
