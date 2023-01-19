# add children of 'disease by infectious agent' to DO_infectious_disease_slim

PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>
PREFIX DOID: <http://purl.obolibrary.org/obo/DOID_>
PREFIX doid: <http://purl.obolibrary.org/obo/doid#>

INSERT { ?class oboInOwl:inSubset doid:DO_infectious_disease_slim . }
WHERE {
    ?class a owl:Class ;
        rdfs:subClassOf* DOID:0050117 .

    FILTER NOT EXISTS { ?class oboInOwl:inSubset doid:DO_infectious_disease_slim . }
    FILTER NOT EXISTS { ?class owl:deprecated ?any . }
}
