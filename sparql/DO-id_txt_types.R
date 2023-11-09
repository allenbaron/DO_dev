SELECT ?id ?type ?text
WHERE {
    {
        ?class a owl:Class ;
            oboInOwl:id ?id ;
            rdfs:label ?text .
        BIND('label' AS ?type)
    }
    UNION
    {
        VALUES ?syn_types {
            oboInOwl:hasExactSynonym
            oboInOwl:hasBroadSynonym
            oboInOwl:hasNarrowSynonym
            oboInOwl:hasRelatedSynonym
        }
        ?class a owl:Class ;
            oboInOwl:id ?id ;
            ?syn_types ?text .
        BIND(
            CONCAT(
                'synonym_',
                lcase(
                    STRBEFORE(
                        STRAFTER(str(?syn_types), '#has'),
                        'Synonym'
                    )
                )
            ) AS ?type
        )
    }
}
