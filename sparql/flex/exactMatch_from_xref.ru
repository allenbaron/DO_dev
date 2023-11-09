# Create skos:exactMatch from all existing xrefs
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>
PREFIX DOID: <http://purl.obolibrary.org/obo/DOID_>

INSERT { ?class skos:exactMatch ?mapping . }
WHERE {
	VALUES ?class { !<<class_curies>>! }
	VALUES ?mapping { !<<xref_strings>>! }
	?class oboInOwl:hasDbXref ?mapping .
}