# Procedure: Identifying New Diseases
By: J. Allen Baron
Created: 2022-08-26
Updated: ...


# Goal

To expand the diseases contained within the Disease Ontology.


# Potential Data Sources

Possible data sources are those with curated disease information, which include:
- OMIM
- Orphanet
- GARD (Genetic and Rare Diseases)
- MeSH
- MONDO
- PubMed

We already have a collaboration with MGI to add/update OMIM diseases but there is not any mechanism for identifying diseases in these other resources that are not in the DO. It does have historic relationships with MeSH (input) and MONDO (output) but the links have not been maintained.


# Plan

New disease identification will probably be prioritized as follows: OMIM > Orphanet > GARD > MeSH ~ MONDO ~ PubMed. Lynn is already in the process of adding new OMIM diseases.

## Orphanet

Compare DO to ORDO as follows:

1. Extract ORDO xrefs from DO.
	- Minimally check these for accuracy by comparing the ORDO label with DO's corresponding disease label & synonyms (ORDO names should _probably_ be included as synonyms).
2. Identify Orphanet diseases that do not have xrefs in DO and extract the following information:
	- Class
	- Label
	- Synonyms
	- Parent Class + Label
	- Xrefs
	- Xref type (type = my wording): Orphanet records information about Xrefs (that is similar in nature to skos mappings) in logical axioms
		- Axiom format:
			- annotatedSource: Class
			- annotatedProperty: Xref predicate
			- annotatedTarget: Specific Xref
			- ECO_0000218 (Manual Assertion): Xref type
		- Xref types (abbreviations):
			- E: Exact
			- NTBT: narrow term mapped to broader term
			- BTNT: broad term mapped to narrower term
			- ND: not decided/uncertain
			- W: wrong
		- Xref format:
			- grep pattern for abbreviations in OWL file: ">[- ]*([A-Z]+)" --> followed by text description of abbreviation
			- SPARQL extract pattern: "[- ]*([A-Z]+)"
				- Some abbreviations are prefixed with "- " (not sure why)
2. Use ORDO Xrefs to exclude matches already in DO
	- This process should create mappings between ORDO & DO (Xrefs) that are missing and could be accomplished with one or more of the following procedures:
	1. Direct comparison of Xrefs between DO & ORDO
		- Example:
			1. Exclude ORDO terms that are already Xrefs in DO.
			2. ORDO has particular OMIM/GARD Xref --> find DOID with that Xref (if it exists)
			3. Add ORDO Xref to identified DOID.
	2. Curate mapping suggestions from biomappings.
	3. Use fuzzy string matching to identify Xrefs.


### Possible Approaches for Automated New Disease Recommendations

1. Extract descendants/ancestors of Orphanet terms that _are_ Xrefs in DO and then add ORDO diseases that _are not_ in DO maintaining a same/similar relationship to DO term (DO mirrors ORDO hierarchy).
	- Need to ensure descendants/ancestors are not already in DO.
	- Nothing is known about the similarity or dissimilarity of ORDO and DO disease trees.
2. Review all ORDO diseases not Xrefs in DO.
