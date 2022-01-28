# Procedure: Validation of Equivalent Class and SubClassOf Axioms
By: J. Allen Baron
Initiated: 2022-01-10


# Background

The DO team has observed unexplained loss of `owl:equivalentClass` (EQclass) and `rdfs:subClassOf` (subclass) axioms after some releases (including the most recent release). These undesirable changes have only come to our attention because of other work and are not identified in a systematic manner. We are, therefore, concerned that some of these axioms have been lost without our notice and that there may be more lost in future releases without our knowledge.


## GOALS

1. Compare EQclass and subclass axioms across recent releases to identify axioms that have been lost or changed.
2. Establish an automated test, to be implemented during release builds, for the loss of these axioms.


## Current State

During release builds, EQclass and subclass axioms are counted with the SPARQL query `HumanDiseaseOntology/src/sparql/build/logical-definitions-report.rq`, however, individual axioms are not extracted or reviewed in any way.

Lynn has created a SPARQL query to extract a list of EQclass axioms `HumanDiseaseOntology/src/sparql/extra/EQ_in_DO.rq` but not subclass axioms. This lists the DOIDs, with their labels, which is fine but not sufficient. While DOID classes, at this point, have no more than one EQclass axiom, there are many classes with multiple subclass axioms and these are not currently uniquely identified.


## Work Needed

1. Update Lynn's EQclass SPARQL query to uniquely identify each axiom, in case multiple are created for a class in the future.
2. Create a SPARQL query to extract subclass axioms and ensure each is uniquely identified.
3. Write script to extract axioms from the most recent releases of DO, using SPARQL queries from #1 and #2 along with git.
    - Analyze changes in these axioms across these releases.
4. Add an automated test to the release build to identify removed (and maybe also modified) axioms.


## Considerations

In order to identify when axioms disappear it may be necessary to uniquely identify each axiom. Since DO terms only have one `owl:equivalentClass` each, it may be sufficient to simply identify DO terms with these axioms. However, many DO terms have multiple `owl:subClassOf` axioms and these axioms tend to be much more complex.


# Methods

## Initial Approach: SPARQL queries

### Identify Unique `owl:equivalentClass` axioms

At this point in time [2022-01-26], all well-formed axioms of this type are blank classes composed of the `owl:intersectionOf` (i.e. "AND") of a higher-level DO term and an `owl:Restriction`, which is composed of an `owl:onProperty` (e.g. RO/IDO terms) and `owl:someValuesFrom` (i.e. 'some') a target (e.g. UBERON, SO, CL, some combination via a blank class node, etc.).

I created two SPARQL queries to extract components of these axioms in an effort to uniquely identify them:

1. `sparql/DO-eqC_axiom.rq` returns a bunch of axiom variables without modification.
2. `sparql/DO-eqC_axiom-tidy.rq` returns a single axiom variable with the components of the axiom combined in a more human readable form (**PREFERRED**).

These SPARQL queries take _~ 2 sec_ to execute but _ignore malformed axioms_. To identify potential axiom problems (there is one currently in DO that I identified as part of this work; it has been fixed in the doid-edit.owl by Lynn), I created another SPARQL query: `sparql/DO-eqC_axiom-problem.rq`.


#### Result

This approach uniquely identifies most equivalentClass axioms but I did not extract enough information from the more complex axioms (those made up of blank classes derived from the union/intersection of multiple classes).


### Identify Unique `owl:subClassOf` axioms

[2022-01-27] These axioms are defined in one of two ways: as an `owl:Restriction` or a blank `owl:Class`. While most of the Restriction versions are simple, comprised of an `owl:onProperty` and `owl:someValuesFrom`, there are quite a few of both types that are very complex.

I created two SPARQL queries to extract components of these axioms in an effort to uniquely identify them:

1. `sparql/DO-subClass_axiom.rq` returns a bunch of axiom variables without modification.
2. `sparql/DO-subClass_axiom-tidy.rq` returns a single axiom variable with the components of the axiom combined in a more human readable form (**PREFERRED**).

The various versions of these queries I've created are all slow, taking _>15 sec_. Depending on how complete the queries are, they may not complete in any reasonable timeframe (_> 3 min_).


#### Result

[2022-01-28] While these queries are currently in a functional state, I do not consider them complete and it's unlikely they extract sufficient information to uniquely identify the subClassOf axioms.


## Exploration for Alternate Methods

### SPARQL Query Count Method

Lynn and I previously discussed just listing DO term IDs and labels as many times as they have an equivalent class or subClassOf axiom. I've created the following 'count' SPARQL queries for this purpose:

1. `sparql/DO-eqC_axiom-count.rq` to count equivalent class axioms by DO term.
2. `sparql/DO-subClass_axiom-count.rq` to count subClassOf axioms by DO term.
3. `sparql/DO-class_axiom-count.rq` to count both equivalent class and subClassOf axioms by DO term.

Ultimately, these will fail to help us identify axioms that have disappeared where 1 or more axioms of the same type are added in a release. This may not be an issue for equivalent class axioms but it could definitely be an issue with subClassOf axioms. I started this effort because Lynn wanted to have a list of terms or terms + axioms that we could use to identify when they are lost.


### Extraction Directly from doid-edit.owl

The pattern for equivalent class axioms in the doid-edit.owl file is straightforward: they always start with "EquivalentClasses". I can also identify what DO term they belong to as it is the first listed in parentheses. `grep "EquivalentClasses" src/ontology/doid-edit.owl` effectively returns all 721 current equivalent class axioms.

The pattern for subClassOf axioms is more complex. All subclasses, including non-axioms, start with "SubClassOf" and first list the DO term they belong to. The subClassOf axioms can be identified as not having a second DO term immediately following the first. These can be identified with the following code: `grep "SubClassOf" src/ontology/doid-edit.owl | grep -E -v -c "SubClassOf\(obo:DOID_[0-9]+ obo:DOID"`, which identifies the 7,457 subClassOf axioms in the current release.

After a little more examination I can see that subClassOf axioms can be identified by simpler code: `grep -c "SubClassOf.*Object" src/ontology/doid-edit.owl` because they all contain one of various "Object" statements that identify blank class construction (e.g. one of `ObjectSomeValuesFrom`, `ObjectUnionOf`, `ObjectIntersectionOf`, `ObjectExactCardinality`, `ObjectMinCardinality`; all corresponding to owl URIs).

The restriction-based subClassOf axioms (using the conventions I identified earlier) all have `ObjectSomeValuesFrom` immediately preceding the DO term the axiom belongs to, with most having only this statement, while the class-based subClassOf axioms all have `ObjectUnionOf` or `ObjectIntersectionOf` in this position.

**This is a sure-fire method for identifying these axioms**, assuming the doid-edit.owl format has not changed, and is also much faster than the SPARQL queries.


### Conclusion of Exploration

The approach most likely to succeed, be simple, and fast is direct extraction from the doid-edit.owl file. This will be my final implementation.


## Final Approach: Direct Extraction from doid-edit.owl
