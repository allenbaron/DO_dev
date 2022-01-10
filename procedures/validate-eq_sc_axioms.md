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
