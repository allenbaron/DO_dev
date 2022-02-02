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

### Considerations

I have a Python script with code that iterates through tags of a git repo. This was made for getting DO release information but is not generalizable as is. If possible, I'd prefer to do this in R because I just am not familiar enough with Python to get this done within the next 2 business days, which is the time I have remaining to extract this data. I might be able to write it as another python module inside the DO.utils R package but the interface between R and Python via `reticulate` is still pretty hard to manage/understand. The tidyverse team appears to use the R package `gert` for git management and, on quick glance, that looks like it could work.


### Methods

1. Create R function to read in doid-edit.owl file as lines.
2. Create R `extract_*_axiom()` family of functions to do just that.

As I started writing functions to iterate through git tags utilizing `gert` I discovered that it lacks the ability to checkout tags. While I found a workaround, creating temporary branches and checking those out, I then discovered that I could not easily sort tags by date. To do so requires `git_tag_list()` followed by iterating through the listed commits and extracting info with `git_commit_info()`. While this does get that information I need, that last function is extremely slow. It would take minutes just to get the tag date (and this takes no observable time in the python implementation).

[2022-01-29] Given the challenges with using `gert` for iteration through git tags, I've decided to try my hand at python. The interface between R and Python via `reticulate` is way too confusing to be manageable at this point so I think my best bet is to create a stand alone Python package.

3. Create placeholder files for a basic Python package, currently called "pyDOID".
4. Create the `DOrepo` class that builds on the `git.Repo` class provided by GitPython.
5. Convert the code that iterates through git tags from the script to the `tag_iterate()` method of `DOrepo`.

At this point I realized it would likely be easier to create an interface between R and Python with a single Python package and I was already migrating or creating in pyDOID much of the functionality I'd intended to put in those DO.utils modules, so I decided to migrate their functionality to this package. They were not required to validate class axioms but the methods were already fully written and were added with little to no modification.

5. Create owl module inside pyDOID and the owl xml class.
6. Migrate `rdflib`-based functions from DO.utils python module 'py_rdf' to the owl module.

Back to procedures directly relevant to class axioms...

7. Create the owl functional class for handling the doid-edit.owl file.
8. Define owl functional `extract_class_axioms()` method.
9. Make pyDOID a working python package by adding dependencies and install directives, as well as, fixing errors.

At this point I had the ability to iterate through tags and extract class axioms from doid-edit.owl, so I installed pyDOID in a virtual environment (`pip install -e pyDOID` along with it's dependencies) and wrote a script in this project (`scripts/DO_tags-extract_class_axioms.py`) to combine these functionalities and accomplish the class axiom extraction.

**SUCCESS** (executed with the working directory set to `scripts`)
