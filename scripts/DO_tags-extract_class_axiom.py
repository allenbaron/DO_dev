import pyDOID
import os
import pandas as pd

# inputs/outputs
do_path = pyDOID.util.standardize_path("~/Documents/Ontologies/HumanDiseaseOntology")
axiom_file = pyDOID.util.standardize_path("~/Documents/DO_dev/data/DO_release/DO-class_axiom-by_tag-raw.csv")
tag_file = pyDOID.util.standardize_path("~/Documents/DO_dev/data/DO_release/DO-tags.csv")


# Read data
r = pyDOID.DOrepo(do_path)
if os.path.exists(axiom_file):
    axioms = pd.read_csv(axiom_file, index_col=["tag", "index"])
else:
    axioms = pd.DataFrame(columns=["id", "label", "type", "axiom"])


# Update list of tags
tag_dict = {}
for t in r.tags:
    tag_dict[t.name] = t.commit.committed_datetime

tags = pd.DataFrame.from_dict(
    tag_dict,
    columns=["datetime"],
    orient="index"
)
tags.to_csv(tag_file, index_label="tag")


# Identify new tags
processed_tags = set(axioms.index.get_level_values('tag'))
all_tags = set(tags.index.values)
new_tags = list(all_tags - processed_tags)


# Add axioms from new tags to raw file
a = r.tag_iterate(r.doid_edit.extract_class_axioms, which=new_tags)
new_axioms = pd.concat(a)
new_axioms.to_csv(axiom_file, mode="a", index_label=["tag", "index"])
