import pyDOID
import pandas as pd

do_path = "~/Documents/Ontologies/HumanDiseaseOntology"
r = pyDOID.DOrepo(do_path)
a = r.tag_iterate(r.doid_edit.extract_class_axioms)
df = pd.concat(a)
df.to_csv("../data/DO_release/DO-class_axiom-by_tag-raw.csv")

t_dict = {}
for t in r.tags:
    t_dict[t.name] = t.commit.committed_datetime
t_df = pd.DataFrame.from_dict(t_dict, orient = "index")
t_df.to_csv("../data/DO_release/DO-tags.csv")
