# Python script to execute DO release reports (term, def counts) on all
#   releases.
# Created: 2021-08-06; Updated: 2022-02-28

import os
from datetime import datetime
import pyDOID
import pandas as pd

start = datetime.now()

# set file input/output
do_dir = '~/Documents/Ontologies/HumanDiseaseOntology/'
doid_owl = os.path.join(do_dir, 'src/ontology/doid.owl')

do_dev = '~/Documents/DO_dev/'
release_stat_dir = os.path.join(do_dev, "data/DO_release")
release_file = os.path.join(release_stat_dir, 'DO_term_def_counts.csv')

# load repo & identify new tags
rel_df = pd.read_csv(release_file)
do_repo = pyDOID.DOrepo(do_dir)

tags = do_repo.tags
tags_sorted = sorted(do_repo.tags, key=lambda t: t.commit.committed_datetime)
tag_names = [t.name for t in tags]

new_tag_names = list(set(tag_names).difference(rel_df['tag_name'].to_list()))

# define sparql queries
q = """
PREFIX owl: <http://www.w3.org/2002/07/owl#>

SELECT (COUNT(?s) AS ?terms) (COUNT(?d) AS ?defs) WHERE {
?s a owl:Class .
OPTIONAL { ?s obo:IAO_0000115 ?d . }
FILTER STRSTARTS(str(?s), "http://purl.obolibrary.org/obo/DOID_")
FILTER NOT EXISTS {?s owl:deprecated ?any}
}
"""


res_dict = do_repo.tag_iterate(
    do_repo.doid.query,
    start=new_tag_names[0],
    query=q,
    load=True
)

res_dict
df = pd.concat(res_dict)
df = df.droplevel(1)
df.index.name = "tag_name"
df = df.reset_index()
print(df)

df_app = pd.concat([rel_df, df], ignore_index=True)
df_app.to_csv(release_file, index = False)

print(datetime.now() - start)
