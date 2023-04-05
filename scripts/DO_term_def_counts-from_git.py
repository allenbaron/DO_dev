# Python script to execute DO release reports (term, def counts) on all
#   releases.
# Created: 2021-08-06; Updated: 2022-02-28

import os
from datetime import datetime
import pyDOID
import pandas as pd

start = datetime.now()

# set file input/output
do_dir = pyDOID.util.standardize_path(
    '~/Documents/Ontologies/HumanDiseaseOntology/'
)
doid_owl = os.path.join(do_dir, 'src/ontology/doid.owl')

do_dev = pyDOID.util.standardize_path('~/Documents/DO_dev/')
release_stat_dir = os.path.join(do_dev, "data/DO_release")
release_file = os.path.join(release_stat_dir, 'DO_term_def_counts.csv')

# load repo & identify new tags
if os.path.isfile(release_file):
    rel_df = pd.read_csv(release_file)
else:
    rel_df = None

do_repo = pyDOID.DOrepo(do_dir)

tags = do_repo.tags
tags_sorted = sorted(do_repo.tags, key=lambda t: t.commit.committed_datetime)
tag_names = [t.name for t in tags]

if isinstance(rel_df, pd.DataFrame):
    new_tag_names = list(set(tag_names).difference(rel_df['tag_name'].to_list()))
else:
    new_tag_names = list(tag_names)

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
    which=new_tag_names,
    query=q,
    reload=True
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
