# Python script to execute DO release reports (term, def counts) on all
#   releases.
# Created: 2021-08-06

import os
from datetime import datetime
from git import Repo
import rdflib
import pandas as pd

start = datetime.now()

do_dir = '../Ontologies/HumanDiseaseOntology/'
release_stat_dir = "data/DO_release"

do_repo = Repo(do_dir)
do_tags = do_repo.tags

doid_owl = os.path.join(do_dir, 'src/ontology/doid.owl')

q_term = """
PREFIX owl: <http://www.w3.org/2002/07/owl#>

SELECT (COUNT(?s) AS ?classes) WHERE {
?s a owl:Class .
FILTER STRSTARTS(str(?s), "http://purl.obolibrary.org/obo/DOID_")
FILTER NOT EXISTS {?s owl:deprecated ?any}
}
"""

q_def = """
PREFIX obo: <http://purl.obolibrary.org/obo/>
PREFIX owl: <http://www.w3.org/2002/07/owl#>

SELECT (COUNT(?s) AS ?classes) WHERE {
  ?s a owl:Class .
  FILTER STRSTARTS(str(?s), "http://purl.obolibrary.org/obo/DOID_") .
  ?s obo:IAO_0000115 ?definition .
  FILTER NOT EXISTS {?s owl:deprecated ?any}
}
"""

do_dict = {}

for t in do_tags:
    do_repo.git.checkout(t)

    g = rdflib.Graph()
    g.parse(doid_owl, format='application/rdf+xml')

    r_term = g.query(q_term)
    r_def = g.query(q_def)

    do_dict[str(t)] = {'terms': int([r[0] for r in r_term][0]), 'defs': int([r[0] for r in r_def][0])}

    g.close()

df = pd.DataFrame.from_dict(do_dict, orient='index')

print(df)

df.to_csv(os.path.join(release_stat_dir, 'DO_term_def_counts.csv'))

print(datetime.now() - start)
