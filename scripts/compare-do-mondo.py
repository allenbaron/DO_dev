# This python script compares classes in the latest versions of the Human Disease
# Ontology (DO) and the Monarch Disease Ontology (MONDO).
#
# The following questions are addressed:
#   1. Which MONDO diseases are equivalent to DO terms (according to MONDO's mappings)?
#   2. Which umapped MONDO diseases chain map to DO terms (limited to OMIM, UMLS,
#      Orphanet, NCI, or GARD)?
#   3. Which unmapped MONDO diseases can be mapped to DO terms by name matching (labels
#      & synonyms)?
#   4. Which MONDO and DO diseases cannot be mapped to one another by any of the above
#      methods?
#
# Output includes the full comparison data in TSV format and summary statistics.

from datetime import datetime
import os
import numpy as np
import pandas as pd
import rdflib
import re
import requests

############################################################################
#### CUSTOM FUNCTIONS
############################################################################


def get_version(filepath):
    # Compile the regex for efficiency
    regex = re.compile(r"versionIRI.*obo/[^/]+/releases/([^/]+)/")

    if not os.path.exists(filepath):
        return None

    with open(filepath, "r") as file:
        for line in file:
            match = regex.search(line)
            if match:
                break

    if not match:
        raise ValueError(f"versionIRI not found in {filepath}")

    version_date = match.group(1)
    version_date = datetime.strptime(version_date, "%Y-%m-%d")

    return version_date


def as_str(value):
    if isinstance(value, rdflib.Literal):
        return value.toPython()
    return value


_MONDO_MAPPING_NORMALIZERS = [
    (re.compile(r"^OMIM:|.*omim\.org/entry/"), "MIM:"),
    (re.compile(r"^OMIMPS:|.*omim\.org/phenotypicSeries/PS"), "MIM:PS"),
    (re.compile(r"^.*Orphanet[:_]"), "ORDO:"),
    (re.compile(r"^UMLS:|.*umls/id/"), "UMLS_CUI:"),
    (re.compile(r"^SCTID:|.*snomedct/"), "SNOMEDCT_US:"),
    (re.compile(r"^.*(ICD|browse)(9|10)(CM|WHO|.*2019/en#)?[:/]"), r"ICD\2CM:"),
    (re.compile(r".*mesh/"), "MESH:"),
    (re.compile(r".*medgen/"), "MEDGEN:"),
    (re.compile(r"^MedDRA|.*meddra/"), "MEDDRA:"),
    (re.compile(r"icd11\.foundation:|http://id\.who\.int/icd/entity/"), "ICD11:"),
    (re.compile(r".*NCIT[:_]"), "NCI:"),
    (re.compile(r".*/([^:_]+)[:_]"), r"\1:"),
]


def normalize_mondo_mapping(mapping):
    for pattern, replacement in _MONDO_MAPPING_NORMALIZERS:
        result, n = pattern.subn(replacement, mapping, count=1)
        if n:
            return result
    return mapping


def create_compare_record(
    mondo_id,
    mondo_data,
    doid,
    do_data,
    mapped_by,
    map_shared=None,
    synonym_shared=None,
    label_match=None,
):
    if map_shared is None:
        # sort list alphabetically for easier reading in TSV
        map_shared = sorted(mondo_data["mappings"].intersection(do_data["mappings"]))

    if synonym_shared is None:
        synonym_shared = sorted(
            mondo_data["synonyms"].intersection(do_data["synonyms"])
        )

    if label_match is None:
        label_match = (
            do_data["label"] == mondo_data["label"]
            or do_data["label"] in mondo_data["synonyms"]
        )

    comparison = {
        "mondo_id": mondo_id,
        "mondo_label": mondo_data["label"],
        "mondo_node_type": mondo_data["node_type"],
        "doid": doid,
        "do_label": do_data["label"],
        "do_node_type": do_data["node_type"],
        "mapped_by": mapped_by,
        "has_do_label": label_match,
        "mapping_ratio": f'{len(map_shared)}/{len(do_data["mappings"])}',
        "synonym_ratio": f'{len(synonym_shared)}/{len(do_data["synonyms"])}',
        "mappings_shared": "|".join(sorted(map_shared)),
        "mappings_do": "|".join(sorted(do_data["mappings"].difference(map_shared))),
        "mappings_mondo": "|".join(
            sorted(mondo_data["mappings"].difference(map_shared))
        ),
        "synonyms_shared": "|".join(sorted(synonym_shared)),
        "synonyms_do": "|".join(sorted(do_data["synonyms"].difference(synonym_shared))),
        "synonyms_mondo": "|".join(
            sorted(mondo_data["synonyms"].difference(synonym_shared))
        ),
    }

    if mondo_data["root"]:
        comparison["mondo_root"] = mondo_data["root"]
    return comparison


def standardize_df(df):
    for col in df.columns:
        if "root" in col:
            df[col] = df[col].fillna("none")
            df[col] = pd.Categorical(
                df[col],
                categories=[
                    "disease",
                    "human disease",
                    "non-human animal disease",
                    "disease susceptibility",
                    "injury",
                    "disease characteristic",
                    "none",
                ],
                ordered=True,
            )
        if col == "mapped_by":
            df[col] = pd.Categorical(
                df[col],
                categories=[
                    "direct_mapping",
                    "chain_mapping",
                    "label_matching",
                    "synonym_matching",
                ],
                ordered=True,
            )
        if "node_type" in col:
            df[col] = pd.Categorical(
                df[col],
                categories=[
                    "branch",
                    "leaf",
                    "deprecated",
                ],
                ordered=True,
            )
        if isinstance(df[col].iloc[0], set):
            df[col] = df[col].apply(lambda x: "|".join(sorted(x)))
    return df


# Define mapping source priority for identification of unmapped terms (quasi-type info)
priority_order = ["MIM", "ORDO", "NCI", "UMLS", "GARD", "MESH"]


def has_priority_mapping(mappings):
    if mappings == "":
        return "none"
    for prefix in priority_order:
        if any(mapping.startswith(prefix) for mapping in mappings.split("|")):
            return prefix
    return "other"


def compare_deprecated(row):
    if row["mondo_node_type"] == "deprecated" and row["do_node_type"] == "deprecated":
        return "both"
    elif row["mondo_node_type"] == "deprecated":
        return "mondo_only"
    elif row["do_node_type"] == "deprecated":
        return "do_only"
    else:
        return "neither"


############################################################################
#### DOWNLOAD LATEST OWL FILES
############################################################################

data_dir = os.path.join("data", "compare_do_mondo")
os.makedirs(data_dir, exist_ok=True)

doid_url = "http://purl.obolibrary.org/obo/doid.owl"
doid_path = os.path.join(data_dir, "doid.owl")
doid_version = get_version(doid_path)
if doid_version is None or (datetime.now() - doid_version).days > 28:
    with open(doid_path, "wb") as f:
        f.write(requests.get(doid_url).content)

mondo_url = "http://purl.obolibrary.org/obo/mondo.owl"
mondo_path = os.path.join(data_dir, "mondo.owl")
mondo_version = get_version(os.path.join(data_dir, "mondo.owl"))
if mondo_version is None or (datetime.now() - mondo_version).days > 28:
    with open(mondo_path, "wb") as f:
        f.write(requests.get(mondo_url).content)


############################################################################
#### EXTRACT DATA
############################################################################

# query to extract general information about each term
# rdflib only parses this correctly if the query is in a file
sparql_path = os.path.join("sparql", "compare-do-mondo.rq")
with open(sparql_path, "r") as f:
    sparql_query = f.read()


# DO data -----------------------------------------------------------------------
doid_graph = rdflib.Graph()
doid_graph.parse(os.path.join(data_dir, "doid.owl"), format="xml")
do_res = doid_graph.query(sparql_query)

do_terms = {}
for row in do_res:
    id, label, node_type, synonym, mapping = row
    id = as_str(id)
    label = as_str(label)
    node_type = as_str(node_type)
    synonym = as_str(synonym)
    mapping = as_str(mapping)
    if id not in do_terms:
        do_terms[id] = {
            "label": label,
            "node_type": node_type,
            "synonyms": set(),
            "mappings": set(),
        }
    if synonym:
        do_terms[id]["synonyms"].add(synonym)
    if mapping:
        # drop dates from SNOMEDCT_US mappings
        if mapping.startswith("SNOMEDCT_US_"):
            mapping = "SNOMEDCT_US:" + mapping.split(":")[-1]
        do_terms[id]["mappings"].add(mapping)

# MONDO data --------------------------------------------------------------------
mondo_graph = rdflib.Graph()
mondo_graph.parse(os.path.join(data_dir, "mondo.owl"), format="xml")
mondo_res = mondo_graph.query(sparql_query)

mondo_terms = {}
for row in mondo_res:
    id, label, node_type, synonym, mapping_raw = row
    id = as_str(id)
    label = as_str(label)
    node_type = as_str(node_type)
    synonym = as_str(synonym)
    mapping_raw = as_str(mapping_raw)
    if id not in mondo_terms:
        mondo_terms[id] = {
            "label": label,
            "node_type": node_type,
            "root": None,
            "doid": set(),
            "synonyms": set(),
            "mappings": set(),
        }
    if synonym:
        mondo_terms[id]["synonyms"].add(synonym)
    if mapping_raw:
        mapping = normalize_mondo_mapping(mapping_raw)
        if mapping.startswith("DOID:"):
            mondo_terms[id]["doid"].add(mapping)
        else:
            mondo_terms[id]["mappings"].add(mapping)


# MONDO root terms are at or above level of 'human disease'
mondo_root_query = """
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX obo: <http://purl.obolibrary.org/obo/>

    SELECT ?id ?root
    WHERE {
        ?iri a owl:Class .
        FILTER(REGEX(STR(?iri), "MONDO"))
        OPTIONAL {
            VALUES ?type_iri {
                obo:MONDO_0021178  # injury
                obo:MONDO_0042489  # disease susceptibility
                obo:MONDO_0700096  # human disease
                obo:MONDO_0005583  # non-human animal disease
                obo:MONDO_0021125  # disease characteristic -- for information purposes
            }
            ?iri rdfs:subClassOf* ?type_iri .
            ?type_iri rdfs:label ?root_raw .
        }

        # drop lang tags
        BIND(STR(?root_raw) AS ?root)
        # ID BINDING
        BIND(REPLACE(STR(?iri), "^.*(MONDO)_([0-9]+)", "$1:$2") AS ?id)
    }
"""

mondo_root = mondo_graph.query(mondo_root_query)
for row in mondo_root:
    id, root_label = row
    id = as_str(id)
    root_label = as_str(root_label)
    if id in mondo_terms:
        if root_label:
            mondo_terms[id]["root"] = root_label
        else:
            if id != "MONDO:0000001" and mondo_terms[id]["node_type"] != "deprecated":
                raise ValueError(f"{id} without root is not deprecated")
        # DO NOT USE THIS CONDITION: Some MONDO terms do NOT have a label
        # else:
        #     raise ValueError(f"{id} found in root query but not in general query")
    elif root_label:
        raise ValueError(f"{id} found in root query that is not deprecated")
    else:
        mondo_terms[id] = {
            "label": None,
            "node_type": "deprecated",
            "root": None,
            "doid": set(),
            "synonyms": set(),
            "mappings": set(),
        }


# single correction to 'root' for "disease" (uncalculated by SPARQL query)
mondo_terms["MONDO:0000001"]["root"] = "disease"


############################################################################
#### COMPARE MONDO & DO
############################################################################

mapped = []
mondo_unmapped = {}
do_mapped = []

# 1.Which MONDO diseases are equivalent to DO terms (according to MONDO's mappings)
for id, mondo_data in mondo_terms.items():
    if mondo_data["doid"]:
        for doid in mondo_data["doid"]:
            if not do_terms[doid]:
                raise ValueError(
                    f"{id} mapped to {doid}, but could not be found in DO terms"
                )
            comparison = create_compare_record(
                id, mondo_data, doid, do_terms[doid], "direct_mapping"
            )
            mapped.append(comparison)
            do_mapped.append(doid)
    else:
        mondo_unmapped[id] = mondo_data

do_unmapped = {k: v for k, v in do_terms.items() if k not in do_mapped}

# 2. Which MONDO diseases, map to DO terms by bridging mappings from OMIM, UMLS, Orphanet, NCI, or GARD?
#    The first DOID with any mapping match is used, not necessarily the one with the most mapping matches
do_rm = None
mondo_rm = []
# fix indentation to terminate this for loop
for id, mondo_data in mondo_unmapped.items():
    match_prefixes = ["MIM:", "ORDO:", "UMLS_CUI:", "NCI:", "GARD:"]
    for doid, do_data in do_unmapped.items():
        map_shared = mondo_data["mappings"].intersection(do_data["mappings"])
        if map_shared and any(
            m.startswith(prefix) for m in map_shared for prefix in match_prefixes
        ):
            comparison = create_compare_record(
                id,
                mondo_data,
                doid,
                do_terms[doid],
                "chain_mapping",
                map_shared=map_shared,
            )
            mapped.append(comparison)
            do_rm = doid
            break
        else:
            do_rm = None
    if do_rm:
        del do_unmapped[do_rm]
        mondo_rm.append(id)

for id in mondo_rm:
    del mondo_unmapped[id]

# 3. Which MONDO diseases not found in 3a or 3b can be mapped to DO terms by matching names (labels & synonyms)?
#    The first DOID with any name match is used (DO label prioritized), not necessarily the one with the most name matches
do_rm = None
mondo_rm = []
for id, mondo_data in mondo_unmapped.items():
    for doid, do_data in do_unmapped.items():
        label_match = (
            do_data["label"] == mondo_data["label"]
            or do_data["label"] in mondo_data["synonyms"]
        )
        synonyms_shared = mondo_data["synonyms"].intersection(do_data["synonyms"])

        # would be better to have label_match & synonyms_shared "label_synonym_match"
        if label_match:
            map_type = "label_matching"
        elif synonyms_shared:
            map_type = "synonym_matching"
        else:
            do_rm = None
            continue

        comparison = create_compare_record(
            id,
            mondo_data,
            doid,
            do_terms[doid],
            map_type,
            synonym_shared=synonyms_shared,
            label_match=label_match,
        )
        mapped.append(comparison)
        do_rm = doid
        break
    if do_rm:
        del do_unmapped[do_rm]
        mondo_rm.append(id)

for id in mondo_rm:
    del mondo_unmapped[id]


############################################################################
#### ANALYZE & SAVE
############################################################################

# General Statistics -------------------------------------------------------

ontology_stats_path = os.path.join(data_dir, "ontology_stats.tsv")

# DO terms
do_flat_dict = [{"id": k, **v} for k, v in do_terms.items()]
do_df = standardize_df(pd.DataFrame(do_flat_dict)[["id", "node_type"]]).sort_values(
    by="node_type"
)

do_total = do_df["id"].nunique()
do_node_count = do_df.groupby("node_type")["id"].nunique()
with open(ontology_stats_path, "w") as f:
    f.write(f"DOID version:\t{doid_version.strftime("%Y-%m-%d")}\n")
    f.write(f"Total DO terms:\t{do_total}\n")
    f.write("\n")
    do_node_count.to_csv(f, sep="\t")


# MONDO terms
mondo_flat_dict = [{"id": k, **v} for k, v in mondo_terms.items()]
mondo_df = standardize_df(
    pd.DataFrame(mondo_flat_dict)[["id", "root", "node_type"]]
).sort_values(by=["root", "node_type"])

mondo_total = mondo_df["id"].nunique()
mondo_root_count = mondo_df.groupby("root")["id"].nunique()
mondo_root_node_count = mondo_df.groupby(["root", "node_type"])["id"].nunique()
with open(ontology_stats_path, "a") as f:
    f.write("\n\n")
    f.write(f"MONDO version:\t{mondo_version.strftime("%Y-%m-%d")}\n")
    f.write(f"Total MONDO terms:\t{mondo_total}\n")
    f.write("\n")
    mondo_root_count.to_csv(f, sep="\t")
    f.write("\n")
    mondo_root_node_count.to_csv(f, sep="\t")


# Detailed Mapping Outputs ---------------------------------------------------

# Mapped diseases
mapped_df = standardize_df(pd.DataFrame(mapped))
mapped_df["deprecated"] = mapped_df.apply(compare_deprecated, axis=1)
mapped_df["deprecated"] = pd.Categorical(
    mapped_df["deprecated"],
    categories=["neither", "do_only", "mondo_only", "both"],
    ordered=True,
)
mapped_out = mapped_df.sort_values(
    by=["deprecated", "mondo_root", "mapped_by", "do_label", "mondo_label"]
)

mapped_out.to_csv(os.path.join(data_dir, "mapped_diseases.tsv"), index=False, sep="\t")


# MONDO unmapped
mondo_unmapped_flat_dict = [{"id": k, **v} for k, v in mondo_unmapped.items()]
mondo_unmapped_df = standardize_df(pd.DataFrame(mondo_unmapped_flat_dict))
mondo_unmapped_df["foremost_mapping"] = mondo_unmapped_df["mappings"].apply(
    has_priority_mapping
)

priority_order2 = [*priority_order, "other", "none"]
mondo_unmapped_df["foremost_mapping"] = pd.Categorical(
    mondo_unmapped_df["foremost_mapping"],
    categories=priority_order2,
    ordered=True,
)
mondo_unmapped_out = mondo_unmapped_df.sort_values(
    by=["root", "node_type", "foremost_mapping", "label"]
)

mondo_unmapped_out.to_csv(
    os.path.join(data_dir, "mondo_unmapped.tsv"), index=False, sep="\t"
)


# DO unmapped
do_unmapped_flat_dict = [{"id": k, **v} for k, v in do_unmapped.items()]
do_unmapped_df = standardize_df(pd.DataFrame(do_unmapped_flat_dict))
do_unmapped_df["foremost_mapping"] = do_unmapped_df["mappings"].apply(
    has_priority_mapping
)
do_unmapped_df["foremost_mapping"] = pd.Categorical(
    do_unmapped_df["foremost_mapping"],
    categories=priority_order2,
    ordered=True,
)
do_unmapped_out = do_unmapped_df.sort_values(
    by=["node_type", "foremost_mapping", "label"]
)

do_unmapped_out.to_csv(os.path.join(data_dir, "do_unmapped.tsv"), index=False, sep="\t")


# Detailed Mapping Statistics ---------------------------------------------------
mapping_stats_path = os.path.join(data_dir, "mapping_stats.tsv")

#### Mapped diseases ####
mapped_by_count = (
    mapped_out[
        [
            "mondo_id",
            "doid",
            "mondo_root",
            "mapped_by",
            "deprecated",
        ]
    ]
    .groupby(
        [
            "mondo_root",
            "mapped_by",
            "deprecated",
        ]
    )
    .size()
    .rename("mappings")
    .to_frame()
)
mapped_by_count["pct"] = (
    mapped_by_count["mappings"] / mapped_by_count["mappings"].sum()
) * 100
mapped_by_count["pct"] = mapped_by_count["pct"].round(1)


#### DO unmapped ####
do_unmapped_count = (
    do_unmapped_out.groupby("node_type")["id"]
    .nunique()
    .rename("unmapped")
    .to_frame()
    .join(do_node_count.rename("total"), how="left")
)

do_unmapped_count["pct"] = (
    do_unmapped_count["unmapped"] / do_unmapped_count["total"]
) * 100
do_unmapped_count["pct"] = do_unmapped_count["pct"].round(1)

# by foremost mapped source
do_unmapped_src = (
    do_unmapped_out.groupby("foremost_mapping")["id"]
    .nunique()
    .rename("unmapped")
    .to_frame()
)
do_unmapped_src["pct"] = (
    do_unmapped_src["unmapped"] / do_unmapped_src["unmapped"].sum()
) * 100
do_unmapped_src["pct"] = do_unmapped_src["pct"].round(1)


#### MONDO unmapped ####
mondo_unmapped_count = (
    mondo_unmapped_out.groupby(["root", "node_type"])["id"]
    .nunique()
    .rename("unmapped")
    .to_frame()
    .join(mondo_root_node_count.rename("total"), how="left")
)
mondo_unmapped_count["pct"] = (
    mondo_unmapped_count["unmapped"] / mondo_unmapped_count["total"]
) * 100
mondo_unmapped_count["pct"] = mondo_unmapped_count["pct"].round(1)

# by foremost mapped source
mondo_human_unmapped = mondo_unmapped_out[mondo_unmapped_out["root"] == "human disease"]
mondo_unmapped_src = (
    mondo_human_unmapped.groupby("foremost_mapping")["id"]
    .nunique()
    .rename("unmapped")
    .to_frame()
)
mondo_unmapped_src["pct"] = (
    mondo_unmapped_src["unmapped"] / mondo_unmapped_src["unmapped"].sum()
) * 100
mondo_unmapped_src["pct"] = mondo_unmapped_src["pct"].round(1)

#### SAVE ####
with open(mapping_stats_path, "w") as f:
    f.write("TOTALS:\n")
    f.write("ontology\tmapped\tunmapped\n")
    f.write(
        "DO\t{}\t{}\n".format(
            mapped_out["doid"].nunique(), do_unmapped_out["id"].nunique()
        )
    )
    f.write(
        "MONDO\t{}\t{}\n".format(
            mapped_out["mondo_id"].nunique(), mondo_human_unmapped["id"].nunique()
        )
    )
    f.write("\n")
    f.write("Mapped -- complete details:\n")
    mapped_by_count.to_csv(f, sep="\t")
    f.write("\n\n")
    f.write("DO unmapped:\n")
    do_unmapped_count.to_csv(f, sep="\t")
    f.write("\n")
    f.write("DO unmapped -- by primary source:\n")
    do_unmapped_src.to_csv(f, sep="\t")
    f.write("\n\n")
    f.write("MONDO unmapped -- by root:\n")
    mondo_unmapped_count.to_csv(f, sep="\t")
    f.write("\n")
    f.write("MONDO unmapped -- by primary source:\n")
    mondo_unmapped_src.to_csv(f, sep="\t")


## MONDO from DO only -- DO as source of truth

authoritative_mappings = [
    "NCI",
    "SNOMEDCT_US",
    "ICD9CM",
    "OGMS",
    "ORDO",
    "NANDO",
    "NORD",
    "ONCOTREE",
    "MIM",
    "GARD",
    "ICD11",
    "MEDGEN",
    "Wikipedia",
    "ICD10CM",
    "MEDDRA",
    "EFO",
    "ICDO",
    "DECIPHER",
    "HP",
    "PMID",
    "CSP",
    "MTH",
    "NDFRT",
    "SCDO",
]
other_mappings = [
    "MESH",
    "KEGG",
    "birnlex",
    "UMLS_CUI",
    "HGNC",
    "MFOMD",
    "IDO",
    "OBI",
    "VeNom",
    "GTR",
]

# exclude MONDO terms with any match from other_mappings in 'mappings_shared' or 'mappings_mondo'
mondo_do_primary = mapped_out[
    ~mapped_out["mappings_shared"].str.contains(
        r"|".join(authoritative_mappings), case=False, na=False
    )
    & ~mapped_out["mappings_mondo"].str.contains(
        r"|".join(authoritative_mappings), case=False, na=False
    )
].copy()

mondo_do_primary["primary_source"] = np.where(
    (mondo_do_primary["mappings_shared"] == "")
    & (mondo_do_primary["mappings_mondo"] == ""),
    "DO_only",
    "DO_major",
)

mondo_do_primary.to_csv(
    os.path.join(data_dir, "mapped_DO_primary.tsv"), index=False, sep="\t"
)

mondo_do_primary_stats = (
    mondo_do_primary.groupby("primary_source")["mondo_id"]
    .nunique()
    .rename("count")
    .to_frame()
)

with open(mapping_stats_path, "a") as f:
    f.write("\n\n")
    f.write("MONDO mapped to DO as primary source:\n")
    mondo_do_primary_stats.to_csv(f, sep="\t")
