"""This script generates mappings from ICDO terms to DOID terms.

ICDO terms come from an excel sheet sent to me by
`Allen Baron <https://github.com/allenbaron>`_. It's included in
this repo since ICD and related resources are notoriously hard to
find and get.
"""

import pandas as pd
from gilda.grounder import ScoredMatch
from pyobo.gilda_utils import get_grounder
from tqdm import tqdm
import os

HERE = os.getcwd()
INPUT = os.path.join(HERE, "data/mapping", "ICD-O-3.2_with_synonym_mod.csv")
OUTPUT = os.path.join(HERE, "data/mapping", "biomappings-icdo_mod-doid.tsv")


def main():
    df = pd.read_csv(INPUT)
    df.columns = ["identifier", "type", "text"]
    df = df[df.identifier.notna() & df.text.notna()]

    grounder = get_grounder("doid")
    results = []
    it = tqdm(df.groupby("identifier"), unit="term", desc="Predicting DOID mappings")
    for icdo_id, sdf in it:
        preferred = None
        synonyms = []
        synonym_mods = []
        matches: list[ScoredMatch] = []
        for _, text_type, text in sdf.values:
            if text_type == "Preferred":
                preferred = text
            elif text_type == "Synonym_mod":
                synonym_mods.append(text)
            else:
                synonyms.append(text)
            matches.extend(grounder.ground(text))
        if not matches:
            continue

        # Deduplicate synonym sets
        synonyms = sorted(set(synonyms))
        synonym_mods = sorted(set(synonym_mods))

        # If no preferred term, pop the first off of synonyms
        if not preferred and synonyms:
            preferred = synonyms.pop(0)

        # Get best match (i.e., with the highest score)
        best_match: ScoredMatch = max(matches, key=lambda match: match.score)

        results.append(dict(
            identifier=icdo_id,
            preferred=preferred,
            synonyms="|".join(synonyms),
            synonym_mods="|".join(synonym_mods),
            score=round(best_match.score, 2),
            do_lui=best_match.term.id,
            do_name=best_match.term.entry_name
        ))

    df = pd.DataFrame(results)
    df.to_csv(OUTPUT, sep='\t', index=False)


if __name__ == '__main__':
    main()
