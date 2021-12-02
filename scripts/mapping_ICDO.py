"""This script generates mappings from ICDO terms to DOID terms.

ICDO terms come from an excel sheet sent to me by
`Allen Baron <https://github.com/allenbaron>`_. It's included in
this repo since ICD and related resources are notoriously hard to
find and get.
"""

from pathlib import Path

import pandas as pd
from gilda.grounder import ScoredMatch
from pyobo.gilda_utils import get_grounder
from tqdm import tqdm

HERE = Path(__file__).parent.resolve()
INPUT = HERE.joinpath("ICD-O-3.2_final_15112019.xls")
OUTPUT = HERE.joinpath("predictions.tsv")


def main():
    df = pd.read_excel(INPUT, usecols=[0, 1, 2], skiprows=1)
    df.columns = ["identifier", "type", "text"]
    df = df[df.identifier.notna() & df.text.notna()]

    grounder = get_grounder("doid")
    results = []
    it = tqdm(df.groupby("identifier"), unit="term", desc="Predicting DOID mappings")
    for icdo_id, sdf in it:
        preferred = None
        synonyms = []
        matches: list[ScoredMatch] = []
        for _, text_type, text in sdf.values:
            if text_type == "Preferred":
                preferred = text
            else:
                synonyms.append(text)
            matches.extend(grounder.ground(text))
        if not matches:
            continue

        # Deduplicate synonyms
        synonyms = sorted(set(synonyms))

        # If no preferred term, pop the first off of synonyms
        if not preferred and synonyms:
            preferred = synonyms.pop(0)

        # Get best match (i.e., with the highest score)
        best_match: ScoredMatch = max(matches, key=lambda match: match.score)

        results.append(dict(
            identifier=icdo_id,
            preferred=preferred,
            synonyms="|".join(synonyms),
            score=round(best_match.score, 2),
            do_lui=best_match.term.id,
            do_name=best_match.term.entry_name
        ))

    df = pd.DataFrame(results)
    df.to_csv(OUTPUT, sep='\t', index=False)


if __name__ == '__main__':
    main()
