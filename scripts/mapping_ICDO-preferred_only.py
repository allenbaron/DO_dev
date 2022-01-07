"""
This script generates mappings from ICD-O terms to DOID terms.
The input should be a .tsv file with a single header row and
two columns: ICDO3.2 (with the code) and Term (with the label).

Specify the relative path to INPUT and OUTPUT files on the
command line (both .tsv, 1 header line).
"""

import pandas as pd
from gilda.grounder import ScoredMatch
from pyobo.gilda_utils import get_grounder
from tqdm import tqdm


def main(INPUT, OUTPUT):
    df = pd.read_csv(INPUT, sep = "\t")
    df.columns = ["identifier", "text"]
    df = df[df.identifier.notna() & df.text.notna()]

    grounder = get_grounder("doid")
    results = []
    it = tqdm(df.values, unit="term", desc="Predicting DOID mappings")
    for icdo_id, text in it:
        matches: list[ScoredMatch] = []
        matches.extend(grounder.ground(text))
        if not matches:
            continue

        # Get best match (i.e., with the highest score)
        best_match: ScoredMatch = max(matches, key=lambda match: match.score)

        results.append(dict(
            ICDO_2021=icdo_id,
            ICDO_label=text,
            DOID="DOID:"+best_match.term.id,
            DO_label=best_match.term.entry_name,
            score=round(best_match.score, 2)
        ))

    df = pd.DataFrame(results)
    df.to_csv(OUTPUT, sep='\t', index=False)


if __name__ == '__main__':
    import os
    import sys

    HERE = os.getcwd()
    file_in = os.path.join(HERE, sys.argv[1])
    file_out = os.path.join(HERE, sys.argv[2])

    main(INPUT = file_in, OUTPUT = file_out)
