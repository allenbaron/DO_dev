# DOID-ICDO Mapping Procedure
By: J. Allen Baron
Initiated: 2021-11-09

# Purpose
To programmatically recommend/predict DOID-ICDO mappings for curator review (using ICD-O_3.2 excel file).


# Programmatic Prediction Methods & Results

Initially, my intention was to use fuzzy string matching. For this purpose, I compared ICD-O term names with DO term names to identify systematic differences in the patterns used by each terminology. This exploration can be found in `notebooks/ICDO-fuzzy_mapping.Rmd`.

After this exploration, I decided to request assistance from Charlie Hoyt (biopragmatics/INDRA) to use INDRA's smarter lexical matching algorithm, GILDA. He wrote a script to execute GILDA and provided the script, and everything else needed to make his work reproducible as a [Github repo](https://github.com/cthoyt/icdo-doid-mappings) (2021-11-10). I downloaded the `predictions.tsv` file as `data/mapping/biomappings-icdo_doid.tsv`.

GILDA identified 424 high quality lexical matches in the DO for the ~1100 total ICD-O terms. I briefly interactively examined the ICD-O terms that GILDA did not match and many seemed to exhibit the systematic differences I identified initially (see prior paragraph). This suggests that systematic changes made beforehand could improve GILDA's ability to identify likely matches. I downloaded the script that Charlie used for the initial matches (2021-12-02), with the intention of making systematic changes to term names, rerunning GILDA with those modified names, and then replacing the modified names with the originals.


# Curation

## Preparation

To preserve formatting of DOIDs and ICD-O codes, I interactively copied `data/mapping/biomappings-icdo_doid.tsv` to a Google Sheet using R with the following code (2021-12-23):

```
library(tidyverse)
library(googlesheets4)

icdo_doid <- readr::read_tsv(
    "data/mapping/biomappings-icdo_doid.tsv",
     col_types = readr::cols(
         score = readr::col_double(),
         .default = readr::col_character()
    )
)

googlesheets4::write_sheet(
    data = icdo_doid,
    ss = "https://docs.google.com/spreadsheets/d/1aw2ughb0rOqHh_snwwIkdwVX-c0hB9Wn7aYDaxpmvnE/edit?usp=sharing",
    sheet = "original"
)
```

_This approach explicitly sets DOIDs and ICD-O IDs as text to avoid the reformatting by Google Sheets/Excel, which recognize these as being of numeric types (numbers, dates, etc.), often leading to modification of these IDs._
