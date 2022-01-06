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


## Execution

1. [2021-12-23] Identify DOID labels that exactly match ICD-O labels or have exact matches in the synonym column ("synonym_match").
    - Review of a few of these indicates some xrefs are already in DO.
2. [2022-01-03] Extract all DO xrefs and add existing ICD-O xrefs to Google sheet, sheet name "already_in_DO-2022_01_03". Then, identify those DOID-ICDO relationships that are already in DO on Google sheet.
    - I identified an updated version of the ICD-O3.2 table at http://www.iacr.com.fr/index.php?option=com_content&view=category&layout=blog&id=100&Itemid=577 dated 2020-09-10, whereas the file originally used is dated 2019-11-15. I compared these for differences and only identified the following minor changes:
        - **9875/3** preferred name was changed to "Chronic myeloid leukemia, BCR/ABL positive" and "Chronic myelogenous leukemia, BCR/ABL positive" was made a synonym; additional synonyms also added: "Chronic myeloid leukemia, Philadelphia chromosome (Ph1) positive", "Chronic myeloid leukemia, t(9;22)(q34;q11)"
        - **9912/3** spelling error ("myeleoid" -> "myeloid") was corrected to make label "Acute myeloid leukemia with BCR-ABL1"
        - _No need to repeat DOID-ICDO mapping with newer file._
    - I also identified another US specific version of the table at https://www.naaccr.org/icdo3/ (the 2021 update, dated 2020-10-01, and 2022 update, dated 2021-07-29, are the same (named "ICD-O-3.2_MFin_17042019_web.xls") but differ from the versions I already have (though most of those differences appear to be minor modifications related to spelling or the presence of "NOS"). _I'm not going to use this version at this time, since I can't tell when/why it's been modified and the modifications seem trivial._
3. [2022-01-03] Prepare to add other missing mappings as well (ICD-10 codes, UMLS-based codes missing).
    - ICD-O to ICD-9/10/10-CM conversion table: https://seer.cancer.gov/tools/conversion/.
    - ICD-10-CM browser: https://icd10cmtool.cdc.gov/?fy=FY2022
    - UMLS Metathesaurus browser: https://uts.nlm.nih.gov/uts/umls/home
    - General sources:
        - Charlie Hoyt's [pyobo](https://github.com/pyobo/pyobo/issues/28) python package parses a number of resources and lists others.
        - NCI's [Enterprise Vocabulary Service (EVS)](https://evs.nci.nih.gov/) maintains NCI Thesaurus but also ingests, cross-references, and makes available various related terminologies including SNOMED, ICD-10-CM, MedDRA, etc.
            - NCI terminologies: https://ncit.nci.nih.gov/ncitbrowser/pages/multiple_search.jsf?nav_type=terminologies
            - NCI mappings: https://ncit.nci.nih.gov/ncitbrowser/pages/mapping_search.jsf?nav_type=mappings&b=0&m=0
            - FTP site for EVS downloads: https://evs.nci.nih.gov/ftp1/


During initial curation I noticed at least two near exact matches between DOID and ICD-O that differed only by an added ", NOS" in the ICD-O label. These matches were not identified by Charlie Hoyt with GILDA. There are likely hundreds more.

[2021-01-05] I systematically processed the ICD-O codes as noted previously in "Programmatic Prediction Methods & Results" (see `notebooks/ICDO-fuzzy_mapping.Rmd`), updated Charlie's script (`scripts/mapping_ICDO.py`), and finally reran it locally.
