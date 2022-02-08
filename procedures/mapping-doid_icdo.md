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


## Explore Mappings

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

[2021-01-05] I systematically processed the ICD-O codes as noted previously in "Programmatic Prediction Methods & Results" (see `notebooks/ICDO-fuzzy_mapping.Rmd`), updated Charlie's script (`scripts/mapping_ICDO.py`), and finally reran it locally. _The output was never explored in detail._



# ClinGen-Assisted Curation Plan

## ICD-O CNS file Preparation

[2021-01-06] ClinGen has offered to help with curation of DOID-ICDO mappings with regard to the brain and leukemias. They also provided an updated ICD-O code file for CNS terms (`data/mapping/CNS ICD-O terms.xls`), which included both topography and morphology codes. I reformatted the morphology codes and terms to make it machine readable as listed below, ignoring the topography codes (which are meaningless for DO on their own):

    - Removed header lines without codes
    - Removed `*` and `†` from codes & terms
    - Made tab-delimited
    - Added a placeholder "Level" column with `NA` (I assume these are all preferred terms)
    - Added headings to match previous ICD-O data files
    - Saved output as `data/mapping/CNS ICD-O terms.csv`

I then reviewed the tidied CNS dataset provided by ClinGen and found that there are multiple instances where terms share an ICD-O code. These do not appear to be synonyms but similar diseases (in my opinion, without CNS cancer expertise). Many of these appear to have "Related" status in the previous (complete) ICD-0-3.2 file I was using. This will likely present a point of confusion because Charlie's script assumed everything not labelled "Preferred" could be considered a synonym, which I'm now realizing may not be the case. _I need to discuss this with Lynn._

_This CNS files will not be used._


## Step-wise Mapping Plan

Discussion with Lynn has led to a modified proposal for identifying potential matches. The input will be an Excel file with the complete ICD-O-3.2 2021 update (that Lynn got from ClinGen). Lynn partitioned the data into multiple sheets, as follows:

- 'all terms': Full set of terms
- 'preferred terms': Preferred terms only (with or without ", NOS")
- 'leukemia': leukemia preferred terms only (without ", NOS")
- 'brain': brain-related cancer preferred terms only (without ", NOS")

**NOTE:** In each step the only modification to ICD-O terms will be removal of ", NOS". Other changes attempted previously did not significantly improve mapping (< 125 more matches of 700).

Mapping will take place in two phases:


### Phase 1:

1. Use _ONLY_ preferred ICD-O terms (with ", NOS" removed, since DO always drops this) from the 'preferred terms' sheet as input for GILDA "grounding" (i.e. matching).
2. Copy leukemia & brain matches found to appropriate sheets for ClinGen review
3. Personally curate remaining matches.


### Phase 2 (if desired):

1. Use all ICD-O terms with a preferred term that has not been curated (again with ", NOS" removed) from the 'all terms' sheet and use GILDA to identify the best match for each term.
2. Group the terms by preferred term and sort by match score
3. Copy leukemia & brain matches found to appropriate sheets for ClinGen review
4. Personally curate remaining matches.


## PHASE 1 - GILDA Execution

- I modified Charlie's script to accomplish this and saved it as `scripts/mapping_ICDO-preferred_only.py`.
- I reduced the 'preferred terms' sheet to two columns ("ICDO3.2" & "Term (NOS removed)") and saved it as `ICDO_3.2_2021-preferred_only.tsv`. I used R to preserve data types as "text" and created the file to minimize customization of the python script.
- I executed the script with PyCharm with the `DO_dev` root as the working directory and the parameters: `data/mapping/ICDO_3.2_2021-preferred_only.tsv` `data/mapping/biomappings-ICDO_2021_preferred-DOID.tsv`.

[2022-01-07] I found a need for more data manipulation than I'm familiar with in Python, so I wrote `script/mapping_ICDO.R` to do this work. Before completing everything, I migrated the R functions, which wrap PyOBO, to DO.utils.

I completed PHASE 1 and save the data to Google sheets including:

1. All preferred terms +/- mappings along with note of exact or recommended by biomappings.
2. Exact matches were also indicated as "Added to DO".
3. Leukemia and brain subsets were created using Lynn's lists for ClinGen review.
4. ROBOT template for addition of exact matches to DO was created.


### Results - Statistics

Match_Type                  | `preferred terms` | leukemia | brain
----------------------------|-------------------|----------|-------
exact match via biomappings |        313        |     8    |   23
recommended by biomappings  |        132        |     NA   |    3
NA                          |        699        |     51   |   40


## PHASE 1 (additional) - Fuzzy String Execution

GILDA identified some good matches but missed a number that are sufficiently similar that we anticipate fuzzy string matching could identify them. We have, therefore, decided to supplement with it.

1. Test speed of matching 5 ICD-O terms with all DO cancer terms, to determine feasibility (`scripts/mapping_ICDO-fz.R`).
    - **RESULT:** It is feasible. All 5 found matches with a total time of 0.01-0.032s, which means the full comparison of preferred ICD-O terms with preferred DO cancer terms should take no more than 2.5-8s.
