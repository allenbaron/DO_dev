library(here)
library(tidyverse)
library(googlesheets4)
library(DO.utils)


# Read suggested UMLS xrefs (in Github Issue #1242) -----------------------

gs <- "1zcMIM0GrD2cJ6WKl_aHROw7994zXWBRLVQvN3EXOk7Y"
sheet <- "original_list"

suggested <- googlesheets4::read_sheet(gs, sheet) %>%
    DO.utils::lengthen_col(umls, delim = "|") %>%
    unique() # some duplicates exist


# Load current diseases and xrefs ---------------------------------

de_path <- here::here("../Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl")

q_file <- tempfile(fileext = ".rq")
readr::write_lines(
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>

    SELECT ?doid ?do_label ?xref
    WHERE {
        ?class oboInOwl:id ?doid ;
            rdfs:label ?do_label .
        FILTER NOT EXISTS { ?class owl:deprecated true . }
        OPTIONAL { ?class oboInOwl:hasDbXref ?xref . }
    }",
    q_file
)

do_xref_file <- tempfile(fileext = ".tsv")
DO.utils::robot("query", i = de_path, query = q_file, do_xref_file)

do_xref <- readr::read_tsv(do_xref_file) %>%
    DO.utils::tidy_sparql() %>%
    dplyr::mutate(
        do_label = stringr::str_remove(do_label, "@.*"),
        source = stringr::str_remove(xref, ":.*"),
        lui = stringr::str_remove(xref, ".*:")
    )


#  Load suggested UMLS_CUIs from MRCONSO.RRF  -----------------------------

# Download MRCONSO.RRF version 2023AB manually from https://www.nlm.nih.gov/research/umls/licensedcontent/umlsknowledgesources.html
# Full link to file: https://download.nlm.nih.gov/umls/kss/2023AB/umls-2023AB-mrconso.zip?_gl=1*1tvrdh*_ga*NzA5NzAyMTc3LjE2OTYyNzI1NzU.*_ga_7147EPK006*MTcwNjU0NjQxNi40My4xLjE3MDY1NDc2NzQuMC4wLjA.*_ga_P1FPTH9PL4*MTcwNjU0NjQxNi40Mi4xLjE3MDY1NDc2NzQuMC4wLjA.
#
# For MRCONSO.RRF format, refer to https://www.ncbi.nlm.nih.gov/books/NBK9685/table/ch03.T.concept_names_and_sources_file_mr/
# For description of abbreviations, refer to https://www.nlm.nih.gov/research/umls/knowledge_sources/metathesaurus/release/abbreviations.html
mrconso_zip <- here::here("data/mapping/src/umls-2023AB-mrconso.zip")
mrconso_file <- here::here("data/mapping/src/umls-2023AB-mrconso.rrf")

if (file.exists(mrconso_zip)) {
    mrconso_tmp <- utils::unzip(mrconso_zip, junkpaths = TRUE)
    file.rename(mrconso_tmp, mrconso_file)
}

mrconso <- vroom::vroom(
    mrconso_file,
    delim = "|",
    col_names = c("cui", "lat", "ts", "lui", "stt", "sui", "ispref", "aui",
                  "saui", "scui", "sdui", "sab", "tty", "code", "str", "srl",
                  "suppress", "cvf"),
    col_types = vroom::cols_only(
        cui = col_character(),
        lat = col_character(),
        ts = col_character(),
        stt = col_character(),
        ispref = col_character(),
        sab = col_character(),
        tty = col_character(),
        code = col_character(),
        str = col_character(),
        srl = col_double()
    )
) %>%
    dplyr::filter(
        lat == "ENG", # language
        cui %in% suggested$umls
    ) %>%
    dplyr::select(
        cui, source = sab, lui = code, label = str, type = tty, status = ts, ispref
    )

# subset with mappings meaningful to DO
umls_map <- mrconso %>%
    dplyr::filter(
        source %in% c("NCI", "MSH", "OMIM", "ICD9CM", "SNOMEDCT_US", "ORPHANET", "ICD10CM"),
        type != "PTCS" # not real OMIM IDs
    )

# umls_map <- mrconso %>%
#     dplyr::mutate(
#         name = label[status == "P" & ispref == "Y"][1],
#         .by = cui
#     ) %>%
#     dplyr::filter(
#         source %in% c("NCI", "MSH", "OMIM", "ICD9CM", "SNOMEDCT_US", "ORPHANET", "ICD10CM"),
#         type != "PTCS" # not real OMIM IDs
#     ) %>%
#     dplyr::mutate(
#         source = dplyr::recode(source, MSH = "MESH", ORPHANET = "ORDO"),
#         xref = paste(source, lui, sep = ":")
#     )

# Identify low quality suggestions ---------------------------------------

in_do <- suggested %>%
    dplyr::semi_join(
        dplyr::filter(do_xref, source == "UMLS_CUI"),
        by = c("doid", "umls" = "lui")
    ) # most already in DO

googlesheets4::write_sheet(in_do, gs, "in_DO")

suggested <- suggested %>%
    dplyr::anti_join(in_do)

# identifying low(er) quality mappings (may have changed since 2023AA);
#   de-prioritizing these for now; can review these manually when I get around
#   to them
low_quality <- suggested %>%
    dplyr::mutate(
        status = dplyr::case_when(
            !umls %in% mrconso$cui ~ "deprecated in UMLS",
            !doid %in% do_xref$doid ~ "deprecated in DO",
            !umls %in% umls_map$cui ~ "ignored source",
            !umls %in% dplyr::filter(umls_map, type == "PT")$cui ~ "source secondary",
            all_duplicated(umls) ~ "multimaps to DO",
            all_duplicated(doid) ~ "multimaps to UMLS"
        )
    ) %>%
    dplyr::filter(!is.na(status))

suggested <- suggested %>%
    dplyr::anti_join(low_quality)

googlesheets4::write_sheet(
    low_quality %>%
    dplyr::mutate(
        doid = DO.utils::build_hyperlink(
            x = stringr::str_remove(doid, "DOID:"),
            url = "DOID",
            text = doid,
            as = "gs"
        ),
        umls = DO.utils::build_hyperlink(
            x = umls,
            url = "UMLS_CUI",
            as = "gs"
        )
    ) %>%
    dplyr::arrange(status, doid, umls),
    gs,
    "low_quality"
)

# TS: P = preferred, S = secondary/synonym

# %>%
#     dplyr::mutate(status = dplyr::if_else(status == "P", "pref", "syn")) %>%
#     tidyr::pivot_wider(
#         names_from = status



# Calculate overlapping sub-mappings between DO & UMLS --------------------

# create new graph & query shared xrefs
umls_tmp <- mrconso %>%
    # assign name to UMLS_CUI (don't have their own name)
    dplyr::mutate(
        name = label[status == "P" & ispref == "Y"][1],
        .by = cui
    ) %>%
    # use only xrefs in DO
    dplyr::filter(
        source %in% c("NCI", "MSH", "OMIM", "ICD9CM", "SNOMEDCT_US", "ORPHANET", "ICD10CM"),
        type != "PTCS", # not real OMIM IDs
        cui %in% suggested$umls # limit to suggestions
    ) %>%
    # convert SOURCE/LUI to URI (to create relationships in RDF)
    dplyr::mutate(
        source = dplyr::recode(source, MSH = "MESH", ORPHANET = "ORDO"),
        id = dplyr::if_else(
            !is.na(cui),
            paste0("https://uts.nlm.nih.gov/uts/umls/concept/", cui),
            NA_character_
        ),
        map_id = dplyr::if_else(
            !is.na(lui),
            paste0("http://fake/", source, "/", lui),
            NA_character_
        )
    )

umls_triples <- umls_tmp %>%
    dplyr::select(id, name, map_id) %>%
    DO.utils::collapse_col(map_id, delim = "|")

src_triples <- umls_tmp %>%
    dplyr::select(id = map_id, synonym = label) %>%
    unique() %>%
    DO.utils::collapse_col(synonym, delim = "|")

do_triples <- suggested %>%
    dplyr::select(doid, umls) %>%
    dplyr::left_join(do_xref, by = "doid") %>%
    dplyr::mutate(
        id = DO.utils::to_uri(doid),
        exactMatch = dplyr::if_else(
            !is.na(umls),
            paste0("https://uts.nlm.nih.gov/uts/umls/concept/", umls),
            NA_character_
        ),
        map_id = dplyr::if_else(
            !is.na(lui),
            paste0("http://fake/", source, "/", lui),
            NA_character_
        )
    ) %>%
    dplyr::select(id, name = do_label, exactMatch, map_id) %>%
    DO.utils::collapse_col(.cols = c(exactMatch, map_id))

rt <- Reduce(dplyr::bind_rows, list(do_triples, umls_triples, src_triples)) %>%
    tibble::add_row(
        id = "ID",
        name = "A rdfs:label",
        exactMatch = "A skos:exactMatch SPLIT=|",
        map_id = "A skos:closeMatch SPLIT=|",
        synonym = "A oboInOwl:hasExactSynonym SPLIT=|",
        .before = 1
    )

rt_file <- tempfile(fileext = ".tsv")
readr::write_tsv(rt, rt_file)
googlesheets4::write_sheet(rt, gs, "robot_template-shared_xrefs")

rt_owl <- tempfile(fileext = ".ofn")
DO.utils::robot(
    "template",
    i = de_path,
    "no merge" = "",
    template = rt_file,
    o = rt_owl
)


    # MESH = "https://meshb.nlm.nih.gov/record/ui?ui=",
    # SNOMEDCT_US = "https://browser.ihtsdotools.org/?perspective=full&conceptId1=",
    # NCI = "https://ncit.nci.nih.gov/ncitbrowser/ConceptReport.jsp?dictionary=NCI_Thesaurus&ns=ncit&code=",
    # ORDO = "https://www.orpha.net/consor/cgi-bin/Disease_Search.php?lng=EN&data_id=",

    umls_map %>%
    dplyr::group_by(cui) %>%
    dplyr::mutate(
        name = unique(label[status == "P" & ispref == "Y"]),
        name = dplyr::if_else(
            is.na(name),
            unique(label[status == "P"]),
    dplyr::filter(
        label =
    dplyr::select(
    , id = cui, xref
# Quick Analysis ----------------------------------------------------------

# identifying low(er) quality mappings (may have changed since 2023AA)
suggested <- suggested %>%
    dplyr::mutate(
        status = dplyr::case_when(
            !umls %in% mrconso$cui ~ "deprecated",
            !umls %in% umls_map$cui ~ "ignored source",
            !umls %in% dplyr::filter(umls_map, type == "PT")$cui ~ "source secondary",
            all_duplicated(umls) ~ "multimaps to DO",
            all_duplicated(doid) ~ "multimaps to UMLS",

        )
    )

low_quality <- list(
    deprecated = dplyr::filter(suggested, !umls %in% mrconso$cui),
    src_ignored = dplyr::filter(suggested, !


mrconso <- mrconso_raw[stringr::str_detect(mrconso_raw, mrconso_keep)] %>%
    readr::read_delim(
        delim = "|",
        col_names = c("CUI", "LAT", "TS", "LUI", "STT", "SUI", "ISPREF", "AUI",
                      "SAUI", "SCUI", "SDUI", "SAB", "TTY", "CODE", "STR", "SRL",
                      "SUPPRESS", "CVF")
    )

    dplyr::mutate(umls = paste0("UMLS_CUI:", umls))
