# to extract symptom ontology to UMLS mappings provided in SymptomOntology,
# issue #16 (https://github.com/DiseaseOntology/SymptomOntology/issues/16)

library(gh)
library(tidyverse)
library(DO.utils)
library(googlesheets4)

# destination: SYMP_UMLS_mappings google sheet
gs <- "1pCPkA3PgJBBUVLghCRphhulX5kzT9sc4rzA4Xd_IQnI"
sheet <- "mappings"

q <- 'query {
    repository(owner:"DiseaseOntology", name:"SymptomOntology") {
        issue(number:16) {
            body
        }
    }
}'

issue_res <- gh::gh_gql(
    query = q,
    .token = gh::gh_token("https://CRimpact@github.com")
)

mappings_matr <- issue_res$data$repository$issue$body %>%
    stringr::str_match_all(
        "(?<id>SYMP_[0-9]+) +- +(?<label>[^*]+)\\*\\*[^\\[]*\\[(?<xref>UMLS_CUI:[^\\]]+)\\]"
    ) %>%
    `[[`(1)

mappings <- mappings_matr[, colnames(mappings_matr) != ""] %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
        id = stringr::str_replace(id, "_", ":"),
        id = DO.utils::build_hyperlink(
            x = stringr::str_remove(id, ".*:"),
            url = "SYMP",
            txt = id,
            as = "gs"
        ),
        xref = DO.utils::build_hyperlink(
            x = stringr::str_remove(xref, ".*:"),
            url = "UMLS_CUI",
            txt = xref,
            as = "gs"
        )
    )

googlesheets4::write_sheet(
    mappings,
    gs,
    sheet
)
