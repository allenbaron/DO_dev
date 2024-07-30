# disambiguation of dysequilibrium syndrome
# 2024-07-22

library(europepmc)
library(tidyverse)
library(xml2)
library(here)

data_file <- here::here("data/disease_info/DES_disambiguation-20240724.rda")

# custom functions --------------------------------------------------------

# get_ftxt_safely() will automatically get PMC articles or books and will NOT
#   fail if on errors caused by individual download failures
safe_epmc_ftxt <- purrr::safely(europepmc::epmc_ftxt, otherwise = NA, quiet = FALSE)
safe_epmc_ftxt_bk <- purrr::safely(europepmc::epmc_ftxt_book, otherwise = NA, quiet = FALSE)

get_ftxt_safely <- function(pmcid = NA, bookid = NA) {
    if (is.na(pmcid) && is.na(bookid) ) return(NA)
    if (!is.na(pmcid)) {
        list(safe_epmc_ftxt(pmcid))
    } else {
        list(safe_epmc_ftxt_bk(bookid))
    }
}


# parse_ftxt_xml() parses results from get_ftxt_safely()
parse_ftxt_xml <- function(safe_ftxt_xml, xml_accessor) {
    if (!is.null(safe_ftxt_xml$error)) {
        return(paste0("ERROR: ", safe_ftxt_xml$error$message))
    }
    out <- safe_ftxt_xml$result %>%
        xml2::xml_find_all(xml_accessor) %>%
        xml2::xml_text()

    if (length(out) == 0) {
        out <- paste0(
            "ERROR [NO BODY]: ",
            xml2::xml_text(safe_ftxt_xml$result)
        )
        if (length(out) == 0) {
            out <- "ERROR: No text extractable"
        }
    } else if (length(out) > 1) {
        out <- DO.utils::vctr_to_string(out, delim = "%%%%%") %>%
            paste0("WARNING: Multilength output, separated by %%%%%.")
    }

    out
}


# EuropePMC search --------------------------------------------------------

# full-text available ONLY for open access articles
des <- europepmc::epmc_search(
    'OPEN_ACCESS:y AND ("dysequilibrium syndrome" OR "disequilibrium syndrome")',
    synonym = FALSE,
    limit = 1000
)

save(des, file = data_file)


# EuropePMC full text -----------------------------------------------------

des_ftxt <- des %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ft_xml = get_ftxt_safely(pmcid, bookid)) %>%
    dplyr::mutate(ft = parse_ftxt_xml(ft_xml, "//body"))

save(des_ftxt, file = data_file)


# Analysis ----------------------------------------------------------------

des_res <- des_ftxt %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
        des_nm = stringr::str_extract_all(
            ft,
            stringr::regex(
                "(dialysis )?d[iy]sequilibrium syndrome",
                ignore_case = TRUE
            )
        ),
        des_acronym = stringr::str_detect(ft, "\\bDES\\b"),
        dds_acronym = stringr::str_detect(ft, "\\bDDS\\b")
    ) %>%
    dplyr::filter(
        purrr::map_int(des_nm, length) > 0
    ) %>%
    dplyr::mutate(
        des_nm = purrr::map(des_nm, ~ unique(stringr::str_to_lower(.x))),
        dial_prop = purrr::map_dbl(
            des_nm,
            ~ sum(stringr::str_detect(.x, "dialysis")) / length(.x)
        ),
        des_nm_type = dplyr::case_when(
            dial_prop == 1 ~ "dialysis only",
            dial_prop > 0 & dial_prop < 1 ~ "both",
            dial_prop == 0 & stringr::str_detect(ft, "dialysis") ~ "dial + des",
            dial_prop == 0 ~ "no dialysis"
        ),
        acronym = dplyr::case_when(
            des_acronym & dds_acronym ~ "DES + DDS",
            des_acronym ~ "DES",
            dds_acronym ~ "DDS",
            TRUE ~ NA
        )
    )

save(des_res, file = data_file)

des_summary <- des_res %>%
    dplyr::count(des_nm_type, acronym) %>%
    dplyr::mutate(prop = round(n / sum(n) * 100, 1))


# Subtype synonyms & acronyms ---------------------------------------------

des_subtype_search <- paste0(
    '"disequilibrium syndrome ', 1:4, '" OR ',
    '"dysequilibrium syndrome ', 1:4, '"'
)

des_st_res <- purrr::map(
    des_subtype_search,
    ~ europepmc::epmc_search(.x, synonym = FALSE, limit = 1000)
)

save(des_res, des_st_res, des_summary, file = data_file)
