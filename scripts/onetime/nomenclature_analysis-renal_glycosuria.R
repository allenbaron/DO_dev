# nomenclature analysis of renal glycosuria
# 2024-07-31

library(europepmc)
library(tidyverse)
library(xml2)
library(here)

data_file <- here::here("data/disease_info/nom_anal-renal_glycosuria-20240724.rda")

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

glys <- europepmc::epmc_search(
    'OPEN_ACCESS:y AND ("renal glycosuria" OR "renal glucosuria")',
    synonym = FALSE,
    limit = 1000
)

save(glys, file = data_file)


# EuropePMC full text -----------------------------------------------------

glys_ftxt <- glys %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ft_xml = get_ftxt_safely(pmcid, bookid)) %>%
    dplyr::mutate(ft = parse_ftxt_xml(ft_xml, "//body"))

save(glys_ftxt, file = data_file)


# Analysis ----------------------------------------------------------------

glys_res <- glys_ftxt %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
        glys_gene = stringr::str_extract_all(
            ft,
            "\\b[A-Z]{3}[A-Z0-9]{1,}\\b"
        ),
        glys_nm = stringr::str_extract_all(
            ft,
            stringr::regex(
                "(familial|hereditary) renal gl[yu]cosuria",
                ignore_case = TRUE
            )
        ),
        glys_acronym = stringr::str_detect(ft, "\\bFRG\\b")
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
        dplyr::across(
            dplyr::starts_with("glys_"),
            ~ DO.utils::unique_to_string(stringr::str_to_lower(.x), delim = "|")
        )
    )

save(glys_res, file = data_file)



# Analysis ----------------------------------------------------------------

# how many with familial?
dplyr::count(glys_res, glys_nm, sort = TRUE)
    # vast majority don't have it
    # glucosuria is more common than glycosuria
    # familial much more common than hereditary (4 instances)

# what about FRG acronym?
dplyr::count(glys_res, glys_acronym, glys_nm) %>%
    dplyr::arrange(glys_acronym, dplyr::desc(n))
    # most don't use it, even when they have the full name (< 1/2 in this case)
    # it is fairly common though

# how many mention specific gene - 462 / 194
gene_in <- dplyr::filter(glys_res, stringr::str_detect(glys_gene, "SLC5A2|SGLT2"))

# of those that do have gene, how do they use terminology/acronym?
dplyr::count(gene_in, glys_acronym, glys_nm) %>%
    dplyr::arrange(glys_acronym, dplyr::desc(n))
    # most have familial/hereditary but a sizable amount don't (< 1/2)
    # pretty much all usage of FRG is with gene
anti_join(glys_res, gene_in) %>%
    dplyr::count(glys_acronym)
    # only 1 uses acronym without mentioning gene

# how many mention diabetes if +/- familial?
glys_res <- glys_res %>%
    dplyr::mutate(
        diabetes = stringr::str_detect(
            ft,
            stringr::coll("diabetes", ignore_case = TRUE)
            )
    )

dplyr::count(glys_res, diabetes, glys_nm) %>%
    dplyr::arrange(diabetes, dplyr::desc(n))
    # most papers mention diabetes, period
    # the great majority of familial papers, also mention diabetes
    # most of the papers that DON'T mention diabetes also don't have "familial"


# what genes are mentioned in papers with "familial"?
glys_res %>%
    DO.utils::lengthen_col(glys_gene, delim = "|") %>%
    dplyr::mutate(
        glys_nm = dplyr::if_else(is.na(glys_nm), FALSE, TRUE)
    ) %>%
    dplyr::count(glys_nm, glys_gene,) %>%
    dplyr::arrange(glys_nm, dplyr::desc(n))

# what do the papers with only renal glucosuria discuss?
no_fam <- dplyr::filter(glys_res, is.na(glys_nm))
head(no_fam$ft)
