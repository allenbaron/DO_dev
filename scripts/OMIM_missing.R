# identify missing OMIM xrefs

library(here)
library(DO.utils)
library(tidyverse)

# paths -- UPDATE AS NECESSARY (always for Google Sheet, gs_output)
omim_csv <- here::here("DEL_omim.csv")
de_path <- here::here(
    "../Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl"
)
gs_output <- "https://docs.google.com/spreadsheets/d/1u8ULuuQDsxUHmKT6qNexTsprRp5JL1diHHsLuS_qzvs/edit#gid=1371246675"
timestamp <- format(Sys.Date(), "%Y%m%d") # appended to end of sheet names


# custom function
iff_all <- function(x, options) {
    present <- all(options %in% x)
    only <- all(x %in% options)

    present & only
}

# get OMIM xrefs from doid-edit.owl file
de_owl_path <- tempfile(fileext = ".owl")

DO.utils::robot(
    "convert",
    i = de_path,
    o = de_owl_path
)

de_owl <- DO.utils::owl_xml(de_owl_path)

do_omim <- de_owl$query(
    "SELECT ?id ?label ?omim ?dep
    WHERE {
        ?class oboInOwl:id ?id ;
            rdfs:label ?label ;
            oboInOwl:hasDbXref ?omim .
        FILTER( CONTAINS( ?omim, 'OMIM' ) )
        OPTIONAL { ?class owl:deprecated ?dep . }
    }"
) %>%
    DO.utils::tidy_sparql() %>%
    tidyr::replace_na(list(dep = FALSE))


# get OMIM from manual save of PS (+ added info)
omim <- readr::read_csv(omim_csv, skip = 1) %>%
    purrr::set_names(
        c("location", "phenotype", "inheritance", "phenotype_mapping_key",
          "phenotype_mim_number", "gene_locus", "gene_locus_mim_number")
    ) %>%
    dplyr::mutate(
        omim = paste0("OMIM:", phenotype_mim_number),
        geno_inheritance = dplyr::case_when(
            inheritance == "AR" ~ 'autosomal recessive inheritance',
            inheritance == "AD" ~ 'autosomal dominant inheritance',
            inheritance == "XLR" ~ 'X-linked recessive inheritance',
            inheritance == "XLD" ~ 'X-linked recessive inheritance',
            iff_all(inheritance, c("AR", "AD")) ~ 'autosomal inheritance',
            iff_all(inheritance, c("XLR", "XLD")) ~ 'X-linked inheritance',
            .default = NA_character_
        )
    )

# check for obsolete DO terms with OMIM xrefs
dep1 <- do_omim %>% dplyr::filter(dep)

if (nrow(dep1) > 0) {
    rlang::abort(
        c(
            paste0(
                "Obsolete diseases with OMIM xrefs: ",
                dplyr::n_distinct(dep1$id)
            ),
            "Pause to examine and possibly fix with dep_fix!!!"
        )
    )
    dep_fix <- dplyr::filter(do_omim, omim %in% dep1$omim)
    scan()
}

do_omim2 <- do_omim %>%
    dplyr::filter(!dep) %>%
    dplyr::select(-dep)

# new OMIM terms to add
new_omim <- dplyr::anti_join(omim, do_omim2, by = "omim") %>%
    dplyr::mutate(
        omim_link = DO.utils::build_hyperlink(
            x = phenotype_mim_number,
            url = "https://www.omim.org/MIM:",
            as = "gs",
            txt = omim
        ),
        sort = str_remove(phenotype, "^\\?")
    ) %>%
    dplyr::arrange(sort) %>%
    dplyr::select(-sort)

inDO <- dplyr::inner_join(do_omim2, omim, by = "omim")

# save
if (nrow(new_omim) > 0) {
    googlesheets4::write_sheet(
        data = new_omim,
        ss = gs_output,
        sheet = paste0("new_", timestamp)
    )
} else{
    message("No OMIM from this PS are new.")
}


if (nrow(inDO) > 0) {
    googlesheets4::write_sheet(
        data = inDO,
        ss = gs_output,
        sheet = paste0("inDO_", timestamp)
    )
} else {
    message("All OMIM in this PS are new!!")
}
