# DO Impact -- Biomedical resources

library(here)
library(DO.utils)
library(httr)
library(tidyverse)
library(lubridate)


# Collect web available data -----------------------------------------------

# set output directories for data
data_dir <- here::here("data/impact")
input_dir <- file.path(data_dir, "input")
if (!dir.exists(input_dir)) dir.create(input_dir, recursive = TRUE)


# Alliance of Genome Resources (Alliance)
alliance_disease_tsv <- DO.utils::download_alliance_tsv(dest_dir = input_dir)


# Mammal ncRNA–Disease Repository (MNDR)
rna_soc_avail <- try(httr::HEAD("https://www.rna-society.org"))
if (class(rna_soc_avail) == "try-error") {
    rlang::warn("rna-society.org is NOT accessible, skipping...")
    have_mndr <- FALSE
} else {
    mndr_disease_url <- "https://www.rna-society.org/mndr/download/All%20ncRNA-disease%20information.zip"
    mndr_disease_file <- file.path(input_dir, "MNDR_disease_all_ncRNA-RAW.tsv.zip")
    mndr_file_fixed <- file.path(input_dir, "MNDR_disease_all_ncRNA.tsv")
    if (!file.exists(mndr_disease_file)) {
        dl_mndr <- download.file(mndr_disease_url, mndr_disease_file)
        if (dl_mndr == 0) {
            have_mndr <- TRUE
        } else {
            rlang::warn("Could not download MNDR, skipping...")
            have_mndr <- FALSE
        }
    } else {
        have_mndr <- TRUE
    }

    if (have_mndr && !file.exists(mndr_file_fixed)) {
        # the downloaded file contains extra tabs in it in varying locations -->
        #   read in, strip leading/trailing tabs, write to tsv file
        readr::read_lines(mndr_disease_file) %>%
            stringr::str_replace_all(c("^\t+" = "", "\t+$" = "")) %>%
            readr::write_lines(mndr_file_fixed)
        if (!file.exists(mndr_file_fixed)) {
            rlang::warn("Unable to fix downloaded MNDR file.")
            have_mndr <- FALSE
        }
    }
}


# Bioconductor R packages: DOSE & DOdb
bioc <- purrr::map(
    2022:2023,
    ~ DO.utils::get_bioc_pkg_stats(
        pkg = c("DOSE", "DO.db"),
        pkg_type = c("software", "annotation"),
        yr = .x
    )
) %>%
    dplyr::bind_rows()

bioc_split <- list(
    whole_yr = bioc %>%
        dplyr::filter(Month == "all") %>%
        dplyr::arrange(pkg, Year),
    monthly = bioc %>%
        dplyr::filter(Month != "all") %>%
        dplyr::arrange(pkg, Year)
)

bioc_files <- file.path(
    input_dir,
    paste0("bioc_dl_stats-", names(bioc_split), ".tsv")
)

purrr::walk2(
    bioc_split,
    bioc_files,
    ~ readr::write_tsv(.x, .y)
)


# Load & tidy data, where needed ------------------------------------------

# Alliance
alliance_dis <- DO.utils::read_alliance(alliance_disease_tsv)

# MNDR
if (have_mndr) {
    mndr_dis <- readr::read_tsv(mndr_file_fixed) %>%
        dplyr::rename(DOID = "DO ID") %>%
        dplyr::rename_with(.fn = ~ stringr::str_replace_all(.x, " ", "_"))
    if (!exists("mndr_dis")) {
        have_mndr <- FALSE
    }
}



# Calculate impact summaries ----------------------------------------------

##### Alliance #####

# All unique DOIDs by species (with overall unique DOID for Alliance added)
alliance_doid <- DO.utils::count_alliance_records(
    alliance_dis,
    record_lvl = "disease",
    by_type = FALSE,
    assign_to = "species"
) %>%
    tibble::add_row(
        species = "Alliance_TOTAL",
        disease_n = dplyr::n_distinct(alliance_dis$DOID)
    )


# All unique records annotated with a DOID by species (with overall for Alliance
# added)
alliance_annotated <- DO.utils::count_alliance_records(
    alliance_dis,
    record_lvl = "full_record",
    by_type = TRUE,
    assign_to = "species"
) %>%
    # need to add full alliance as option in DO.utils
    dplyr::bind_rows(
        alliance_dis %>%
            unique() %>%
            dplyr::summarize(
                full_record_n = length(DBobjectType),
                .by = DBobjectType
            ) %>%
            dplyr::mutate(
                DBobjectType = stringr::str_remove(DBobjectType, ".*_")
            ) %>%
            tidyr::pivot_wider(
                names_from = DBobjectType,
                values_from = full_record_n,
                names_glue = "{DBobjectType}.full_record_n"
            ) %>%
            dplyr::mutate(species = "Alliance_TOTAL")
    )

readr::write_tsv(alliance_doid, file.path(data_dir, "alliance_unique_doid.tsv"))
readr::write_tsv(
    alliance_annotated,
    file.path(data_dir, "alliance_annotated_records.tsv")
)


##### MNDR #####

if (have_mndr) {
    mndr_count <- mndr_dis %>%
        dplyr::select(ncRNA_symbol, ncRNA_Category, Species) %>%
        unique() %>%
        dplyr::count(Species, ncRNA = ncRNA_Category) %>%
        tidyr::pivot_wider(
            names_from = ncRNA,
            names_glue = "{ncRNA}_n",
            values_from = n
        )

    readr::write_csv(mndr_count, "MNDR-object_count.csv")
}


##### Bioconductor #####

# mean download by Unique IP over the last year (last month included = last
# complete month)
cur_month_start <- lubridate::floor_date(lubridate::today(), "month")
bioc_meanIP <- bioc_split$monthly %>%
    dplyr::mutate(
        month_start = lubridate::ym(paste(Year, Month, sep = "-"))
    ) %>%
    dplyr::filter(
        month_start >= cur_month_start - lubridate::years(1),
        month_start < cur_month_start
    ) %>%
    dplyr::summarize(
        year_range = paste(
            Month[month_start == min(month_start)],
            Year[month_start == min(month_start)],
            "-",
            Month[month_start == max(month_start)],
            Year[month_start == max(month_start)],
            sep = " "
        ),
        dplyr::across(
            dplyr::starts_with("Nb"),
            mean,
            .names = "mean_monthly_{.col}"),
        .by = c(pkg, pkg_type)
    ) %>%
    dplyr::rename_with(
        .cols = dplyr::contains("Nb"),
        .fn = ~ stringr::str_remove(.x, "Nb_of_")
    )

readr::write_tsv(bioc_meanIP, file.path(data_dir, "bioconductor_year_mean.tsv"))
