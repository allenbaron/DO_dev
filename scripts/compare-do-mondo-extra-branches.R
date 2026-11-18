library(DO.utils)
library(here)
library(tidyverse)

mondo_path <- here::here("data/compare_do_mondo/mondo.owl")
query <- here::here("sparql/compare-do-mondo-extra-branches.rq")

out_dir <- here::here("data/compare_do_mondo")
out_path <- file.path(out_dir, "mondo-extra-branches-R-no-UMLS.tsv")
mapping_out_path <- file.path(out_dir, "mondo-extra-branches-R-details-no-UMLS.tsv")


# data processing --------------------------------------------------------------

hierarchy_order <- c(
  "SNOMEDCT_US",
  "NCI",
  "ORDO",
  "EFO",
  "MESH",
  "ICD9CM",
  "ICD10CM",
  "MEDDRA"#,
  # "UMLS_CUI"
)

extra_res <- DO.utils::robot_query(
  i = mondo_path,
  query = query,
  tidy_what = "everything"
)

extra_tidy <- extra_res |>
  dplyr::mutate(
    mappings = stringr::str_split(mappings, "\\|"),
    mappings = purrr::map(
      mappings,
      ~ stringr::str_replace_all(
        .x,
        c(
          "^OMIM:|.*omim\\.org/entry/" = "MIM:",
          "^OMIMPS:|.*omim\\.org/phenotypicSeries/PS" = "MIM:PS",
          "^.*Orphanet[:_]" = "ORDO:",
          "^UMLS:|.*umls/id/" = "UMLS_CUI:",
          "^SCTID:|.*snomedct/" = "SNOMEDCT_US:",
          "^.*(ICD|browse)(9|10)(CM|WHO|.*2019/en#)?[:/]" = "ICD\\2CM:",
          ".*mesh/" = "MESH:",
          ".*medgen/" = "MEDGEN:",
          "^MedDRA|.*meddra/" = "MEDDRA:",
          "icd11\\.foundation:|http://id\\.who\\.int/icd/entity/" = "ICD11:",
          ".*NCIT[:_]" = "NCI:",
          ".*/([^:_]+)[:_]" = "\\1:"
        )
      ) |>
      unique() |>
      sort()
    ),
    hierarchy_sources = purrr::map(
      mappings,
      ~ stringr::str_remove(.x, ":.*$") |>
        unique() |>
        factor(levels = hierarchy_order) |>
        na.omit() |>
        sort()
    ),
    source_count = purrr::map_int(hierarchy_sources, length),
    primary_source = purrr::map_chr(hierarchy_sources, ~ as.character(.x[1]))
  )

extra_out <- extra_tidy |>
  dplyr::mutate(
    mappings = purrr::map_chr(mappings, ~ paste(.x, collapse = "|")),
    hierarchy_sources = purrr::map_chr(
      hierarchy_sources,
      ~ paste(sort(.x), collapse = "-")
    ),
    hierarchy_sources = dplyr::na_if(hierarchy_sources, "")
  )


# calculate stats --------------------------------------------------------------

source_count <- dplyr::count(extra_tidy, source_count, name = "terms_count") |>
  dplyr::rename("sources_count" = "source_count") |>
  dplyr::arrange(sources_count)

overlap_n <- extra_tidy$hierarchy_sources |>
  unlist() |>
  table() |>
  tibble::as_tibble(.name_repair = ~ c("hierarchy_source", "n")) |>
  dplyr::mutate(
    hierarchy_source = factor(hierarchy_source, levels = hierarchy_order)
  ) |>
  dplyr::arrange(hierarchy_source)

all_source_n <- extra_out |>
  dplyr::count(hierarchy_sources, name = "count") |>
  dplyr::rename("hierarchy_source_group" = "hierarchy_sources") |>
  dplyr::arrange(hierarchy_source_group)

dplyr::count(extra_tidy, primary_source)



# write output -----------------------------------------------------------------

# fully detailed output
readr::write_tsv(extra_out, mapping_out_path, na = "")


# mapping stats output
readr::write_lines(
  "MONDO 'extra' branch classes -- hierarchical source count:\t",
  out_path
)
readr::write_tsv(source_count, out_path, append = TRUE, col_names = TRUE)

readr::write_lines(
  "\t\n\t\nMONDO 'extra' branch classes -- by source (overlapping):\t",
  out_path,
  append = TRUE
)
readr::write_tsv(overlap_n, out_path, append = TRUE, col_names = TRUE)

readr::write_lines(
  "\t\n\t\nMONDO 'extra' branch classes (non-overlapping):\t",
  out_path,
  append = TRUE
)
readr::write_tsv(all_source_n, out_path, append = TRUE, col_names = TRUE)
