library(cli)
library(DO.utils)
library(dplyr)
library(glue)
library(googlesheets4)
library(here)
library(purrr)
library(stringr)
library(tidyr)
library(vroom)


# MANUAL INPUTS -----------------------------------------------------------

gs <- "https://docs.google.com/spreadsheets/d/1u9ZgOGDJ0Cm7nhgg54zKnrLsY9_Y66tiUDbrG9ImLiI/"

edit_path <- "~/Documents/Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl"


# SUPPORTING DATA & FUNCTIONS ---------------------------------------------

sssom_cols <- c(
  "subject_id",
  "subject_label",
  "predicate_id",
  "predicate_modifier",
  "object_id",
  "object_label",
  "mapping_justification",
  "curation_rule",
  "comment",
  "author_id",
  "mapping_date",
  "subject_source_version",
  "object_source_version"
)

review_cols <- c(
  "exclude",
  "reviewer_id",
  "reviewer_curation_rule",
  "reviewer_comment",
  "object_source",
  "subject_obsoleted_version",
  "object_obsoleted_version"
)

standardize_sssom_review <- function(df, ...) {
  # standard processing for all review datasets
  out <- df |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ na_if(.x, "")),
      mapping_date = as.Date(.data$mapping_date)
    )

  # standardize & validate SNOMEDCT_US mappings (date in object_source_version)
  out <- df |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ na_if(.x, "")),
      mapping_date = as.Date(.data$mapping_date)
    ) |>
    dplyr::mutate(
      object_source_version = dplyr::if_else(
        !is.na(.data$object_source_version),
        .data$object_source_version,
        stringr::str_replace(
          .data$object_id,
          "SNOMEDCT_US_([0-9]{4})_([0-9]{2})_([0-9]{2}):.*",
          "\\1-\\2-\\3"
        )
      ),
      object_id = stringr::str_remove(
        .data$object_id,
        "_([0-9]{4})_([0-9]{2})_([0-9]{2})"
      )
    )

  snomed_err <- which(
    stringr::str_detect(out$object_id, "SNOMEDCT_US") &
      (stringr::str_detect(out$object_id, "([0-9]{4})_([0-9]{2})_([0-9]{2})") |
        !stringr::str_detect(
          out$object_source_version,
          "([0-9]{4})-([0-9]{2})-([0-9]{2})"
        ))
  )
  err_n <- length(snomed_err)
  if (err_n > 0) {
    cli::cli_abort(
      c(
        "There are SNOMEDCT_US records with invalid formatting.",
        x = paste0("{err_n} row{?s}: ", paste0(snomed_err, collapse = ", "))
      )
    )
  }

  # process exclude column if present
  if ("exclude" %in% names(out)) {
    out <- out |>
      dplyr::mutate(
        exclude = dplyr::if_else(
          is.na(.data$exclude),
          FALSE,
          as.logical(.data$exclude)
        )
      )
  }

  out |>
    # drop heading/separator rows
    dplyr::filter(!is.na(.data$subject_id)) |>
    # standardize column order
    dplyr::select(dplyr::all_of(sssom_cols), dplyr::any_of(review_cols), ...)
}


format_values <- function(df, col_nm) {
  if (length(col_nm) == 1) {
    return(paste0(sort(df[[col_nm]]), collapse = "\n"))
  }

  formatted <- df |>
    dplyr::select(dplyr::all_of(col_nm)) |>
    dplyr::arrange(dplyr::across(dplyr::everything())) |>
    dplyr::mutate(object_id = DO.utils::sandwich_text(.data$object_id, '"')) |>
    tidyr::unite(
      col = "final",
      "subject_id",
      "predicate_id",
      "object_id",
      sep = " "
    ) |>
    (\(x) x$final)() |>
    DO.utils::sandwich_text(c("(", ")"))

  uniq <- unique(formatted)
  diff <- length(formatted) - length(uniq)
  if (diff > 0) {
    cli::cli_warn("{diff} duplicate records were removed.")
  }

  paste0(uniq, collapse = "\n")
}

format_sparql_update <- function(df) {
  formatted <- df |>
    dplyr::select("subject_id", "predicate_id", "object_id") |>
    dplyr::arrange(.data$subject_id, .data$predicate_id, .data$object_id) |>
    dplyr::mutate(object_id = DO.utils::sandwich_text(.data$object_id, '"')) |>
    tidyr::unite(
      col = "final",
      "subject_id",
      "predicate_id",
      "object_id",
      sep = " "
    ) |>
    (\(x) x$final)() |>
    paste0(" .")

  uniq <- unique(formatted)
  diff <- length(formatted) - length(uniq)
  if (diff > 0) {
    cli::cli_warn("{diff} duplicate records were removed.")
  }

  paste0(uniq, collapse = "\n")
}


# LOAD & STANDARDIZE DATA -------------------------------------------------

## new_cui_from_xrefs ##
new_from_xref <- googlesheets4::read_sheet(
  gs,
  "new_cuis_from_xrefs",
  col_types = "c"
) |>
  standardize_sssom_review()

# additional standardization -- specifically needed for this time!
new_auto <- new_from_xref |>
  dplyr::mutate(
    # treat xrefs as exactMatch if human reviewed
    predicate_id = dplyr::if_else(
      reviewer_id == "orcid:0000-0002-0593-3569" &
        .data$predicate_id == "oboInOwl:hasDbXref",
      "skos:exactMatch",
      .data$predicate_id
    )
  )


## no_cui_found ##
no_cui_found <- googlesheets4::read_sheet(
  gs,
  "no_cui_found",
  col_types = "c"
) |>
  standardize_sssom_review()

# additional standardization & validation -- specifically needed for this time!

# error if any obsoleted objects that are not already marked for exclusion
obs <- !is.na(no_cui_found$object_obsoleted_version) & !no_cui_found$exclude
obs_n <- sum(obs)
if (obs_n > 0) {
  cli::cli_abort(
    c(
      "There are {obs_n} obsoleted object{?s} in the 'no_cui_found' sheet.",
      setNames(no_cui_found$object_label[obs], rep("i", obs_n))
    )
  )
}

# identify incomplete records & report
new_manual <- no_cui_found |>
  dplyr::mutate(
    incomplete = purrr::pmap_lgl(
      list(
        .data$subject_id,
        .data$predicate_id,
        .data$object_id,
        .data$mapping_justification,
        .data$curation_rule,
        .data$author_id,
        .data$mapping_date
      ),
      function(s, p, o, m, r, a, d, e) {
        sum(is.na(c(
          s,
          p,
          o,
          m,
          if (isTRUE(o == "sssom:NoTermFound")) "not_needed" else r,
          a,
          d
        ))) >
          0
      }
    )
  )

# report on incomplete records
new_manual |>
  dplyr::filter(.data$incomplete) |>
  dplyr::mutate(
    incomplete = dplyr::if_else(.data$exclude, "excluded", "needs_review")
  ) |>
  dplyr::count(incomplete)

# mark incomplete records for exclusion & drop extra 'incomplete' col
new_manual <- new_manual |>
  dplyr::mutate(exclude = .data$exclude | .data$incomplete)


## restore -- specifically needed for this time! ##
restore <- googlesheets4::read_sheet(gs, "restore", col_types = "c") |>
  standardize_sssom_review() |>
  # drop anything found in other reviewed data
  dplyr::anti_join(new_auto, by = c("subject_id", "object_id")) |>
  dplyr::anti_join(new_manual, by = c("subject_id", "object_id")) |>
  # convert to exactMatch (all human reviewed)
  dplyr::mutate(
    predicate_id = dplyr::if_else(
      .data$predicate_id == "oboInOwl:hasDbXref",
      "skos:exactMatch",
      .data$predicate_id
    )
  )


## for_manual_review ##
# did all this review in other sheets -> just make sure that these were
# removed correctly if not in another review sheet
for_manual_review <- googlesheets4::read_sheet(
  gs,
  "for_manual_review-rm_curation_only",
  col_types = "c"
) |>
  standardize_sssom_review() |>
  # drop anything found in other reviewed data
  dplyr::anti_join(new_auto, by = c("subject_id", "object_id")) |>
  dplyr::anti_join(new_manual, by = c("subject_id", "object_id")) |>
  dplyr::anti_join(restore, by = c("subject_id", "object_id")) |>
  dplyr::mutate(
    # convert to exactMatch (to cover bases)
    predicate_id = dplyr::if_else(
      .data$predicate_id == "oboInOwl:hasDbXref",
      "skos:exactMatch",
      .data$predicate_id
    ),
    # mark all as exclude -> no curation judgement made
    exclude = TRUE
  )


# CENTRALIZE DATA ---------------------------------------------------------

# merge & prep for deduplication
all_data_raw <- list(
  new_cuis_from_xrefs = new_auto,
  no_cui_found = new_manual,
  restore = restore,
  for_manual_review = for_manual_review
) |>
  dplyr::bind_rows(.id = "source")

# deduplicate keeping best record
all_data_tidy <- all_data_raw |>
  # rank record quality for deduplication
  dplyr::mutate(
    rank = dplyr::case_when(
      .data$author_id == "orcid:0000-0002-0593-3569" ~ 1L,
      .data$reviewer_id == "orcid:0000-0002-0593-3569" ~ 2L,
      !is.na(.data$author_id) ~ 3L,
      TRUE ~ 4L
    ),
    .by = c("subject_id", "predicate_id", "predicate_modifier", "object_id")
  ) |>
  # secondary rank of records based on most columns with data
  dplyr::mutate(
    rank2 = purrr::pmap_int(
      dplyr::pick(dplyr::everything()),
      ~ sum(is.na(c(...)))
    )
  ) |>
  dplyr::filter(
    .data$rank == min(.data$rank),
    .by = c("subject_id", "predicate_id", "predicate_modifier", "object_id")
  ) |>
  dplyr::filter(
    .data$rank2 == min(.data$rank2),
    .by = c("subject_id", "predicate_id", "predicate_modifier", "object_id")
  )

# validate no subject-object with differing predicates
diff_pred <- all_data_tidy |>
  dplyr::filter(
    DO.utils::all_duplicated(paste0(.data$subject_id, .data$object_id))
  )
if (nrow(diff_pred) > 0) {
  cli::cli_abort(
    c(
      "There are subject-object pairs with multiple differing predicates.",
      i = "See `diff_pred` dataset for details."
    )
  )
}

# identify discarded records for review
discarded <- dplyr::anti_join(
  all_data_raw,
  all_data_tidy,
  by = names(all_data_raw)
)
cli::cli_inform(
  c(i = "There are {nrow(discarded)} discarded records after deduplication.")
)

# validate no duplicates remain
dup_still <- all_data_tidy |>
  dplyr::select(
    "subject_id",
    "predicate_id",
    "predicate_modifier",
    "object_id"
  ) |>
  DO.utils::all_duplicated()
if (any(dup_still)) {
  cli::cli_abort(
    c(
      "There are still duplicates in the dataset after deduplication.",
      setNames(
        paste(
          all_data_tidy$subject_label[dup_still],
          all_data_tidy$object_label[dup_still],
          sep = " - "
        ),
        rep("i", sum(dup_still))
      )
    )
  )
}


### FINALIZED CENTRAL DATASET ###
dataset <- all_data_tidy |>
  dplyr::filter(is.na(.data$incomplete) | !.data$incomplete) |>
  dplyr::select(-"incomplete", -"source", -"rank", -"rank2")

vroom::vroom_write(
  dataset,
  here::here("data", "mapping", "doid.sssom.tsv"),
  na = ""
)


# save incomplete records as well
incomplete <- all_data_tidy |>
  dplyr::filter(.data$incomplete) |>
  dplyr::select(-"incomplete", -"source", -"rank", -"rank2")

vroom::vroom_write(
  incomplete,
  here::here("data", "mapping", "doid-incomplete.sssom.tsv"),
  na = ""
)



# FORMAT DATA FOR UPDATE -----------------------------------------------

# generate xref data (from exact & close matches, must have object_id)
xref_data <- all_data_tidy |>
  dplyr::filter(
    !is.na(.data$object_id),
    .data$predicate_id %in%
      c("skos:exactMatch", "skos:closeMatch") |
      is.na(.data$predicate_id)
  ) |>
  dplyr::mutate(predicate_id = "oboInOwl:hasDbXref")


# create full update dataset
all_data <- all_data_tidy |>
  # drop all SNOMEDCT_US skos mappings & records w/o predicate_id or object_id
  dplyr::filter(
    !(.data$predicate_id != "oboInOwl:hasDbXref" &
      stringr::str_detect(.data$object_id, "SNOMEDCT_US")),
    !is.na(.data$predicate_id),
    !is.na(.data$object_id),
  ) |>
  dplyr::bind_rows(xref_data) |>
  dplyr::distinct() |>
  # reformat SNOMEDCT_US to SNOMEDCT_US_YYYY_MM_DD
  dplyr::mutate(
    object_id = dplyr::if_else(
      !stringr::str_detect(.data$object_id, stringr::coll("SNOMEDCT_US")),
      .data$object_id,
      paste0(
        stringr::str_remove(.data$object_id, ":.*"),
        "_",
        stringr::str_replace_all(.data$object_source_version, "-", "_"),
        ":",
        stringr::str_remove(.data$object_id, ".*:")
      )
    )
  )


# SPLIT
rm_exclude <- all_data |>
  dplyr::filter(
    .data$exclude |
      .data$object_id == "sssom:NoTermFound" |
      .data$predicate_modifier == "Not"
  )

add_df <- all_data |>
  dplyr::anti_join(rm_exclude, by = names(rm_exclude))


# UPDATE ONTOLOGY --------------------------------------------------------

# needed due to bug in robot
prefix_file <- tempfile(fileext = ".json")
write(
  '{
    "@context": {
      "dc": "http://purl.org/dc/elements/1.1/",
      "obo": "http://purl.obolibrary.org/obo/",
      "owl": "http://www.w3.org/2002/07/owl#",
      "rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "xml": "http://www.w3.org/XML/1998/namespace",
      "xsd": "http://www.w3.org/2001/XMLSchema#",
      "doid": "http://purl.obolibrary.org/obo/doid#",
      "rdfs": "http://www.w3.org/2000/01/rdf-schema#",
      "skos": "http://www.w3.org/2004/02/skos/core#",
      "terms": "http://purl.org/dc/terms/",
      "oboInOwl": "http://www.geneontology.org/formats/oboInOwl#"
    }
  }',
  file = prefix_file
)

# remove mappings
rm_file <- tempfile(fileext = ".ofn")
rm_query <- glue::glue(
  'PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>
  PREFIX DOID: <http://purl.obolibrary.org/obo/DOID_>

  DELETE { ?subject_id ?predicate_id ?object_id }
  WHERE {
    VALUES (?subject_id ?predicate_id ?object_id) { @values@ }
    ?subject_id ?predicate_id ?object_id .
  }',
  values = format_values(
    rm_exclude,
    c("subject_id", "predicate_id", "object_id")
  ),
  .open = "@",
  .close = "@"
)
rm_res <- DO.utils::robot_query(
  `add-prefixes` = prefix_file,
  i = edit_path,
  query = rm_query,
  output = rm_file
)

# add mappings
add_file <- tempfile(fileext = ".ofn")
add_query <- glue::glue(
  'PREFIX owl: <http://www.w3.org/2002/07/owl#>
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>
  PREFIX DOID: <http://purl.obolibrary.org/obo/DOID_>

  INSERT { @add_triples@ }
  WHERE {
    VALUES ?subject_id { @values@ }
    ?subject_id a owl:Class .
  }',
  values = format_values(rm_exclude, "subject_id"),
  add_triples = format_sparql_update(add_df),
  .open = "@",
  .close = "@"
)
add_res <- DO.utils::robot_query(
  `add-prefixes` = prefix_file,
  i = rm_file,
  query = add_query,
  output = add_file
)

# restore to edit file
mv_res <- file.rename(add_file, edit_path)
if (!mv_res) {
  cli::cli_abort("Failed to move updated file back to {edit_path}.")
} else {
  cli::cli_inform(
    c(
      "Successfully updated {edit_path} with reviewed mappings.",
      i = "Please review the changes and commit to the DO repository."
    )
  )
}
