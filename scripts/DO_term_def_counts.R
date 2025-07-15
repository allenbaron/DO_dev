# R script to execute DO release reports (term, def counts) on unrecorded releases

library(here)
library(DO.utils)
library(dplyr)
library(readr)
library(git2r)

# Set file input/output
do_dir <- path.expand('~/Documents/Ontologies/HumanDiseaseOntology/')
doid_owl <- file.path(do_dir, 'src/ontology/doid.owl')

do_dev <- here::here()
release_stat_dir <- file.path(do_dev, "data/DO_release")
release_file <- file.path(release_stat_dir, 'DO_term_def_counts.csv')

# Load repo & identify new tags
repo <- git2r::repository(do_dir)
tags <- git2r::tags(repo)
tag_names <- names(tags)

if (file.exists(release_file)) {
  rel_df <- readr::read_csv(release_file, show_col_types = FALSE)
  old_tags <- rel_df$tag_name
  new_tag_names <- setdiff(tag_names, old_tags)
} else {
  rel_df <- NULL
  new_tag_names <- tag_names
}

# Define SPARQL query
q <- '
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX obo: <http://purl.obolibrary.org/obo/>

SELECT (COUNT(?s) AS ?terms) (COUNT(?d) AS ?defs) WHERE {
  ?s a owl:Class .
  OPTIONAL { ?s obo:IAO_0000115 ?d . }
  FILTER STRSTARTS(str(?s), "http://purl.obolibrary.org/obo/DOID_")
  FILTER NOT EXISTS {?s owl:deprecated ?any}
}
'

results_list <- list()

if (length(new_tag_names) > 0) {
  for (tag in new_tag_names) {
    # Checkout the tag
    git2r::checkout(repo, tag)
    # Run the SPARQL query
    res <- tryCatch({
      DO.utils::robot_query(doid_owl, q, tidy_what = "header")
    }, error = function(e) {
      message(sprintf("Error on tag %s: %s", tag, e$message))
      return(NULL)
    })
    if (!is.null(res)) {
      res$tag_name <- tag
      results_list[[tag]] <- res
    }
  }
  # Restore to main branch (optional)
  git2r::checkout(repo, "main")

  if (length(results_list) > 0) {
    df <- dplyr::bind_rows(results_list)
    df <- df %>% dplyr::select(tag_name, dplyr::everything())
    print(df)
    if (!is.null(rel_df)) {
      df_app <- dplyr::bind_rows(rel_df, df)
    } else {
      df_app <- df
    }
    readr::write_csv(df_app, release_file)
  }
} else {
  message('No new releases to extract term & definition counts from. Skipping...')
}
