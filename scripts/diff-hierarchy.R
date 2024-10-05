library(here)
library(DO.utils)
library(tidyverse)

do_dir <- here::here("../Ontologies/HumanDiseaseOntology")
de_path <- file.path(do_dir, "src/ontology/doid-edit.owl")
owl_paths <- c(
    old = file.path(do_dir, "build/doid-last.owl"),
    new = file.path(do_dir, "build/doid-reasoned.owl")
)

# make sure last official doid-merged.owl file is at doid-last.owl
#   --> downloads if not already updated today
if (format(file.mtime(owl_paths["old"]), "%Y-%m-%d") != Sys.Date()) {
    download.file(
        "https://purl.obolibrary.org/obo/doid.owl",
        destfile = owl_paths["old"]
    )
}

hier_paths <- c(
    old = tempfile(tmpdir = ".", fileext = ".tsv"),
    new = tempfile(tmpdir = ".", fileext = ".tsv")
)

# create reasoned output (pre-release)
DO.utils::robot(
    "reason",
    i = de_path,
    "create-new-ontology" = FALSE,
    "annotate-inferred-axioms" = FALSE,
    "exclude-duplicate-axioms" = TRUE,
    o = owl_paths["new"],
    .robot_path = file.path(do_dir, "build/robot.jar")
)


# get axioms
hier <- purrr::map2(
    owl_paths,
    hier_paths,
    function(.x, .y) {
        DO.utils::robot(
            "export --header 'ID|SubClass Of [ID NAMED]'",
            i = .x,
            export = .y
        )
        readr::read_tsv(.y, col_types = "c") |>
            dplyr::rename(
                id = ID, parent = `SubClass Of [ID NAMED]`
            ) |>
            dplyr::filter(stringr::str_detect(id, "DOID"))
    }
) |>
    dplyr::bind_rows(.id = "file_version")


# get new labels, deprecation info
do_info <- DO.utils::robot_query(
    owl_paths["new"],
    "sparql/DO-id_label-w_dep.rq",
    tidy_what = c("header", "rm_lang_tag", "lgl_NA_FALSE")
)

sc_recode <- paste0(do_info$label, " (", do_info$id, ")") |>
    purrr::set_names(do_info$id)

hier_review <- hier |>
    DO.utils::lengthen_col("parent") |>
    dplyr::mutate(parent = dplyr::recode(parent, !!!sc_recode)) |>
    DO.utils::collapse_col("parent") |>
    dplyr::filter(!all_duplicated(paste0(.data$id, .data$parent))) |>
    dplyr::mutate(
        change_type = dplyr::case_when(
            all(c("new", "old") %in% file_version) ~ "changed",
            all(file_version == "new") ~ "added",
            all(file_version == "old") ~ "removed"
        ),
        .by = "id"
    ) |>
    dplyr::mutate(
        change_type = factor(change_type, levels = c("added", "removed", "changed"))
    ) |>
    tidyr::pivot_wider(
        names_from = file_version,
        values_from = "parent",
        values_fn = ~ DO.utils::vctr_to_string(.x)
    ) |>
    dplyr::left_join(do_info, by = "id") |>
    dplyr::relocate(change_type, id, label, deprecated, old, .before = new) |>
    dplyr::arrange(new, old, change_type, id)


# review changes if many
if (sum(hier_review$change_type == "changed") > 5) {
    change_review <- dplyr::filter(hier_review, change_type == "changed") |>
        dplyr::mutate(
            dplyr::across(
                old:new,
                ~ stringr::str_replace_all(.x, stringr::coll("|"), "\n")
            )
        )

    write_csv(change_review, "DEL-hier_review.csv")
}

View(hier_review)
