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
                id = "ID", parent = "SubClass Of [ID NAMED]"
            ) |>
            dplyr::filter(stringr::str_detect(.data$id, "DOID"))
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

hier_tmp <- hier |>
    # drop IDs with no changed parents
    dplyr::filter(
        !all(c("new", "old") %in% .data$file_version),
        .by = c("id", "parent")
    ) |>
    DO.utils::lengthen_col("parent") |>
    DO.utils::collapse_col("file_version") |>
    dplyr::mutate(
        change_type = dplyr::case_when(
            all(c("new", "old") %in% .data$file_version, na.rm = TRUE) ~ "changed",
            all(!"old" %in% .data$file_version) ~ "added",
            all(!"new" %in% .data$file_version) ~ "removed"
        ),
        .by = "id"
    ) |>
    dplyr::mutate(
        file_version = dplyr::if_else(
            stringr::str_detect(.data$file_version, stringr::coll("|")),
            "unchanged",
            .data$file_version
        ),
        parent = dplyr::recode(.data$parent, !!!sc_recode),
        change_type = factor(
            .data$change_type,
            levels = c("added", "removed", "changed")
        )
    )
# ensure relocate works when any of new, old, or unchanged is missing
fversion <- factor(
    unique(hier_tmp$file_version),
    levels = c("unchanged", "old", "new")
) |>
    sort() # set order

hier_review <- hier_tmp |>
    tidyr::pivot_wider(
        names_from = "file_version",
        values_from = "parent",
        values_fn = ~ DO.utils::vctr_to_string(.x)
    ) |>
    dplyr::left_join(do_info, by = "id") |>
    dplyr::relocate(
        "change_type", "id", "label", "deprecated", dplyr::all_of(fversion)
    ) |>
    dplyr::arrange(.data$label, .data$change_type)


# write changes for review
change_review <- hier_review |>
    dplyr::mutate(
        dplyr::across(
            dplyr::all_of(fversion),
            ~ stringr::str_replace_all(.x, stringr::coll("|"), "\n")
        )
    )
write_csv(change_review, "DEL-hier_review.csv")

View(hier_review)
