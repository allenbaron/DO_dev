library(here)
library(DO.utils)
library(tidyverse)

do_dir <- here::here("../Ontologies/HumanDiseaseOntology")

owl_paths <- c(
    old = file.path(do_dir, "build/doid-last.owl"),
    new = file.path(do_dir, "src/ontology/doid-edit.owl")
)

# make sure last official doid-merged.owl file is at doid-last.owl
#   --> downloads if not already updated today
if (format(file.mtime(owl_paths["old"]), "%Y-%m-%d") != Sys.Date()) {
    download.file(
        "https://purl.obolibrary.org/obo/doid.owl",
        destfile = owl_paths["old"]
    )
}

axiom_paths <- c(
    old = tempfile(tmpdir = ".", fileext = ".tsv"),
    new = tempfile(tmpdir = ".", fileext = ".tsv")
)

info_path <- tempfile(fileext = ".tsv")

# get axioms
axioms <- purrr::map2(
    owl_paths,
    axiom_paths,
    function(.x, .y) {
        DO.utils::robot(
            "export --header 'ID|Equivalent Class|SubClass Of [ANON]'",
            i = .x,
            export = .y
        )
        readr::read_tsv(.y, col_types = "c") %>%
            dplyr::rename(
                id = ID, eqClass = `Equivalent Class`,
                subClass = `SubClass Of [ANON]`
            ) %>%
            dplyr::filter(stringr::str_detect(id, "DOID")) %>%
            tidyr::pivot_longer(
                cols = c(eqClass, subClass),
                names_to = "type",
                values_to = "value",
                values_drop_na = TRUE
            )
    }
) %>%
    dplyr::bind_rows(.id = "file_version")

# diff
system2("git", c("diff --word-diff --no-index", unname(axiom_paths)), stdout = "release_axiom.diff")

# get new labels, deprecation info
DO.utils::robot(
    "query",
    i = owl_paths["new"],
    query = "sparql/DO-id_label-w_dep.rq",
    info_path
)

do_info <- readr::read_tsv(info_path) %>%
    dplyr::rename_with(.fn = ~ stringr::str_remove_all(.x, "\\?")) %>%
    dplyr::mutate(
        dplyr::across(dplyr::everything(), ~ stringr::str_remove(.x, "@en"))
    )


axiom_review <- axioms %>%
    dplyr::filter(!all_duplicated(dplyr::select(axioms, -file_version))) %>%
    dplyr::group_by(id, type) %>%
    dplyr::mutate(
        change_type = dplyr::case_when(
            all(c("new", "old") %in% file_version) ~ "changed",
            all(file_version == "new") ~ "added",
            all(file_version == "old") ~ "removed"
        )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
        change_type = factor(change_type, levels = c("added", "removed", "changed"))
    ) %>%
    tidyr::pivot_wider(
        names_from = file_version,
        values_from = value
    ) %>%
    # in case axioms are only added or removed and one of the cols is missing
    DO.utils::append_empty_col(col = c("new", "old")) %>%
    dplyr::left_join(do_info, by = "id") %>%
    dplyr::select(change_type, id, label, deprecated, dplyr::everything()) %>%
    dplyr::arrange(change_type, id, type, new, old)


# review changes if many
if (sum(axiom_review$change_type == "changed") > 5) {
    change_review <- dplyr::filter(axiom_review, change_type == "changed") %>%
        dplyr::mutate(
            dplyr::across(
                old:new,
                ~ stringr::str_replace_all(.x, stringr::coll("|"), "\n")
            )
        )

    write_csv(change_review, "DEL-axiom_review.csv")
}

View(axiom_review)
