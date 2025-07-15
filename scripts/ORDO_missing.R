# add ordo xrefs

library(DO.utils)
library(googlesheets4)
library(tidyverse)

##### MANUAL INPUTS #####

# curation sheet info (if exists)
ss <- "https://docs.google.com/spreadsheets/d/1VsdPThppGWY0gSBJL9SRAqzfscRO8uBxRlUfPqBxzMU/edit?gid=9983579#gid=9983579"
sheet <- "curation-20241125"

# terms of interest
ordo_tsv <- "DEL_ordo.tsv"

# path to doid-edit.owl
doid_edit <- "~/Documents/Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl"


##### LOAD INPUTS #####

# read ORDO terms of interest
ordo_check <- readr::read_tsv("DEL_ordo.tsv", show_col_types = FALSE)

# read ORDO_SSSOM
ordo_sssom <- googlesheets4::read_sheet(
	"https://docs.google.com/spreadsheets/d/1ziIO01RM-0NSfqAxGR5AjphnbAy5pIn9UnRTr3oYcZs/edit?gid=1544086303#gid=1544086303",
	sheet = "in_ORDO"
) |>
	# merge ORDO mappings with equivalent scope
	DO.utils::collapse_col(c(object_id, object_label))

# read curation sheet (if it exists)
if (!is_missing(ss)) {
	cur_sheet <- googlesheets4::read_sheet(ss, sheet = sheet)
} else {
	cur_sheet <- NULL
}


#### NEED DO.utils::inventory_mapping() function!! for this #####

# load existing ORDO mappings
mappings <- DO.utils::robot_query(
	doid_edit,
	query = system.file("sparql", "mapping-all.rq", package = "DO.utils"),
	tidy_what = c("header", "uri_to_curie")
)

ordo_mapping <- mappings |>
	dplyr::filter(stringr::str_detect(.data$mapping, "^ORDO")) |>
	dplyr::rename(doid = "id") |>
	dplyr::rename_with(
		.fn = ~ paste0("do_", .x),
		.cols = c(label, dep)
	)

# check for existing ORDO xrefs from ordo_check
out <- ordo_check |>
	dplyr::rename(ordo_label = "label") |>
	dplyr::left_join(ordo_mapping, by = "mapping") |>
	DO.utils::append_empty_col(
		col = c("exists", "mapping_type", "doid", "do_label", "do_dep")
	) |>
	dplyr::mutate(exists = !is.na(.data$doid)) |>
    dplyr::relocate(
		dplyr::all_of(c("exists", "mapping_type")),
		.before = dplyr::all_of("doid")
	)

ordo_mm <- DO.utils:::multimaps(out$mapping, out$mapping_type, out$doid)
doid_mm <- DO.utils:::multimaps(out$doid, out$mapping_type, out$mapping)
out <- dplyr::mutate(
	out,
	multimaps = dplyr::case_when(
		ordo_mm & doid_mm ~ "both_ways",
		ordo_mm ~ "ordo_to_doid",
		doid_mm ~ "doid_to_ordo",
		TRUE ~ NA_character_
	)
)
class(out) <- c("ordo_inventory", "mapping_inventory", class(out))

ordo_check2 <- dplyr::filter(out, !exists) |>
	dplyr::select(ordo_label, mapping)
out <- dplyr::filter(out, exists)


#### Fuzzy match labels from ORDO with DO labels & exact synonyms #####

# Get Query from SPARQLqueries: DOq040 (exact-synonyms-in-DO.rq)
q_exact_syn <- readr::read_lines( 
    "https://raw.githubusercontent.com/DiseaseOntology/SPARQLqueries/refs/heads/main/Datasets/Synonyms/exact-synonyms-in-DO.rq"
)

do_synonym <- DO.utils::robot_query(
	doid_edit,
	query = q_exact_syn,
	tidy_what = c("header", "uri_to_curie", "rm_lang_tag")
) |>
	dplyr::mutate(
		exact_synonym = stringr::str_remove(exact_synonym, "@en"),
    	syn_is_acronym = as.logical(syn_is_acronym)
	) |>
	# drop acronyms
    dplyr::filter(!syn_is_acronym) |>
    dplyr::select(-dplyr::all_of("syn_is_acronym"))

do_text <- do_synonym |>
	tidyr::pivot_longer(
		c("label", "exact_synonym"),
		names_to = "type",
		values_to = "text",
		values_drop_na = TRUE
	) |>
	unique()


# get closest match in DO
ordo_do_fz <- DO.utils::match_fz(ordo_check2$ordo_label, do_text$text, maxDist = 25) |>
	# add doid of matches
	dplyr::left_join(do_text, by = c("table_match" = "text")) |>
	dplyr::rename(ordo_label = x, match_to = type) |>
	# add labels of all & separate label/synonym columns
	dplyr::left_join(
		dplyr::filter(do_text, type == "label") |>
			dplyr::select(-type),
		by = "id"
	) |>
	dplyr::rename(do_label = text) |>
	dplyr::rename(do_syn = table_match) |>
	dplyr::mutate(
		do_syn = dplyr::if_else(match_to == "label", NA_character_, do_syn)
	) |>
	# add ORDO IDs (mapping col)
	dplyr::left_join(
		dplyr::select(ordo_check2, ordo_label, mapping),
		by = "ordo_label"
	) |>
	# set mapping_type, exists, and do_dep
	dplyr::mutate(
		mapping_type = paste0("fz(", dist, "): ", match_to),
		exists = dplyr::case_when(
			dist == 0 ~ "TRUE",
			!is.na(dist) ~ "maybe",
			TRUE ~ "FALSE"
		),
		do_dep = dplyr::if_else(exists != "FALSE", FALSE, NA)
	) |>
	dplyr::select(-dist, -match_to) |>
	dplyr::rename(doid = id) |>
	dplyr::relocate(
		mapping, exists, mapping_type, doid, do_label,
		.before = do_syn
	)


##### COMBINE INVENTORY APPROACHES #####
out1 <- out |>
	dplyr::mutate(exists = as.character(exists)) |>
	dplyr::bind_rows(ordo_do_fz) |>
	dplyr::left_join(
		dplyr::select(ordo_sssom, -dplyr::ends_with("label")),
		by = c("mapping" = "subject_id")
	) |>
	dplyr::rename(ordo_pred = predicate_id, ordo_mapping = object_id)


##### ADD TO CURATION SHEET #####
# googlesheets4::write_sheet(out, ss, "ordo_inventory-20241125")
