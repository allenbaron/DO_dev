anon_inherit <- dm_owl$query(
    "SELECT ?id ?label ?eq ?ancestor ?axiom
    WHERE {
        ?class a owl:Class ;
            oboInOwl:id ?id ;
            rdfs:label ?label .
        FILTER NOT EXISTS { ?class owl:deprecated ?any . }
        FILTER(CONTAINS(str(?class), 'DOID'))

        OPTIONAL {
            ?class rdfs:subClassOf* ?ancestor .
            ?ancestor rdfs:subClassOf|owl:equivalentClass* ?axiom .
        }
        OPTIONAL { ?class owl:equivalentClass ?eq . }
    }") %>%
    tidy_sparql()

anon_inherit1 <- anon_inherit %>%
    mutate(
        eq = !is.na(eq),
        axiom = !is.na(axiom) & str_detect(axiom, "^N"),
        ancestor = !is.na(ancestor) & str_detect(ancestor, "^N")
    ) %>%
    unique() %>%
    group_by(id, label) %>%
    summarize(
        has_axiom = dplyr::if_else(any(eq), "eq", NA_character_),
        has_axiom = dplyr::if_else(any(axiom), paste(na.omit(has_axiom), "axiom"), has_axiom),
        has_axiom = dplyr::if_else(any(ancestor), paste(na.omit(has_axiom), "ancestor"), has_axiom),
        has_axiom = str_squish(has_axiom)
    ) %>%
    ungroup()

no_anon <- dplyr::filter(anon_inherit1, is.na(has_axiom))

ms_doid_only <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1qZGENJRrPvXt0zC2RTBgccwJZjCerSvpt41C19IS2wA/edit#gid=404979943",
    "terms only DOID axioms",
    range = "A:A",
    col_names = "id",
    col_types = "c"
) %>%
    dplyr::mutate(id = stringr::str_replace(id, "_", ":"))

ms_no_axiom <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1qZGENJRrPvXt0zC2RTBgccwJZjCerSvpt41C19IS2wA/edit#gid=404979943",
    "terms no axioms",
    range = "A:A",
    col_names = "id",
    col_types = "c"
) %>%
    dplyr::mutate(id = stringr::str_replace(id, "_", ":"))

dplyr::anti_join(ms_no_axiom, no_anon, by = "id") %>%
    dplyr::left_join(do_parent, by = "id")

dplyr::anti_join(ms_doid_only, no_anon, by = "id") %>%
    dplyr::left_join(do_parent, by = "id")
