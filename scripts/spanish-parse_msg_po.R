# parse disease-ontology.org Spanish messages.po file to google sheet for access
# to translation
library(tidyverse)
library(googlesheets4)
library(DO.utils)
library(googledrive)

# Spanish messages.po file
es_path <- "~/Documents/DO_website/do_trunk/translations/es/LC_MESSAGES/messages.po"

# Google sheet for translation
ss <- "https://docs.google.com/spreadsheets/d/1c5ih1CX11EL47B0dGJB-Y9jnlMVw2agQ3kOtnZmJCP8/"
sheet <- "Sheet1"


# Custom function ---------------------------------------------------------

# Read & parse function for messages.po files
read_parse_msgpo <- function(path) {
    .raw <- readr::read_file(path)
    .parse <- stringr::str_match_all(
        .raw,
        stringr::regex(
            "\n(?<loc>#: .+?)\nmsgid (?<english>.+?)\nmsgstr (?<spanish>.+?)(?=\n\n#:)",
            dotall = TRUE
        )
    )[[1]] |>
        tibble::as_tibble(.name_repair = ~ c("match", "loc", "english", "spanish")) |>
        dplyr::mutate(
            fuzzy = stringr::str_detect(loc, stringr::coll("#, fuzzy")),
            loc = stringr::str_remove(loc, "\n#, fuzzy")
        )
    list(raw = .raw, parsed = .parse)
}



# Processing --------------------------------------------------------------

es <- read_parse_msgpo(es_path)

es_parsed_prior <- stringr::str_match_all(
    es$raw,
    stringr::regex(
        "\n#~ msgid (?<english>.+?)\n#~ msgstr (?<spanish>.+?)(?=\n\n#~)",
        dotall = TRUE
    )
)[[1]] |>
    tibble::as_tibble(.name_repair = ~ c("match_rm", "english", "spanish")) |>
    dplyr::mutate(
        dplyr::across(
            .cols = c("english", "spanish"),
            .fns = ~ stringr::str_remove_all(.x, "#~ ")
        ),
        english_match = stringr::str_replace_all(
            .data$english,
            "\\\\n|\n|\"",
            " "
        ) |>
            stringr::str_squish()
    ) |>
    dplyr::filter(.data$spanish != '""') |>
    dplyr::select("english_match", spanish_fix = "spanish", "match_rm")

# restore Spanish that only changed due to whitespace
es_fix <- es$parsed |>
    dplyr::mutate(
        english_match = stringr::str_replace_all(
            .data$english,
            "\\\\n|\n|\"",
            " "
        ) |>
            stringr::str_squish()
    ) |>
    dplyr::right_join(es_parsed_prior, by = "english_match") |>
    dplyr::filter(!is.na(.data$match))

if (nrow(es_fix) > 0) {
    message("Restoring Spanish differing only by whitespace")
    es_replace <- es_fix |>
        dplyr::mutate(
            match_rm = paste0("\n", .data$match_rm),
            dplyr::across(dplyr::starts_with("match"), stringr::str_escape),
            replace = paste0(
                "\n", .data$loc,
                "\nmsgid ", .data$english,
                "\nmsgstr ", .data$spanish_fix
            ) |>
                stringr::str_escape()
        ) |>
        (function(.df) {
            purrr::set_names(
                c(.df$replace, rep("", nrow(.df))),
                c(.df$match, .df$match_rm)
            )
        })()

    # replace in raw read of messages.po
    es_raw_new <- stringr::str_replace_all(
        es$raw,
        es_replace
    )

    readr::write_file(es_raw_new, es_path)

    es <- read_parse_msgpo(es_path)
}


if (any(es$parsed$fuzzy)) {
    stop("Fuzzy matches exist!!!")
} else {
    es_gt <- es$parsed |>
        dplyr::filter(.data$spanish == '""') |>
        dplyr::anti_join(es_fix, by = "match") |>
        dplyr::mutate(
            spanish = stringr::str_replace_all(english, "\\\\n|\n|\"", " ") |>
                stringr::str_squish() |>
                DO.utils::sandwich_text('"') |>
                (\(x) paste0("=GOOGLETRANSLATE(", x, ", \"en\", \"es\")"))() |>
                googlesheets4::gs4_formula()
        )

    gs <- googlesheets4::gs4_get(ss)
    message(
        paste0(
            "Submitting ", nrow(es_gt), " units to ", gs$name, " for translation"
        )
    )

    googlesheets4::write_sheet(es_gt, ss = ss, sheet = sheet)
}
