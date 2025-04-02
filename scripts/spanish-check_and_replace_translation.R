# compare previous messages.po extraction with current doc
library(tidyverse)
library(googlesheets4)
library(DO.utils)

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

# Load translation google sheet (assume created today)
es_gt <- googlesheets4::read_sheet(ss, sheet) |>
    dplyr::mutate(spanish = DO.utils::sandwich_text(.data$spanish, '"'))


# Compare messages.po with translation ------------------------------------

# rows in sheet that don't match messages.po
missing <- dplyr::anti_join(es_gt, es$parsed, by = c("loc", "fuzzy", "english"))
if (nrow(missing) > 0) {
    dplyr::count(missing, sort = TRUE)
    stop("Data mismatch!")
}

# identify places where spanish is new (i.e. messages.po = "") or differs
es_diff <- es_gt |>
    dplyr::anti_join(missing) |>
    dplyr::anti_join(
        es$parsed,
        by = c("loc", "fuzzy", "english", "spanish")
    ) |>
    dplyr::left_join(
        dplyr::select(es$parsed, "loc", "fuzzy", "english", es_current = "spanish")
    )


# Replace translation with update from Google Sheet -----------------------

#   google sheets translation is the most up-to-date and will replace any
#   different existing translation already in messages.po

# build replacement dictionary
es_replace <- es_diff  |>
    dplyr::mutate(
        match = stringr::str_escape(.data$match),
        replace = paste0(
            loc,
            "\nmsgid ", english,
            "\nmsgstr ", spanish
        ) |>
            stringr::str_escape()
    ) |>
    (function(.df) { purrr::set_names(.df$replace, .df$match) })()

# replace in raw read of messages.po
es_raw_new <- stringr::str_replace_all(
    es$raw,
    es_replace
)

readr::write_file(es_raw_new, es_path)


#################### NO GUARANTEE CODE BELOW STILL WORKS ####################

# For comparison after updating Spanish (ignore location differences) --------
#   ignore differences in file location (many numbers have changed), fuzzy
#   (removed from file) and differences in lines due to line breaks
# rows in sheet that don't match messages.po

# es_gt_adjust <- dplyr::mutate(
#     es_gt,
#     english_orig = english,
#     english = str_replace_all(english, c('"|\n' = "", " +" = " "))
# )
#
# sparse_adjust <- dplyr::mutate(
#     es$parsed,
#     english_orig = english,
#     english = str_replace_all(english, c('"|\n' = "", " +" = " "))
# )
#
# missing <- dplyr::anti_join(es_gt_adjust, sparse_adjust, by = "english")
# dplyr::count(missing, Notes, sort = TRUE)
# # review rows in z data.frame (excluded ones are expected not to match for reason in Notes column)
# z <- dplyr::filter(missing, !stringr::str_detect(Notes, "NO|ignored|combined") | is.na(Notes))
#
# sp_diff_all <- es_gt_adjust |>
#     dplyr::anti_join(missing) |>
#     dplyr::anti_join(
#         sparse_adjust,
#         by = c("english", "spanish")
#     ) |>
#     dplyr::left_join(
#         dplyr::select(sparse_adjust, english, english_orig, es_current = spanish)
#     ) |>
#     dplyr::relocate(Notes, .after = dplyr::last_col())
#
# # build replacement dictionary
# es_replace_df <- sp_diff_all |>
#     dplyr::rowwise() |>
#     dplyr::mutate(
#         match = paste0(
#             "\nmsgid ", english_orig,
#             "\nmsgstr ", es_current
#         ),
#         replace = paste0(
#             "\nmsgid ", english_orig,
#             "\nmsgstr ", spanish
#         )
#     )
# es_replace <- purrr::set_names(
#     es_replace_df$replace,
#     es_replace_df$match
# )
#
# # replace in raw read of messages.po
# es_raw_new <- stringr::str_replace_all(
#     es$raw,
#     es_replace
# )
#
# readr::write_file(es_raw_new, es_path)
#
#
# ## didn't replace everything... check remainder manually
#
# z1 <- sp_diff_all |>
#     dplyr::mutate(
#         es_current = str_replace_all(es_current, c('"|\n' = "", " +" = " ")),
#         spanish = str_replace_all(spanish, c('"|\n' = "", " +" = " "))
#     ) |>
#     dplyr::filter(es_current != spanish) |>
#     dplyr::select(english, es_current, spanish)
#
# googlesheets4::write_sheet(z1, "https://docs.google.com/spreadsheets/d/1DTKwKr6AYSiLsTmnKavtKN6b52xJM0Kp3LTh1bxxknc/edit?gid=1805918238#gid=1805918238", "temp-fix")

# For manual check... ignoring now that it's completed --------------------

# z <- check %>%
#     dplyr::rowwise() %>%
#     dplyr::mutate(
#         by_temp = list(purrr::set_names(rep(english, length(templates)), templates))
#     ) %>%
#     {.$by_temp} %>%
#     unlist()

# check_by_template <- tibble::tibble(
#     template = names(z),
#     english = z
#     ) |>
#     dplyr::arrange(template)

# readr::write_tsv(check_by_template, "DEL-check_placeholders.tsv")
