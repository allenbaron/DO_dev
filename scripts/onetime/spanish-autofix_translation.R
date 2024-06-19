# ONE TIME SCRIPT!!!
# remove markings from text in templates where translation is not desired;
# the text is identified in the 'New Additions 5/23' sheet of the Spanish
# translation key as "NO" in the Notes column.
library(tidyverse)
library(googlesheets4)

# Get curated data from Google Sheet --------------------------------------

key2 <- googlesheets4::read_sheet(
    ss = "1DTKwKr6AYSiLsTmnKavtKN6b52xJM0Kp3LTh1bxxknc",
    sheet = "New Additions 5/23"
)

rm_mark <- key2 %>%
    dplyr::filter(Notes == "NO") %>%
    dplyr::mutate(
        # identify groups (names, citations, etc.)
        type = dplyr::case_when(
            stringr::str_detect(english, '^""|\\."$') ~ "citation",
            stringr::str_detect(english, '^"&.*;"$') ~ "html only",
            TRUE ~ "name"
        ),
        # identify multi-line in messages.po which may not correspond directly
        # to text in templates... these will need to be checked manually
        multi = stringr::str_detect(english, '^""'),
        # convert template file paths to character vectors
        templates = stringr::str_remove(loc, "^#: ") %>%
            stringr::str_remove_all(":[0-9]+") %>%
            stringr::str_split("(\n)?(#:)? ")
    )

check_manual <- key2 %>%
    dplyr::filter(stringr::str_detect(Notes, "--> [A-Za-z]+ in template"))

key_keep <- dplyr::anti_join(key2, rm_mark) %>%
    dplyr::anti_join(check_manual)

# ensure record retention (by count)
sum(purrr::map_dbl(list(rm_mark, check_manual, key_keep), nrow)) == nrow(key2)


# Auto-remove undesirable markings ----------------------------------------

drop_markings <- function(template, english) {
    txt <- readr::read_file(template)
    .pattern <- paste0('\\{\\{ _\\(', english, '\\) \\}\\}')
    .replace <- stringr::str_remove_all(english, '^"|"$')
    out <- stringr::str_replace_all(txt, .pattern, .replace)
    readr::write_file(out, template)
}

do_web_repo_path <- "~/Documents/DO_website/do_trunk"

# remove all names first (simplest case)
rm_done <- rm_mark %>%
    dplyr::filter(type %in% c("html only", "name")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
        done = list(
            purrr::map(
                .data$templates,
                function(.f) {
                    drop_markings(
                        file.path(do_web_repo_path, .f),
                        .data$english
                    )
                    .f
                }
            )
        )
    )

# remove one-line citations
rm_done2 <- rm_mark %>%
    dplyr::anti_join(rm_done) %>%
    dplyr::filter(!multi)  %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
        done = list(
            purrr::map(
                .data$templates,
                function(.f) {
                    drop_markings(
                        file.path(do_web_repo_path, .f),
                        .data$english
                    )
                    .f
                }
            )
        )
    )

rm_done <- dplyr::bind_rows(rm_done, rm_done2)


# Manually remove undesirable translation placeholders --------------------

# remove multi-line citations (had to be done manually after identifying here)
rm_done2 <- rm_mark %>%
    dplyr::anti_join(rm_done)

rm_done <- dplyr::bind_rows(rm_done, rm_done2)

# list other manual checks needed in csv (easier to review)




# EXTRA -------------------------------------------------------------------

sf <- "~/Documents/DO_website/do_trunk/translations/es/LC_MESSAGES/messages.po"
sraw <- readr::read_file(sf)
sparse <- stringr::str_match_all(
    sraw,
    stringr::regex(
        "\n(?<loc>#: .+?)\nmsgid (?<english>.+?)\nmsgstr (?<spanish>.+?)(?=\n\n#:)",
        dotall = TRUE
    )
)[[1]][, 2:4] %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
        fuzzy = stringr::str_detect(loc, stringr::coll("#, fuzzy")),
        loc = stringr::str_remove(loc, "\n#, fuzzy")
    )
