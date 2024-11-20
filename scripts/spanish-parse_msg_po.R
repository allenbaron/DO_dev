# parse disease-ontology.org Spanish messages.po file to google sheet for access
# to translation
library(tidyverse)
library(googlesheets4)

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

if (!interactive()) {
    sheet_nm <- paste0("translation-", format(Sys.Date(), "%Y%m%d"))
    googlesheets4::write_sheet(
        sparse,
        ss = "1DTKwKr6AYSiLsTmnKavtKN6b52xJM0Kp3LTh1bxxknc",
        sheet = sheet_nm
    )
}
