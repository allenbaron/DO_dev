# compare previous messages.po extraction with current doc
library(tidyverse)
library(googlesheets4)

# load & parse messages.po
sf <- "~/Documents/DO_website/do_trunk/translations/es/LC_MESSAGES/messages.po"
sraw <- readr::read_file(sf)
sparse <- stringr::str_match_all(
    sraw,
    stringr::regex(
        "\n(?<loc>#: .+?)\nmsgid (?<english>.+?)\nmsgstr (?<spanish>.+?)(?=\n\n#:)",
        dotall = TRUE
    )
)[[1]][, 2:4] |>
    tibble::as_tibble() |>
    dplyr::mutate(
        fuzzy = stringr::str_detect(loc, stringr::coll("#, fuzzy")),
        loc = stringr::str_remove(loc, "\n#, fuzzy")
    )

# Load translation google sheet (assume created today)
sheet_nm <- paste0("translation-", format(Sys.Date(), "%Y%m%d"))
sf_gs <- googlesheets4::read_sheet(
    ss = "1DTKwKr6AYSiLsTmnKavtKN6b52xJM0Kp3LTh1bxxknc",
    sheet = sheet_nm
)


# Compare messages.po with google sheet -----------------------------------

# rows in sheet that don't match messages.po
missing <- dplyr::anti_join(sf_gs, sparse, by = c("loc", "fuzzy", "english"))
dplyr::count(missing, sort = TRUE)

# remaining rows that do match messages.po
remain <- dplyr::semi_join(sf_gs, sparse, by = c("loc", "fuzzy", "english"))

# check those that state a check is needed in Notes column (likely fix after spanish replacement)
check <- remain |>
    # dplyr::filter(str_detect(Notes, "check in")) |>
    dplyr::mutate(
        # identify multi-line in messages.po which may not correspond directly
        # to text in templates... these will need to be checked manually
        multi = stringr::str_detect(english, '^""'),
        # convert template file paths to character vectors
        templates = stringr::str_remove(loc, "^#: ") |>
            stringr::str_remove_all(":[0-9]+") |>
            stringr::str_split("(\n)?(#:)? ")
    )

# identify places where spanish is new (i.e. messages.po = "") or differs
sp_diff_all <- sf_gs |>
    dplyr::anti_join(missing) |>
    dplyr::anti_join(
        sparse,
        by = c("loc", "fuzzy", "english", "spanish")
    ) |>
    dplyr::left_join(
        dplyr::select(sparse, loc, fuzzy, english, sp_current = spanish)
    )

# new only
sp_diff <- sp_diff_all |>
    dplyr::filter(sp_current == '""' | is.na(sp_current))


# Replace translation with update from Google Sheet -----------------------

#   google sheets translation is the most up-to-date and will replace any
#   different existing translation already in messages.po

# build replacement dictionary
match_replace_df <- sp_diff_all |>
    dplyr::rowwise() |>
    dplyr::mutate(
        match = paste0(
            "\n", loc,
            "\nmsgid ", english,
            "\nmsgstr ", sp_current
        ),
        replace = paste0(
            "\n", loc,
            "\nmsgid ", english,
            "\nmsgstr ", spanish
        )
    )
match_replace <- purrr::set_names(
    match_replace_df$replace,
    match_replace_df$match
)

# replace in raw read of messages.po
sraw_new <- stringr::str_replace_all(
    sraw,
    match_replace
)

readr::write_file(sraw_new, sf)


# For comparison after updating Spanish (ignore location differences) --------
#   ignore differences in file location (many numbers have changed), fuzzy
#   (removed from file) and differences in lines due to line breaks
# rows in sheet that don't match messages.po
sf_gs_adjust <- dplyr::mutate(
    sf_gs,
    english_orig = english,
    english = str_replace_all(english, c('"|\n' = "", " +" = " "))
)

sparse_adjust <- dplyr::mutate(
    sparse,
    english_orig = english,
    english = str_replace_all(english, c('"|\n' = "", " +" = " "))
)

missing <- dplyr::anti_join(sf_gs_adjust, sparse_adjust, by = "english")
dplyr::count(missing, Notes, sort = TRUE)
# review rows in z data.frame (excluded ones are expected not to match for reason in Notes column)
z <- dplyr::filter(missing, !stringr::str_detect(Notes, "NO|ignored|combined") | is.na(Notes))

sp_diff_all <- sf_gs_adjust |>
    dplyr::anti_join(missing) |>
    dplyr::anti_join(
        sparse_adjust,
        by = c("english", "spanish")
    ) |>
    dplyr::left_join(
        dplyr::select(sparse_adjust, english, english_orig, sp_current = spanish)
    ) |>
    dplyr::relocate(Notes, .after = dplyr::last_col())

# build replacement dictionary
match_replace_df <- sp_diff_all |>
    dplyr::rowwise() |>
    dplyr::mutate(
        match = paste0(
            "\nmsgid ", english_orig,
            "\nmsgstr ", sp_current
        ),
        replace = paste0(
            "\nmsgid ", english_orig,
            "\nmsgstr ", spanish
        )
    )
match_replace <- purrr::set_names(
    match_replace_df$replace,
    match_replace_df$match
)

# replace in raw read of messages.po
sraw_new <- stringr::str_replace_all(
    sraw,
    match_replace
)

readr::write_file(sraw_new, sf)


## didn't replace everything... check remainder manually

z1 <- sp_diff_all |>
    dplyr::mutate(
        sp_current = str_replace_all(sp_current, c('"|\n' = "", " +" = " ")),
        spanish = str_replace_all(spanish, c('"|\n' = "", " +" = " "))
    ) |>
    dplyr::filter(sp_current != spanish) |>
    dplyr::select(english, sp_current, spanish)

googlesheets4::write_sheet(z1, "https://docs.google.com/spreadsheets/d/1DTKwKr6AYSiLsTmnKavtKN6b52xJM0Kp3LTh1bxxknc/edit?gid=1805918238#gid=1805918238", "temp-fix")

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
