library(DO.utils) # >= 3.2.1
library(googledrive)
library(googlesheets4)
#library(rorcid) # archived... only works for my record with the API I have... need something else
#   refer to https://info.orcid.org/documentation/api-tutorials/api-tutorial-read-data-on-a-record/
library(tableHTML)
library(tidyverse)

gs_contrib <- "https://docs.google.com/spreadsheets/d/1kD7rgOWO2uVUwKYoKFSLBEpv1WZFf-GDhEusAq_H5sM/edit?gid=261400100#gid=261400100"

contrib_path <- "~/Documents/DO_website/do_trunk/templates/disease_ontology/community/contributors.html"
indiv_nm <- "individualContributors"
resource_nm <- "resourceContributors"


# Read existing HTML & tables ---------------------------------------------

html <- readr::read_file(contrib_path)
indiv_table <- DO.utils:::get_html_table(html, indiv_nm)
resource_table <- DO.utils:::get_html_table(html, resource_nm)

indent <- DO.utils:::get_html_indent(indiv_table)

# custom indent function
add_table_indent <- function(html, indent) {
    increment_key <- c(
        "[ \t]*(<[ /]*table)" = 0,
        "[ \t]*(<[ /]*t(head|body|foot))" = 1,
        "[ \t]*(<[ /]*tr)" = 2,
        "[ \t]*(< *t[hd][^e])" = 3
    )
    indent_min <- paste0(rep(indent$type, indent$min), collapse = "")
    indent_increment <- paste0(rep(indent$type, indent$increment), collapse = "")
    indent_key <- purrr::map_chr(
        increment_key,
        ~ paste0(
            indent_min,
            paste0(rep(indent_increment, .x), collapse = ""),
            "\\1"
        )
    )

    stringr::str_replace_all(html, indent_key)
}

# custom function to add tfoot
add_tfoot <- function(html) {
    tfoot <- stringr::str_extract(
        html,
        stringr::regex("[ \t]*<thead>.*</thead>", dotall = TRUE)
    ) |>
        stringr::str_replace_all("thead", "tfoot")
    stringr::str_replace(
        html,
        "(</table>)",
        paste0(tfoot, "\n\\1")
    )
}

brand_fa <- c(
    "orcid" = "fa-orcid",
    "github" = "fa-github",
    "linkedin" = "fa-linkedin",
    "bsky" = "fa-bluesky",
    "mastodon" = "fa-mastodon",
    "x" = "fa-x-twitter",
    "x.com" = "fa-x-twitter",
    "twitter" = "fa-x-twitter",
    "youtube" = "fa-youtube",
    "instagram" = "fa-instagram",
    "facebook" = "fa-facebook"
)

# custom function to add font awesome brand logos
as_fa_icon <- function(brand, size = "fa-lg", report_unknown = 3) {
    brand_tidy <- stringr::str_trim(stringr::str_to_lower(brand))

    icon <- paste0(
        '<i class="fa-brands ',
        dplyr::recode(brand_tidy, !!!brand_fa),
        ' ',
        size,
        '"></i>'
    )

    # optionally warn about unknown brands; always replace with NA
    missing <- is.na(brand_tidy)
    unknown <- !stringr::str_detect(
        brand_tidy,
        paste0("^", names(brand_fa), "$", collapse = "|")
    )
    icon[unknown | missing] <- NA_character_

    if (any(unknown)) {
        if (report_unknown < 1) return(icon)

        # if all unknown are URLs report them as such (without listing them)
        url <- stringr::str_detect(brand, "^https?://")
        url_n <- sum(url, na.rm = TRUE)
        if (identical(unknown, url)) {
            warning(
                paste0(
                    url_n,
                    " URL(s) were ignored",
                )
            )
        } else {
            unknown_brand <- unique(brand[unknown & !url])
            unknown_n <- length(unknown_brand)
            if (unknown_n > report_unknown) {
                attr(icon, "unknown") <- unknown_brand
                unknown_list <- paste0(
                    c(
                        unknown_brand[1:report_unknown],
                        "...\n  --> Full brand list in 'unknown' attribute"
                    ),
                    collapse = ", "
                )
            } else {
                unknown_list <- paste0(unknown_brand, collapse = ", ")
            }

            unknown_msg <- NULL
            if (url_n > 0) unknown_msg <- paste0(url_n, " URL(s) and ")

            unknown_msg <- paste0(
                unknown_msg,
                unknown_n,
                " unrecognized brand(s) were ignored.\n  Brands: ",
                unknown_list
            )
            warning(unknown_msg)
        }
    }

    icon
}

# Generate Individual Contributors Table ----------------------------------

do_indiv <- googlesheets4::read_sheet(
    gs_contrib,
    sheet = "DO-individuals",
    col_types = "c"
)

brand_regex <- brand_fa[!names(brand_fa) == "x"] |>
    names() |>
    stringr::str_escape() |>
    paste0(collapse = "|")

do_indiv_tbl <- do_indiv |>
    DO.utils::lengthen_col(cols = "links", delim = "\n") |>
    dplyr::mutate(
        # need to add call to ORCID API to try to get personal info
        name = dplyr::if_else(
            !is.na(name),
            .data$name,
            stringr::str_match(.data$links, "github.com/([^/]+)")[, 2]
        ),
        # identify link_type (special handling for URLs)
        link_type = stringr::str_extract(
            .data$links,
            stringr::regex(brand_regex, ignore_case = TRUE)
        ),
        link_type = dplyr::if_else(
            is.na(.data$link_type) & stringr::str_detect(
                .data$links,
                stringr::regex("^https?://", ignore_case = TRUE),
            ),
            "url",
            .data$link_type
        ),
        link_type = factor(
            .data$link_type,
            levels = c(names(brand_fa), "url")
        ),
        # drop private or un-hyperlinkable links (e.g. emails)
        links = dplyr::if_else(
            !is.na(.data$link_type),
            .data$links,
            NA_character_
        )
    ) |>
    # order links by preference (set by brand_fa order)
    dplyr::arrange(.data$name, .data$link_type) |>
    # generate icon links
    dplyr::mutate(
        icon = as_fa_icon(.data$link_type, size = "fa-xl"),
        # use full URLs when needed
        links = dplyr::case_when(
            .data$link_type == "url" ~ DO.utils::format_hyperlink(
                .data$links,
                as = "html",
                target = "_blank"
            ),
            !is.na(.data$icon) ~ DO.utils::format_hyperlink(
                .data$links,
                as = "html",
                text = .data$icon,
                target = "_blank"
            ),
            .default = NA_character_
        )
    ) |>
    # drop rows with missing name or noted as "exclude"
    dplyr::filter(
        !is.na(.data$name),
        is.na(.data$notes) | !stringr::str_detect(.data$notes, "exclude")
    ) |>
    dplyr::select("name", "links", "affiliation") |>
    DO.utils::collapse_col(.cols = "links", delim = ", ") |>
    dplyr::mutate(
        dplyr::across(
            dplyr::everything(),
            ~ stringr::str_replace_all(.x, "\n", "<br>")
        )
    ) |>
    dplyr::rename_with(.fn = stringr::str_to_title)

# convert to HTML
indiv_html <- tableHTML::tableHTML(
    do_indiv_tbl,
    rownames = FALSE,
    class = '"display"',
    # wrapping headers for babel translation
    headers = DO.utils::sandwich_text(
        names(do_indiv_tbl),
        c("{{ _(\"", "\") }}")
    ),
    escape = FALSE,
    replace_NA = ""
) |>
    stringr::str_replace_all(
        c(
            # strip out undesired attributes & starting \n
            "(style|id)=\"[^\"]+\"[; ]*|border=[0-9.]+" = "",
            "< *" = "<",
            " *>" = ">",
            "^\n" = "",
            # add table ID & styling
            "<table( ?)" = paste0(
                "<table id=\"", indiv_nm, "\" style=\"width:100%;\"\\1"
            )
        )
    ) |>
    add_tfoot() |>
    add_table_indent(indent)


# Generate Resource Contributors Table ------------------------------------

do_resource <- googlesheets4::read_sheet(
    gs_contrib,
    sheet = "DO-resources",
    col_types = "c"
)

do_resource_tbl <- do_resource |>
    dplyr::mutate(
        link_content = dplyr::case_when(
            !is.na(.data$logo_path) & !is.na(.data$abbreviation) ~
                DO.utils::as_html_img(src = .data$logo_path, alt = .data$abbreviation, height = 50),
            !is.na(.data$logo_path) ~ DO.utils::as_html_img(src = .data$logo_path, alt = .data$name, height = 50),
            !is.na(.data$abbreviation) ~ .data$abbreviation,
            TRUE ~ .data$link
        ),
        link = dplyr::if_else(
            !is.na(.data$link),
            DO.utils::build_html_element(
                tag = "a",
                content = .data$link_content,
                href = .data$link,
                target = "_blank"
            ),
            NA_character_
        )
    ) |>
    dplyr::filter(
        !is.na(.data$name),
        is.na(.data$notes) | !stringr::str_detect(.data$notes, "exclude")
    ) |>
    dplyr::select("name", "link", "Contributing Since" = "since") |>
    dplyr::rename_with(.fn = stringr::str_to_title)


# convert to HTML
resource_html <- tableHTML::tableHTML(
    do_resource_tbl,
    rownames = FALSE,
    class = '"display"',
    # wrapping headers for babel translation
    headers = DO.utils::sandwich_text(
        names(do_resource_tbl),
        c("{{ _(\"", "\") }}")
    ),
    escape = FALSE,
    replace_NA = ""
) |>
    stringr::str_replace_all(
        c(
            # strip out undesired attributes & starting \n
            "(style|id)=\"[^\"]+\"[; ]*|border=[0-9.]+" = "",
            "< *" = "<",
            " *>" = ">",
            "^\n" = "",
            # add table ID & styling
            "<table( ?)" = paste0(
                "<table id=\"", resource_nm, "\" style=\"width:100%;\"\\1"
            )
        )
    ) |>
    add_tfoot() |>
    add_table_indent(indent)


# Replace in HTML ---------------------------------------------------------

out <- html |>
    stringr::str_replace(
        stringr::str_escape(indiv_table),
        indiv_html
    ) |>
    stringr::str_replace(
        stringr::str_escape(resource_table),
        resource_html
    )

readr::write_file(out, contrib_path)
