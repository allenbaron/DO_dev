library(here)
library(tidyverse)
library(lubridate)
# library(scales)
library(gridExtra)
# library(hrbrthemes) # font-focused themes


# Specify files & locations -----------------------------------------------
plot_out_dir <- "graphics/website"
plot_files <- c("DO_cited_by_count.png", "DO_term_def_count.png",
                "DO_branch_count.png", "DO_xref_count.png")

release_dir <- "data/DO_release"
#### UPDATE CSV FILES BEFORE RUNNING!!! #####

data_files <- c(
    # DO cited_by (i.e. publications citing DO articles)
    # To prepare data:
    #   1. Download -> Save:
    #       i. Lynn's MyNCBI collection (.txt) -> data/citations/myncbi/myNCBI_DO.txt
    #       ii. Scopus cited by list for each DO pub (.csv) -> data/citations/scopus/*
    #           - * = DO pub identifier (e.g. "2019_NAR_Schriml") + "-cited_by.csv"
    #   2. Match & count citations with scripts/match_count-scopus_myncbi.R
    here::here("data/citations/cited_by_count-yr.csv"),

    # DO term & definition counts (aka release details)
    # To prepare data (instructions will not currently work; MODIFICATIONS REQUIRED!!!):
    #   1. Update script to point to local HumanDiseaseOntology repo, if necessary
    #   2. Update term & definition counts with scripts/DO_term_def_counts-from_git.py
    #   MODIFICATIONS NEEDED in scripts/DO_term_def_counts-from_git.py:
    #       1. Modify to check for missing release info & access only those
    #       releases (NEED TO DO THIS)
    #       2. Modify to update release data in data/DO_release/DO_release_details.csv
    here::here(release_dir, "DO_release_details.csv"),

    # DO branch counts
    # To prepare data:
    #   - Manually copy and paste stats from build/reports/branch-count.tsv
    #   generated with each build (local git repo)
    #
    #   OR
    #
    #   - Manually copy & paste stats from Google Sheet - DO_github_release_log
    #   (https://docs.google.com/spreadsheets/d/1-ZSUH43MJloR2EsBqHpGeY6IfKG7Gt8KBcU5remnoGI/edit#gid=269344614)
    here::here(release_dir, "branch_counts.csv"),

    # DO xref counts
    # To prepare data:
    #   - Manually copy & paste stats from Google Sheet - DO_github_release_log
    #   (https://docs.google.com/spreadsheets/d/1-ZSUH43MJloR2EsBqHpGeY6IfKG7Gt8KBcU5remnoGI/edit#gid=269344614)
    #
    #   OR
    #
    #   1. Update local HumanDiseaseOntology repo to latest release
    #   2. Execute ROBOT query (in top-level directory):
    #       robot query --input src/ontology/doid.owl --query src/sparql/build/all-xref-report.rq xref.tsv
    #   3. Manually copy & paste stats from  xref.tsv
    here::here(release_dir, "cross_references.csv")
)


# Set theme for plots ----------------------------------------------

ggplot2::theme_set(theme_dark(base_size = 13))

# DO-related colors
#   mostly saving these for later (didn't work great on dark theme)
# do_colors <- c(
#   website_banner = "#45b4bf",
#   do_logo_blue_dark = "#379da7",
#   do_logo_blue_light = "#52aab3",
#   do_logo_orange_dark = "#e1703d",
#   do_logo_orange_light = "#ea9d7b",
#   stats_greenblue = "#21b0a7",
#   stats_lightblue = "#b0e0e6",
#   do_logo_official = "#329698"
# )

do_stat_colors <- c("#b0e0e6", "#1fb2aa", "#68C9C8") #c("powderblue", "lightseagreen", ..something in between)


# Load Data ---------------------------------------------------------------

cited_by <- readr::read_csv(data_files[1])

releases <- readr::read_csv(data_files[2])

branch <- readr::read_csv(data_files[3])

xref <- readr::read_csv(data_files[4]) %>%
    dplyr::mutate(
        Curation = factor(Curation, levels = c("Manual", "Mixed", "Automated"))
    )


# Generate Plots ----------------------------------------------------------

# Publications citing DO by year
g_cited_by <- ggplot(data = cited_by) +
    geom_col(aes(x = Year, y = Publications), width=0.6, fill=do_stat_colors[1]) +
    xlab("Year") +
    ylab("Count") +
    ggtitle("Publications Citing DO")


# Trend of DO Terms

## Custom release df tidy function
pivot_releases <- function(release_df) {
    release_df %>%
        tidyr::pivot_longer(
            cols = c(no_disease_terms, no_defined),
            names_to = "variable",
            values_to = "value"
        )
}

## Update/Tidy Data
releases_tidy <- releases %>%
    # add year
    dplyr::mutate(Year = lubridate::year(date)) %>%
    # drop (smaller?) releases that happen on same day (for plotting by date)
    dplyr::group_by(date) %>%
    dplyr::arrange(desc(n_terms), desc(n_defs)) %>%
    dplyr::filter(!duplicated(date)) %>%
    dplyr::ungroup() %>%
    # drop extra columns
    dplyr::select(
        date, Year, release_number = rel_num, release_version = rel_version,
        no_disease_terms = n_terms, no_defined = n_defs
    )


###### example release plots ######

# releases_long <- pivot_releases(releases_tidy)
#
# original
# rel_vs_incl <- c("16-Jun", "17-Jan", "17-Jun", "18-Mar", "18-Jul", "19-Feb",
#                  "19-May", "19-Oct", "19-Nov", "20-Apr", "20-Apr", "20-May",
#                  "20-Jun", "20-Jul", "20-Aug", "20-Sep")
#
#
# releases_long %>%
#   dplyr::filter(release_version %in% rel_vs_incl) %>%
#   ggplot() +
#   geom_col(
#     aes(x = release_version, y = value, fill = variable),
#     position="dodge", width=0.6
#   ) +
#   scale_fill_manual(
#     values = do_stat_colors,
#     labels = c("No. Disease Terms Defined ", "No. Disease Terms")) +
#   labs(fill = "Key") +
#   theme(axis.text.x = element_text(angle=90))

# original - UPDATED to even out release periods
# rel_vs_incl <- c("16-Jul", "17-Jan", "17-Jul", "18-Jan", "18-Jul", "19-Jan",
#                  "19-Jul", "20-Jan", "20-Jul", "21-Jan",
#                  "21-Jul")
#
# releases_long %>%
#   dplyr::filter(release_version %in% rel_vs_incl) %>%
#   ggplot() +
#   geom_col(
#     aes(x = release_version, y = value, fill = variable),
#     position="dodge", width=0.6
#   ) +
#   scale_fill_manual(
#     values = do_stat_colors,
#     labels = c("No. Disease Terms Defined ", "No. Disease Terms")) +
#   labs(fill = "Key") +
#   theme(axis.text.x = element_text(angle=90))
#
# original - ALL releases
# g_rel <- ggplot(releases_long)
#
# g_rel +
#   geom_col(
#     aes(x = release_number, y = value, fill = variable),
#     position="dodge", width=0.6
#   ) +
#   scale_fill_manual(
#     values = do_stat_colors,
#     labels = c("No. Disease Terms Defined ", "No. Disease Terms")) +
#   labs(fill ="Key") +
#   theme(axis.text.x = element_text(angle=90))
#
# thick line (looks less like plateau)
# g_rel +
#   geom_line(
#     aes(x = date, y = value, color = variable), size = 1
#   ) +
#   scale_color_manual(
#     values = do_stat_colors
#       # c(
#       # do_colors[["do_logo_blue_dark"]],
#       # do_colors[["do_logo_orange_dark"]]
#       # )
#     )

## Create Area Plot - NEW version, 2021-08-11
g_terms <- releases_tidy %>%
    dplyr::mutate(
        n_terms = no_disease_terms,
        no_disease_terms = n_terms - no_defined
    ) %>%
    pivot_releases() %>%
    dplyr::mutate(
        variable = factor(
            variable,
            levels = c("no_disease_terms", "no_defined")
        )
    ) %>%
    ggplot() +
    geom_area(
        aes(x = date, y = value, fill = variable), size = 1
    ) +
    scale_fill_manual(
        values = do_stat_colors,
        #   c(
        #   do_colors[["do_logo_orange_light"]],
        #   do_colors[["do_logo_blue_light"]]
        # ),
        labels = c("Terms", "Terms Defined")
    ) +
    labs(title = "Trend of DO Terms",
         fill = "Total", x = "Release Date", y = "Count") +
    scale_y_continuous(
        # adds comma to separate thousands place (prefer without; requires
        #   scales pkg
        #labels = comma,
        breaks = seq(0, 12000, by = 2000)#,
        # removes empty space between area and axes, looks better for this plot
        #   but inconsistency with other plots (which look worse with expand) is
        #   a deal-breaker
        # expand = expansion(mult = c(0.005, 0.03))
    ) +
    scale_x_date(
        date_breaks = "1 year",
        date_labels = "%Y"#,
        # expand = expansion(mult = c(0.005, 0.03))
    )


# DO Branch Counts

## Clean Up Branch Names
branch_order <- c(syndrome = "Syndrome",
                  physicalDisorder = "Physical Disorder",
                  genetic = "Genetic Disease",
                  metabolism = "Metabolism",
                  mentalHealth = "Mental Health",
                  cellularProliferation = "Cellular Proliferation",
                  anatomicalEntity = "Anatomical Location",
                  infectiousAgent = "Infectious Disease")

branch_tidy <- branch %>%
    dplyr::mutate(
        DO_branches = factor(
            recode(DO_branches, !!!branch_order),
            levels = branch_order
        )
    )

## Create plot
g_branch <- ggplot(data = branch_tidy) +
    geom_col(
        aes(x = DO_branches, y = Count),
        width = 0.6, fill = do_stat_colors[1] #do_colors["website_banner"]
    ) +
    ggtitle("DO Branch Counts") +
    coord_flip() +
    theme(axis.title.y = element_blank()) +
    scale_y_continuous(
        breaks = seq(0, JABmisc::round_up(max(branch_tidy$Count), -3), by = 1000)
    )


# DO Cross References
g_xref <- ggplot(data = xref) +
    geom_col(aes(x = Cross_References, y = Count, fill = Curation), width = 0.6) +
    scale_fill_manual(
        values = do_stat_colors[c(1, 3, 2)]
        #c(do_colors["do_logo_orange_dark"], "#98A99D",
        # do_colors["do_logo_blue_dark"])
    ) +
    ggtitle("DO Cross-References") +
    coord_flip() +
    scale_y_continuous(
        breaks = seq(0, JABmisc::round_up(max(xref$Count), -3), by = 2000)
    )

# Using DO blue, orange, and something in between (better contrast but
#   doesn't fit well with dark theme)
# g_xref <- ggplot(data=xref) +
#   geom_col(aes(x = Cross_References, y = Count, fill = Curation), width = 0.6) +
#   scale_fill_manual(
#     values = c(do_colors["do_logo_orange_light"], "#919D91",
#     do_colors["do_logo_blue_dark"])
#   ) +
#   ggtitle("DO Cross-References") +
#   coord_flip() +
#   do_stat_theme()


# Visualize plots ---------------------------------------------------------
plots <- list(g_cited_by, g_terms, g_branch, g_xref)
grid.arrange(grobs = plots, nrow = 2, ncol = 2)


# SAVE PLOTS --------------------------------------------------------------
purrr::walk2(
    .x = plot_files,
    .y = plots,
    ~ ggsave(
        filename = here::here(plot_out_dir, .x),
        plot = .y,
        # size of previous files (4x3in at 300 dpi); makes them squish weird
        # width = 4,
        # height = 3,
        # units = "in",
        # dpi = 600
    )
)
