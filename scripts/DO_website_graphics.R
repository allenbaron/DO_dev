library(here)
library(tidyverse)
library(lubridate)
# library(scales)
library(gridExtra)
# library(hrbrthemes) # font-focused themes


# Specify files & locations -----------------------------------------------
plot_out_dir <- "graphics/website"
plot_files <- paste(
    stringr::str_remove_all(lubridate::today(), "-"),
    c("DO_cited_by_count.png", "DO_term_def_count.png",
      "DO_branch_count.png", "DO_xref_count.png"),
    sep = "-"
)

release_dir <- "data/DO_release"

#### UPDATE CSV FILES BEFORE RUNNING!!! #####
files <- c(
    # DO cited_by (i.e. publications citing DO articles)
    # To prepare data EXECUTE scripts/citedby_full_procedure.R
    citedby = here::here("data/citedby/DO_citedby.csv"),

    # DO release details
    # To prepare data EXECUTE scripts/DO_release_details.R
    release = here::here(release_dir, "DO_release_details.csv"),

    # DO term & definition counts
    # To prepare data:
    #   1. Setup python virtual environment by running
    #       scripts/install_reticulate_python.R
    #   2. Update term & definition counts with scripts/DO_term_def_counts.R
    #   MODIFICATIONS NEEDED in scripts/DO_term_def_counts-from_git.py:
    #       1. Modify to check for missing release info & access only those
    #       releases (NEED TO DO THIS)
    #       2. Modify to update release data in data/DO_release/DO_release_details.csv
    term_def_counts = here::here(release_dir, "DO_term_def_counts.csv"),

    # DO branch counts
    # To prepare data:
    #   - Manually copy and paste stats from build/reports/branch-count.tsv
    #   generated with each build (local git repo)
    #
    #   OR
    #
    #   - Manually copy & paste stats from Google Sheet - DO_github_release_log
    #   (https://docs.google.com/spreadsheets/d/1-ZSUH43MJloR2EsBqHpGeY6IfKG7Gt8KBcU5remnoGI/edit#gid=269344614)
    branch_counts = here::here(release_dir, "branch_counts.csv"),

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
    xref_counts = here::here(release_dir, "cross_references.csv")
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

do_stat_colors <- c("#b0e0e6", "#379da7", "#73BEC6") #c("powderblue", "lightseagreen", ..something in between)


# Load Data ---------------------------------------------------------------

cited_by <- readr::read_csv(files["citedby"]) %>%
    dplyr::mutate(Year = lubridate::year(pub_date)) %>%
    dplyr::count(Year, name = "Publications")

releases <- readr::read_csv(files["release"])
term_def_counts <- readr::read_csv(files["term_def_counts"]) %>%
    dplyr::rename(release = `...1`)
release_details <- dplyr::left_join(
    releases,
    term_def_counts,
    by = c("tag_name" = "release")
)

branch <- readr::read_csv(files["branch_counts"])

xref <- readr::read_csv(files["xref_counts"]) %>%
    dplyr::filter(!is.na(Curation)) %>%
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
releases_tidy <- release_details %>%
    # add year
    dplyr::mutate(date = lubridate::date(created_at)) %>%
    # drop bug fix releases that happen on same day (for plotting by date)
    dplyr::group_by(date) %>%
    dplyr::arrange(desc(created_at)) %>%
    dplyr::filter(!duplicated(date)) %>%
    dplyr::ungroup() %>%
    # drop extra columns
    dplyr::select(date, terms, defs) %>%
    dplyr::mutate(
        n_terms = terms - defs,
        n_defs = defs
    ) %>%
    dplyr::select(-terms, -defs) %>%
    tidyr::pivot_longer(
        cols = c(n_terms, n_defs),
        names_to = "variable",
        values_to = "value"
    ) %>%
    dplyr::mutate(
        variable = factor(
            variable,
            levels = c("n_terms", "n_defs")
        )
    )

## Create Area Plot - NEW version, 2021-08-11
g_terms <-ggplot(releases_tidy) +
    geom_area(
        aes(x = date, y = value, fill = variable), size = 1
    ) +
    scale_fill_manual(
        values = do_stat_colors,
        labels = c("Terms", "Terms Defined")
    ) +
    labs(title = "Trend of DO Terms",
         fill = "Total", x = "Release Date", y = "Count") +
    scale_y_continuous(
        breaks = seq(0, 12000, by = 2000)#,
    ) +
    scale_x_date(
        date_breaks = "1 year",
        date_labels = "%Y"#,
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
    xlab("Cross References") +
    coord_flip() +
    scale_y_continuous(
        breaks = seq(0, JABmisc::round_up(max(xref$Count), -3), by = 2000)
    )


# Visualize plots ---------------------------------------------------------
plots <- list(g_cited_by, g_terms, g_branch, g_xref)
grid.arrange(grobs = plots, nrow = 2, ncol = 2)


# SAVE PLOTS --------------------------------------------------------------
purrr::pmap(
    .l = list(
        .x = plot_files,
        .y = plots,
        # set size of plots so they don't look weird (may adjust individually
        #   in future
        .w = list(8, 8, 8, 8),
        .h = list(5.6, 5.6, 5.6, 5.6)
    ),
    function(.x, .y, .w, .h) {
        ggsave(
            filename = here::here(plot_out_dir, .x),
            plot = .y,
            width = .w,
            height = .h,
            units = "in",
            dpi = 600
        )
    }
)
