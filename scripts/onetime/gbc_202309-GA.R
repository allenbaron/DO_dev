# Summarize Google Analytics data for Global Biodata Coalition submission,
# 2023-09-27

library(here)
library(tidyverse)
library(DO.utils)
library(lubridate)

gd_local <- here::here("~/Google Drive/My Drive")
gd_anal_dir <- file.path(
    gd_local,
    "Disease_Ontology/DO_Impact/Google_analytics/universal-end_20230630"
)
daily_count_files <- list.files(
    file.path(gd_anal_dir, "daily_counts"),
    pattern = ".*\\.csv",
    full.names = TRUE
)

daily_counts <- purrr::map(
    daily_count_files,
    ~ DO.utils::read_ga(.x)
) %>%
    dplyr::bind_rows() %>%
    DO.utils::collapse_col(.cols = bounce_rate_pct:users, na.rm = TRUE) %>%
    dplyr::mutate(
        dplyr::across(dplyr::where(is.character), readr::parse_guess)
    ) %>%
    dplyr::select(
        day_index, users, new_users, pageviews, pages_per_session, sessions,
        dplyr::everything()
    )


# monthly average for 2019-2021: sessions, unique IPs, pageviews
monthly_counts <- daily_counts %>%
    dplyr::mutate(
        month = lubridate::month(day_index),
        yr = lubridate::year(day_index)
    ) %>%
    dplyr::summarize(
        dplyr::across(users:sessions, ~ sum(.x, na.rm = TRUE)),
        dplyr::across(
            bounce_rate_pct:avg_session_duration,
            ~ mean(.x, na.rm = TRUE)
        ),
        .by = c(yr, month)
    )

gbc_mon_avg <- monthly_counts %>%
    dplyr::filter(dplyr::between(yr, 2019, 2021)) %>%
    dplyr::summarize(
        dplyr::across(users:avg_session_duration, mean),
        .by = yr
    )


gbc_users_per_month <- read_ga(
    file.path(gd_anal_dir, "GBC/UA_Users-month_20190101-20211231.csv")
) %>%
    dplyr::mutate(
        month_index = as.integer(month_index) + 1,
        month = month_index %% 12,
        month = as.integer(dplyr::if_else(month == 0, 12, month)),
        year = as.integer(2019 + (month_index - 1) %/% 12)
    )

gbc_user_mon <- gbc_users_per_month %>%
    dplyr::summarize(users = mean(users), .by = year)
