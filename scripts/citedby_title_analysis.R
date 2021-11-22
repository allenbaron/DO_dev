# Find article highlights for Buzz Newsletter
# J. Allen Baron
# 2021-11-12

# REQUIRES: Prior execution of scripts/citedby_full_procedure.R to merge all
#   data together

library(here)
library(DO.utils)
library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud2)

citedby <- readr::read_csv(
    here::here("data/citedby/DO_citedby-20211112.csv")
)

# for tf-idf with tidytext
title_words <- citedby %>%
    dplyr::select(title) %>%
    tidytext::unnest_tokens(word, title, drop = FALSE) %>%
    dplyr::count(title, word, sort = TRUE) %>%
    dplyr::add_count(title, wt = n, name = "total")

title_tf_idf <- title_words %>%
    # drop stop words
    dplyr::anti_join(tidytext::stop_words, by = "word") %>%
    tidytext::bind_tf_idf(word, title, n) %>%
    dplyr::arrange(dplyr::desc(tf_idf))


# for wordcloud with wordcloud2
# all words, no stemming
words_only <- title_words %>%
    dplyr::select(word) %>%
    dplyr::anti_join(tidytext::stop_words, by = "word") %>%
    dplyr::count(word, sort = TRUE)

# stemming - 2 methods
shortest_word <- function(x) {
    str_len <- stringr::str_length(x)
    x[str_len == min(str_len)][1]
}

add_prefix <- function(df, prefix) {
    dplyr::rename_with(
        df,
        .fn = ~ paste(prefix, .x, sep = "_"),
        .cols = !matches(paste0("^(", prefix, "|rank)"))
    )
}

stems <- words_only %>%
    dplyr::mutate(sb_stem = SnowballC::wordStem(word)) %>%
    dplyr::mutate(hun_stem = hunspell::hunspell_stem(word)) %>%
    tidyr::unnest(hun_stem) %>%
    dplyr::add_count(sb_stem, wt = n, name = "sb_n") %>%
    dplyr::add_count(hun_stem, wt = n, name = "hun_n")

hun_prefer <- stems %>%
    dplyr::group_by(hun_stem) %>%
    dplyr::summarize(
        word = shortest_word(word),
        n = unique(hun_n)
    ) %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::mutate(rank = dplyr::row_number())

sb_prefer <- stems %>%
    dplyr::group_by(sb_stem) %>%
    dplyr::summarize(
        word = shortest_word(word),
        n = unique(sb_n)
    ) %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::mutate(rank = dplyr::row_number())

stem_comb <- dplyr::full_join(
    add_prefix(hun_prefer, "hun"),
    add_prefix(sb_prefer, "sb"),
    by = "rank"
)
# %>%
#     tidyr::pivot_longer(
#         cols = matches("^(hun|sb)"),
#         names_to = c("type", ".value"),
#         names_sep = "_"
#     )

# summarized tf_idf
tf_idf_comb <- title_tf_idf %>%
    count(word, wt = tf_idf, sort = TRUE)

# clouds
wordcloud2::wordcloud2(words_only) # no stem

hun_prefer %>%
    dplyr::select(-hun_stem) %>%
    wordcloud2::wordcloud2()

sb_prefer %>%
    dplyr::select(-sb_stem) %>%
    wordcloud2::wordcloud2()

wordcloud2::wordcloud2(tf_idf_comb)
