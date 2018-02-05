#' ---
#' output:
#'   html_document:
#'     toc: false
#'     fig_width: 8
#'     fig_height: 8
#' author: ""
#' date: "`r format(Sys.Date())`"
#' params:
#'   filepath_tweets: NULL
#'   names: NULL
#'   name_main: NULL
#'   names_main: NULL
#'   augmented: TRUE
#'   augmented_col: "name"
#'   download: FALSE
#'   download_method: c("timline", "search")
#'   screen_names: NULL
#'   tweets_min_download: 1000
#'   num_main_max: 6
#'   color_main: NULL
#'   colors_main: NULL
#'   tweet_cnt_min: 1000
#'   trim_time: FALSE
#'   dd_cnt_min: 7
#'   yyyy_cnt_min: 1
#'   mm_cnt_min: 12
#'   wday_cnt_min: 7
#'   hh_cnt_min: 2
#'   kinds_features: c("hashtag", "link")
#'   kinds_types: c("quote", "reply", "rt")
#'   report_title: "Analysis of Tweets"
#' title: "`r params$report_title`"
#' ---
#'
#+ global_setup
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

# knitr::opts_chunk$set(fig.width = 10, fig.height = 7)
# #' title: "`r params$report_title`"
# #' date: "`r format(Sys.Date(), "%Y-%m-%d")`"
# #'     css: www/markdown7.css

#+ global_setup_2
library("dplyr")
library("stringr")
library("ggplot2")
library("tidyr")

# Use package::function format for these.
# library("lubridate")
# library("scales")
# library("tidytext")
# library("rtweet")

# Need to call directly so that fonts are loaded.
library("temisc")

filepaths_function <- list.files(pattern = "function", recursive = TRUE)
# filepaths_function <- file.path("functions-analyze.R")
filepaths_function <- normalizePath(filepaths_function, winslash = "/")
sapply(filepaths_function, source, .GlobalEnv)

# # ~~TODO: Embed CSS in report.~~
# ```{css setup_css_direct}
# h1 {
#   font-size: 10px;
# }
# ```
#'
#+ validate_params
params_proc <- validate_params(params)

#'
#+ process_params

params_proc <- process_params(params_proc)

#'
#+ names_grid
names_distinct <- params_proc$names_main
names_grid <-
  bind_cols(x = names_distinct, y = names_distinct) %>%
  tidyr::complete(x, y) %>%
  filter(x != y) %>%
  mutate(xy = paste0(x, "_", y)) %>%
  mutate(i = row_number())
xy_names <- names_grid %>% pull(xy)

#'
#+ clean_tweets
# Process. ----
tweets <- clean_tweets(params_proc$tweets, cols_extra = params_proc$augmented_col)
color_main_inv <- temisc::get_color_hex_inverse(params_proc$color_main)
colors_dual <- c(params_proc$color_main, color_main_inv)
# names(colors_dual) <- c(params_proc$name_main, "other")
# NOTE: Colors canNOT be named.
names(colors_dual) <- NULL

#'
#+ compute_tweet_timefilter
tweet_timefilter <- compute_tweet_timefilter(tweets)

#'
#'
#'
#+ trim_tweets_bytime
if(params_proc$trim_time) {
  date_start <- tweet_timefilter$date_start
  date_end <- tweet_timefilter$date_end
} else {
  date_start <- -Inf
  date_end <- Inf
}
tweets <- tweets %>% trim_tweets_bytime(start = date_start, end = date_end)
max(tweets$created_at)
#'
#' # Report "Meta-Data".
#'
#' Total number of tweets: `r format(nrow(tweets), big.mark = ",")`.
#'
#' First tweet: `r min(tweets$created_at)`.
#'
#' Last tweet: `r max(tweets$created_at)`.
#'
#' # Tweet Volume
#'
#' How often does each Twitter handle tweet?
#' Does the volume of tweets look different for
#' temporal periods?
#'
#+ viz_bytime_create
# Inspired by https://juliasilge.com/blog/ten-thousand-tweets/ here.
# cnt_bytime ----

#'
#+ viz_bytime_all_create, eval = (min(tweet_timefilter$data$dd_elapsed) >= params_proc$wday_cnt_min)
lab_subtitle_all <-
  paste0(
    "From ",
    strftime(tweet_timefilter$date_start, "%Y-%m-%d"),
    " to ",
    strftime(tweet_timefilter$date_end, "%Y-%m-%d")
  )

viz_bytime_all <-
  tweets %>%
  ggplot(aes(x = timestamp)) %>%
  add_viz_bytime_elements(geom = "hist", colors = params_proc$colors_main, lab_subtitle = lab_subtitle_all)

#'
#+ viz_bytime_all_show, eval = (min(tweet_timefilter$data$dd_elapsed) >= params_proc$wday_cnt_min), results = "asis", fig.show = "asis"
viz_bytime_all

#'
#+ viz_bytime_yyyy_create, eval = (min(tweet_timefilter$data$yyyy_elapsed) >= params_proc$yyyy_cnt_min)
viz_bytime_yyyy <-
  tweets %>%
  ggplot(aes(x = lubridate::year(timestamp))) %>%
  add_viz_bytime_elements(geom = "bar", colors = params_proc$colors_main, lab_subtitle = "By Year")

#'
#'
#'
#+ viz_bytime_yyyy_show, eval = (min(tweet_timefilter$data$yyyy_elapsed) >= params_proc$yyyy_cnt_min), results = "asis", fig.show = "asis"
viz_bytime_yyyy

#'
#+ viz_bytime_mm_create, eval = (min(tweet_timefilter$data$mm_elapsed) >= params_proc$mm_cnt_min)
viz_bytime_mm <-
  tweets %>%
  ggplot(aes(x = lubridate::month(timestamp))) %>%
  add_viz_bytime_elements(geom = "bar", colors = params_proc$colors_main, lab_subtitle = "By Month")

#'
#'
#+ viz_bytime_mm_show, eval = (min(tweet_timefilter$data$mm_elapsed) >= params_proc$mm_cnt_min), results = "asis", fig.show = "asis"
viz_bytime_mm

#'
#+ viz_bytime_wday_create, eval = (min(tweet_timefilter$data$dd_elapsed) >= params_proc$wday_cnt_min)
viz_bytime_wday <-
  tweets %>%
  ggplot(aes(x = lubridate::wday(timestamp, label = TRUE))) %>%
  add_viz_bytime_elements(geom = "bar", colors = params_proc$colors_main, lab_subtitle = "By Day of Week")

#'
#+ viz_bytime_wday_show, eval = (min(tweet_timefilter$data$dd_elapsed) >= params_proc$wday_cnt_min), results = "asis", fig.show = "asis"
viz_bytime_wday

#'
#+ viz_bytime_hh_create, eval = (min(tweet_timefilter$data$hh_elapsed) >= params_proc$hh_cnt_min),
viz_bytime_hh <-
  tweets %>%
  ggplot(aes(x = name, y = time, fill = name)) +
  scale_y_continuous(
    limits = c(1, 24),
    breaks = c(6, 12, 18),
    labels = c("6am", "Noon", "6pm")
  ) +
  scale_fill_manual(values = params_proc$colors_main, guide = FALSE) +
  geom_violin(size = 0, alpha = 0.7) +
  geom_hline(yintercept = seq(3, 24, by = 3),
             color = "gray",
             size = 0.1) +
  labs(x = NULL, y = NULL, title = "Count of Tweets Over Time", lab_subtitle = "By Time of Day") +
  temisc::theme_te_b_dx() +
  theme(panel.grid = element_blank()) +
  coord_flip()

#'
#+ viz_bytime_hh_show, eval = (min(tweet_timefilter$data$hh_elapsed) >= params_proc$hh_cnt_min), results = "asis", fig.show = "asis"
viz_bytime_hh

#'
#' # Tweet Behavior
#'
#' What proportion of tweets include more than just plain text
#' (e.g. hashtags or links)?
#' What proportion are **not** undirected or self-authored tweets
#' (i.e. RTs, replies, or quotes)?
#'
#'
#+ bykind_create
# Inspired by https://juliasilge.com/blog/ten-thousand-tweets/ here.
# tweets_bykind ----
tweets <- add_tweet_kind_data(tweets)

kinds <- c(params_proc$kinds_features, params_proc$kinds_types)
cols_summarize <- kinds
tweets_byname_bykind_summary_tidy <-
  tweets %>%
  group_by(name) %>%
  summarize_at(vars(c(cols_summarize)), funs(compute_pct(.))) %>%
  ungroup() %>%
  tidyr::gather(kind, value, -name)
# tweets_byname_bykind_summary_tidy

#'
#+ viz_byname_bytype_create, eval = (length(params_proc$kinds_types) > 0)
viz_byname_bytype <-
  tweets_byname_bykind_summary_tidy %>%
  filter(kind %in% params_proc$kinds_types) %>%
  mutate(kind = str_to_title(kind)) %>%
  visualize_byname_bykind(geom = "lollipop", colors = params_proc$colors_main, lab_kind = "By Types")

#'
#+ viz_byname_bytype_show, eval = (length(params_proc$kinds_types) > 0), results = "asis", fig.show = "asis"
viz_byname_bytype
#'
#+ viz_byname_byfeature_create, eval = (length(params_proc$kinds_features) > 0)
viz_byname_byfeature <-
  tweets_byname_bykind_summary_tidy %>%
  filter(kind %in% params_proc$kinds_features) %>%
  mutate(kind = str_to_title(kind)) %>%
  visualize_byname_bykind(geom = "lollipop", colors = params_proc$colors_main, lab_kind = "By Features")

#'
#+ viz_byname_byfeature_show, eval = (length(params_proc$kinds_features) > 0), results = "asis", fig.show = "asis"
viz_byname_byfeature

#'
#' # Tweet Content
#'
#' How long are the tweets?
#'
#+ viz_byname_chars_cnts_create
# Note that there are some tweets above 140 characters.
# Inspired by https://juliasilge.com/blog/ten-thousand-tweets/ here.
viz_byname_chars_cnt <-
  tweets %>%
  # mutate(chars_cnt = str_length(text)) %>%
  mutate(chars_cnt = display_text_width) %>%
  ggplot(aes(x = chars_cnt)) +
  geom_density(aes(fill = name)) +
  scale_fill_manual(values = params_proc$colors_main) +
  scale_x_continuous(limits = c(0, 200)) +
  # scale_y_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = NULL) +
  facet_wrap( ~ name, scales = "free") +
  labs(x = NULL, y = NULL) +
  labs(title = "# of Characters Per Tweet", subtitle = "By Name") +
  temisc::theme_te_b_facet() +
  theme(panel.grid.major.x = element_blank()) +
  theme(legend.position = "none")

#'
#'
#'
#+ viz_byname_chars_cnt_show, results = "asis", fig.show = "asis"
viz_byname_chars_cnt

#'
#' ## Word Frequency and Usage
#'
#' Which words are used most frequently?
#'
#+ tweets_tidy_unigrams
# Inspired by https://www.tidytextmining.com/twitter.html here.
# Bigrams inspired by https://www.tidytextmining.com/ngrams.html and
# https://buzzfeednews.github.io/2018-01-trump-twitter-wars/.
# tweets_tidy_unigrams ----
# library("tidytext")
tweets_tidy_unigrams <-
  tweets %>%
  mutate(text = text_plain) %>%
  tidy_tweets_unigrams()
# tweets_tidy_unigrams %>% select(word, name, status_id) %>% count(word, sort = TRUE)

tweets_tidy_bigrams <-
  tweets %>%
  mutate(text = text_plain) %>%
  tidy_tweets_bigrams()
# tweets_tidy_bigrams %>% select(bigram, name, status_id) %>% count(bigram, sort = TRUE)

#'
#+ viz_byname_cnt_create
viz_byname_cnt <-
  tweets_tidy_unigrams %>%
  visualize_byname_cnt()
#'
#+ viz_byname_cnt_show, results = "asis", fig.show = "asis"
viz_byname_cnt

#'
#+ viz_byname_cnt_byname_create
viz_byname_cnt_byname <-
  tweets_tidy_unigrams %>%
  visualize_byname_cnt(num_top = 10, facet = TRUE, colors = params_proc$colors_main)

#'
#+ viz_byname_cnt_byname_show, results = "asis", fig.show = "asis"
viz_byname_cnt_byname

#'
#+ ngrams_byname_freqs
# ngrams_byname_freqs ----
unigrams_cnt <-
  tweets_tidy_unigrams %>%
  count(name, sort = TRUE)
# unigrams_cnt

bigrams_cnt <-
  tweets_tidy_bigrams %>%
  count(name, sort = TRUE)
# bigrams_cnt

unigrams_byname_freqs <-
  tweets_tidy_unigrams %>%
  count(name, word, sort = TRUE) %>%
  left_join(unigrams_cnt %>% rename(total = n), by = "name") %>%
  mutate(freq = n / total)
# unigrams_byname_freqs

bigrams_byname_freqs <-
  tweets_tidy_bigrams %>%
  count(name, bigram, sort = TRUE) %>%
  left_join(bigrams_cnt %>% rename(total = n), by = "name") %>%
  mutate(freq = n / total)
# bigrams_byname_freqs

#'
#+ viz_ngrams_byname_freqs_wordcloud_create
num_par_row <- ceiling(length(params_proc$names_main) / 3)
num_par_col <- min(length(params_proc$names_main), 3)

#'
#+  viz_unigrams_byname_freqs_wordcloud_show, fig.show = "asis"
par(mfrow = c(num_par_row, num_par_col))
purrr::map2(
  params_proc$names_main,
  params_proc$colors_main,
  ~visualize_ngrams_byname_freqs_wordcloud(
    data = unigrams_byname_freqs,
    name_filter = .x,
    color = .y
  )
)
par(mfrow = c(1, 1))


#'
#' What about *pairs* of words are used most frequently?
#'
#+ viz_ngrams_byname_freqs_create
num_top_bigram_freq <- floor(12 / length(params_proc$names_main))
bigrams_byname_freqs_viz <-
  bigrams_byname_freqs %>%
  group_by(name) %>%
  mutate(rank = row_number(desc(freq))) %>%
  filter(rank <= num_top_bigram_freq) %>%
  ungroup() %>%
  arrange(name) %>%
  mutate(bigram = str_replace_all(bigram, " ", "\n")) %>%
  mutate(bigram = forcats::fct_reorder(factor(bigram), freq))
# bigrams_byname_freqs_viz

viz_bigrams_byname_freqs <-
  bigrams_byname_freqs_viz %>%
  ggplot(aes(x = name, y = bigram, color = name, size = freq)) +
  geom_point() +
  scale_y_discrete(position = "right") +
  scale_color_manual(values = params_proc$colors_main) +
  scale_size_area(max_size = 25) +
  labs(x = NULL, y = NULL) +
  labs(title = "Most Frequently Used Pairs of Words", subtitle = "By Name") +
  temisc::theme_te_b() +
  theme(legend.position = "none") +
  coord_flip()

#'
#+ viz_ngrams_byname_freqs_show, results = "asis", fig.show = "asis"
viz_bigrams_byname_freqs

#'
#+  viz_bigrams_byname_freqs_wordcloud_show, fig.show = "asis"
par(mfrow = c(num_par_row, num_par_col))
purrr::map2(
  params_proc$names_main,
  params_proc$colors_main,
  ~visualize_ngrams_byname_freqs_wordcloud(
    data = bigrams_byname_freqs %>% rename(word = bigram),
    name_filter = .x,
    color = .y
  )
)
par(mfrow = c(1, 1))

#'
#' Which words are most likely to be used by one name compared to the other?
#'


#'
#+ ngrams_freqs_wide
unigrams_freqs_wide <-
  wrapper_func(grid = names_grid,
               names = xy_names,
               data = unigrams_byname_freqs,
               func = compute_unigrams_freqs)
#'
#+ viz_ngrams_freqs_create
# TODO: Convert this into a "prepare" function?
unigrams_freqs_wide_viz <-
  unigrams_freqs_wide %>%
  filter(name_x == params_proc$name_main) %>%
  mutate(name_xy = paste0(name_x, " vs. ", name_y))

viz_unigrams_freqs <-
  unigrams_freqs_wide_viz %>%
  ggplot(aes(x = x, y = y)) +
  geom_text(aes(label = word, size = x + y), check_overlap = TRUE) +
  # scale_x_log10(labels = scales::percent_format()) +
  # scale_y_log10(labels = scales::percent_format()) +
  scale_x_log10(labels = NULL) +
  scale_y_log10(labels = NULL) +
  geom_abline(color = "red") +
  facet_wrap( ~ name_xy, scales = "free") +
  labs(x = NULL, y = NULL) +
  labs(title = "Relative Word Frequency") +
  temisc::theme_te_b_facet() +
  theme(legend.position = "none")

#'
#+ ngrams_freqs_show, results = "asis", fig.show = "asis"
viz_unigrams_freqs

#'
#+ ngrams_ratios_wide
# TODO: Not workiing when called by render_proj_io?
# ngrams_ratios_wide ----
unigrams_ratios_wide <-
  wrapper_func(grid = names_grid,
               names = xy_names,
               data = tweets_tidy_unigrams,
               func = compute_logratio)

#'
#+ viz_ngrams_ratios_create
# TODO: Convert this into a "prepare" function?
unigrams_ratios_wide_viz <-
  unigrams_ratios_wide %>%
  mutate(logratio_direction = if_else(logratio < 0, TRUE, FALSE)) %>%
  group_by(name_xy, logratio_direction) %>%
  arrange(name_xy, desc(abs(logratio))) %>%
  slice(1:5) %>%
  ungroup() %>%
  filter(name_x == params_proc$name_main) %>%
  mutate(color_lab = if_else(logratio_direction == TRUE, name_x, "other")) %>%
  mutate(color_lab = factor(color_lab, levels = c(params_proc$name_main, "other"))) %>%
  mutate(word = reorder(word, -logratio)) %>%
  mutate(name_xy = paste0(name_x, " vs. ", name_y))

viz_unigrams_ratios <-
  unigrams_ratios_wide_viz %>%
  ggplot(aes(x = word, y = logratio, fill = color_lab)) +
  geom_col() +
  facet_wrap(~ name_xy, scales = "free") +
  scale_fill_manual(values = colors_dual) +
  coord_flip() +
  labs(x = NULL, y = "Log Odds Ratio") +
  labs(title = "Most Unique Words") +
  temisc::theme_te_b_facet() +
  theme(legend.position = "bottom", legend.title = element_blank())

#'
#+ ngrams_ratios_show, results = "asis", fig.show = "asis"
viz_unigrams_ratios

#'
#' How are the words related?
#'
#+ viz_ngrams_byx_corrs_create
# Inspired by http://varianceexplained.org/r/seven-fav-packages/ and
# http://varianceexplained.org/r/stacksurveyr/ here.
num_top_ngrams <- 50
num_top_corrs <- 50

unigrams_byx_corrs_viz <-
  tweets_tidy_unigrams %>%
  prepare_viz_ngrams_byx_corrs(
    num_top_ngrams = num_top_ngrams,
    num_top_corrs = num_top_corrs,
    col_word = "word",
    col_feature = "status_id"
  )

viz_unigrams_byx_corrs <-
  unigrams_byx_corrs_viz %>%
  visualize_ngrams_byx_corrs()

#'
#+ viz_ngrams_byx_corrs_show, results = "asis", fig.show = "asis"
viz_unigrams_byx_corrs


#'
#+ change_byname_bytime
# Inspired by https://www.tidytextmining.com/twitter.html here.
# TODO: change_byname_bytime? ----

#'
#+ pop_byname_byfeature
# Inspired by https://www.tidytextmining.com/twitter.html here.
# TODO: pop_byname_byfeature? ----

#'
#' # Sentiment Analysis
#'
#' What is the sentiment (i.e. "tone") of the tweets?
#'
#+ sents_byname
# sents ----
bing <-
  tidytext::get_sentiments(lexicon = "bing") %>%
  select(word, sentiment)

# afinn <-
#   tidytext::get_sentiments(lexicon = "afinn") %>%
#   select(word, sentiment = score) %>%
#   mutate(sentiment = sentiment / 5)

unigrams_cnt_byname_bytweet <-
  tweets_tidy_unigrams %>%
  group_by(name) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  distinct(status_id, name, total_words)

sents_bing_byname <-
  bing %>%
  summarize_sent_byname(
    tweets = tweets_tidy_unigrams,
    unigrams = unigrams_cnt_byname_bytweet
  )
# sents_afinn_byname_0 <-
#   afinn %>%
#   summarize_sent_byname(
#     tweets = tweets_tidy_unigrams,
#     unigrams = unigrams_cnt_byname_bytweet
#   )
#
# sents_afinn_byname_chars <-
#   sents_afinn_byname_0 %>%
#   mutate(sentiment = if_else(sentiment <= 0, "negative", "positive")) %>%
#   group_by(name, sentiment) %>%
#   summarize(total_words = first(total_words), words = sum(words)) %>%
#   ungroup()

# sents_afinn_byname_nums <-
#   sents_afinn_byname_0 %>%
#   group_by(name) %>%
#   summarize(
#     sentiment = sum(sentiment * words) / sum(words),
#     total_words = first(total_words)
#   ) %>%
#   ungroup()


#'
#+ sents_diffs_poisson
sents_bing_diffs_poisson <-
  wrapper_func(grid = names_grid,
               names = xy_names,
               data = sents_bing_byname,
               func = compute_sentdiffs_poisson)

#'
#'
#'
#+ viz_sents_diffs_poisson_create
sents_bing_diffs_poisson_viz <-
  prepare_sents_diffs_poisson(
    sents_bing_diffs_poisson,
    name_filter = params_proc$name_main
  )
viz_sents_bing_diffs_poisson <-
  sents_bing_diffs_poisson_viz %>%
  visualize_sents_diffs_poission(
    colors = colors_dual,
    lab_lexicon = "Bing"
  )

#'
#+ viz_sents_diffs_poisson_show, results =  "asis", fig.show = "asis"
viz_sents_bing_diffs_poisson

#'
#+ viz_sents_ratios_create
num_top_sents_ratio <- ceiling(12 / length(params_proc$names_main))
sents_bing_ratios_wide_viz <-
  create_sents_ratios_wide(
    sents = bing,
    unigrams_ratios = unigrams_ratios_wide,
    name_filter = params_proc$name_main,
    num_top = num_top_sents_ratio
  )

viz_sents_bing_ratios_pos <-
  sents_bing_ratios_wide_viz %>%
  visualize_sents_ratios(
    sentiment_filter = "positive",
    colors = colors_dual
  )

viz_sents_bing_ratios_neg <-
  sents_bing_ratios_wide_viz %>%
  visualize_sents_ratios(
    sentiment_filter = "negative",
    colors = colors_dual
  )
#'
#+ viz_sents_ratios_show, results = "asis", fig.show = "asis"
viz_sents_bing_ratios_pos
viz_sents_bing_ratios_neg

#'
#'

