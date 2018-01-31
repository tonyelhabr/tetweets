#' ---
#' title: "Analysis"
#' output: html_document
#' params:
#'   thing1:
#' ---
#'
# + include = FALSE
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

params <-
  list(
    filepath_tweets = "data/iso_tweets_augmented.csv",
    names = c("CAISO", "ERCOT", "MISO", "NYISO", "PJM"),
    name_main = "ERCOT",
    augmented = TRUE,
    # download = FALSE,
    # screen_names = NULL,
    # tweets_min_download = 1000
    pal = ercotr::ercot_palette2
  )

filepath_tweets <- params$filepath_tweets
augmented <- params$augmented
pal <- params$pal
names <- params$names
name_main <- params$name_main

#'
#'
#'
# Setup. ----
library("dplyr")
library("stringr")
library("ggplot2")
library("tidyr")
# library("lubridate")
# library("scales")
# library("tidytext")
library("rtweet")

tweets <- readr::read_csv(filepath_tweets)

#'
#'
#'
# Process. ----
tweets <-
  tweets %>%
  mutate(timestamp = lubridate::ymd_hms(created_at)) %>%
  mutate(timestamp = lubridate::with_tz(timestamp, "America/Chicago")) %>%
  mutate(time = as.numeric(timestamp - trunc(timestamp, "days"))) %>%
  mutate(time = as.POSIXct(time, origin = "1970-01-01"))
#'
#'
#'
# + include = FALSE, eval = FALSE
# Check for bad values...
tweets %>%
  mutate(
    hour = lubridate::hour(timestamp),
    min = lubridate::minute(timestamp),
    sec = lubridate::second(timestamp)
  ) %>%
  filter(hour == 0 & min == 0 & sec == 0) %>%
  nrow()

#'
#'
#'
# tweets_times ----
# This is "original" processing needed to trim tweets appropriately/dynamically
# given an unknown data set.
tweets_cnt <- tweets %>% count(name, sort = TRUE)
tweets_cnt

# Modified from a SO answer.
compute_elapsed_time <- function(start_date, end_date, type) {
  if (type == "years" | type == "months") {
    sd <- as.POSIXlt(start_date)
    ed <- as.POSIXlt(end_date)
    if (type == "years") {
      (ed$year - sd$year) - 1
    } else if (type == "months") {
      12 * (ed$year - sd$year) + (ed$mon - sd$mon) - 1
    }
  } else if (type == "days" | type == "hours") {
    (difftime(end_date, start_date, units = type) - 1) %>%
      round(0) %>%
      as.numeric()
  }
}


tweets_times <-
  tweets %>%
  group_by(name) %>%
  arrange(timestamp) %>%
  mutate(start_date = first(timestamp),
         end_date = last(timestamp)) %>%
  slice(1) %>%
  ungroup() %>%
  select(name, start_date, end_date) %>%
  mutate(
    years_elapsed = compute_elapsed_time(start_date, end_date, "years"),
    months_elapsed = compute_elapsed_time(start_date, end_date, "months"),
    days_elapsed = compute_elapsed_time(start_date, end_date, "days"),
    hours_elapsed = compute_elapsed_time(start_date, end_date, "hours")
  )
tweets_times

tweet_firstend <- min(tweets_times$end_date)
tweet_laststart <- max(tweets_times$start_date)
tweets <-
  tweets %>%
  filter(timestamp < tweet_firstend, timestamp >= tweet_laststart)

tweets_cnt_trimmed <- tweets %>% count(name, sort = TRUE)
tweets_cnt_trimmed

#'
#' The following table list the number of tweets that were originally extracted.
#'
# + results = "asis", fig.show = "asis"
tweets_cnt %>% temisc::create_kable_html()
#'
#' The oldest tweet collected is from
#' `r strftime(min(tweets_times$start_date), "%Y-%m-%d")`.
#' The newest tweet is from
#' `r strftime(max(tweets_times$end_date), "%Y-%m-%d")`.
#'
#' The tweets were trimmed in order to
#' align the dates of the most recent first tweet and the oldest last tweet
#' from the screen names.
#'
# + results = "asis", fig.show = "asis"
tweets_cnt_trimmed %>% temisc::create_kable_html()

#' # Tweet Volume
#'
#' How often does each Twitter handle tweet?
#' Does the volume of tweets look different for
#' temporal periods?
#'
# Inspired by https://juliasilge.com/blog/ten-thousand-tweets/ here.
# cnt_bytime ----
# NOTE: This function takes a gg object as an input
# (so it should be preceded by a pipe, and, if followed by other ggplot2 commands,
#  succedeeded by a `+`.)
add_viz_bytime_elements <-
  function(viz,
           geom = c("bar", "hist"),
           colors = pal) {
    geom <- match.arg(geom)
    viz_labs <-
      labs(x = NULL, y = NULL, title = "Count of Tweets Over Time")
    viz_theme <-
      temisc::theme_te_facet() +
      theme(panel.grid.major.x = element_blank()) +
      # theme(legend.position = "bottom", legend.title = element_blank())
      theme(legend.position = "none")

    if (geom == "bar") {
      viz <-
        viz +
        geom_bar(aes(y = ..count.., fill = name))
    } else if (geom == "hist") {
      viz <-
        viz +
        geom_histogram(aes(y = ..count.., fill = name), bins = 30)
    }
    viz <-
      viz +
      scale_fill_manual(values = colors) +
      facet_wrap( ~ name, ncol = 1, strip.position = "right") +
      viz_labs +
      viz_theme
    viz
  }

viz_bytime_all <-
  tweets %>%
  ggplot(aes(x = timestamp)) %>%
  add_viz_bytime_elements(geom = "hist") +
  labs(subtitle =
         str_c(
           "From ",
           strftime(tweet_laststart, "%Y-%m-%d"),
           " to ",
           strftime(tweet_firstend, "%Y-%m-%d")
         ))
viz_bytime_all

# TODO: Define these at the top?
yyyy_cnt_min <- 1
mm_cnt_min <- 3
wday_cnt_min <- 7
hh_cnt_min <- 24

# NOTE: The if statements here are intended for use with a parameterized report, where the
# number of tweets for a given time period for a given input file can vary strongly.
if (min(tweets_times$years_elapsed) >= yyyy_cnt_min) {
  viz_bytime_yyyy <-
    tweets %>%
    ggplot(aes(x = lubridate::year(timestamp))) %>%
    add_viz_bytime_elements(geom = "bar") +
    labs(subtitle = "By Year")
  viz_bytime_yyyy
}

if (min(tweets_times$months_elapsed) >= mm_cnt_min) {
  viz_bytime_mm <-
    tweets %>%
    ggplot(aes(x = lubridate::month(timestamp))) %>%
    add_viz_bytime_elements(geom = "bar") +
    labs(subtitle = "By Month")
  viz_bytime_mm
}

if (min(tweets_times$days_elapsed) >= wday_cnt_min) {
  viz_bytime_wday <-
    tweets %>%
    ggplot(aes(x = lubridate::wday(timestamp, label = TRUE))) %>%
    add_viz_bytime_elements(geom = "bar") +
    labs(subtitle = "By Day of Week")
  viz_bytime_wday
}

if (min(tweets_times$hours_elapsed) >= hh_cnt_min) {
  viz_bytime_hh <-
    tweets %>%
    ggplot(aes(x = time)) %>%
    add_viz_bytime_elements(geom = "hist") +
    scale_x_datetime(
      breaks = scales::date_breaks("4 hours"),
      labels = scales::date_format("%H")
    ) +
    labs(subtitle = "By Hour of Day")
  viz_bytime_hh
}

#'
#'
#'
# + results = "asis", fig.show = "asis"
viz_bytime_all
viz_bytime_yyyy
viz_bytime_mm
viz_bytime_wday
viz_bytime_hh

#'
#' # Tweet Behavior
#'
#' What proportion of tweets include more than just plain text
#' (e.g. hashtags, links, etc.)?
#' What proportion are not undirected, self-authored tweets
#' (i.e. RTs or replies)?
#'
#'
# Inspired by https://juliasilge.com/blog/ten-thousand-tweets/ here.
# tweets_bykind ----
# TODO: Define these at the top?
kinds_features <- c("hashtag", "link")
kinds_types <- c("quote", "reply", "rt")
kinds <- c(kinds_features, kinds_types)

tweets <-
  tweets %>%
  mutate(
    hashtag = if_else(!is.na(hashtags), 1, 0),
    link = if_else(!is.na(media_t.co) & !is.na(urls_t.co), 1, 0),
    rt = if_else(is_retweet, 1, 0),
    quote = if_else(is_quote, 1, 0),
    reply = if_else(!is.na(reply_to_status_id), 1, 0)
  ) %>%
  mutate(type =
           case_when(
             rt == TRUE ~ "RT",
             reply == TRUE ~ "reply",
             quote == TRUE ~ "quote",
             TRUE ~ "original"
           ))

compute_pct <- function(x,
                        value = 1,
                        digits_round = 4) {
  # round(sum(x == value) / sum(!is.na(x)), digits_round)
  sum(x == value) / sum(!is.na(x))
}

cols_summarize <- kinds
tweets_bykind_summary_tidy <-
  tweets %>%
  group_by(name) %>%
  summarize_at(vars(c(cols_summarize)), funs(compute_pct(.))) %>%
  ungroup() %>%
  tidyr::gather(kind, value, -name)
tweets_bykind_summary_tidy

visualize_bykind <-
  function(data,
           geom = c("col", "lollipop"),
           colors = pal,
           data_labels = FALSE) {
    geom <- match.arg(geom)

    viz_labs <-
      labs(x = NULL, y = NULL, title = "% of Tweets")

    viz_theme <-
      temisc::theme_te_facet() +
      theme(panel.grid.major.x = element_blank()) +
      theme(legend.position = "none")

    if (geom == "col") {
      viz <-
        data %>%
        ggplot(aes(x = name, y = value, fill = name)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = colors)
    } else if (geom == "lollipop") {
      viz <-
        data %>%
        ggplot(aes(x = name, y = value, color = name)) +
        ggalt::geom_lollipop(point.size = 2) +
        scale_color_manual(values = colors)
    }

    if (data_labels) {
      viz <-
        viz +
        geom_text(aes(group = name,
                      label = sprintf("%.0f %%", 100 * value)),
                  position = position_dodge(width = 1))
    }

    viz <-
      viz +
      scale_y_continuous(labels = scales::percent_format()) +
      facet_wrap( ~ kind, scales = "free") +
      viz_labs +
      viz_theme
  }

viz_bytype <-
  tweets_bykind_summary_tidy %>%
  filter(kind %in% kinds_types) %>%
  mutate(kind = str_to_title(kind)) %>%
  visualize_bykind() +
  labs(subtitle = "Types")
viz_bytype

viz_byfeature <-
  tweets_bykind_summary_tidy %>%
  filter(kind %in% kinds_features) %>%
  mutate(kind = str_to_title(kind)) %>%
  visualize_bykind() +
  labs(subtitle = "Features")
viz_byfeature

# Inspired by https://juliasilge.com/blog/ten-thousand-tweets/ here.
# NOTE: This could still use some work...
# tweets %>% count(name, type, sort = TRUE)
viz_bytype_temporal <-
  tweets %>%
  ggplot(aes(x = timestamp, fill = type)) +
  geom_histogram(position = "fill", bins = 30) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = pal) +
  facet_wrap( ~ name,
              ncol = 1,
              scales = "free",
              strip.position = "right") +
  labs(x = NULL, y = NULL) +
  labs(title = "Distribution of Tweets by Type Over Time") +
  temisc::theme_te_facet() +
  theme(panel.grid.major.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank())
viz_bytype_temporal

#'
#'
#'
#+ results = "asis", fig.show = "asis"
viz_bytype
viz_bytype_temporal
viz_byfeature

#'
#' # Tweet Content
#'
#' How long are the tweets?
#'
#+ include = FALSE
# Note that there are some tweets above 140 characters.
# Inspired by https://juliasilge.com/blog/ten-thousand-tweets/ here.
viz_cnt_chars <-
  tweets %>%
  mutate(cnt_chars = str_length(text)) %>%
  ggplot(aes(x = cnt_chars)) +
  geom_histogram(aes(fill = ..count..), binwidth = 10) +
  viridis::scale_fill_viridis(discrete = FALSE,
                              option = "B",
                              direction = -1) +
  scale_x_continuous(limits = c(0, 280)) +
  facet_wrap( ~ name,
              ncol = 1,
              scales = "free",
              strip.position = "right") +
  labs(x = NULL, y = NULL) +
  labs(title = "Distribution of Characters Per Tweet") +
  temisc::theme_te_facet() +
  theme(panel.grid.major.x = element_blank()) +
  theme(legend.position = "none")
viz_cnt_chars

#'
#'
#'
#+ results = "asis", fig.show = "asis"
viz_cnt_chars

#'
#' ## Word Frequency and Usage
#'
#' Which words are used most frequently?
#'
#+ tweets_tidy ----
# Inspired by https://www.tidytextmining.com/twitter.html here.
# library("tidytext")
rgx_unnest <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
rgx_pattern <-
  "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"

tweets_tidy <-
  tweets %>%
  # filter(!str_detect(text, "^(RT|@)")) %>%
  mutate(text = str_replace_all(text, rgx_pattern, "")) %>%
  tidytext::unnest_tokens(word, text, token = "regex", pattern = rgx_unnest) %>%
  # anti_join(tidytext::stop_words, by = "word") %>%
  # filter(str_detect(word, "[a-z]"))
  filter(!word %in% tidytext::stop_words$word,
         str_detect(word, "[a-z]"))

# Inspired by https://github.com/dgrtwo/dgrtwo.github.com/blob/master/_R/2016-08-09-trump-tweets.Rmd here.
# TODO: Improve this.
visualize_cnt <-
  function(data,
           num_top = 20,
           facet = FALSE,
           colors = pal) {
    # browser()
    if (facet) {
      data_proc <-
        data %>%
        count(name, word, sort = TRUE) %>%
        group_by(name) %>%
        filter(row_number(desc(n)) <= num_top) %>%
        mutate(word = reorder(word, n)) %>%
        # arrange(desc(n)) %>%
        # mutate(word = forcats::fct_inorder(word)) %>%
        ungroup()
    } else {
      data_proc <-
        data %>%
        count(word, sort = TRUE) %>%
        filter(row_number(desc(n)) <= num_top) %>%
        mutate(word = reorder(word, n))
    }

    # browser()
    viz <-
      data_proc %>%
      ggplot(aes(x = word, y = n)) +
      geom_bar()

    if (facet) {
      viz <-
        viz +
        geom_bar(stat = "identity", aes(color = name)) +
        # geom_col(aes(color = name)) +
        scale_fill_manual(values = colors) +
        facet_wrap( ~ name, scales = "free") +
        temisc::theme_te_facet() +
        labs(subtitle = "By Name")
    } else {
      viz <-
        data_proc %>%
        ggplot(aes(x = word, y = n)) +
        geom_bar(stat = "identity") +
        # geom_col() +
        temisc::theme_te()
    }
    # browser()
    viz <-
      viz +
      labs(x = NULL, y = NULL) +
      labs(title = "Count of Words") +
      theme(panel.grid.major.y = element_blank()) +
      coord_flip()
    viz
  }
viz_cnt <-
  tweets_tidy %>%
  visualize_cnt()
viz_cnt

# viz_cnt_byname <-
#   tweets_tidy %>%
#   visualize_cnt(facet = TRUE)
# viz_cnt_byname
#'
#'
#'
# This was a fix added after some analysis. It could be added directly
# to the creation of the tweets_tidy variable.
rgx_pattern_hex <- "^[0-9]{2}[0-9a-f]{2}$"
words_hex <-
  tweets_tidy %>%
  filter(str_detect(word, rgx_pattern_hex)) %>%
  select(name, word, created_at)
words_hex

tweets_tidy <-
  tweets_tidy %>%
  anti_join(words_hex, by = "word")

tweets_tidy_summary <-
  tweets_tidy %>%
  count(name, sort = TRUE)
tweets_tidy_summary

# words_freqs ----
words_freqs <-
  tweets_tidy %>%
  count(name, word, sort = TRUE) %>%
  left_join(tweets_tidy_summary %>% rename(total = n), by = "name") %>%
  mutate(freq = n / total)
words_freqs

# Debugging...
words_freqs %>%
  inner_join(words_freqs, by = "word", suffix = c("_x", "_y")) %>%
  filter(name_x != name_y)

names_distinct <- words_freqs %>% distinct(name) %>% pull(name)
names_grid <-
  bind_cols(x = names_distinct, y = names_distinct) %>%
  tidyr::complete(x, y) %>%
  filter(x != y) %>%
  mutate(xy = str_c(x, "_", y)) %>%
  mutate(i = row_number())
xy_names <- names_grid %>% pull(xy)

filter_xy_names <- function(xy_names, i) {
  xy_i <- xy_names[i]
  xy_i_row <- names_grid %>% filter(xy == xy_i)
  x_i <- xy_i_row %>% pull(x)
  y_i <- xy_i_row %>% pull(y)
  out <- list(x = x_i, y = y_i, xy = xy_i)
  out
}

preprocess_xy_data <- function(data, xy_info) {
  out <-
    data %>%
    filter(name %in% c(xy_info$x, xy_info$y))
  out
}

postprocess_xy_data <- function(data, xy_info) {
  out <-
    data %>%
    mutate(name_x = xy_info$x, name_y = xy_info$y) %>%
    mutate(name_xy = paste0(name_x, "_", name_y)) %>%
    rename(x = !!rlang::sym(xy_info$x),
           y = !!rlang::sym(xy_info$y)) %>%
    select(name_x, name_y, name_xy, word, x, y, everything())
  out
}

compute_words_freqs <- function(data) {
  out <-
    data %>%
    select(name, word, freq) %>%
    tidyr::spread(name, freq)
  out
}

# TODO: Figure out how to use `purrr::map()` here.
wrapper_func <- function(xy_names, data, func) {
  i <- 1
  while (i < length(xy_names)) {
    xy_i_info <- filter_xy_names(xy_names, i)
    data_i_preproc <- preprocess_xy_data(data, xy_i_info)
    data_i_proc <- do.call(func, list(data_i_preproc))
    # browser()
    data_i_postproc <- postprocess_xy_data(data_i_proc, xy_i_info)
    if (i == 1) {
      out <- data_i_postproc
    } else {
      out <- bind_rows(out, data_i_postproc)
    }
    i <- i + 1
  }
  out
}

words_freqs_wide <-
  wrapper_func(xy_names = xy_names,
               data = words_freqs,
               func = compute_words_freqs)
words_freqs_wide

# Debugging...
words_freqs_wide %>%
  count(name_xy, name_x) %>%
  group_by(name_x) %>%
  filter(row_number(desc(n)) <= 2) %>%
  ungroup() %>%
  distinct(n, .keep_all = TRUE)

# TODO: Need to make this more dynamic (with if statements)...
# words_freqs_wide_top <-
#   words_freqs_wide %>%
#   count(name_xy, name_x) %>%
#   group_by(name_x) %>%
#   filter(row_number(desc(n)) == 1)

words_freqs_wide_viz <-
  words_freqs_wide %>%
  # semi_join(words_freqs_wide_top, by = "name_xy") %>%
  filter(name_x == name_main) %>%
  mutate(name_xy = paste0(name_x, " vs. ", name_y))

viz_words_freq <-
  words_freqs_wide_viz %>%
  ggplot(aes(x = x, y = y)) +
  # geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word, size = x + y), check_overlap = TRUE) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  geom_abline(color = "red") +
  facet_wrap( ~ name_xy, scales = "free") +
  labs(x = NULL, y = NULL) +
  labs(title = "Relative Word Frequency") +
  temisc::theme_te_facet() +
  theme(panel.grid.major.x = element_blank()) +
  theme(legend.position = "none")
viz_words_freq

#'
#' Which words are most likely to be used by one name compared to the other?
#'
# words_ratios ----
# NOTE: Filter out replies because they would make up a disproportional share of the
# top results.
# NOTE: Modified original code a bit because it doesn't really
compute_logratio <- function(data) {
  out <-
    data %>%
    filter(is.na(reply_to_status_id)) %>%
    count(word, name) %>%
    filter(n >= 10) %>%
    tidyr::spread(name, n, fill = 0)

  nms <- names(out)
  out <-
    out %>%
    setNames(c("word", "x", "y")) %>%
    mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
    mutate(logratio = log(x / y))

  x_i <- nms[2]
  y_i <- nms[3]
  out <-
    out %>%
    setNames(c("word", x_i, y_i, "logratio")) %>%
    arrange(desc(logratio))
  out
}

words_ratios_wide <-
  wrapper_func(xy_names = xy_names,
               data = tweets_tidy,
               func = compute_logratio)
words_ratios_wide %>% count(name_xy, sort = TRUE)
words_ratios_wide %>% count(name_x, name_y, sort = TRUE)

# These words are the most and least likely to be tweeted by either name.
words_ratios_wide %>% group_by(name_xy) %>% arrange(abs(logratio))
words_ratios_wide %>% group_by(name_xy) %>% arrange(desc(abs(logratio)))

# Separating this because there's a couple of step involved...
words_ratios_wide_viz <-
  words_ratios_wide %>%
  mutate(logratio_dir = (logratio < 0)) %>%
  group_by(name_xy, logratio_dir) %>%
  # top_n(15, abs(logratio)) %>%
  arrange(name_xy, desc(abs(logratio))) %>%
  slice(1:10) %>%
  ungroup() %>%
  # mutate(word = reorder(word, logratio)) %>%
  filter(name_x == name_main) %>%
  mutate(word = reorder(word, -logratio)) %>%
  mutate(name_xy = paste0(name_x, " vs. ", name_y))

# pal_rdbu <- RColorBrewer::brewer.pal(n = 3, name = "RdBu")
# pal_rdbu_red <- pal_rdbu[1]
# pal_rdbu_blue <- pal_rdbu[length(pal_rdbu)]
# pal_rdbu_rb <- c(pal_rdbu_red, pal_rdbu_blue)
viz_ratios <-
  words_ratios_wide_viz %>%
  ggplot(aes(x = word, y = logratio, fill = logratio_dir)) +
  geom_col() +
  facet_wrap(~name_xy, scales = "free") +
  # scale_fill_manual(values = c("red", "cyan")) +
  # scale_fill_manual(values = pal_rdbu_rb) +
  # viridis::scale_fill_viridis(option = "D", begin = 0.25, end = 0.75, direction = -1, discrete = TRUE) +
  scale_fill_manual(values = pal, labels = c(name_main, "other")) +
  coord_flip() +
  labs(x = NULL, y = "log odds ratio") +
  labs(title = "Most Unique Words") +
  temisc::theme_te_facet() +
  # theme(legend.position = "none") +
  theme(legend.position = "bottom", legend.title = element_blank())
viz_ratios

#'
#'
#'
# Inspired by https://www.tidytextmining.com/twitter.html here.
# TODO: bytime? ----


#'
#'
#'
# TODO: byfeature_bypop? ----

#'
#' # Sentiment Analysis
#'
#' What is the sentiment (i.e. "tone") of the tweets?
#'




