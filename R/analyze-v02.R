#' ---
#' title: "Analysis"
#' output: html_document
#' params:
#'   filepath_tweets: NULL
#'   names: NULL
#'   name_main: NULL
#'   augmented: TRUE
#'   augmented_colname: "name"
#'   download: FALSE
#'   download_method: c("timline", "search")
#'   screen_names: NULL
#'   tweets_min_download: 1000
#'   trim: FALSE
#'   pal: viridis::viridis(option = "C", direction = -1, n = 2)
#'   dd_cnt_min: 7
#'   yyyy_cnt_min: 1
#'   mm_cnt_min: 12
#'   wday_cnt_min: 7
#'   hh_cnt_min: 24
#'   kinds_features: c("hashtag", "link")
#'   kinds_types: c("quote", "reply", "rt")
#' ---
#'
# + include = FALSE
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

# NOTE: This is temporary!
name_main <- "spurs"
names_main <- c("spurs", "warriors", "cavs", "raptors", "lakers", "knicks")
colors_dict <-
  dplyr::mutate(
    readr::read_csv("data-raw/tms-nba.csv"),
    colors = stringr::str_replace_all(colors, ",.*$", "")
  )

pal_main <- dplyr::pull(colors_dict, colors)
data_main <- dplyr::filter(colors_dict, name == name_main)
color_main <- dplyr::pull(data_main, colors)
names(color_main) <- dplyr::pull(data_main, name)
color_main
datas_main <- dplyr::filter(colors_dict, name  %in% names_main)
colors_main <- dplyr::pull(datas_main, colors)
names(colors_main) <- dplyr::pull(datas_main, name)
colors_main

# augmented filepath MUST have a "name" column, which will be used for entity
# identification instead of screen_name internally. This is important in order
# to allow the script to work for both timelines (where the name will
# simply be the screen_name) or for a search, where the name will correspond with
# the search term.
# A single `name_main` should be specified in order to simply the facetted plots.

params <-
  list(
    filepath_tweets = "data/tweets-search-nba.rds",
    names = NULL,
    name_main = name_main,
    names_main = names_main,
    augmented = TRUE,
    augmented_colname = "name",
    download = FALSE,
    download_method = "search",
    trim = FALSE,
    screen_names = NULL,
    tweets_min_download = 1000,
    num_main_max = 6,
    pal_main = pal_main,
    color_main = color_main,
    colors_main = colors_main,
    tweet_cnt_min = 1000,
    dd_cnt_min = 7,
    yyyy_cnt_min = 1,
    mm_cnt_min = 3,
    wday_cnt_min = 7,
    hh_cnt_min = 24,
    kinds_features = c("hashtag", "link"),
    # kinds_types = c("quote", "reply", "rt")
    kinds_types = c("quote", "reply")
  )

validate_params <- function(params) {
  if (is.null(params$names) & !is.null(params$filepath_tweets)) {
    message("`names` will be inferred from the provided data.")
  }
  if (is.null(params$filepath_tweets) & !params$download) {
    params$download <- TRUE
  }

  if(is.null(params$names_main)) {
    stop("", call. = FALSE)
  }

  if(!(params$name %in% params$names_main)) {
    stop("", call. = FALSE)
  }

  if(is.null(names(params$color_main))) {
    stop("", call. = FALSE)
  }

  if(is.null(names(params$colors_main))) {
    stop("", call. = FALSE)
  }

  if(length(params$names_main) != length(params$colors_main)) {
    stop("", call. = FALSE)
  }

  if(length(params$names_main) > params$num_main) {
    stop("", call. = FALSE)
  }

  invisible(params)
}

params <- validate_params(params)

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
# library("rtweet")
library("temisc")

# NOTE: Unfortunately, it doesn't seem like get_timelines() and search_tweets2()
# really work as I initially intended. Some like of loop with get_timeline() and search_tweet()
# will be necessary.
implement_params <- function(params) {
  if (!is.null(params$filepath_tweets)) {
    tweets <-
      # try(rtweet::read_twitter_csv(params$filepath_tweets),
      #     silent = TRUE)
      try(readRDS(params$filepath_tweets),
          silent = TRUE)
    if (inherits(tweets, "try-error")) {
      stop(
        sprintf(
          "Could not retrieve tweets from `filepath_tweets` %s.",
          params$filepath_tweets
        ),
        call. = FALSE
      )
    }

    tweets <-
      tweets %>%
      filter(name %in% params$names_main)

    # TODO: Need to validate length of params$names vs. screen params$names,
    # that name_main is in params$names, etc.
    # browser()
    if (is.null(params$names)) {
      if (params$augmented) {
        colname <- params$augmented_colname
      } else {
        colname <- "screen_name"
      }
      params$names <-
        tweets %>%
        distinct(!!rlang::sym(colname)) %>%
        arrange(!!rlang::sym(colname)) %>%
        pull(!!rlang::sym(colname))
    } else {
      params$names <- params$names
    }
  } else if (params$download) {
    if (is.null(screen_names)) {
      stop(sprintf("Please provide `screen_names`."), call. = FALSE)
    }

    if (method == "timeline") {
      tweets <- try(rtweet::get_timelines(params$screen_names))
    } else if (method == "search") {
      tweets <- try(rtweet::search_tweets(params$screen_names))
    }
    if (inherits(tweets, "try-error")) {
      stop(sprintf("Could not download tweets."), call. = FALSE)
    }
    params$names <- params$names
  } else {
    stop("An unexpected combination of inputs was provided.", call. = FALSE)
  }
  out <- c(list(tweets = tweets, names = params$names), params)
  out
}
params <- implement_params(params)

#'
#'
#'
# Process. ----
# `time` is replicated from https://buzzfeednews.github.io/2018-01-trump-twitter-wars/.
# Other columns are replicated from https://juliasilge.com/blog/ten-thousand-tweets/.
cols_keep <-
  c(
    "name",
    "status_id",
    "created_at",
    "user_id",
    "screen_name",
    "text",
    "display_text_width",
    "reply_to_status_id",
    "is_quote",
    "is_retweet",
    "favorite_count",
    "retweet_count",
    "hashtags",
    "symbols",
    "urls_url",
    "urls_expanded_url",
    "media_expanded_url",
    "ext_media_expanded_url"
  )

# See https://github.com/mkearney/rstudioconf_tweets/blob/master/README.Rmd.
round_time <- function(x, sec) {
  as.POSIXct(hms::hms(as.numeric(x) %/% sec * sec))
}

# START. ----
tweets <-
  params$tweets %>%
  select(one_of(c(cols_keep))) %>%
  mutate(timestamp = lubridate::ymd_hms(created_at)) %>%
  # mutate(timestamp = lubridate::mdy_hm(created_at)) %>%
  mutate(timestamp = lubridate::with_tz(timestamp, "America/Chicago")) %>%
  mutate(time_fromorigin = as.numeric(timestamp - trunc(timestamp, "days"))) %>%
  mutate(time_fromorigin = as.POSIXct(time_fromorigin, origin = "1970-01-01")) %>%
  mutate(time = round_time(timestamp, 60 * 60)) %>%
  mutate(time = lubridate::hour(timestamp) + lubridate::minute(timestamp) / 60) %>%
  mutate(text_plain = rtweet::plain_tweets(text))

#'
#'
#'
# + include = FALSE, eval = FALSE
# # Check for bad values...
# tweets %>%
#   mutate(
#     hour = lubridate::hour(timestamp),
#     min = lubridate::minute(timestamp),
#     sec = lubridate::second(timestamp)
#   ) %>%
#   filter(hour == 0 & min == 0 & sec == 0) %>%
#   nrow()

#'
#'
#'
# Modified from a SO answer.
compute_elapsed_time <- function(date_start, date_end, type) {
  if (type == "years" | type == "months") {
    date_start <- as.POSIXlt(date_start)
    date_end <- as.POSIXlt(date_end)
    if (type == "years") {
      out <- (date_end$year - date_start$year) - 1
    } else if (type == "months") {
      out <- 12 * (date_end$year - date_start$year) + (date_end$mon - date_start$mon) - 1
    }
  } else if (type == "days" | type == "hours") {
    out <-
      (difftime(date_end, date_start, units = type) - 1) %>%
      round(0) %>%
      as.numeric()
  }
  out
}

compute_tweet_timefilter <- function(data, colnames_group = "name") {
  data_proc <-
    data %>%
    group_by(!!!rlang::syms(colnames_group)) %>%
    arrange(timestamp) %>%
    mutate(date_start = first(timestamp),
           date_end = last(timestamp)) %>%
    slice(1) %>%
    ungroup() %>%
    select(name, date_start, date_end) %>%
    mutate(
      yyyy_elapsed = compute_elapsed_time(date_start, date_end, "years"),
      mm_elapsed = compute_elapsed_time(date_start, date_end, "months"),
      dd_elapsed = compute_elapsed_time(date_start, date_end, "days"),
      hh_elapsed = compute_elapsed_time(date_start, date_end, "hours")
    )
  out <-
    list(data = data_proc, date_start = max(data_proc$date_start), date_end = min(data_proc$date_end))
  out
}

tweet_timefilter <-
  tweets %>%
  compute_tweet_timefilter()

#'
#'
#'
#+ eval = (params$trim), results = "asis", fig.show = "asis"
# tweets ----
# This is "original" processing needed to trim tweets appropriately/dynamically
# given an unknown data set.
trim_tweets_bytime <-
  function(data,
           colname_time = "timestamp",
           start = NULL,
           end = NULL) {
    # browser()
    out <-
      data %>%
      filter(!!rlang::sym(colname_time) < end, !!rlang::sym(colname_time) >= start)
    out
  }

# cat(
#   sprintf(
#     "The tweets were trimmed in order to align the dates of the
#     most recent first tweet and the oldest last tweet."
#   )
# )
if(params$trim) {
  tweets <-
    tweets %>%
    trim_tweets_bytime(start = tweet_timefilter$date_start, end = tweet_timefilter$date_end)
}
#'
#' # Tweet Volume
#'
#' How often does each Twitter handle tweet?
#' Does the volume of tweets look different for
#' temporal periods?
#'
# Inspired by https://juliasilge.com/blog/ten-thousand-tweets/ here.
# cnt_bytime ----
# NOTE: This function takes a gg object as an input
# (so it should/can be preceded by a pipe, and, if followed by other ggplot2 commands,
#  succedeeded by a `+`.)
add_viz_bytime_elements <-
  function(viz,
           geom = c("bar", "hist"),
           colors = params$pal_main) {
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

if (min(tweet_timefilter$data$dd_elapsed) >= params$dd_cnt_min) {
  lab_subtitle <-
    paste0(
      "From ",
      strftime(tweet_laststart, "%Y-%m-%d"),
      " to ",
      strftime(tweet_firstend, "%Y-%m-%d")
    )

  viz_bytime_all <-
    tweets %>%
    ggplot(aes(x = timestamp)) %>%
    add_viz_bytime_elements(geom = "hist") +
    labs(subtitle = lab_subtitle)
  viz_bytime_all
}

if (min(tweet_timefilter$data$yyyy_elapsed) >= params$yyyy_cnt_min) {
  viz_bytime_yyyy <-
    tweets %>%
    ggplot(aes(x = lubridate::year(timestamp))) %>%
    add_viz_bytime_elements(geom = "bar") +
    labs(subtitle = "By Year")
  viz_bytime_yyyy
}

if (min(tweet_timefilter$data$mm_elapsed) >= params$mm_cnt_min) {
  viz_bytime_mm <-
    tweets %>%
    ggplot(aes(x = lubridate::month(timestamp))) %>%
    add_viz_bytime_elements(geom = "bar") +
    labs(subtitle = "By Month")
  viz_bytime_mm
}

if (min(tweet_timefilter$data$dd_elapsed) >= params$wday_cnt_min) {
  viz_bytime_wday <-
    tweets %>%
    ggplot(aes(x = lubridate::wday(timestamp, label = TRUE))) %>%
    add_viz_bytime_elements(geom = "bar") +
    labs(subtitle = "By Day of Week")
  viz_bytime_wday
}

if (min(tweet_timefilter$data$hh_elapsed) >= params$hh_cnt_min) {
  viz_bytime_hh <-
    tweets %>%
    ggplot(aes(x = time_fromorigin)) %>%
    add_viz_bytime_elements(geom = "hist") +
    scale_x_datetime(
      breaks = scales::date_breaks("4 hours"),
      labels = scales::date_format("%H")
    ) +
    labs(subtitle = "By Hour of Day")
  viz_bytime_hh

  viz_bytime_hh_2 <-
    tweets %>%
    ggplot(aes(x = name, y = time, fill = name)) +
    scale_y_continuous(limits = c(1,24),
                       breaks = c(6,12,18),
                       labels = c("6am","Noon","6pm")) +
    scale_fill_manual(values = params$colors_main, guide = FALSE) +
    geom_violin(size = 0, alpha = 0.7) +
    geom_hline(yintercept = seq(3, 24, by = 3), color = "gray", size = 0.1) +
    labs(x = NULL, y = NULL, title = "Distribution of Tweets By Time of Day") +
    temisc::theme_te_dx() +
    theme(panel.grid = element_blank()) +
    coord_flip()
  viz_bytime_hh_2
}

#'
#'
#'
# + results = "asis", fig.show = "asis"
if(exists("viz_bytime_all")) viz_bytime_all
if(exists("viz_bytime_yyyy")) viz_bytime_yyyy
if(exists("viz_bytime_mm")) viz_bytime_mm
if(exists("viz_bytime_wday")) vviz_bytime_wday
if(exists("viz_bytime_hh")) vviz_bytime_hh
if(exists("viz_bytime_hh_2")) vviz_bytime_hh_2
if(length(ls(pattern = "^viz_bytime_")) == 0) {
  cat(
    sprintf(
      "Tweets do not meet any criteria for temporal visualization."
    )
  )
}

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
add_tweet_kind_data <- function(data) {
  out <-
    data %>%
    mutate(
      hashtag = if_else(!is.na(hashtags), 1, 0),
      link = if_else(!is.na(media_expanded_url) & !is.na(ext_media_expanded_url), 1, 0),
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
  out
}

compute_pct <- function(x,
                        value = 1,
                        digits_round = 4) {
  # round(sum(x == value) / sum(!is.na(x)), digits_round)
  sum(x == value) / sum(!is.na(x))
}

tweets <-
  tweets %>%
  add_tweet_kind_data()

kinds <- c(params$kinds_features, params$kinds_types)
cols_summarize <- kinds
tweets_byname_bykind_summary_tidy <-
  tweets %>%
  group_by(name) %>%
  summarize_at(vars(c(cols_summarize)), funs(compute_pct(.))) %>%
  ungroup() %>%
  tidyr::gather(kind, value, -name)
tweets_byname_bykind_summary_tidy

visualize_byname_bykind <-
  function(data,
           geom = c("col", "lollipop"),
           colors = params$colors_main,
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
        ggalt::geom_lollipop(size = 2, point.size = 4) +
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
    viz
  }

viz_byname_bytype <-
  tweets_byname_bykind_summary_tidy %>%
  filter(kind %in% params$kinds_types) %>%
  mutate(kind = str_to_title(kind)) %>%
  visualize_byname_bykind() +
  labs(subtitle = "Types")
viz_byname_bytype

viz_byname_byfeature <-
  tweets_byname_bykind_summary_tidy %>%
  filter(kind %in% params$kinds_features) %>%
  mutate(kind = str_to_title(kind)) %>%
  visualize_byname_bykind() +
  labs(subtitle = "Features")
viz_byname_byfeature

# Inspired by https://juliasilge.com/blog/ten-thousand-tweets/ here.
if (min(tweet_timefilter$data$dd_elapsed) >= params$dd_cnt_min) {
  viz_byname_bytype_temporal <-
    tweets %>%
    ggplot(aes(x = timestamp, fill = type)) +
    geom_histogram(position = "fill", bins = 30) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = viridis::viridis(n = length(params$kinds_types) + 1)) +
    facet_wrap(~ name,
               ncol = 1,
               scales = "free",
               strip.position = "right") +
    labs(x = NULL, y = NULL) +
    labs(title = "Distribution of Tweets by Type Over Time") +
    temisc::theme_te_facet() +
    theme(panel.grid.major.x = element_blank()) +
    theme(legend.position = "bottom", legend.title = element_blank())
  viz_byname_bytype_temporal
}

#'
#'
#'
#+ results = "asis", fig.show = "asis"
viz_byname_bytype
if(exists("viz_bytype_temporal")) viz_byname_bytype_temporal
viz_byname_byfeature

#'
#' # Tweet Content
#'
#' How long are the tweets?
#'
#+ include = FALSE
# Note that there are some tweets above 140 characters.
# Inspired by https://juliasilge.com/blog/ten-thousand-tweets/ here.
viz_byname_chars_cnt <-
  tweets %>%
  # mutate(chars_cnt = str_length(text)) %>%
  mutate(chars_cnt = display_text_width) %>%
  ggplot(aes(x = chars_cnt)) +
  geom_density(aes(fill = name)) +
  scale_fill_manual(values = params$colors_main) +
  scale_x_continuous(limits = c(0, 200)) +
  facet_wrap( ~ name, scales = "free") +
  labs(x = NULL, y = NULL) +
  labs(title = "Distribution of Characters Per Tweet") +
  temisc::theme_te_facet() +
  theme(panel.grid.major.x = element_blank()) +
  theme(legend.position = "none")
viz_byname_chars_cnt

#'
#'
#'
#+ results = "asis", fig.show = "asis"
viz_byname_chars_cnt

#'
#' ## Word Frequency and Usage
#'
#' Which words are used most frequently?
#'
# Inspired by https://www.tidytextmining.com/twitter.html here.
# Bigrams inspired by https://www.tidytextmining.com/ngrams.html and
# https://buzzfeednews.github.io/2018-01-trump-twitter-wars/.
# tweets_tidy ----
# library("tidytext")
rgx_unnest <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
rgx_pattern <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"

tweets_tidy <-
  tweets %>%
  # filter(!str_detect(text, "^(RT|@)")) %>%
  filter(is_retweet == FALSE) %>%
  mutate(text = text_plain) %>%
  mutate(text = str_replace_all(text, rgx_pattern, "")) %>%
  tidytext::unnest_tokens(word, text, token = "regex", pattern = rgx_unnest) %>%
  anti_join(tidytext::stop_words, by = "word") %>%
  filter(str_detect(word, "[a-z]"))
tweets_tidy %>% select(word, name, status_id) %>% count(word, sort = TRUE)

tweets_tidy_bigrams <-
  tweets %>%
  mutate(text = text_plain) %>%
  filter(is_retweet == FALSE) %>%
  tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("first","second"), sep = " ", remove = FALSE) %>%
  anti_join(tidytext::stop_words, by = c("first" = "word")) %>%
  anti_join(tidytext::stop_words, by = c("second" = "word")) %>%
  filter(str_detect(first, "[a-z]"), str_detect(second, "[a-z]"))
tweets_tidy_bigrams %>% select(bigram, name, status_id) %>% count(bigram, sort = TRUE)

# Inspired by https://github.com/dgrtwo/dgrtwo.github.com/blob/master/_R/2016-08-09-trump-tweets.Rmd here.
# TODO: Improve this.
visualize_byname_cnt <-
  function(data,
           num_top = 20,
           facet = FALSE,
           colors = params$pal_main) {
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
        ggalt::geom_lollipop(aes(color = name), size = 2, point.size = 2) +
        scale_fill_manual(values = colors) +
        facet_wrap( ~ name, scales = "free") +
        temisc::theme_te_facet() +
        labs(subtitle = "By Name")
    } else {
      viz <-
        data_proc %>%
        ggplot(aes(x = word, y = n)) +
        # geom_bar(stat = "identity") +
        ggalt::geom_lollipop(size = 2, point.size = 2) +
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
viz_byname_cnt <-
  tweets_tidy %>%
  visualize_byname_cnt()
viz_byname_cnt

# viz_byname_cnt_byname <-
#   tweets_tidy %>%
#   visualize_byname_cnt(facet = TRUE)
# viz_byname_cnt_byname
#'
#'
#'
#+ results = "asis", fig.show = "asis"
viz_byname_cnt
# viz_byname_cnt_byname

#'
#'
#'
unigrams_cnt <-
  tweets_tidy %>%
  # group_by(name) %>%
  count(name, sort = TRUE)
unigrams_cnt

bigrams_cnt <-
  tweets_tidy_bigrams %>%
  count(name, sort = TRUE)
  # count(bigram, sort = TRUE)
bigrams_cnt

# freqs ----
unigrams_byname_freqs <-
  tweets_tidy %>%
  count(name, word, sort = TRUE) %>%
  left_join(unigrams_cnt %>% rename(total = n), by = "name") %>%
  mutate(freq = n / total)
unigrams_byname_freqs

bigrams_byname_freqs <-
  tweets_tidy_bigrams %>%
  count(name, bigram, sort = TRUE) %>%
  left_join(bigrams_cnt %>% rename(total = n), by = "name") %>%
  mutate(freq = n / total)
bigrams_byname_freqs

# Debugging...
unigrams_byname_freqs %>%
  inner_join(unigrams_byname_freqs, by = "word", suffix = c("_x", "_y")) %>%
  filter(name_x != name_y)
bigrams_byname_freqs %>%
  inner_join(bigrams_byname_freqs, by = "bigram", suffix = c("_x", "_y")) %>%
  filter(name_x != name_y)

num_top_bigram_freq <- floor(12 / length(params$names_main))
bigrams_byname_freqs_viz <-
  bigrams_byname_freqs %>%
  group_by(name) %>%
  mutate(rank = row_number(desc(freq))) %>%
  filter(rank <= num_top_bigram_freq) %>%
  ungroup() %>%
  arrange(name) %>%
  mutate(bigram = str_replace_all(bigram, " ", "\n")) %>%
  mutate(bigram = forcats::fct_reorder(factor(bigram), freq))
bigrams_byname_freqs_viz

viz_bigrams_byname_freqs <-
  bigrams_byname_freqs_viz %>%
  ggplot(aes(x = name, y = bigram, color = name, size = freq)) +
  geom_point() +
  scale_y_discrete(position = "right") +
  scale_color_manual(values = params$colors_main) +
  scale_size_area(max_size = 25) +
  labs(x = NULL, y = NULL) +
  labs(title = "Top Bigrams") +
  temisc::theme_te(base_family = "") +
  theme(legend.position = "none") +
  coord_flip()
viz_bigrams_byname_freqs

#'
#'
#'
#+ results = "asis", fig.show = "asis"
bigrams_byname_freqs_viz

#'
#'
#'
# TODO: Word cloud (bigram only)
visualize_bigrams_byname_freqs_wordcloud <-
  function(name_filter, data = bigrams_byname_freqs) {
    data_proc <-
      data %>%
      filter(name == name_filter)

    color <-
      params$colors_main[names(params$colors_main) == name_filter]
    out <-
      wordcloud::wordcloud(
        word = data_proc$bigram,
        freq = data_proc$n,
        max.words = 50,
        random.order = FALSE,
        colors = color
      )
    out
  }
num_par_row <- ceiling(length(params$names_main) / 3)
num_par_col <- min(length(params$names_main), 3)

# viz_bigrams_byname_freqs_wordclouds <-
#   lapply(params$names_main, visualize_bigrams_byname_freqs_wordcloud)

#'
#'
#'
#+ results = "asis", fig.show = "asis"
bigrams_byname_freqs_viz
# par(mfrow = c(num_par_row, num_par_col))
# lapply(params$names_main, visualize_bigrams_byname_freqs_wordcloud)
# par(mfrow = c(1, 1))

#'
#'
#'

names_distinct <- unigrams_byname_freqs %>% distinct(name) %>% pull(name)
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
  # browser()
  out <-
    data %>%
    mutate(name_x = xy_info$x, name_y = xy_info$y) %>%
    mutate(name_xy = paste0(name_x, "_", name_y))
  if (length(setdiff(c(xy_info$x, xy_info$y), names(data))) == 0) {
    out <-
      out %>%
      rename(x = !!rlang::sym(xy_info$x),
             y = !!rlang::sym(xy_info$y)) %>%
      select(name_x, name_y, name_xy, x, y, everything())
  } else {
    out <- out %>% select(name_x, name_y, name_xy, everything())
  }
  out
}

compute_unigrams_freqs <- function(data) {
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
    # browser()
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

unigrams_freqs_wide <-
  wrapper_func(xy_names = xy_names,
               data = unigrams_byname_freqs,
               func = compute_unigrams_freqs)
unigrams_freqs_wide

# Debugging...
unigrams_freqs_wide %>%
  count(name_xy, name_x) %>%
  group_by(name_x) %>%
  filter(row_number(desc(n)) <= 2) %>%
  ungroup() %>%
  distinct(n, .keep_all = TRUE)

# TODO: Need to make this more dynamic (with if statements)...
# unigrams_freqs_wide_top <-
#   unigrams_freqs_wide %>%
#   count(name_xy, name_x) %>%
#   group_by(name_x) %>%
#   filter(row_number(desc(n)) == 1)

unigrams_freqs_wide_viz <-
  unigrams_freqs_wide %>%
  # semi_join(unigrams_freqs_wide_top, by = "name_xy") %>%
  filter(name_x == name_main) %>%
  mutate(name_xy = paste0(name_x, " vs. ", name_y))

viz_unigrams_freq <-
  unigrams_freqs_wide_viz %>%
  ggplot(aes(x = x, y = y)) +
  # geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word, size = x + y), check_overlap = TRUE) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  geom_abline(color = "red") +
  facet_wrap( ~ name_xy, scales = "free") +
  labs(x = NULL, y = NULL) +
  labs(title = "Relative Word Frequency") +
  temisc::theme_te_facet(base_family = "") +
  theme(legend.position = "none")
viz_unigrams_freq


#'
#'
#'
#+ results = "asis", fig.show = "asis"
viz_unigrams_ratios

#'
#' Which words are most likely to be used by one name compared to the other?
#'
# unigrams_ratios ----
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

unigrams_ratios_wide <-
  wrapper_func(xy_names = xy_names,
               data = tweets_tidy,
               func = compute_logratio)
unigrams_ratios_wide %>% count(name_xy, sort = TRUE)

# These words are the most and least likely to be tweeted by either name.
unigrams_ratios_wide %>% group_by(name_xy) %>% arrange(abs(logratio))
unigrams_ratios_wide %>% group_by(name_xy) %>% arrange(desc(abs(logratio)))

unigrams_ratios_wide_viz <-
  unigrams_ratios_wide %>%
  mutate(logratio_dir = (logratio < 0)) %>%
  group_by(name_xy, logratio_dir) %>%
  # top_n(15, abs(logratio)) %>%
  arrange(name_xy, desc(abs(logratio))) %>%
  slice(1:5) %>%
  ungroup() %>%
  # mutate(word = reorder(word, logratio)) %>%
  filter(name_x == name_main) %>%
  mutate(word = reorder(word, -logratio)) %>%
  mutate(name_xy = paste0(name_x, " vs. ", name_y))

# pal_rdbu <- RColorBrewer::brewer.params$pal_main(n = 3, name = "RdBu")
# pal_rdbu_red <- pal_rdbu[1]
# pal_rdbu_blue <- pal_rdbu[length(pal_rdbu)]
# pal_rdbu_rb <- c(pal_rdbu_red, pal_rdbu_blue)
color_main_inv <- temisc::get_color_hex_inverse(params$color_main)
colors_dual <- c(params$color_main, color_main_inv)
names(colors_dual) <- NULL # c(params$color_main, "other")
viz_unigrams_ratios <-
  unigrams_ratios_wide_viz %>%
  ggplot(aes(x = word, y = logratio, fill = logratio_dir)) +
  geom_col() +
  facet_wrap(~ name_xy, scales = "free") +
  # scale_fill_manual(values = c("red", "cyan")) +
  # scale_fill_manual(values = pal_rdbu_rb) +
  scale_fill_manual(values = colors_dual, labels = c(params$color_main, "other")) +
  coord_flip() +
  labs(x = NULL, y = "log odds ratio") +
  labs(title = "Most Unique Words") +
  temisc::theme_te_facet() +
  # theme(legend.position = "none") +
  theme(legend.position = "bottom", legend.title = element_blank())
viz_unigrams_ratios

#'
#'
#'
#+ results = "asis", fig.show = "asis"
viz_unigrams_ratios
#'
#'
#'
# Inspired by https://www.tidytextmining.com/twitter.html here.
# TODO: change_bytime? ----

#'
#'
#'
# Inspired by https://www.tidytextmining.com/twitter.html here.
# TODO: byfeature_bypop? ----

#'
#' # Sentiment Analysis
#'
#' What is the sentiment (i.e. "tone") of the tweets?
#'
# sents_diffs ----
bing <-
  tidytext::sentiments %>%
  filter(lexicon == "bing") %>%
  select(word, sentiment)

afinn <-
  tidytext::sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, sentiment)

unigrams_cnt_bytweet <-
  tweets_tidy %>%
  group_by(name) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  distinct(status_id, name, total_words)
unigrams_cnt_bytweet

sents_bing_byname <-
  tweets_tidy %>%
  inner_join(bing, by = "word") %>%
  count(status_id, sentiment) %>%
  tidyr::complete(sentiment, status_id, fill = list(n = 0)) %>%
  inner_join(unigrams_cnt_bytweet, by = "status_id") %>%
  group_by(name, sentiment, total_words) %>%
  summarize(words = sum(n)) %>%
  ungroup()
sents_bing_byname

compute_sentdiffs_poisson <- function(data) {
  out <-
    data %>%
    group_by(sentiment) %>%
    do(broom::tidy(poisson.test(.$words, .$total_words))) %>%
    ungroup()
  out
}

sents_bing_diffs_poisson <-
  wrapper_func(xy_names = xy_names,
               data = sents_bing_byname,
               func = compute_sentdiffs_poisson)
sents_bing_diffs_poisson

sents_bing_diffs_poisson_viz <-
  sents_bing_diffs_poisson %>%
  filter(name_x == name_main) %>%
  mutate(name_xy = paste0(name_x, " vs. ", name_y)) %>%
  mutate(sentiment = reorder(sentiment, estimate)) %>%
  # mutate_each(funs(. - 1), estimate, conf.low, conf.high) %>%
  mutate_at(vars(estimate, conf.low, conf.high), funs(. - 1))

viz_sents_bing_diffs_poisson <-
  sents_bing_diffs_poisson_viz %>%
  ggplot(aes(x = estimate, y = sentiment)) +
  geom_point(size = 2) +
  # geom_point(aes(color = name_x), size = 2) +
  # scale_color_manual(values = params$pal_main) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), linetype = "solid") +
  geom_vline(aes(xintercept = 0), size = 2) +
  facet_wrap(~ name_xy, scales = "free") +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(x = NULL, y = NULL) +
  labs(title = "Sentiment Analysis") +
  temisc::theme_te_facet() +
  theme(legend.position = "none")
viz_sents_bing_diffs_poisson

#'
#'
#'
# results =  "asis", fig.show = "asis"
viz_sents_bing_diffs_poisson

#'
#'
#'
sents_bing_ratios_wide_viz_0 <-
  unigrams_ratios_wide %>%
  inner_join(nrc, by = "word") %>%
  # filter(!sentiment %in% c("positive", "negative")) %>%
  # filter(sentiment %in% c("positive", "negative")) %>%
  filter(name_x == name_main) %>%
  group_by(word, sentiment) %>%
  mutate(logratio = mean(logratio)) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, -logratio),
         word = reorder(word, -logratio)) %>%
  # group_by(name_xy, sentiment) %>%
  group_by(sentiment) %>%
  mutate(rank = row_number(desc(logratio))) %>%
  filter(rank <= 2 | rank >= (max(rank) - 1)) %>%
  ungroup()
sents_ratios_wide_viz_0

# Already filtered with the previous statement, so this isn't really neceessary...
sents_bing_ratios_wide_viz <-
  sents_bing_ratios_wide_viz_0
sents_bing_ratios_wide_viz

viz_sents_bing_ratios <-
  sents_bing_ratios_wide_viz %>%
  ggplot(aes(x = word, y = logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  geom_hline(aes(yintercept = 0)) +
  scale_fill_manual(values = params$pal_main, labels = c(name_main, "other")) +
  facet_wrap( ~ sentiment, scales = "free") +
  labs(x = NULL, y = "Log Odds Ratio") +
  labs(title = "Most Influential Words Contributing to Sentiment Differences") +
  temisc::theme_te_facet_dx() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "bottom", legend.title = element_blank())
viz_sents_bing_ratios

#'
#'
#'
#+ results = "asis", fig.show = "asis"
viz_sents_bing_ratios
#'
#'
#'
# Inspired by http://varianceexplained.org/r/seven-fav-packages/ and
# http://varianceexplained.org/r/stacksurveyr/ here.
# new ----

unigrams_byword_cnt <-
  tweets_tidy %>%
  count(word, sort = TRUE)

num_top_cnt <- 100
unigrams_byword_cnt_top <-
  unigrams_byword_cnt %>%
  mutate(rank = row_number(desc(n))) %>%
  filter(rank <= num_top_cnt)

unigrams_bytweet_corrs <-
  tweets_tidy %>%
  semi_join(unigrams_byword_cnt_top, by = "word") %>%
  widyr::pairwise_cor(word, status_id, sort = TRUE, upper = FALSE)
unigrams_bytweet_corrs

num_top_corr <- 50
# Arrange this way to see what the lowest correlations are.
unigrams_bytweet_corrs_viz <-
  unigrams_bytweet_corrs %>%
  mutate(rank = row_number(desc(correlation))) %>%
  filter(rank <= num_top_corr)
unigrams_bytweet_corrs_viz
unigrams_bytweet_corrs_viz %>% arrange(desc(rank))

set.seed(42)
viz_unigrams_bytweet_corrs <-
  unigrams_bytweet_corrs_viz %>%
  igraph::graph_from_data_frame(vertices = unigrams_byword_cnt_top) %>%
  ggraph::ggraph(layout = "fr") +
  ggraph::geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation)) +
  ggraph::geom_node_point(aes(size = n), color = params$pal_main[1]) +
  ggraph::geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  # labs(title = "Word Correlations") +
  theme(legend.position = "none")
viz_unigrams_bytweet_corrs

#'
#'
#'



