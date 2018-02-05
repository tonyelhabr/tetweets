#' ---
#' title: "Analysis"
#' output: html_document
#' params:
#'   tweets: NULL
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
#'   pal_main: NULL
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
#' ---
#'
#+ global_setups
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# params <- params_i
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

#'
#+ validate_params
params_proc <- validate_params(params)

#'
#+ process_params

params_proc <- process_params(params_proc)
#'
#+ remove_params
# rm(list = c("params", "params_valid"))

# Process. ----
tweets <- clean_tweets(params_proc$tweets, cols_extra = params_proc$augmented_col)
tweets_tidy_unigrams <-
  tweets %>%
  mutate(text = text_plain) %>%
  tidy_tweets_unigrams()
unigrams_cnt <-
  tweets_tidy_unigrams %>%
  count(name, sort = TRUE)
unigrams_byname_freqs <-
  tweets_tidy_unigrams %>%
  count(name, word, sort = TRUE) %>%
  left_join(unigrams_cnt %>% rename(total = n), by = "name") %>%
  mutate(freq = n / total)

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
#+ ngrams_freqs_wide
# wrapper_func <- function(names, data, func) {
#   i <- 1
#   while (i < length(names)) {
#     xy_i_info <- filter_xy_names(names, i)
#     data_i_preproc <- preprocess_xy_data(data, xy_i_info)
#     # browser()
#     data_i_proc <- do.call(func, list(data_i_preproc))
#     # browser()
#     data_i_postproc <- postprocess_xy_data(data_i_proc, xy_i_info)
#     if (i == 1) {
#       out <- data_i_postproc
#     } else {
#       out <- bind_rows(out, data_i_postproc)
#     }
#     i <- i + 1
#   }
#   out
# }

unigrams_freqs_wide <-
  wrapper_func(names = xy_names,
               data = unigrams_byname_freqs,
               func = compute_unigrams_freqs)

