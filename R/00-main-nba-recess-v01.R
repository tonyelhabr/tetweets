

rm(list = ls())
library("dplyr")

filepath_input <- "R/analyze-v03.R"
filepath_tweets <- file.path("data", "recess.rds")
filepath_tweets_augmented <- file.path("data", "recess-augmented.rds")

dd_offset <- 1
dd_lag <- 6
time_scaling_factor <- (60 * 60 * 24)
tweets <- readRDS(filepath_tweets)

tweets_proc <-
  tweets %>%
  # filter(created_at >= Sys.Date() - 6) %>%
  filter(created_at < max(created_at) - (dd_offset) * time_scaling_factor) %>%
  filter(created_at >= max(created_at) - (dd_lag - dd_offset) * time_scaling_factor) %>%
  arrange(created_at) %>%
  mutate(yyyymmdd = format(lubridate::ymd(format(created_at, "%Y-%m-%d")))) %>%
  mutate(yyyymmdd_char = strftime(yyyymmdd, "%Y-%m-%d"),
         md_char = strftime(yyyymmdd, "%m-%d")) %>%
  # select(yyyymmdd, yyyymmdd_char, md_char, everything()) %>%
  select(-yyyymmdd, -md_char) %>%
  select(name = yyyymmdd_char, everything())
tweets_proc
tweets_proc %>% count(name, sort = TRUE)

names_main <- tweets_proc %>% distinct(name) %>% arrange(name) %>% pull(name)
name_main <- rev(names_main)[1]
colors_main <- viridis::viridis(n = length(names_main))
color_main <- rev(colors_main)[1]

saveRDS(tweets_proc, filepath_tweets_augmented)
rm(list = c("tweets", "tweets_proc", "filepath_tweets"))

yyyymmdd <-  format(Sys.Date(), "%Y-%m-%d")
dir_output <- "output"
md_start <- strftime(rev(names_main)[1], "%m-%d")
md_end <- strftime(names_main[1], "%m-%d")
yyyy <- strftime(rev(names_main)[1], "%Y")
filename_output <- paste0("nbatwitter", "-", tolower(name_main), "-", paste0("last", dd_lag), "-", yyyymmdd)
report_title_i <- paste0("Analysis of NBA Twitter Tweets From ", md_start, " To ", md_end, ", ", yyyy)

params_i <-
  list(
    # These only need to be specified if running the analysis script interactively.
    dd_cnt_min = 1,
    yyyy_cnt_min = 2,
    mm_cnt_min = 12,
    wday_cnt_min = 7,
    hh_cnt_min = 2,
    download = FALSE,
    download_method = "search",
    screen_names = NULL,
    tweets_min_download = 1000,
    num_main_max = 6,
    filepath_tweets = filepath_tweets_augmented,
    name_main = name_main,
    names_main = names_main,
    augmented = TRUE,
    augmented_col = "name",
    color_main = color_main,
    colors_main = colors_main,
    tweet_cnt_min = 1000,
    trim_time = FALSE,
    kinds_features = c("hashtag", "link"),
    kinds_types = c("quote", "reply"),
    report_title = report_title_i
  )
# params <- params_i

do <- TRUE
if(do) {
  out <-
    teproj::render_proj_io(
      # render = FALSE,
      # keep_rmd = TRUE,
      render_params = params_i,
      filepath_input = filepath_input,
      filename_output = filename_output,
      dir_output = dir_output
    )
  out

}


