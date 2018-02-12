

rm(list = ls())
library("dplyr")
library("teproj")

# NOTE: This is probably best to do manually.
redownload <- FALSE
if (redownload) {
  filepath_google_token <- "../google_token.rds"
  filepath_data_gdrive <- file.path("data", "recess.rds")
  filepath_data_local <- file.path("data", "recess.rds")
  filepath_data_local_backup <-
    file.path("data", paste0("recess", "-", format(Sys.Date(), "%Y-%m-%d"), ".", "rds"))

  googledrive::drive_auth(filepath_google_token)
  dribble <- googledrive::drive_get(filepath_data_gdrive)
  if (nrow(dribble) != 1) {
    stop("Oops! Found more than one file!")
  }

  file.exists(filepath_data_local)
  file.copy(from = filepath_data_local, to = filepath_data_local_backup)
  file.exists(filepath_data_local_backup)
  filepath_data_dl_actual <-
    googledrive::drive_download(dribble$path, path = filepath_data_local, overwrite = TRUE)

  if(!identical(filepath_data_dl_actual$local_path, filepath_data_local)) {
    stop("Oops! There is something wrong with the downloaded file!")
  }
}

filepath_input <- "R/analyze-v03.R"
filepath_tweets <- file.path("data", "recess.rds")
filepath_tweets_augmented <- file.path("data", paste0("recess", "-", "augmented.rds"))

dd_offset <- 2
dd_lag <- 6
time_scaling_factor <- (60 * 60 * 24)
tweets <- readRDS(filepath_tweets)

tweets_proc <-
  tweets %>%
  # filter(created_at >= Sys.Date() - 6) %>%
  filter(created_at < (max(created_at) - (dd_offset * time_scaling_factor))) %>%
  filter(created_at >= (max(created_at) - ((dd_lag - 1) * time_scaling_factor))) %>%
  arrange(created_at) %>%
  mutate(yyyymmdd = format(lubridate::ymd(format(created_at, "%Y-%m-%d")))) %>%
  mutate(
    yyyymmdd_char = strftime(yyyymmdd, "%Y-%m-%d"),
    mdy_char = strftime(yyyymmdd, "%m-%d-%y"),
    md_char = strftime(yyyymmdd, "%m-%d")
  )
yyyymmdd_chars_distinct <-
  tweets_proc %>%
  distinct(yyyymmdd_char) %>%
  arrange(yyyymmdd_char) %>%
  pull(yyyymmdd_char)
yyyymmdd_chars_distinct

if(length(yyyymmdd_chars_distinct) != dd_lag) {
  stop("Unexpected number of distinct names found.")
}

tweets_proc <-
  tweets_proc %>%
  # mutate(md_char = factor(md_char, levels = md_chars_distinct)) %>%
  select(-yyyymmdd, -yyyymmdd_char, -md_char) %>%
  # select(name = yyyymmdd_char, everything())
  select(name = mdy_char, everything())
unique(tweets_proc$name)
tweets_proc %>% count(name, sort = TRUE)

names_main <- tweets_proc %>% distinct(name) %>% arrange(name) %>% pull(name)
name_main <- names_main[1]
colors_main <- viridis::viridis(n = length(names_main))
color_main <- rev(colors_main)[1]

saveRDS(tweets_proc, filepath_tweets_augmented)
rm(list = c("tweets", "tweets_proc", "filepath_tweets"))

yyyymmdd <-  format(Sys.Date(), "%Y-%m-%d")
dir_output <- "output"
# mdy_start <- strftime(yyyymmdd_chars_distinct[1], "%m-%d-%y")
# mdy_end <- strftime(rev(yyyymmdd_chars_distinct)[1], "%m-%d-%y")
mdy_start <- names_main[1]
mdy_end <- rev(names_main)[1]
# yyyy <- strftime(yyyymmdd_chars_distinct[1], "%Y")
yyyy <- format(Sys.Date(), "%Y")
filename_output <- paste0("nbatwitter", "-", tolower(name_main), "-", paste0("last", dd_lag), "-", yyyymmdd)
report_title_i <- paste0("Analysis of NBA Twitter Tweets From ", mdy_start, " To ", mdy_end)

params_i <-
  list(
    # These only need to be specified if running the analysis script interactively.
    dd_cnt_min = 1,
    yyyy_cnt_min = 2,
    mm_cnt_min = 12,
    wday_cnt_min = 5,
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
    kinds_types = c("quote", "reply", "rt"),
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


