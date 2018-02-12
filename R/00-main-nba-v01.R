
rm(list = ls())
library("dplyr")
library("teproj")

filepath_input <- file.path("R", "analyze-v03.R")

filename_tweets <- "tweets-search-nba"
ext_tweets <- "rds"
filepath_tweets_augmented <- file.path("data", "rds")
filepath_tweets_augmented <- file.path("data", "tweets-search-nba-augmented.rds")

dir_output <- "output"
yyyymmdd <-  format(Sys.Date(), "%Y-%m-%d")

filepath_db_nba <- "O:/_other/projects/nba/data/db_nba.xlsm"
ws_tms <- "nba_tms"
tms_nba <-
  filepath_db_nba %>%
  readxl::read_excel(sheet = ws_tms) %>%
  janitor::clean_names() %>%
  filter(status == 1)

colors_dict <- tms_nba %>% select(name = tm, color = teamcolors_color_primary)
div_names <- tms_nba %>% distinct(div_name) %>% pull(div_name)

i <- 1
while(i <= length(div_names)) {
  data_filter <- div_names[i]
  names_main <-
    tms_nba %>%
    filter(div_name == data_filter) %>%
    pull(tm)
  name_main <- names_main[1]

  pal_main <- colors_dict %>% pull(color)
  data_main <- colors_dict %>% filter(name == name_main)
  color_main <- data_main %>% pull(color)
  datas_main <-  colors_dict %>% filter(name %in% names_main)
  colors_main <- datas_main %>% pull(color)

  filename_output <- paste0("nba", "-", tolower(name_main), "-", tolower(data_filter), "-", yyyymmdd)
  report_title_i <- paste0("Analysis of Tweets about ", name_main, " And the NBA's ", data_filter, " Division")

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
        rgx_input = rgx_input,
        filepath_input = filepath_input,
        filename_output = filename_output,
        dir_output = dir_output
      )
    out

  }
  i <- i + 1
}

