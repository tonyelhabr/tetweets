
library("dplyr")
# Need to instatiate the package options?
# devtools::install_github("aelhabr/teproj")
# require("teproj")

filepath_db_nba <- "O:/_other/projects/nba/data/db_nba.xlsm"
ws_tms <- "nba_tms"
tms_nba <-
  filepath_db_nba %>%
  readxl::read_excel(sheet = ws_tms) %>%
  janitor::clean_names() %>%
  filter(status == 1)

colors_dict <- tms_nba %>% select(name = tm, color = teamcolors_color_primary)
div_names <- tms_nba %>% distinct(div_name) %>% pull(div_name)

i <- 5
while(i <= length(div_names)) {
  data_filter <- div_names[i]
  names_main <-
    tms_nba %>%
    filter(div_name == data_filter) %>%
    # filter(tm != name_main) %>%
    pull(tm)
  name_main <- names_main[1]

  pal_main <- colors_dict %>% pull(color)
  data_main <- colors_dict %>% filter(name == name_main)
  color_main <- data_main %>% pull(color)
  # names(color_main) <- data_main %>% pull(name)
  # color_main
  datas_main <-  colors_dict %>% filter(name %in% names_main)
  colors_main <- datas_main %>% pull(color)
  # names(colors_main) <- datas_main %>% pull(name)
  # colors_main

  rgx_input <- "analyze"
  dir_input <- "R"
  yyyymmdd <-  format(Sys.Date(), "%Y-%m-%d")
  filename_output <- paste0("nba", "-", tolower(name_main), "-", tolower(data_filter), "-", yyyymmdd)
  dir_output <- "output"
  report_title_i <- paste0("Analysis of Tweets about ", name_main, " And the NBA's ", data_filter, " Division")

  params_i <-
    list(
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

      filepath_tweets = "data/tweets-search-nba.rds",
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

  filepath_input <- "R/analyze-v03.R"
  # filepath_input <- "R/debug.R"
  do <- TRUE
  if(do) {
    out <-
      teproj::render_proj_io(
        # render = FALSE,
        # keep_rmd = TRUE,
        render_params = params_i,
        rgx_input = rgx_input,
        filepath_input = filepath_input,
        # dir_input = dir_input,
        filename_output = filename_output,
        dir_output = dir_output
      )
    out

  }
  i <- i + 1
}

