
library("dplyr")
# Need to instatiate the package options?
# devtools::install_github("aelhabr/teproj")
# require("teproj")


filepath_input <- "data/tweets-seach-nba.csv"
filepath_pal <- "data-raw/tms-nba.csv"
data_pal <- readr::read_csv(filepath_pal)
pal <- data_pal %>% mutate(colors = stringr::str_replace_all(colors, "\\,.*$", "")) %>% pull(colors)
names <- data_pal %>% pull(tm)

params <-
  list(
    filepath_tweets = filepath_input,
    # names = names,
    name_main = "SAS",
    augmented = TRUE,
    download = FALSE,
    pal = pal
  )

rgx_input <- "analyze"
dir_input <- "R"
filename_output <- "nba"
dir_output <- "output"

out <-
  teproj::render_proj_io(
    # render = FALSE,
    render_params = params,
    rgx_input = rgx_input,
    dir_input = dir_input,
    filenames_output = filename_output,
    dir_output = dir_output
    )
out

