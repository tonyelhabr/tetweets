

# Setup. ----
library("dplyr")
# library("rtweet")

scrape <- FALSE
export <- scrape

filepath_tweets <- "data/tweets-timeline/iso.csv"
filepath_info <- "data-raw/tweets_dict-timeline-iso.csv"
num_tweets <- 2000

# Other twitter handles...
# twitter_handle <- "theAESO"
# twitter_handle <- "IESO_Tweets"
iso_dict <-
  tibble::tribble(
    ~abbrv, ~screen_name, ~color_company, ~iso_order,
    "CAISO", "California_ISO",  "", 2,
    "ERCOT", "ERCOT_ISO", "#00AEC7", 1,
    "MISO", "miso_energy", "", 3,
    "NYISO", "NewYorkISO", "#6dc2e9", 4,
    "PJM", "pjminterconnect", "#99b3e6", 5
  ) %>%
  mutate(name = abbrv) %>%
  select(screen_name, everything())

info <- teproj::import_ext_csv(filepath = filepath_info)

# Scrape. ----
if(scrape) {
  iso_tweets <- rtweet::get_timelines(iso_dict %>% pull(screen_name), n = num_tweets)
  tweets <-
    tweets %>%
    inner_join(info, by = "screen_name") %>%
    mutate(name = forcats::fct_reorder(factor(name), x = iso_order))
}
# Export. ----
if(export) {
  rtweet::write_as_csv(iso_tweets, file_name = filepath_tweets)
}
teproj::export_ext_csv(filepath = filepath_info, export = export)
