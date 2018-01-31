

# Setup. ----
library("dplyr")
library("rtweet")

# Other twitter handles...
# twitter_handle <- "theAESO"
# twitter_handle <- "IESO_Tweets"
iso_dict <-
  tibble::tribble(
    ~abbrv, ~screen_name, ~color_ercot, ~color_company, ~iso_order,
    "CAISO", "California_ISO", ercotr::ercot_palette2[2], "", 2,
    "ERCOT", "ERCOT_ISO", ercotr::ercot_palette2[1], ercotr::ercot_palette2[1], 1,
    "MISO", "miso_energy", ercotr::ercot_palette2[3], "", 3,
    "NYISO", "NewYorkISO", ercotr::ercot_palette2[4], "#6dc2e9", 4,
    "PJM", "pjminterconnect", ercotr::ercot_palette2[5], "#99b3e6", 5
  ) %>%
  mutate(name = abbrv) %>%
  select(screen_name, everything())

num_search <- 10000
tweets_ercot <- search_tweets("#ercot", n = num_search)
tweets_caiso <- search_tweets("#caiso", n = num_search)
tweets_miso <- search_tweets("#miso", n = num_search)
tweets_nyiso <- search_tweets("#nyiso", n = num_search)
tweets_pjm <- search_tweets("#pjm", n = num_search)

tweets_miso %>%
  select(hashtags) %>%
  tidyr::unnest() %>%
  mutate(hashtags = tolower(hashtags)) %>%
  count(hashtags, sort = TRUE) %>%
  # filter(hashtags != "miso") %>%
  top_n(10)

rt <- search_tweets(
  "jemele hill", n = 10000, retryonratelimit = TRUE
)

# Scrape. ----
if(scrape) {
  iso_tweets <- rtweet::get_timelines2(iso_dict %>% pull(screen_name), n = num_tweets)
}
# Export. ----
if(export) {
  rtweet::write_as_csv(iso_tweets, file_name = filepath_tweets)
}
teproj::export_ext_csv(filepath = filepath_info, export = export)
