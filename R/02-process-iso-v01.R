

# Setup. ----
library("dplyr")

# Should put this process stuff in a separate file...
# Process. ----
tweets <- teproj::import_ext_csv(filepath = filepath_tweets)
info <- teproj::import_ext_csv(filepath = filepath_info)
tweets <-
  tweets %>%
  inner_join(info, by = "screen_name") %>%
  mutate(name = forcats::fct_reorder(factor(name), x = iso_order))

teproj::export_ext_csv(tweets, filepath = filepath_tweets_augmented)
