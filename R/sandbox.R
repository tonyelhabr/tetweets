

library("rtweet")
library("dplyr")
library("stringr")

# rtweet::stopwordslangs %>% filter(lang == "en")
# ratelimits <- rtweet::rate_limits()
# ?rtweet::plain_tweets

telists <- rtweet::lists_users("TonyElHabr")
users_phd_general <-
  telists %>%
  filter(name == "PhD - General Studies") %>%
  pull(list_id) %>%
  rtweet::lists_members()
# Or...
list_id_phd_general <- 781232226532855808
# users_phd_general <- lists_members(list_id_phd_general)
tweets_phd_general <- rtweet::lists_statuses(list_id_phd_general, n = 10000, retryonratelimit = TRUE)
tweets_phd_general %>% select(created_at, text) %>% filter(created_at <= max(created_at) - 5)
tweets_phd_general %>% filter(!is.na(hashtags)) %>% select(hashtags) %>% tidyr::unnest(hashtags)
tweets_phd_general %>% count(screen_name, sort = TRUE)
library("ggplot2")
tweets_phd_general %>% ggplot(aes(x = created_at)) + geom_histogram(bins = 30)
rtweet::write_as_csv(tweets_phd_general, "data/tweets-list-nba-phd_general-2018-02-03.csv")

users_phd_general <-
  telists %>%
  filter(name == "PhD - General Studies") %>%
  pull(list_id) %>%
  rtweet::lists_members()


# library("googlesheets")
# gs_auth(new_user = TRUE)
# gs_ls()
