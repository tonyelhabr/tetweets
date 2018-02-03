
# Setup. ----
library("dplyr")
library("rtweet")

tweets_search <- search_tweets("#spurs", n = 1000)
tweets_search %>%
  select(hashtags) %>%
  tidyr::unnest() %>%
  mutate(hashtags = tolower(hashtags)) %>%
  count(hashtags, sort = TRUE) %>%
  filter(hashtags != "spurs") %>%
  top_n(10)

tweets_timeline <- get_timeline("spurs", n = 100)

num_search <- 10000
# tweets_ercot <- search_tweets("#ercot", n = num_search)
# tweets_caiso <- search_tweets("#caiso", n = num_search)
# tweets_miso <- search_tweets("#miso", n = num_search)
# tweets_nyiso <- search_tweets("#nyiso", n = num_search)
# tweets_pjm <- search_tweets("#pjm", n = num_search)
tweets_search <- search_tweets2(c("#ercot", "#caiso", "#nyiso", "#pjm"), n = num_search

library("nbastatR")
d <- "01-20-2018"
get_day_nba_game_scores("2018-01-20")
?get_day_nba_game_scores
get_day_nba_game_scores(d)
get_day_nba_games(d)
get_day_nba_matchups(d)
