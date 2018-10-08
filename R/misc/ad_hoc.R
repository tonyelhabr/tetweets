
library("dplyr")
library("rtweet")

query <- "curling"
num_tweets <- 18000
tweets <-
  rtweet::search_tweets(
    q = query,
    n = num_tweets
  )

saveRDS(tweets, file.path("data", "curling-2018-02-17.rds"))

query <-"#rstats AND blog"
num_tweets <- 1000
tweets_recent <-
  rtweet::search_tweets(
    q = query,
    type = "recent",
    n = num_tweets
  )
tweets_mixed <-
  rtweet::search_tweets(
    q = query,
    type = "mixed",
    n = num_tweets
  )
tweets_popular <-
  rtweet::search_tweets(
    q = query,
    type = "popular",
    n = num_tweets
  )

tweets <-
  bind_rows(
    tweets_recent %>% mutate(type = "recent"),
    tweets_mixed %>% mutate(type = "mixed"),
    tweets_popular %>% mutate(type = "popular"),
  ) %>%
  select(type, everything())
saveRDS(tweets, file.path("data", "rstats-blogs-2018-02-13.rds"))


filepath_out <- "woj.json"
rtweet::stream_tweets(search_method =
  "wojespn",
  timeout = 60 * 60 * 1,
  file_name = filepath_out,
  parse = FALSE
)

stream <- rtweet::parse_stream(filepath_out)
stream %>% mutate(text = rtweet::plain_tweets(text)) %>% select(screen_name, text)
