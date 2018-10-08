
library("dplyr")
library("rtweet")
# max_tweets_per_session <- 150000
max_tweets_per_session <- 90000

queries <- c("rstats", "python", "d3")
queries <- paste0(queries, " AND blog")
# num_users <- max_tweets_per_session / length(queries)
num_tweets <- 100
filepath_out <- file.path("data", "tweets-blogs-2018-02-18.rds")

# References:
# + https://github.com/maelle/maelle.github.io/blob/master/_posts/2018-01-01-sortinghat.md

# tweets_1 <-
#   rtweet::search_tweets(
#     q = queries[1],
#     n = num_tweets,
#     include_rts = FALSE
#   )
# tweets_2 <-
#   rtweet::search_tweets(
#     q = queries[2],
#     n = num_tweets,
#   )
# tweets_3 <-
#   rtweet::search_tweets(
#     q = queries[3],
#     n = num_tweets,
#   )
# tweets <-
#   bind_rows(
#     tweets_rstats %>% mutate(query = "rstats AND blog"),
#     tweets_python %>% mutate(query = "python AND blog"),
#     tweets_d3 %>% mutate(query = "d3 AND blog"),
#   ) %>%
#   select(query, everything())

get_tweets_search <- function(query, num, include_rts = FALSE) {
  data <- rtweet::search_tweets(q = query, n = num, include_rts = include_rts)
  data$query <- query
  data
}

tweets <- purrr::map2_df(queries, num_tweets, get_tweets_search)
# saveRDS(tweets, file = filepath_out)
