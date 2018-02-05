

library("rtweet")
library("dplyr")
library("stringr")

# rtweet::stopwordslangs %>% filter(lang == "en")
# ratelimits <- rtweet::rate_limits()
# ?rtweet::plain_tweets

telists <- rtweet::lists_users("TonyElHabr")
list_id_list <-
  telists %>%
  filter(name == "PhD - Recess") %>%
  pull(list_id)



# tweets_list <- rtweet::lists_statuses(list_id_list, n = 10000)
get_filepath <-
  function(dir,
           filename,
           ext) {
   file.path(dir, paste0(filename, ".", ext))
  }


dir_out <- "data/recess"
dir.exists(dir_out)
filename_out_base <- "recess"
ext_out <- "rds"

max_tweets_per_session <- 15000
users_list <- rtweet::lists_members(list_id_list)
users <- users_list %>% pull(screen_name)
num_users <- length(users)
tweets_per_user_max <- floor(max_tweets_per_session / num_users)

num_i <- length(users)
# num_i <- 10
tweets_per_user <- tweets_per_user_max
# tweets_per_user <- 1000

i <- 1
while(i <= num_i) {
  user_i <- users[i]
  timeline_i <- rtweet::get_timeline(user_i, n = tweets_per_user)
  filepath_out_i <- get_filepath(dir_out, paste0(filename_out_base, "-", user_i), ext_out)
  saveRDS(timeline_i, filepath_out_i)
  i <- i + 1
}


tweets_list %>% count(screen_name, sort = TRUE)
library("ggplot2")
tweets_list %>% ggplot(aes(x = created_at)) + geom_histogram(bins = 30)
# rtweet::write_as_csv(tweets_list, "data/tweets-list-nba-list-2018-02-03.csv")


users_list <-
  telists %>%
  filter(name == "PhD - General Studies") %>%
  pull(list_id) %>%
  rtweet::lists_members()

# library("googlesheets")
# gs_auth(new_user = TRUE)
# gs_ls()
