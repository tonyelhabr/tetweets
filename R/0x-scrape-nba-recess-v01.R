

library("rtweet")
library("dplyr")
library("stringr")

# rtweet::stopwordslangs %>% filter(lang == "en")
# ratelimits <- rtweet::rate_limits()
# ?rtweet::plain_tweets

get_filepath <-
  function(dir,
           filename,
           ext) {
    file.path(dir, paste0(filename, ".", ext))
  }
max_tweets_per_session <- 150000 # 18000
types_valid <- c("nba-tms", "nba-recess", "nba-manual")
type <- "nba-recess"

# One-time setup. ----
one_time <- FALSE
if (one_time) {
  if(type == "nbatms") {
  telists <- rtweet::lists_users("TonyElHabr")
  # list_name <- "PhD - Recess"
  list_name <- "NBAteams"
  list_id <-
    telists %>%
    filter(name == list_name) %>%
    pull(list_id)
  # tweets_list <- rtweet::lists_statuses(list_id, n = 10000)
  } else if (type == "recess") {

  }

  users_dict <- rtweet::lists_members(list_id)
  users <- users_dict %>% pull(screen_name)

  dir_dict <- "data-raw"
  # filename_dict <- paste0("recess")
  filename_dict <- type
  ext_dict <- "csv"
  filepath_dict <- get_filepath(dir_dict, filename_dict, ext_dict)
  # readr::write_csv(users_dict %>% select(screen_name), filepath_dict)
}
# Setup session (out). ----
yyyymmdd <- strftime(Sys.Date(), "%Y-%m-%d")
timestamp_scrape <- strftime(Sys.time(), "%Y-%m-%d@%H-%M-%S")

dir_out_session <- file.path("data")
dir.exists(dir_out_session)
dir_out_existing <- dir_out_session
dir_out_i <- file.path("data", yyyymmdd)
dir.create(dir_out_i, recursive = TRUE, showWarnings = TRUE)

filename_out_base <- "recess"

filename_out_session <- paste0(filename_out_base, "-", yyyymmdd)
filename_out_existing <- filename_out_base
filename_out_i_base <- paste0(filename_out_base)

ext_out_session <- "rds"
ext_out_existing <- ext_out_session
ext_out_i <- ext_out_session

filepath_out_session <-
  get_filepath(dir_out_session, filename_out_session, ext_out_session)
filepath_out_existing <-
  get_filepath(dir_out_existing, filename_out_existing, ext_out_existing)

# Setup session (dict). ----
dir_dict <- "data-raw"
filename_dict <- type
ext_dict <- "csv"
filepath_dict <- get_filepath(dir_dict, filename_dict, ext_dict)
users_dict <- readr::read_csv(filepath_dict)
users <- users_dict %>% pull(1)

num_users <- length(users)
tweets_per_user_max <- floor(max_tweets_per_session / num_users)

num_i <- length(users)
# num_i <- 10
tweets_per_user <- tweets_per_user_max
# tweets_per_user <- 1000

# Retrieve. ----
retrieve <- TRUE
if (retrieve) {
  filepath_google_token <- "../google_token.rds"
  filepath_data_gdrive <- file.path("data", "recess.rds")
  filepath_data_gdrive_backup <- file.path("data", paste0("recess-", yyyymmdd, ".rds"))
  filepath_data_dl_temp <-
    file.path("data", paste0("recess", "-", "downloaded.rds"))
  filepath_data_session <- filepath_out_session
  # filepath_data_session <- file.path("data", "recess", "recess.rds")

  googledrive::drive_auth(filepath_google_token)
  dribble <- googledrive::drive_get(filepath_data_gdrive)
  if (nrow(dribble) != 1) {
    stop("Oops! Found more than one file!")
  }

  googledrive::drive_cp(file = filepath_data_gdrive, path = filepath_data_gdrive_backup)

  filepath_data_dl_actual <-
    googledrive::drive_download(dribble$path, path = filepath_data_dl_temp, overwrite = TRUE)
  if (!identical(filepath_data_dl_temp,
                 filepath_data_dl_actual$local_path)) {
    stop("Oops! There is something wrong with the downloaded file!")
  }

  data_dl <- readRDS(filepath_data_dl_actual$local_path)
  tweets_byuser <-
    data_dl %>%
    group_by(screen_name) %>%
    filter(created_at == max(created_at)) %>%
    ungroup() %>%
    mutate(rn = row_number(screen_name)) %>%
    select(rn, screen_name, created_at, status_id)
  # tweets_byuser
} else {
  if(one_time) {
  tweets_byuser <-
    users_dict %>%
    mutate(rn = row_number())
  }
}

# Scrape. ----
scrape <- FALSE
if (scrape) {
  i <- 1
  while (i <= num_i) {
    # user_i <- users[i]
    user_i <- tweets_byuser %>% filter(rn == i) %>% pull(screen_name)
    if(!one_time) {
      status_id_i <- tweets_byuser %>% filter(rn == i) %>% pull(status_id)
      timeline_i <- rtweet::get_timeline(user_i, max_id = status_id_i)
    } else {
      # status_id_i <- tweets_byuser %>% filter(rn == i) %>% pull(status_id)
      # tweets_per_user <- 300
      timeline_i <- rtweet::get_timeline(user_i, n = tweets_per_user)
    }

    filepath_out_i <-
      get_filepath(dir_out_i,
                   paste0(filename_out_i_base, "-", user_i),
                   ext_out_i)
    saveRDS(timeline_i, filepath_out_i)
    i <- i + 1
  }
}


# Bind everything from this session. ----

bind <- TRUE
if (bind) {
  filepaths_session <-
    list.files(pattern = ".",
               path = dir_out_i,
               full.names = TRUE,)
  out_session <-
    purrr::reduce(purrr::map(filepaths_session, readRDS), rbind)
  saveRDS(out_session, filepath_out_session)
  if(file.exists(filepath_out_session)) {
    unlink(filepaths_session)
    unlink(dir_out_i)
  }
}

# Finalize. ----
# NOTE: Do this if successful with retrieving tweets for all queries in one session.
upload <- TRUE
if (upload) {
    filepath_google_token <- "google_token.rds"
    filepath_data_gdrive <- file.path("data", "recess.rds")
    filepath_data_dl_temp <-
      file.path("data", paste0("recess", "-", "downloaded.rds"))
    filepath_data_session <- filepath_out_session
    # filepath_data_session <- file.path("data", "recess", "recess.rds")

    googledrive::drive_auth(filepath_google_token)
    dribble <- googledrive::drive_get(filepath_data_gdrive)
    filepath_data_dl_actual <-
      googledrive::drive_download(dribble$path, path = filepath_data_dl_temp, overwrite = TRUE)

    data_dl <- readRDS(filepath_data_dl_actual$local_path)
    data_session <- readRDS(filepath_data_session)
    data_new <-
      rtweet::do_call_rbind(list(data_dl, data_session))

    data_unique <- data_new[!duplicated(data_new$status_id), ]
    saveRDS(data_unique, filepath_data_session)
    # num_new <- (nrow(data_dl) + nrow(data_session)) - nrow(data_unique)
    # print(num_new)
    # googledrive::drive_trash(file = filepath_data_gdrive)
    # googledrive::drive_upload(media = filepath_data_session, path = filepath_data_gdrive)
    googledrive::drive_update(file = filepath_data_gdrive, media = filepath_data_session)
    unlink(filepath_data_dl_temp)

  }

# Address errors from this session. ----
# NOTE: Do this if failed to retrieve tweets for all queries in one session.
retry <- FALSE
if (retry) {
  queries <- users_dict %>% pull(1)
  rgx_queries <- paste0("(", paste(queries, collapse = ")|("), ")")
  rgx_retry <- paste0(rgx_queries)
  filepaths_retry <-
    list.files(
      pattern = rgx_retry,
      path = dir_out_i,
      full.names = TRUE,
    )
  print(sort(filepaths_retry))

  out_retry <-
    purrr::reduce(purrr::map(filepaths_retry, readRDS), rbind)
  saveRDS(out_session, filepath_out_session)
  # Cleanup.
  if(file.exists(filepath_out_session)) {
    unlink(filepaths_retry)
    unlink(dir_out_i)
  }
}

