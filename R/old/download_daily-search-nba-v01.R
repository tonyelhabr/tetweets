
#  Setup. ----
topic <- "nba"
yyyymmdd <- strftime(Sys.Date(), "%Y-%m-%d")
timestamp_scrape <- strftime(Sys.time(), "%Y-%m-%d@%H-%M-%S")

dir_dict <- "data-raw"
dir_dict_backup <- dir_dict
dir_dict_existing <- dir_dict
dir_out <- "data"
dir_out_backup <- dir_out
dir_out_existing <- dir_out
dir_out_i <- file.path("data", yyyymmdd)
dir.create(dir_out_i, recursive = TRUE)

filename_dict_base <- "tweets_dict-search"
filename_out_base <- "tweets-search"

filename_dict_existing <- paste0(filename_dict_base, "-", topic)
filename_dict <-
  paste0(filename_dict_base, "-", topic, "-", yyyymmdd)
filename_dict_backup <-
  paste0(filename_dict_base, "-", topic, "-", timestamp_scrape)

filename_out_existing <- paste0(filename_out_base, "-", topic)
filename_out <- paste0(filename_out_base, "-", topic, "-", yyyymmdd)
filename_out_backup <-
  paste0(filename_out_base, "-", topic, "-", timestamp_scrape)
filename_out_i_base <- paste0("")

ext_dict <- "csv"
ext_out <- ext_dict
ext_out_existing <- "rds"

get_filepath <-
  function(filename,
           dir,
           ext,
           filepath) {
    if (missing(filepath)) {
      # filepath <- paste0(dir, filename, ".", ext)
      filepath <- file.path(dir, paste0(filename, ".", ext))
    }
    filepath
  }

filepath_dict_existing <-
  get_filepath(filename_dict_existing, dir_dict, ext_dict)
filepath_dict <- get_filepath(filename_dict, dir_dict, ext_dict)
filepath_dict_backup <-
  get_filepath(filename_dict_backup, dir_dict, ext_dict)


filepath_out_existing <-
  get_filepath(filename_out_existing, dir_out, ext_out_existing)
filepath_out <- get_filepath(filename_out, dir_out, ext_out)
filepath_out_backup <-
  get_filepath(filename_out_backup, dir_out, ext_out)

num_n <- 10000
sec_sleep <- 1

# Value originally from http://thoughtfaucet.com/search-twitter-by-location/examples/.
# geocode_usa <- "44.467186,-73.214804,2500km"
geocode_usa <- rtweet::lookup_coords("usa")


# Data import. ----
filepath_db_nba <- "O:/_other/projects/nba/data/db_nba.xlsm"
ws_tms <- "nba_tms"
tms_nba <-
  filepath_db_nba %>%
  readxl::read_excel(sheet = ws_tms) %>%
  janitor::clean_names() %>%
  filter(status == 1)

dict_names <-
  tms_nba %>%
  select(query = twitter_query_manual) %>%
  mutate(query = paste(query, collapse = " OR "))
# Old...
# dict <- read.csv(filepath_dict_existing, stringsAsFactors = FALSE)
dict_backup <- dict

# Loop prep. ----
num_i <- NROW(dict)
# num_i <- 23
out_list <- vector("list", num_i)

i <- 1
# i <- 23
while (i <= num_i) {
  query_i <- dict[i, "query"]
  # query_i <- paste(query_i, collapse = " OR ")
  since_id_i <- dict[i, "since_id"]
  # NOTE: Maybe should be using `max_id` parameters instead of `since_id`?
  out_i <-
    rtweet::search_tweets(
      q = query_i,
      include_rts = FALSE,
      n = num_n,
      since_id = since_id_i,
      geocode = geocode_usa,
      lang = "en"
    )

  if (NROW(out_i) == 0) {
    # next
    break
  }
  dict_backup[i, "since_id"] <- out_i$status_id[1]

  out_i$name <- query_i
  num_names <- length(names(out_i))
  out_i <- out_i[, c(num_names, 1:(num_names - 1))]
  # if(i == 1 | !exists("out")) {
  #   out <- out_i
  # } else {
  #   out <- rbind(out, out_i)
  # }

  # names(out_list[i]) <- i
  out_list[i] <- list(out_i)

  filename_out_i <- paste0(query_i, "-", timestamp_scrape)
  filepath_out_i <- get_filepath(filename_out_i, dir_out_i, ext_out)
  rtweet::write_as_csv(out_i, filepath_out_i)

  i <- i + 1
  msg <-
    sprintf("Retrieved %.0f tweets for query '%s'.", NROW(out_i), query_i)
  if (interactive()) {
    cat(msg, "\n", sep = "")
  } else {
    message(msg)
  }
  Sys.sleep(sec_sleep)
}

success <- FALSE
if (success) {
  # out <- do.call("rbind", out_list)
  out <- rtweet::do_call_rbind(out_list)

  write.csv(dict_backup, filepath_dict, row.names = FALSE)
  write.csv(dict_backup, filepath_dict_backup, row.names = FALSE)

  rtweet::write_as_csv(out, filepath_out)
  rtweet::write_as_csv(out, filepath_out_backup)
}


# NOTE: Do this if failed to retrieve tweets for all queries in one session.
retry <- FALSE
if (retry) {
  queries <- dict[, "query"]
  rgx_queries <- paste0("(", paste(queries, collapse = ")|("), ")")
  rgx_retry <- paste0(rgx_queries)
  filepaths_retry <-
    list.files(pattern = rgx_retry,
               path = file.path("data", yyyymmdd),
               full.names = TRUE,
    )
  # cat(sort(filepaths_retry), sep = "\n")
  print(sort(filepaths_retry))

  out_retry <-
    purrr::reduce(purrr::map(filepaths_retry, rtweet::read_twitter_csv),
                  rbind)
  rtweet::write_as_csv(out_retry, filepath_out_backup)
  # Cleanup.
  # unlink(filepaths_retry)
}

# NOTE: Do this if successful with retrieving tweets for all queries in one session.
success <- FALSE
if (success) {
  if (file.exists(filepath_out)) {
    out <- rtweet::read_twitter_csv(filepath_out)
  }
  if (file.exists(filepath_out_existing)) {
    out_existing <- readRDS(filepath_out_existing)
    out_new <- do_call_rbind(list(out_existing, out))

    out_new <- out_new[!duplicated(out_new$status_id),]
    # rtweet::write_as_csv(out_new, filepath_out_existing)
    saveRDS(out_new, filepath_out_existing)
  }
}
