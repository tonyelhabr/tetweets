

library("dplyr")

filepath_db_nba <- "O:/_other/projects/nba/data/db_nba.xlsm"
readxl::excel_sheets(filepath_db_nba)
tms_nba <-
  readxl::read_excel(filepath_db_nba, sheet = "nba_tms") %>% janitor::clean_names()
tms_nbastatr <- nbastatR::get_nba_teams() %>% janitor::clean_names()
tms_nba_proc <-
  tms_nbastatr %>%
  select(
    tm_name_full = nameteam,
    tm = slugteam,
    city = cityteam,
    nickname = teamname,
    nickname_upper = teamnamefull,
    colors = colorsteam,
    url_thumbnail = urlthumbnailteam
  ) %>%
  semi_join(tms_nba, by = c("tm_name_full" = "tm_name_full")) %>%
  filter(!is.na(nickname)) %>%
  arrange(tm_name_full)
tms_nba_proc

# Could also change "cavaliers" to "cavs"...
tms_nba_final <-
  tms_nba_proc %>%
  mutate(name = nickname) %>%
  mutate(name = ifelse(name == "timberwolves", "twolves", ifelse(name == "cavaliers", "cavs", name))) %>%
  select(name, everything())

readr::write_csv(tms_nba_final, "data-raw/tms-nba.csv")

nba_dict <-
  tms_nba_final %>%
  select(query = name) %>%
  mutate(status_id = NULL)
readr::write_csv(nba_dict, "data-raw/tweets_dict-search-nba.csv")
