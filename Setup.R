library(mltools)
library(data.table)
library(xgboost)
library(SHAPforxgboost)
library(caret)
library(DiagrammeR)
library(zoo)
library(rvest)

passing_pbp <- read_csv("passing_feed (13).csv")
epa_data <- read_csv("epa_data.csv") %>% 
  select(game_id, play_id, yards_to_go, seconds_left_in_quarter, EP, EPA)

passing_pbp <- passing_pbp %>%
  mutate(home_team = case_when(
    home_team == "SD" ~ "LAC",
    home_team == "BLT" ~ "BAL",
    home_team == "OAK" ~ "LV",
    home_team == "HST" ~ "HOU",
    home_team == "SL" ~ "LA",
    home_team == "CLV" ~ "CLE", 
    home_team == "ARZ" ~ "ARI",
    TRUE ~ home_team
  )) %>%
  mutate(away_team = case_when(
    away_team == "SD" ~ "LAC",
    away_team == "BLT" ~ "BAL",
    away_team == "OAK" ~ "LV",
    away_team == "HST" ~ "HOU",
    away_team == "SL" ~ "LA",
    away_team == "CLV" ~ "CLE", 
    away_team == "ARZ" ~ "ARI",
    TRUE ~ away_team
  )) %>%  mutate(offense = case_when(
    offense == "SD" ~ "LAC",
    offense == "BLT" ~ "BAL",
    offense == "OAK" ~ "LV",
    offense == "HST" ~ "HOU",
    offense == "SL" ~ "LA",
    offense == "CLV" ~ "CLE", 
    offense == "ARZ" ~ "ARI",
    TRUE ~ offense
  )) %>%  mutate(defense = case_when(
    defense == "SD" ~ "LAC",
    defense == "BLT" ~ "BAL",
    defense == "OAK" ~ "LV",
    defense == "HST" ~ "HOU",
    defense == "SL" ~ "LA",
    defense == "CLV" ~ "CLE", 
    defense == "ARZ" ~ "ARI",
    TRUE ~ defense
  ))

passing_pbp2021 <- read_csv("passing_feed (40).csv")

epa2021 <- read_csv("epa_21_ccg.csv") %>% 
  select(game_id, play_id, yards_to_go, seconds_left_in_quarter, EP, EPA)

passing_pbp2021 <- left_join(passing_pbp2021, epa2021, by = c('game_id', 'play_id')) %>%
  mutate(home_team = case_when(
    home_team == "SD" ~ "LAC",
    home_team == "BLT" ~ "BAL",
    home_team == "OAK" ~ "LV",
    home_team == "HST" ~ "HOU",
    home_team == "SL" ~ "LA",
    home_team == "CLV" ~ "CLE", 
    home_team == "ARZ" ~ "ARI",
    TRUE ~ home_team
  )) %>%
  mutate(away_team = case_when(
    away_team == "SD" ~ "LAC",
    away_team == "BLT" ~ "BAL",
    away_team == "OAK" ~ "LV",
    away_team == "HST" ~ "HOU",
    away_team == "SL" ~ "LA",
    away_team == "CLV" ~ "CLE", 
    away_team == "ARZ" ~ "ARI",
    TRUE ~ away_team
  )) %>%  mutate(offense = case_when(
    offense == "SD" ~ "LAC",
    offense == "BLT" ~ "BAL",
    offense == "OAK" ~ "LV",
    offense == "HST" ~ "HOU",
    offense == "SL" ~ "LA",
    offense == "CLV" ~ "CLE", 
    offense == "ARZ" ~ "ARI",
    TRUE ~ offense
  )) %>%  mutate(defense = case_when(
    defense == "SD" ~ "LAC",
    defense == "BLT" ~ "BAL",
    defense == "OAK" ~ "LV",
    defense == "HST" ~ "HOU",
    defense == "SL" ~ "LA",
    defense == "CLV" ~ "CLE", 
    defense == "ARZ" ~ "ARI",
    TRUE ~ defense
  ))

passing_pbp <- left_join(passing_pbp, epa_data, by = c('game_id', 'play_id'))
passing_pbp_all <- rbind(passing_pbp, passing_pbp2021)

passing_pbp_all <- passing_pbp_all %>% 
  mutate(seconds_left_in_half = ifelse(quarter %in% c(1, 3), seconds_left_in_quarter + 900, seconds_left_in_quarter))

passing_pbp_all$shotgun[is.na(passing_pbp_all$shotgun)] <- 0
passing_pbp_all$mofo_coverage_shown[is.na(passing_pbp_all$mofo_coverage_shown)] <- 0
passing_pbp_all$blitz[is.na(passing_pbp_all$blitz)] <- 0
passing_pbp_all$pressure[is.na(passing_pbp_all$pressure)] <- 0

passing_pbp2 <- passing_pbp_all %>% 
  mutate(
    shotgun = ifelse(shotgun == 'S', 1, 0),
    mofo_coverage_shown = ifelse(mofo_coverage_shown == 'C', 1, 0),
    seconds_left_in_game = ifelse(quarter %in% c(1, 2), ifelse(quarter == 1, 
                            seconds_left_in_half + 1800, seconds_left_in_half + 900), seconds_left_in_half))

colSums(is.na(passing_pbp_all))

passing_pbp2 <- passing_pbp2 %>% 
  group_by(defense, season) %>%
  do({rowwise(.) %>% 
      mutate(def_ADOT = mean(.$pass_depth[.$week < week]))})

passing_pbp2$def_ADOT[is.na(passing_pbp2$def_ADOT)] <- 0

passing_pbp2 <- passing_pbp2 %>% 
  filter(!is.na(seconds_left_in_quarter), !is.na(pass_depth))

passes_select <- passing_pbp2 %>%
  filter(!is.na(seconds_left_in_quarter), !is.na(pass_depth)) %>% 
  ungroup() %>% 
  select(pass_depth, yards_to_go, seconds_left_in_quarter, seconds_left_in_half,
         seconds_left_in_game, down, shotgun, EP, mofo_coverage_shown,
         def_ADOT, distance)

colSums(is.na(passes_select))

passes_select$down <- as.factor(passes_select$down)
passes_select$shotgun <- as.factor(passes_select$shotgun)
passes_select$mofo_coverage_shown <- as.factor(passes_select$mofo_coverage_shown)

passes_select <- one_hot(as.data.table(passes_select)) %>% 
  rename(label = pass_depth)

