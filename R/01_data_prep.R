# ==============================================================================
# R/01_data_prep.R — Pulizia e armonizzazione dati grezzi
# ==============================================================================
# Questa logica prima viveva dentro cap2.R (righe iniziali). Spostata qui
# perché è un passaggio propedeutico a TUTTI i capitoli, non solo all'EDA:
# 03_frequentist.R e 04_bayesian.R devono ripartire dallo stesso dataset
# pulito, non ricalcolarlo ciascuno per conto proprio.

library(tidyverse)
library(worldfootballR)
library(engsoccerdata)  # fornisce il dataset `italy` (storico Serie A pre-2020)

source("config.R")
data("italy", package = "engsoccerdata")

# 1. Mappa di armonizzazione nomi squadre -------------------------------------
name_map <- c(
  "Lazio Roma" = "Lazio", "AS Roma" = "Roma", "AC Milan" = "Milan",
  "SSC Napoli" = "Napoli", "ACF Fiorentina" = "Fiorentina",
  "Hellas Verona" = "Verona", "Torino FC" = "Torino", "Genoa CFC" = "Genoa",
  "Bologna FC" = "Bologna", "Parma AC" = "Parma", "Parma FC" = "Parma",
  "Frosinone Calcio" = "Frosinone", "Salernitana Calcio 1919" = "Salernitana",
  "Salernitana Sport" = "Salernitana", "Udinese Calcio" = "Udinese",
  "Cagliari Calcio" = "Cagliari", "AC Venezia" = "Venezia",
  "Empoli FC" = "Empoli", "Pisa SC" = "Pisa", "Sassuolo Calcio" = "Sassuolo",
  "US Cremonese" = "Cremonese", "Spezia Calcio" = "Spezia",
  "Como Calcio" = "Como", "US Lecce" = "Lecce", "FC Crotone" = "Crotone",
  "Brescia Calcio" = "Brescia"
)

clean_team_name <- function(x) dplyr::recode(x, !!!name_map, .default = x)

# 2. Dati storici pre-2020 --------------------------
italy_prev21 <- italy %>%
  filter(Season < 2020) %>%
  rename(date = Date, periods = Season, home_team = home,
         away_team = visitor, home_goal = hgoal, away_goal = vgoal) %>%
  select(date, periods, home_team, away_team, home_goal, away_goal) %>%
  mutate(date = as.character(date))

# 3. Importazione, unione e pulizia --------------------------------------------
data <- read_csv(file.path(PATH_DATA_RAW, "serie_a_clean.csv")) %>%
  set_names(c('date', 'periods', 'home_team', 'away_team', 'home_goal', 'away_goal')) %>%
  mutate(date = as.character(date)) %>%  # forza il tipo, evita mismatch con italy_prev21
  bind_rows(italy_prev21) %>%
  arrange(date) %>%
  mutate(
    home_team = clean_team_name(home_team),
    away_team = clean_team_name(away_team)
  ) %>%
  distinct(date, home_team, away_team, .keep_all = TRUE)

# 4. Solo partite concluse ------------------------------------------------------
data_clean <- data %>%
  filter(!is.na(home_goal), !is.na(away_goal))

# 5. Salvataggio dataset pulito -------------------------------------------------
write_csv(data_clean, file.path(PATH_DATA_PROCESSED, "serie_a_full.csv"))

message("01_data_prep.R completato: ", nrow(data_clean), " partite salvate in ",
        file.path(PATH_DATA_PROCESSED, "serie_a_full.csv"))