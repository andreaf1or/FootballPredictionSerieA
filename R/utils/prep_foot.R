# ==============================================================================
# utils/prep_foot.R — Preparazione dati per footBayes
# ==============================================================================
# Prima definita identicamente sia in cap3.R che in cap4.R. Spostata qui in modo
# che esista UNA sola versione: se cambia la logica di pulizia, cambia in un
# posto solo e si propaga automaticamente a tutti gli script che la usano.

library(dplyr)

#' Prepara i dati nel formato richiesto da footBayes
#'
#' @param df Un data frame con colonne periods, home_team, away_team,
#'   home_goal(s), away_goal(s)
#' @return Un tibble con colonne periods, home_team, away_team,
#'   home_goals, away_goals
prep_foot <- function(df) {
  # Gestisce sia la codifica singolare (home_goal) sia plurale (home_goals),
  # dato che i CSV grezzi e quelli processati hanno usato nomi diversi in
  # passato — vedi nota in 01_data_prep.R.
  if ("home_goal" %in% names(df)) {
    df <- df %>% rename(home_goals = home_goal, away_goals = away_goal)
  }

  df %>%
    select(periods, home_team, away_team, home_goals, away_goals)
}

#' Costruisce i tre dataset (full, recente, test) a partire dai parametri
#' definiti in config.R. Da chiamare dopo aver fatto source("config.R").
#'
#' @param data Il data frame completo e pulito (output di 01_data_prep.R)
build_datasets <- function(data) {
  list(
    full   = data %>% filter(periods <= TRAIN_FULL_MAX) %>% prep_foot(),
    recent = data %>% filter(periods >= TRAIN_RECENT_MIN,
                              periods <= TRAIN_RECENT_MAX) %>% prep_foot(),
    test   = data %>% filter(periods == TEST_PERIOD) %>% prep_foot()
  )
}

#' Individua le squadre "cold-start" / neopromosse.
#'
#' @param datasets L'output di build_datasets()
identifica_neopromosse <- function(datasets) {
  # Per identificare le 3 neopromosse esatte (Como, Parma, Venezia),
  # controlliamo chi NON era presente nell'ultimissima stagione di training (2023/24).
  ultima_stagione <- max(datasets$recent$periods)
  
  dati_anno_scorso <- datasets$recent %>% filter(periods == ultima_stagione)
  squadre_anno_scorso <- union(dati_anno_scorso$home_team, dati_anno_scorso$away_team)
  
  squadre_test <- union(datasets$test$home_team, datasets$test$away_team)
  
  # Trova chi c'è nel test set ma non c'era l'anno scorso
  setdiff(squadre_test, squadre_anno_scorso)
}
#' Costruisce il test set definitivo con l'esito reale (1/X/2) già codificato,
#' e decide ESPLICITAMENTE se includere o escludere le neopromosse.
#'
#' In precedenza questa scelta era implicita e diversa tra Cap. 3 e Cap. 4
#' (cap3.R le escludeva per necessità tecnica del MLE; cap4.R, nonostante un
#' commento "# 50 partite" ormai obsoleto, di fatto le includeva). Ora la
#' scelta è un parametro esplicito, documentato in ciascuno script che la usa.
#'
#' @param datasets L'output di build_datasets()
#' @param escludi_neopromosse Se TRUE, rimuove le partite con squadre mai
#'   viste nel training (necessario per il MLE frequentista). Se FALSE, le
#'   mantiene (il modello bayesiano può comunque prevederle via partial
#'   pooling — usato per l'analisi del cold-start nel Cap. 4).
build_test_set <- function(datasets, escludi_neopromosse = TRUE) {
  neopromosse <- identifica_neopromosse(datasets)
  test <- datasets$test

  if (length(neopromosse) > 0) {
    if (escludi_neopromosse) {
      message("Escluse dal test set (cold-start, richiesto dal MLE): ",
              paste(neopromosse, collapse = ", "))
      test <- test %>%
        filter(!home_team %in% neopromosse, !away_team %in% neopromosse)
    } else {
      message("Test set include squadre cold-start (gestite via partial pooling): ",
              paste(neopromosse, collapse = ", "))
    }
  }

  test %>%
    mutate(esito_reale = case_when(
      home_goals > away_goals ~ "1",
      home_goals < away_goals ~ "2",
      TRUE                    ~ "X"
    ))
}
