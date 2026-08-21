# ==============================================================================
# utils/metrics.R — Estrazione probabilità 1-X-2 e metriche di valutazione
# ==============================================================================
# Prima della riorganizzazione, questa logica esisteva in due copie leggermente
# diverse: get_probs() in cap3.R e estrai_probs_corrette() in cap4.R. Inoltre
# Brier Score e RPS erano calcolati SOLO nel capitolo bayesiano, il che rendeva
# impossibile un confronto diretto con il capitolo frequentista. Ora entrambi
# i capitoli chiamano le stesse funzioni.

library(dplyr)
library(purrr)
library(tibble)
library(footBayes)

#' Estrae le probabilità 1-X-2 previste da un modello footBayes per ogni
#' partita del test set, gestendo in modo esplicito gli errori (es. squadre
#' assenti dal training set) invece di lasciarli fallire silenziosamente.
#'
#' @param fit Oggetto modello restituito da mle_foot() o stan_foot()
#' @param train_data Dati di training usati per stimare fit
#' @param test_data Dati di test su cui prevedere
#' @param nome_modello Etichetta del modello (per le tabelle riassuntive)
estrai_probabilita <- function(fit, train_data, test_data, nome_modello) {
  data_comb <- bind_rows(train_data, test_data)

  risultati <- map(seq_len(nrow(test_data)), function(i) {
    h_team <- test_data$home_team[i]
    a_team <- test_data$away_team[i]

    tryCatch({
      p_tab <- foot_prob(fit, data_comb, home_team = h_team, away_team = a_team)$prob_table
      tibble(
        modello   = nome_modello,
        home_team = h_team,
        away_team = a_team,
        p1        = p_tab$prob_h,
        pX        = p_tab$prob_d,
        p2        = p_tab$prob_a
      )
    }, error = function(e) {
      message("  [", nome_modello, "] partita ", h_team, " - ", a_team,
              " saltata: ", conditionMessage(e))
      NULL
    })
  })

  bind_rows(risultati)
}

#' Calcola accuratezza, Brier Score e RPS per ogni modello a partire dalle
#' probabilità previste e dagli esiti reali.
#'
#' @param prob_df Output di estrai_probabilita() (eventualmente per più
#'   modelli impilati con bind_rows)
#' @param esiti_reali Un tibble con home_team, away_team, esito_reale
#'   (valori "1", "X", "2")
calcola_metriche <- function(prob_df, esiti_reali) {
  prob_df %>%
    left_join(esiti_reali, by = c("home_team", "away_team")) %>%
    filter(!is.na(esito_reale)) %>%
    mutate(
      segno_prev = case_when(
        p1 >= pX & p1 >= p2 ~ "1",
        pX >= p1 & pX >= p2 ~ "X",
        TRUE                 ~ "2"
      ),
      corretto = segno_prev == esito_reale,
      I1 = as.numeric(esito_reale == "1"),
      IX = as.numeric(esito_reale == "X"),
      I2 = as.numeric(esito_reale == "2"),
      brier_match = (p1 - I1)^2 + (pX - IX)^2 + (p2 - I2)^2,
      # RPS per esiti ordinali 1 < X < 2 (Epstein, 1969)
      rps_match = 0.5 * ((p1 - I1)^2 + ((p1 + pX) - (I1 + IX))^2)
    ) %>%
    group_by(modello) %>%
    summarise(
      partite     = n(),
      corrette    = sum(corretto),
      accuratezza = round(mean(corretto) * 100, 2),
      brier       = round(mean(brier_match), 4),
      rps         = round(mean(rps_match), 4),
      .groups     = "drop"
    ) %>%
    arrange(brier)
}

#' Costruisce il tibble degli esiti reali (1/X/2) da un test set grezzo.
#' Usata sia da 03_frequentist.R che da 04_bayesian.R per garantire che
#' l'etichettatura dell'esito sia identica nei due capitoli.
esiti_da_test_set <- function(test_data) {
  test_data %>%
    mutate(esito_reale = case_when(
      home_goals > away_goals ~ "1",
      home_goals < away_goals ~ "2",
      TRUE                    ~ "X"
    )) %>%
    select(home_team, away_team, esito_reale)
}
