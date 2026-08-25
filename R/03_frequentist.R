# ==============================================================================
# R/03_frequentist.R — Validazione dei modelli frequentisti (Capitolo 3)
# ==============================================================================

library(tidyverse)
library(footBayes)
library(pROC)
library(yardstick)

source("config.R")
source("R/utils/prep_foot.R")
source("R/utils/metrics.R")

# --- Dati -------------------------------------------------------------------
data <- read_csv(file.path(PATH_DATA_PROCESSED, "serie_a_full.csv"))
datasets <- build_datasets(data)

# --- Grafico: Verifica Assunzione di Poisson ----------------------------------
message("Generazione grafico: Assunzione di Poisson...")

# Calcolo dei lambda (medie empiriche dei gol)
lambda_home <- mean(data$home_goal, na.rm = TRUE)
lambda_away <- mean(data$away_goal, na.rm = TRUE)

# Calcolo frequenze empiriche e probabilità teoriche
df_home <- data %>%
  count(Gol = home_goal) %>%
  mutate(
    Proporzione = n / sum(n), 
    Luogo = "Casa", 
    Poisson = dpois(Gol, lambda_home)
  )

df_away <- data %>%
  count(Gol = away_goal) %>%
  mutate(
    Proporzione = n / sum(n), 
    Luogo = "Trasferta", 
    Poisson = dpois(Gol, lambda_away)
  )

# Unione dei due dataframe e limitazione a max 10 gol per asse x pulito
df_poisson <- bind_rows(df_home, df_away) %>%
  filter(Gol <= 10) %>%
  mutate(Luogo = factor(Luogo, levels = c("Casa", "Trasferta")))

# Creazione del grafico
p_poisson <- ggplot(df_poisson, aes(x = Gol)) +
  geom_bar(aes(y = Proporzione), stat = "identity", fill = "grey40", width = 0.7) +
  geom_line(aes(y = Poisson), color = "firebrick", linewidth = 1) +
  geom_point(aes(y = Poisson), color = "firebrick", size = 2) +
  scale_x_continuous(breaks = 0:10) +
  facet_wrap(~ Luogo) +
  labs(
    title = "Distribuzione gol osservata vs Poisson attesa",
    subtitle = "Punti rossi = Poisson con \u03bb stimato dai dati",
    x = "Gol",
    y = "Proporzione"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold")
  )

ggsave(file.path(PATH_FIGURES, "poisson_fit.png"), p_poisson, width = 8, height = 5, dpi = 200, bg = "white")

# --- Creazione Test Set -------------------------------------------------------
# Il MLE non può gestire squadre mai viste nel training: per
# questo, a differenza del capitolo bayesiano, le neopromosse vengono escluse
# dal test set. È una necessità tecnica del MLE, non un'incongruenza.
test_set <- build_test_set(datasets, escludi_neopromosse = TRUE)
n_test <- nrow(test_set)
message("Test set frequentista: ", n_test, " partite (neopromosse escluse)")

# --- Stima modelli MLE --------------------------------------------------------
set.seed(SEED)
fit_dp_full   <- mle_foot(datasets$full,   model = "double_pois", predict = n_test)
set.seed(SEED)
fit_bp_full   <- mle_foot(datasets$full,   model = "biv_pois",    predict = n_test)
set.seed(SEED)
fit_dp_recent <- mle_foot(datasets$recent, model = "double_pois", predict = n_test)
set.seed(SEED)
fit_bp_recent <- mle_foot(datasets$recent, model = "biv_pois",    predict = n_test)

# --- Estrazione probabilità 1-X-2 --------------------------------------------

prob_df <- bind_rows(
  estrai_probabilita(fit_dp_full,   datasets$full,   test_set, "Doppio Poisson (Full)"),
  estrai_probabilita(fit_bp_full,   datasets$full,   test_set, "Poisson Bivariato (Full)"),
  estrai_probabilita(fit_dp_recent, datasets$recent, test_set, "Doppio Poisson (2020-25)"),
  estrai_probabilita(fit_bp_recent, datasets$recent, test_set, "Poisson Bivariato (2020-25)")
)

esiti <- test_set %>% select(home_team, away_team, esito_reale)

# --- Metriche: accuratezza, Brier Score, RPS ----------------------------------

tab_metriche <- calcola_metriche(prob_df, esiti)

cat("\n── Metriche modelli frequentisti ──\n")
print(tab_metriche)

write_csv(tab_metriche, file.path(PATH_TABLES, "metriche_frequentista.csv"))

# --- Curva ROC (previsione vittoria in casa) ----------------------------------
df_roc_lift <- prob_df %>%
  left_join(esiti, by = c("home_team", "away_team")) %>%
  mutate(
    y_home        = as.integer(esito_reale == "1"),
    y_home_factor = factor(if_else(y_home == 1, "vittoria", "altro"),
                           levels = c("vittoria", "altro")),
    gruppo_temporale = if_else(grepl("Full", modello), "Full", "2020-25")
  )

roc_list <- df_roc_lift %>%
  group_split(modello) %>%
  map_dfr(function(df) {
    roc_obj <- roc(df$y_home, df$p1, quiet = TRUE)
    tibble(
      modello = unique(df$modello),
      gruppo  = unique(df$gruppo_temporale),
      fpr     = 1 - roc_obj$specificities,
      tpr     = roc_obj$sensitivities,
      auc     = as.numeric(auc(roc_obj))
    )
  })

p_roc <- roc_list %>%
  mutate(label_modello = paste0(modello, " (AUC = ", round(auc, 3), ")")) %>%
  ggplot(aes(x = fpr, y = tpr, color = label_modello, linetype = gruppo)) +
  geom_line(linewidth = 0.9) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray50") +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "Curva ROC - Modelli MLE",
    subtitle = "Previsione della Vittoria Casalinga",
    x = "1 - Specificità (FPR)", y = "Sensibilità (TPR)",
    color = "Modello", linetype = "Dataset"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "vertical")

ggsave(file.path(PATH_FIGURES, "rocglm.png"), p_roc, width = 8, height = 7, dpi = 200, bg = "white")
# --- Matrice di confusione frequentista -------------------------------------
message("Generazione Confusion Matrix (Frequentista)...")

cm_freq_df <- prob_df %>%
  filter(!is.na(esito_reale)) %>%
  mutate(
    segno_prev = case_when(
      p1 >= pX & p1 >= p2 ~ "1",
      pX >= p1 & pX >= p2 ~ "X",
      TRUE                 ~ "2"
    ),
    esito_reale = factor(esito_reale, levels = c("1", "X", "2")),
    segno_prev  = factor(segno_prev,  levels = c("1", "X", "2"))
  ) %>%
  count(modello, esito_reale, segno_prev, .drop = FALSE)

p_cm_freq <- ggplot(cm_freq_df, aes(x = segno_prev, y = esito_reale, fill = n)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = n), size = 4.5, fontface = "bold") +
  scale_fill_gradient(low = "#F7FBFF", high = "#2171B5") +
  scale_y_discrete(limits = rev) +
  facet_wrap(~ modello, nrow = 1) +
  labs(title = "Matrici di confusione — Modelli Frequentisti",
       subtitle = "Nessun modello prevede correttamente il pareggio",
       x = "Segno previsto", y = "Segno reale", fill = "N") +
  theme_bw(base_size = 11) +
  theme(legend.position = "none", strip.text = element_text(face = "bold", size = 9))

ggsave(file.path(PATH_FIGURES, "confusion_matrix_freq.png"), p_cm_freq, width = 12, height = 4, dpi = 200, bg = "white")

# --- Curva Lift ----------------------------------------------------------------
lift_list <- df_roc_lift %>%
  group_split(modello) %>%
  map_dfr(function(df) {
    lift_curve(df, truth = y_home_factor, p1, event_level = "first") %>%
      mutate(modello = unique(df$modello), gruppo = unique(df$gruppo_temporale))
  })

p_lift <- lift_list %>%
  ggplot(aes(x = .percent_tested, y = .lift, color = modello, linetype = gruppo)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Curva Lift - Modelli MLE",
    subtitle = "Guadagno informativo rispetto a classificazione casuale",
    x = "% di Partite Considerate", y = "Lift",
    color = "Modello", linetype = "Dataset"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "vertical")

ggsave(file.path(PATH_FIGURES, "liftglm.png"), p_lift, width = 8, height = 7, dpi = 200, bg = "white")

message("03_frequentist.R completato: tabelle in ", PATH_TABLES, ", figure in ", PATH_FIGURES)

