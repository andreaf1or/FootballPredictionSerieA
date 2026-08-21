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

# Il MLE non può gestire squadre mai viste nel training (cold-start): per
# questo, a differenza del capitolo bayesiano, le neopromosse vengono escluse
# dal test set. È una necessità tecnica del MLE, non un'incongruenza — il
# capitolo bayesiano (R/04_bayesian.R) userà lo stesso dataset grezzo, ma
# includerà le neopromosse per mostrare il vantaggio del partial pooling.
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
