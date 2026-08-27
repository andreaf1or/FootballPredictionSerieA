# ==============================================================================
# R/04_bayesian.R — Validazione dei modelli bayesiani (Capitolo 4)
# ==============================================================================

library(tidyverse)
library(footBayes)
library(pROC)
library(loo)
library(ggrepel)
library(patchwork)

source("config.R") 
source("R/utils/prep_foot.R")
source("R/utils/metrics.R")

# --- Dati ----------------------------------------------------------------------
data <- read_csv(file.path(PATH_DATA_PROCESSED, "serie_a_full.csv"))
datasets <- build_datasets(data)
n_test = 272

test_set <- build_test_set(datasets, escludi_neopromosse = TRUE)
message("Test set bayesiano (metriche quantitative): ", nrow(test_set),
        " partite (neopromosse escluse, allineato al Cap. 3)")

test_set_esteso <- build_test_set(datasets, escludi_neopromosse = FALSE)
message("Test set bayesiano (esempi qualitativi, §4.5.2): ", nrow(test_set_esteso),
        " partite (neopromosse incluse)")

test_set_raw <- test_set %>%
  select(home_team, away_team, home_goals, away_goals, periods)


# --- Caricamento modelli salvati ------------------------------------------------
modelli_full   <- c(dp_full = "dp_full", bp_full = "bp_full", nb_full = "nb_full",
                    dibp_full = "dibp_full", st_full = "st_full")
modelli_recent <- c(dp_2025 = "dp_2024", bp_2025 = "bp_2024", nb_2025 = "nb_2024",
                    dibp_2025 = "dibp_2024", st_2025 = "st_2024")
# Nota: i nomi dei file .rds usano "2024" (anno di fine training), mentre le
# variabili/etichette in questo script usano "2025" (stagione di test)

for (var_name in names(modelli_full)) {
  assign(var_name, readRDS(file.path(PATH_MODELS, paste0(modelli_full[[var_name]], ".rds"))))
}
for (var_name in names(modelli_recent)) {
  assign(var_name, readRDS(file.path(PATH_MODELS, paste0(modelli_recent[[var_name]], ".rds"))))
}

# Modello di riferimento per l'analisi approfondita (§4.4-4.5)
MODELLO_REF <- "Student-t (2020-25)"
FIT_REF     <- st_2025
TRAIN_REF   <- datasets$recent

configurazioni <- list(
  list(fit = dp_full,   train = datasets$full,   nome = "Double Pois (full)"),
  list(fit = bp_full,   train = datasets$full,   nome = "Biv. Pois (full)"),
  list(fit = nb_full,   train = datasets$full,   nome = "Neg. Bin. (full)"),
  list(fit = dibp_full, train = datasets$full,   nome = "DIBP (full)"),
  list(fit = st_full,   train = datasets$full,   nome = "Student-t (full)"),
  list(fit = dp_2025,   train = datasets$recent, nome = "Double Pois (2020-25)"),
  list(fit = bp_2025,   train = datasets$recent, nome = "Biv. Pois (2020-25)"),
  list(fit = nb_2025,   train = datasets$recent, nome = "Neg. Bin. (2020-25)"),
  list(fit = dibp_2025, train = datasets$recent, nome = "DIBP (2020-25)"),
  list(fit = st_2025,   train = datasets$recent, nome = "Student-t (2020-25)")
)

# --- Estrazione probabilità out-of-sample --------------------------------------

message("Estrazione probabilità out-of-sample tramite foot_prob()...")
message("Questa operazione può richiedere un paio di minuti.")


prob_df_bay<- map_dfr(configurazioni, function(cfg) {
  message("  \u2192 Calcolo in corso per: ", cfg$nome)
  estrai_probabilita(cfg$fit, cfg$train, test_set_raw, cfg$nome)
})

esiti <- test_set %>% select(home_team, away_team, esito_reale)

prob_df_bay <- prob_df_bay %>%
  left_join(esiti, by = c("home_team", "away_team")) %>%
  mutate(
    segno_prev = case_when(
      p1 >= pX & p1 >= p2 ~ "1",
      pX >= p1 & pX >= p2 ~ "X",
      TRUE                 ~ "2"
    ),
    corretto = segno_prev == esito_reale
  )

# --- Metriche: accuratezza, Brier Score, RPS -----------------------------------
# calcola_metriche() è la stessa funzione usata in R/03_frequentist.R.
tab_completo_bay <- calcola_metriche(prob_df_bay %>% select(-any_of(c("esito_reale", "segno_prev", "corretto"))), esiti)
metriche_df_bay <- tab_completo_bay  # per retrocompatibilità col resto dello script

cat("\n── Tabella metriche complete ──\n")
print(tab_completo_bay)

write_csv(tab_completo_bay, file.path(PATH_TABLES, "metriche_bayes.csv"))

# --- Confronto modelli LOO-CV ---------------------------------------------------
message("\nCalcolo LOO-CV (2020-2025)...")
loo_2025 <- list(
  "Double Pois" = dp_2025$fit$loo(),
  "Biv. Pois"   = bp_2025$fit$loo(),
  "Neg. Bin."   = nb_2025$fit$loo(),
  "DIBP"        = dibp_2025$fit$loo(),
  "Student-t"   = st_2025$fit$loo()
)

loo_result <- loo_compare(loo_2025)
cat("\n--- Confronto ELPD (Expected Log Predictive Density) ---\n")
print(loo_result)
capture.output(print(loo_result), file = file.path(PATH_TABLES, "loo_compare_2020_25.txt"))

# --- Grafici: Brier Score per modello --------------------------------------------
message("Generazione grafici di calibrazione...")

p_brier <- tab_completo_bay %>%
  mutate(
    gruppo = if_else(grepl("full", modello), "Storico completo", "2020-25"),
    label  = str_replace(modello, " \\(.*\\)", ""),
    label  = str_replace(label, "DIBP", "DIBP")
  ) %>%
  ggplot(aes(x = reorder(label, -brier), y = brier, color = gruppo)) +
  geom_segment(aes(xend = label, y = 0.55, yend = brier), linewidth = 1.2) +
  geom_point(size = 4) +
  geom_hline(yintercept = 2/3, linetype = "dashed", colour = "grey40", linewidth = 0.7) +
  geom_text(aes(label = round(brier, 3)), hjust = -0.3, size = 3.5, color = "black") +
  coord_flip(ylim = c(0.55, 0.78)) +
  scale_color_manual(values = c("Storico completo" = "#E07B54", "2020-25" = "#5B8DB8")) +
  annotate("text", x = 1.2, y = 2/3 + 0.005, label = "Baseline (0.667)", size = 3.5, colour = "grey40", hjust = 0) +
  labs(title = "Brier Score per modello",
       x = NULL, y = "Brier Score", color = "Dataset training") +
  theme_bw(base_size = 12) + theme(legend.position = "bottom")

# --- Grafici: RPS vs Accuratezza -------------------------------------------------
p_rps <- tab_completo_bay %>%
  mutate(
    gruppo = if_else(grepl("full", modello), "Storico completo", "2020-25"),
    label  = str_replace(modello, " \\(.*\\)", "")
  ) %>%
  ggplot(aes(x = rps, y = accuratezza, colour = gruppo, label = label)) +
  geom_point(size = 3) +
  geom_text_repel(size = 3, max.overlaps = 20) +
  scale_colour_manual(values = c("Storico completo" = "#E07B54", "2020-25" = "#5B8DB8")) +
  labs(title = "RPS medio vs Accuratezza sul segno",
       subtitle = "In basso a destra = massima efficienza predittiva",
       x = "RPS medio (minore \u00e8 meglio)", y = "Accuratezza (%)", colour = "Dataset training") +
  theme_bw(base_size = 12) + theme(legend.position = "bottom")

ggsave(file.path(PATH_FIGURES, "rps_accuratezza.png"), p_rps, width = 8, height = 5, dpi = 200, bg = "white")

# --- Matrice di confusione (modelli 2020-25) -------------------------------------
message("Generazione Confusion Matrix...")
modelli_2025_lbl <- c("Double Pois (2020-25)", "Biv. Pois (2020-25)",
                      "Neg. Bin. (2020-25)", "DIBP (2020-25)", "Student-t (2020-25)")

cm_df <- prob_df_bay %>%
  filter(modello %in% modelli_2025_lbl, !is.na(esito_reale)) %>%
  mutate(
    esito_reale = factor(esito_reale, levels = c("1", "X", "2")),
    segno_prev  = factor(segno_prev,  levels = c("1", "X", "2")),
    modello     = str_replace(modello, " \\(2020-25\\)", "")
  ) %>%
  count(modello, esito_reale, segno_prev, .drop = FALSE)

p_cm <- ggplot(cm_df, aes(x = segno_prev, y = esito_reale, fill = n)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = n), size = 4.5, fontface = "bold") +
  scale_fill_gradient(low = "#F7FBFF", high = "#2171B5") +
  scale_y_discrete(limits = rev) +
  facet_wrap(~ modello, nrow = 1) +
  labs(title = "Matrici di confusione \u2014 modelli 2020-25",
       subtitle = "Righe: esito reale | Colonne: esito previsto",
       x = "Segno previsto", y = "Segno reale", fill = "N") +
  theme_bw(base_size = 11) +
  theme(legend.position = "none", strip.text = element_text(face = "bold", size = 9))

ggsave(file.path(PATH_FIGURES, "confusion_matrix.png"), p_cm, width = 14, height = 4, dpi = 200, bg = "white")

# --- Parametri latenti (abilità e rank) ------------------------------------------
message("Generazione grafici abilità latenti (footBayes)...")
p_abil <- foot_abilities(FIT_REF, TRAIN_REF)
ggsave(file.path(PATH_FIGURES, "posterior_att_def.png"), p_abil, width = 12, height = 8, dpi = 200, bg = "white")

# ==============================================================================
# Griglie probabilità punteggio esatto — partite emblematiche (§4.5.2)
# ==============================================================================

message("Generazione griglie di probabilità esatta...")

prob_ref_esteso <- estrai_probabilita(FIT_REF, TRAIN_REF, test_set_esteso, MODELLO_REF)
esiti_esteso <- test_set_esteso %>% select(home_team, away_team, esito_reale)

prob_ref <- prob_ref_esteso %>%
  filter(!is.na(p1)) %>%
  left_join(esiti_esteso, by = c("home_team", "away_team")) %>%
  mutate(
    segno_prev = case_when(
      p1 >= pX & p1 >= p2 ~ "1",
      pX >= p1 & pX >= p2 ~ "X",
      TRUE                 ~ "2"
    ),
    corretto = segno_prev == esito_reale,
    p_max    = pmax(p1, pX, p2),
    entropia = -(p1 * log(p1 + 1e-9) + pX * log(pX + 1e-9) + p2 * log(p2 + 1e-9)),
    partita  = paste(home_team, "vs", away_team)
  )

data_2020_2025 <- bind_rows(datasets$recent, test_set_esteso %>% select(-esito_reale))

caso_ok  <- prob_ref %>% filter(corretto)  %>% slice_max(p_max, n = 1, with_ties = FALSE)
caso_err <- prob_ref %>% filter(!corretto) %>% slice_max(p_max, n = 1, with_ties = FALSE)
caso_inc <- prob_ref %>% slice_max(entropia, n = 1, with_ties = FALSE)

disegna_griglia_bellissima <- function(caso, fit_obj, data_comb) {
    data_comb_filtrato <- data_comb %>%
      filter(!(home_team == caso$home_team & away_team == caso$away_team & periods != 2025))
    
    plot_base <- foot_prob(fit_obj, data_comb_filtrato,
                           home_team = caso$home_team, away_team = caso$away_team)$prob_plot
  grid <- plot_base$data %>%
    rename(home = Home, away = Away, prob = Prob) %>%
    filter(home <= 5, away <= 5)
  
  idx <- which(test_set_esteso$home_team == caso$home_team & test_set_esteso$away_team == caso$away_team)[1]
  gol_reali_h <- test_set_esteso$home_goals[idx]
  gol_reali_a <- test_set_esteso$away_goals[idx]
  
  sottotitolo_pulito <- paste0(
    "Esito previsto: ", caso$segno_prev, if_else(caso$corretto, " \u2713", " \u2717"),
    "  |  Esito reale: ", gol_reali_h, "-", gol_reali_a, " (", caso$esito_reale, ")"
  )
  
  ggplot(grid, aes(x = factor(away), y = factor(home))) +
    geom_tile(aes(fill = prob), colour = "white") +
    geom_tile(
      data = tibble(home = gol_reali_h, away = gol_reali_a),
      aes(x = factor(away), y = factor(home)),
      colour = "red", fill = NA, linewidth = 1.5,
      width = 0.95, height = 0.95, inherit.aes = FALSE
    ) +
    geom_text(aes(label = if_else(prob >= 0.005, scales::percent(prob, accuracy = 0.1), "")),
              size = 3.2, colour = if_else(grid$prob > 0.06, "white", "grey20")) +
    scale_fill_gradient(low = "#F5F5F5", high = "#2171B5", labels = scales::percent, name = "P(risultato)") +
    scale_y_discrete(limits = rev) +
    labs(
      title    = paste0(caso$home_team, " vs ", caso$away_team, " \u2014 ", MODELLO_REF),
      subtitle = sottotitolo_pulito,
      x = paste("Gol", caso$away_team, "(trasferta)"),
      y = paste("Gol", caso$home_team, "(casa)")
    ) +
    theme_bw(base_size = 12) +
    theme(legend.position = "right", panel.grid = element_blank())
}

p_grid_ok  <- disegna_griglia_bellissima(caso_ok,  FIT_REF, data_2020_2025)
p_grid_err <- disegna_griglia_bellissima(caso_err, FIT_REF, data_2020_2025)
p_grid_inc <- disegna_griglia_bellissima(caso_inc, FIT_REF, data_2020_2025)

ggsave(file.path(PATH_FIGURES, "griglia_caso_corretto.png"),  p_grid_ok,  width = 7, height = 6, dpi = 200, bg = "white")
ggsave(file.path(PATH_FIGURES, "griglia_caso_errato.png"),    p_grid_err, width = 7, height = 6, dpi = 200, bg = "white")
ggsave(file.path(PATH_FIGURES, "griglia_caso_incerto.png"),   p_grid_inc, width = 7, height = 6, dpi = 200, bg = "white")

message("04_bayesian.R completato: tabelle in ", PATH_TABLES, ", figure in ", PATH_FIGURES)
