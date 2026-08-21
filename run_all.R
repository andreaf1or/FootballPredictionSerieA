# ==============================================================================
# run_all.R — Esegue l'intera pipeline in ordine
# ==============================================================================
# Uso: Rscript run_all.R
# oppure, in sessione interattiva: source("run_all.R")
#
# Nota: 04_bayesian.R può richiedere diverse ore la prima volta (stima MCMC).
# Se modelli_salvati/*.rds sono già presenti, vengono caricati invece di
# essere ristimati.

message("== [1/5] Configurazione ==")
source("config.R")

message("== [2/5] Preparazione dati ==")
source("R/01_data_prep.R")

message("== [3/5] Analisi esplorativa (Capitolo 2) ==")
source("R/02_eda.R")

message("== [4/5] Modelli frequentisti (Capitolo 3) ==")
source("R/03_frequentist.R")

message("== [5/5] Modelli bayesiani (Capitolo 4) ==")
source("R/04_bayesian.R")

message("== Pipeline completata. Figure in output/figures/, tabelle in output/tables/ ==")
