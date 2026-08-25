# ==============================================================================
# R/04a_fit_bayesian_models.R — Stima dei 10 modelli bayesiani (footBayes)
# ==============================================================================
# ATTENZIONE: questo script stima via MCMC (Stan, backend cmdstanr) i modelli
# che 04_bayesian.R si aspetta di trovare già pronti in modelli_salvati/.
# Va eseguito UNA SOLA VOLTA (o ogni volta che i dati di training cambiano).
#
# Tempistiche indicative: i modelli sulla finestra "2020-25" (~1.500 partite)
# sono relativamente rapidi; quelli sullo "storico completo" (~28.000 partite,
# dal 1929) possono richiedere molto più tempo. Consiglio: lancia prima il
# blocco 2020-25 (più veloce, ed è la finestra usata come riferimento nel
# Cap. 4), poi il blocco storico completo, eventualmente durante la notte.
#
# Prerequisiti (una tantum):
#   install.packages(c("cmdstanr", "instantiate"),
#                     repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
#   cmdstanr::install_cmdstan()

library(tidyverse)
library(footBayes)
library(cmdstanr)
library(instantiate)

source("config.R")
source("R/utils/prep_foot.R")

if (!instantiate::stan_cmdstan_exists()) {
  stop("CmdStan non trovato. Esegui cmdstanr::install_cmdstan() prima di procedere.")
}

# --- Dati ---------------------------------------------------------------------
data <- read_csv(file.path(PATH_DATA_PROCESSED, "serie_a_full.csv"))
datasets <- build_datasets(data)
# n_test allineato al Cap. 3 (vedi discussione sul test set condiviso)
test_set <- build_test_set(datasets, escludi_neopromosse = TRUE)   # 272 partite VERE, stagione 2024/25
n_test <- nrow(test_set)
test_set_raw <- test_set %>%
  select(home_team, away_team, home_goals, away_goals, periods)


# --- Impostazioni MCMC ----------------------------------------------------------
# Valori di default di stan_foot(): chains = 4, iter_sampling = 1000,
# iter_warmup = iter_sampling. Puoi ridurli per un run di prova più veloce
# (es. CHAINS = 2, ITER_SAMPLING = 300) e poi rilanciare con i valori pieni
# per i risultati definitivi da riportare in tesi.
CHAINS        <- 4
ITER_SAMPLING <- 500
SEED_STAN     <- SEED  # da config.R

fit_e_salva <- function(train_data, model, nome_file, dynamic_type = NULL) {
  message("\n== Stima: ", nome_file, " (model = '", model, "'",
          if (!is.null(dynamic_type)) paste0(", dynamic_type = '", dynamic_type, "'") else "",
          ") ==")
  message("Partite nel training: ", nrow(train_data), " — questo può richiedere tempo...")
  dati_per_stan <- bind_rows(train_data, test_set_raw)
  args <- list(
    data            = dati_per_stan,
    model           = model,
    predict         = n_test,
    chains          = CHAINS,
    parallel_chains = CHAINS,
    iter_sampling   = ITER_SAMPLING,
    seed            = SEED_STAN,
    thin            = 2
  )
  if (!is.null(dynamic_type)) args$dynamic_type <- dynamic_type
  
  fit <- do.call(stan_foot, args)
  
  if (!is.null(fit$fit) && inherits(fit$fit, "CmdStanMCMC")) {
    fit$fit$draws()
    fit$fit$sampler_diagnostics()
  }
  
  path_out <- file.path(PATH_MODELS, paste0(nome_file, ".rds"))
  saveRDS(fit, path_out)
  message("  → salvato in ", path_out)
  fit
}

# ==============================================================================
# Blocco 1: finestra 2020-25, con effetti stagionali dinamici (come da tesi §4.4)
# ==============================================================================
message("\n########## BLOCCO 1: modelli 2020-25 (dynamic_type = 'seasonal') ##########")

dp_2025   <- fit_e_salva(datasets$recent, "double_pois",       "dp_2024",   "seasonal")
bp_2025   <- fit_e_salva(datasets$recent, "biv_pois",          "bp_2024",   "seasonal")
nb_2025   <- fit_e_salva(datasets$recent, "neg_bin",           "nb_2024",   "seasonal")
dibp_2025 <- fit_e_salva(datasets$recent, "diag_infl_biv_pois","dibp_2024", "seasonal")
st_2025   <- fit_e_salva(datasets$recent, "student_t",         "st_2024",   "seasonal")

message("\nBlocco 1 completato. Se vuoi verificare subito i risultati parziali, puoi già",
        " lanciare 04_bayesian.R (fallirà solo sui modelli 'full' non ancora stimati).")

# ==============================================================================
# Blocco 2: finestra storico completo, statico
# ==============================================================================
message("\n########## BLOCCO 2: modelli storico completo (statici) ##########")

dp_full   <- fit_e_salva(datasets$full, "double_pois",       "dp_full")
bp_full   <- fit_e_salva(datasets$full, "biv_pois",          "bp_full")
nb_full   <- fit_e_salva(datasets$full, "neg_bin",           "nb_full")
dibp_full <- fit_e_salva(datasets$full, "diag_infl_biv_pois","dibp_full")
st_full   <- fit_e_salva(datasets$full, "student_t",         "st_full")

message("\n\u2705 Tutti i 10 modelli stimati e salvati in ", PATH_MODELS)
message("Ora puoi lanciare R/04_bayesian.R (o run_all.R) normalmente.")

