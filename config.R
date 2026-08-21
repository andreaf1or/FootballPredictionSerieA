# ==============================================================================
# config.R — Parametri condivisi da tutti gli script
# ==============================================================================
# Questo file è la UNICA fonte di verità per le finestre temporali di
# training/test. Sia 03_frequentist.R che 04_bayesian.R devono leggere
# queste variabili invece di ridefinirle localmente — è il modo per evitare
# che i due capitoli finiscano per usare, senza accorgersene, dati diversi.

# Stagione di test (2025 = stagione 2024/25) ----------------------------------
TEST_PERIOD <- 2025

# Finestra "storico completo" --------------------------------------------------
TRAIN_FULL_MIN <- -Inf   # nessun limite inferiore
TRAIN_FULL_MAX <- 2024   # fino alla stagione 2023/24 inclusa

# Finestra "recente" (usata anche per gli effetti stagionali dinamici) --------
TRAIN_RECENT_MIN <- 2020   # come in cap3.R/cap4.R originali (periods >= 2020);
                            # nei dati reali la prima stagione con questa
                            # codifica è "2021" (2020/21), quindi il filtro
                            # >= 2020 è innocuo ma tenuto per fedeltà all'originale
TRAIN_RECENT_MAX <- 2024   # stagione 2023/24

# Riproducibilità ----------------------------------------------------------
SEED <- 1234

# Percorsi -------------------------------------------------------------------
PATH_DATA_RAW       <- "data/raw"
PATH_DATA_PROCESSED <- "data/processed"
PATH_MODELS         <- "modelli_salvati"
PATH_FIGURES        <- "output/figures"
PATH_TABLES         <- "output/tables"

# Creazione automatica delle cartelle di output se mancanti -------------------
for (p in c(PATH_FIGURES, PATH_TABLES, PATH_MODELS)) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE)
}

# Nota storica -----------------------------------------------------------------
# In precedenza cap3.R e cap4.R definivano il test set in modo indipendente:
# entrambi filtravano `periods == 2025`, ma senza passare da qui il rischio è
# che uno dei due script venga modificato (es. per sottocampionare le ultime
# N partite) senza che l'altro venga aggiornato di conseguenza — esattamente
# il problema riscontrato tra testo e codice. Da ora, qualunque sottoinsieme
# del test set (es. "solo ultime 50 partite") va definito QUI con una variabile
# esplicita (es. TEST_SUBSET_LAST_N <- 50) e letto da entrambi gli script.
