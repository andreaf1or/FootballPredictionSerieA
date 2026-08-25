# Modellazione e previsione dei risultati calcistici in Serie A
### Dall'approccio frequentista all'analisi bayesiana

Tesi di laurea triennale — Statistica per l'Economia e l'Impresa
Università degli Studi di Padova, Dipartimento di Scienze Statistiche (A.A. 2025/2026)

**Autore:** Andrea Fior (matricola 2111172)
**Relatore:** Dott. Francesco Denti

---

## Struttura del progetto

```
.
├── config.R                  # parametri condivisi (finestre temporali, seed) — fonte unica di verità
├── R/
│   ├── 01_data_prep.R        # pulizia e armonizzazione dati grezzi
│   ├── 02_eda.R              # analisi esplorativa (Capitolo 2)
│   ├── 03_frequentist.R      # modelli MLE — Poisson Doppio, Bivariato (Capitolo 3)
│   ├── 04_bayesian.R         # modelli bayesiani via footBayes (Capitolo 4)
│   └── utils/
│       ├── prep_foot.R       # funzione di preparazione dati condivisa
│       └── metrics.R         # accuratezza, Brier Score, RPS — usate da entrambi i capitoli
├── data/
│   ├── raw/                  # dati grezzi (non versionati, vedi sotto)
│   └── processed/            # train_full.csv, train_2020_25.csv, test_2025.csv
├── modelli_salvati/          # oggetti .rds dei modelli bayesiani stimati (Git LFS, vedi sotto)
├── output/
│   ├── figures/              # grafici generati dagli script (.png)
│   └── tables/               # tabelle generate dagli script (.csv)
└── thesis/
    └── tesi.tex              # sorgente LaTeX della tesi
```

## Come riprodurre le analisi

```r
source("config.R")
source("R/01_data_prep.R")     # crea data/processed/*.csv da data/raw/
source("R/02_eda.R")           # genera le figure del Capitolo 2
source("R/03_frequentist.R")   # stima MLE e genera tabelle/figure del Capitolo 3
source("R/04_bayesian.R")      # stima MCMC e genera tabelle/figure del Capitolo 4
```

**Nota:** `04_bayesian.R` richiede tempi di calcolo lunghi (stima MCMC di 10 modelli
via Stan). Se i file in `modelli_salvati/` sono già presenti, lo script li carica
invece di ristimarli da zero.

## Requisiti

- R >= 4.6.0
- Pacchetti: `tidyverse`, `footBayes`, `rstan`, `bayesplot`, `loo`, `pROC`,
  `yardstick`, `ggrepel`, `patchwork`

Installazione rapida:
```r
install.packages(c("tidyverse", "footBayes", "rstan", "bayesplot",
                    "loo", "pROC", "yardstick", "ggrepel", "patchwork"))
```

## Dati

Il progetto utilizza un set di dati completo sulle partite di Serie A, costruito a partire da due fonti principali[cite: 1]:

* **Dati storici (1929 - 2019):** Estratti direttamente dal pacchetto R `engsoccerdata` tramite il dataset `italy`[cite: 1].
* **Dati recenti (2020 - 2025):** I dati grezzi, conservati in `data/raw/serie_a_clean.csv`[cite: 1], sono stati ottenuti avvalendosi del pacchetto R `worldfootballR`[cite: 1].

Lo script `R/01_data_prep.R` si occupa di unire queste due fonti, armonizzare i nomi delle squadre (che variano nel tempo e tra i dataset) e filtrare solo le partite effettivamente concluse[cite: 1]. 

I dataset finali, puliti e pronti per l'analisi esplorativa e la stima dei modelli (frequentisti e bayesiani), vengono salvati automaticamente nella cartella `data/processed/` (es. `serie_a_full.csv`)[cite: 1].

| File | Periodo | N. partite |
|---|---|---|
| `train_full.csv` | 1929/30 – 2023/24 | ~28.400 |
| `train_2020_25.csv` | 2020/21 – 2023/24 | ~1.520 |
| `test_2025.csv` | 2024/25 | 380 |


## Licenza

Codice distribuito con licenza MIT (vedi `LICENSE`).
