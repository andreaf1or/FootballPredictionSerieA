# ==============================================================================
# R/02_eda.R — Analisi esplorativa (Capitolo 2 della tesi)
# ==============================================================================

library(tidyverse)
library(ggrepel)
library(igraph)
library(ggraph)
library(patchwork)

source("config.R")

# Caricamento dato --------------
data_clean <- read_csv(file.path(PATH_DATA_PROCESSED, "serie_a_full.csv"))
data <- data_clean  # alias per compatibilità con il codice sotto

# --- Heatmap dei risultati esatti --------------------------------------------
heatmap_data <- data_clean %>%
  filter(home_goal <= 5, away_goal <= 5) %>%
  count(home_goal, away_goal) %>%
  ungroup() %>%
  mutate(prop = n / sum(n))

p_heatmap <- ggplot(heatmap_data, aes(x = as.factor(away_goal), y = as.factor(home_goal), fill = prop)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1),
                color = prop > 0.04),
            size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = c("black", "white")) +
  scale_fill_gradient(low = "#F3F6F8", high = "#00599C", name = "Frequenza") +
  scale_y_discrete(limits = rev) +
  labs(
    title = "Heatmap dei risultati esatti in Serie A",
    subtitle = "Frequenze relative dei punteggi fino a 5 gol",
    x = "Gol Squadra in Trasferta",
    y = "Gol Squadra in Casa"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(file.path(PATH_FIGURES, "heatmapres.png"), p_heatmap,
       width = 7.5, height = 6, dpi = 300)

# --- Evoluzione lambda casa/trasferta e home advantage -----------------------
lambda_per_season <- data_clean %>%
  group_by(periods) %>%
  summarise(
    lambda_casa      = mean(home_goal),
    lambda_trasferta = mean(away_goal),
    .groups = "drop"
  ) %>%
  pivot_longer(c(lambda_casa, lambda_trasferta),
               names_to = "side", values_to = "lambda") %>%
  mutate(side = if_else(side == "lambda_casa", "Casa", "Trasferta"))

p1 <- ggplot(lambda_per_season, aes(x = periods, y = lambda, color = side)) +
  geom_line(linewidth = 0.8) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, linetype = "dashed", linewidth = 0.6) +
  labs(x = "Stagione", y = "\u03bb (gol medi per partita)",
       title = "Evoluzione di \u03bb casa e trasferta nel tempo",
       color = NULL) +
  theme_bw()

p2 <- data_clean %>%
  group_by(periods) %>%
  summarise(home_adv = mean(home_goal) - mean(away_goal), .groups = "drop") %>%
  ggplot(aes(x = periods, y = home_adv)) +
  geom_line(aes(color = "Dato osservato", linetype = "Dato osservato"), linewidth = 0.7) +
  geom_smooth(aes(color = "Trend", linetype = "Trend"),
              method = "loess", span = 0.3, se = FALSE, linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
  scale_color_manual(name = NULL,
                      values = c("Dato osservato" = "black", "Trend" = "firebrick")) +
  scale_linetype_manual(name = NULL,
                         values = c("Dato osservato" = "solid", "Trend" = "dashed")) +
  labs(x = "Stagione", y = "\u03bb_casa \u2212 \u03bb_trasferta",
       title = "Home advantage nel tempo") +
  theme_bw() +
  theme(legend.position = "right")

p_lambda <- p1 / p2
ggsave(file.path(PATH_FIGURES, "lambdadiff.png"), p_lambda,
       width = 8, height = 9, dpi = 300)

# --- Forza offensiva/difensiva: vista statica ---------------------------------
recent_seasons <- c(2025, 2024, 2023, 2022)

strength <- data_clean %>%
  filter(periods %in% recent_seasons) %>%
  pivot_longer(c(home_team, away_team), names_to = "role", values_to = "team") %>%
  mutate(
    scored   = if_else(role == "home_team", home_goal, away_goal),
    conceded = if_else(role == "home_team", away_goal, home_goal)
  ) %>%
  group_by(team) %>%
  summarise(
    attack  = mean(scored),
    defence = mean(conceded),
    matches = n(),
    .groups = "drop"
  ) %>%
  filter(matches >= 76)

avg_attack  <- mean(strength$attack)
avg_defence <- mean(strength$defence)

inter_x <- strength %>% filter(team == "Inter") %>% pull(attack)
inter_y <- strength %>% filter(team == "Inter") %>% pull(defence)

p_scatter_static <- ggplot(strength, aes(x = attack, y = defence, label = team)) +
  geom_vline(xintercept = avg_attack,  linetype = "dashed", color = "grey60") +
  geom_hline(yintercept = avg_defence, linetype = "dashed", color = "grey60") +
  geom_point(size = 2.5) +
  ggrepel::geom_label_repel(size = 3, max.overlaps = 20) +
  labs(
    x = "Forza offensiva (gol segnati/partita)",
    y = "Forza difensiva (gol subiti/partita)",
    title = "Stagioni 2021/22\u20132024/25"
  ) +
  annotate("text", x = Inf,  y = -Inf, label = "Migliori", hjust = 1.1, vjust = -0.5, size = 3, color = "grey40") +
  annotate("text", x = -Inf, y = Inf,  label = "Peggiori", hjust = -0.1, vjust = 1.5, size = 3, color = "grey40") +
  geom_point(aes(y = inter_y, x = inter_x), pch = 19, col = 2) +
  theme_bw()

# --- Forza offensiva/difensiva: vista dinamica per stagione -------------------
teams <- c("Inter", "Milan", "Napoli", "Juventus", "Roma")
evo_sq <- data %>%
  filter(periods >= 2022 & periods < 2026) %>%
  filter(!is.na(home_goal), !is.na(away_goal)) %>%
  group_by(periods)
mean_gc <- evo_sq %>%
  group_by(periods, home_team) %>%
  summarise(attack = mean(home_goal), defence = mean(away_goal), .groups = "drop") %>%
  rename(team = home_team) %>%
  bind_rows(
    evo_sq %>%
      group_by(periods, away_team) %>%
      summarise(attack = mean(away_goal), defence = mean(home_goal), .groups = "drop") %>%
      rename(team = away_team)
  ) %>%
  group_by(periods, team) %>%
  summarise(attack = mean(attack), defence = mean(defence), .groups = "drop")

plot_data <- mean_gc %>%
  group_by(periods) %>%
  mutate(
    overall_score = attack - defence,
    my_rank = rank(desc(overall_score), ties.method = "min"),
    label_top5 = if_else(my_rank <= 5, as.character(team), NA_character_)
  ) %>%
  ungroup()

data_top5 <- plot_data %>% filter(!is.na(label_top5))
colori_personalizzati <- c(
  "Milan" = "#E32221", "Inter" = "#00599C", "Juventus" = "black",
  "Roma" = "#8E1B3E", "Napoli" = "#12A0D7", "Fiorentina" = "purple",
  "Bologna" = "#1A2F5C", "Lazio" = "lightblue", "Atalanta" = "blue"
)

p_scatter_dyn <- ggplot(plot_data, aes(x = attack, y = defence)) +
  geom_point(data = filter(plot_data, is.na(label_top5)),
             color = "grey60", alpha = 0.5, size = 1.5) +
  geom_point(data = data_top5, aes(color = team), size = 2) +
  ggrepel::geom_label_repel(data = data_top5,
                             aes(label = label_top5, fill = team),
                             color = "white", size = 4, fontface = "bold",
                             box.padding = 0.4, point.padding = 0.3,
                             min.segment.length = 0, segment.color = "grey40",
                             segment.size = 0.6, show.legend = FALSE) +
  scale_fill_manual(values = colori_personalizzati) +
  scale_color_manual(values = colori_personalizzati) +
  facet_wrap(~periods) +
  geom_vline(xintercept = mean(plot_data$attack, na.rm = TRUE),
             linetype = "dashed", color = "grey60") +
  geom_hline(yintercept = mean(plot_data$defence, na.rm = TRUE),
             linetype = "dashed", color = "grey60") +
  labs(x = "", y = "", title = "") +
  theme_minimal() +
  theme(legend.position = "none")

grafico_forza <- p_scatter_static / p_scatter_dyn
ggsave(file.path(PATH_FIGURES, "forza_squadre.png"), grafico_forza,
       width = 9, height = 13, dpi = 300)

# --- Network Analysis dei gol -------------------------------------------------
stag_rete <- 2024
edges <- bind_rows(
  data %>%
    filter(periods == stag_rete) %>%
    select(home_team, away_team, home_goal) %>%
    rename(from = away_team, to = home_team, weight = home_goal),
  data %>%
    filter(periods == stag_rete) %>%
    select(home_team, away_team, away_goal) %>%
    rename(from = home_team, to = away_team, weight = away_goal)
) %>%
  group_by(from, to) %>%
  summarise(weight = sum(weight), .groups = "drop") %>%
  filter(weight > 0)

net <- graph_from_data_frame(edges, directed = TRUE)
pr <- page_rank(net, directed = TRUE, weights = E(net)$weight)$vector

V(net)$pagerank  <- pr
V(net)$team_name <- V(net)$name

set.seed(SEED)  # da config.R, non più un valore fisso locale (42)

p_network <- ggraph(net, layout = "fr") +
  geom_edge_fan(aes(edge_width = weight, edge_alpha = weight),
                arrow = arrow(length = unit(2, "mm"), type = "closed"),
                end_cap = circle(6, "mm"), color = "grey60", show.legend = FALSE) +
  geom_node_point(aes(size = pagerank, color = pagerank), show.legend = FALSE) +
  geom_node_text(aes(label = team_name), repel = TRUE, size = 3.5, fontface = "bold") +
  scale_size_continuous(range = c(2, 12)) +
  scale_edge_width_continuous(range = c(0.2, 1.5)) +
  scale_color_viridis_c(option = "plasma") +
  labs(title = paste("Network dei Gol: Serie A", stag_rete - 1, "/", stag_rete))

ggsave(file.path(PATH_FIGURES, "networkteams.png"), p_network,
       width = 8, height = 8, dpi = 300)

message("02_eda.R completato: figure salvate in ", PATH_FIGURES)
