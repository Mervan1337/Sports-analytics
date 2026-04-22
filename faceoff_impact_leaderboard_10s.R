# faceoff_impact_leaderboard_10s.R
# Per-player faceoff impact score leaderboard — one panel per zone.
#
# Impact score (per faceoff) =
#   (xG created when player WINS) - (xG conceded when player LOSES)
#   divided by total faceoffs in that zone
#
# Zone is from each player's OWN perspective:
#   winner + fo_zone "Offensive Zone" → their OZ
#   loser  + fo_zone "Offensive Zone" → their DZ (opponent's OZ = our DZ)
#   neutral/center ice → NZ for both
#
# Packages: dplyr, ggplot2, patchwork
# Data:     post_faceoff_events_10s.csv

rm(list = ls())
library(dplyr)
library(ggplot2)
library(patchwork)

DATA_FILE    <- "post_faceoff_events_10s.csv"
MIN_FO_OZ   <- 15   # min faceoffs to qualify (OZ — most common)
MIN_FO_NZ   <- 15   # neutral zone
MIN_FO_DZ   <- 15   # defensive zone
TOP_N       <- 10   # players shown per panel

# ── 1. LOAD ────────────────────────────────────────────────────────────────────
df <- read.csv(DATA_FILE, stringsAsFactors = FALSE) %>%
  arrange(gameid, compiledgametime)

# ── 2. PHYSICAL FACEOFF ID (v6) ────────────────────────────────────────────────
df <- df %>%
  mutate(
    is_new_faceoff      = as.integer(eventname == "faceoff" &
                                       compiledgametime != lag(compiledgametime, default = 0)),
    physical_faceoff_id = cumsum(is_new_faceoff)
  ) %>%
  group_by(gameid) %>%
  tidyr::fill(physical_faceoff_id, .direction = "downup") %>%
  ungroup()

# ── 3. LEVERAGE HELPERS ────────────────────────────────────────────────────────
period_weights   <- c("1" = 0.6, "2" = 0.8, "3" = 1.0, "4" = 1.5)
manpower_weights <- c("evenStrength" = 1.0, "powerPlay" = 1.3, "shortHanded" = 1.3)

leverage_score <- function(score_diff, period, manpower, fo_x) {
  score_w   <- 1 / (1 + abs(score_diff))
  period_w  <- period_weights[as.character(period)]
  period_w  <- ifelse(is.na(period_w), 1.0, period_w)
  manpower_w <- manpower_weights[manpower]
  manpower_w <- ifelse(is.na(manpower_w), 1.0, manpower_w)
  zone_w    <- ifelse(abs(fo_x) > 25, 1.3, 1.0)
  score_w * period_w * manpower_w * zone_w
}

# ── 4. FACEOFF REGISTRY ────────────────────────────────────────────────────────
faceoff_registry <- df %>%
  filter(eventname == "faceoff") %>%
  group_by(gameid, physical_faceoff_id) %>%
  summarise(
    winner_team   = teamid[outcome == "successful"][1],
    loser_team    = teamid[outcome == "failed"][1],
    winner_player = playerid[outcome == "successful"][1],
    loser_player  = playerid[outcome == "failed"][1],
    fo_x          = xadjcoord[outcome == "successful"][1],
    period        = first(period),
    score_diff    = first(scoredifferential),
    manpower      = first(manpowersituation),
    .groups       = "drop"
  ) %>%
  filter(!is.na(winner_team), !is.na(loser_team)) %>%
  mutate(
    leverage = leverage_score(score_diff, period, manpower, fo_x),
    fo_zone_winner = case_when(
      fo_x >  25 ~ "Offensive Zone",
      fo_x > -25 ~ "Neutral Zone",
      TRUE       ~ "Defensive Zone"
    )
  )

# ── 4. XG PER FACEOFF ──────────────────────────────────────────────────────────
xg_by_side <- df %>%
  filter(eventname %in% c("shot", "goal")) %>%
  inner_join(
    faceoff_registry %>% select(gameid, physical_faceoff_id, winner_team, loser_team),
    by = c("gameid", "physical_faceoff_id")
  ) %>%
  mutate(side = case_when(
    teamid == winner_team ~ "winner",
    teamid == loser_team  ~ "loser",
    TRUE                  ~ NA_character_
  )) %>%
  filter(!is.na(side)) %>%
  group_by(gameid, physical_faceoff_id, side) %>%
  summarise(total_xg = sum(xg_allattempts, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = side, values_from = total_xg,
                     values_fill = 0, names_prefix = "xg_")

fo_xg <- faceoff_registry %>%
  left_join(xg_by_side, by = c("gameid", "physical_faceoff_id")) %>%
  mutate(
    xg_winner = replace(xg_winner, is.na(xg_winner), 0),
    xg_loser  = replace(xg_loser,  is.na(xg_loser),  0)
  )

# ── 5. PLAYER-LEVEL TABLE ──────────────────────────────────────────────────────
# player_zone = zone from THIS player's own perspective.
# Loser's zone is the mirror of fo_zone: opponent's OZ = our DZ.
player_fo <- bind_rows(
  fo_xg %>% transmute(
    playerid    = winner_player,
    won         = TRUE,
    leverage,
    player_zone = fo_zone_winner,
    xg_for      = xg_winner,
    xg_against  = xg_loser
  ),
  fo_xg %>% transmute(
    playerid    = loser_player,
    won         = FALSE,
    leverage,
    player_zone = case_when(           # loser's zone = mirror of winner's
      fo_zone_winner == "Offensive Zone" ~ "Defensive Zone",
      fo_zone_winner == "Defensive Zone" ~ "Offensive Zone",
      TRUE                               ~ "Neutral Zone"
    ),
    xg_for      = xg_loser,
    xg_against  = xg_winner
  )
)

# ── 6. BUILD LEADERBOARD ───────────────────────────────────────────────────────
build_leaderboard <- function(data, min_fo) {
  data %>%
    group_by(playerid) %>%
    summarise(
      n_fo         = n(),
      n_won        = sum(won),
      win_pct      = mean(won) * 100,
      xg_created   = sum(xg_for[won]      * leverage[won]),
      xg_conceded  = sum(xg_against[!won] * leverage[!won]),
      impact_score = (xg_created - xg_conceded) / n_fo,
      .groups      = "drop"
    ) %>%
    filter(n_fo >= min_fo) %>%
    arrange(desc(impact_score))
}

lb_oz <- player_fo %>% filter(player_zone == "Offensive Zone") %>% build_leaderboard(MIN_FO_OZ)
lb_nz <- player_fo %>% filter(player_zone == "Neutral Zone")   %>% build_leaderboard(MIN_FO_NZ)
lb_dz <- player_fo %>% filter(player_zone == "Defensive Zone") %>% build_leaderboard(MIN_FO_DZ)

cat("Faceoff zone distribution (player perspective):\n")
print(table(player_fo$player_zone))
cat("\nOZ qualifying players:", nrow(lb_oz), "\n")
cat("NZ qualifying players:", nrow(lb_nz), "\n")
cat("DZ qualifying players:", nrow(lb_dz), "\n")
cat("\nOZ avg win%:", round(mean(lb_oz$win_pct), 1), "\n")
cat("DZ avg win%:", round(mean(lb_dz$win_pct), 1), "\n\n")

# ── 7. PLOT ────────────────────────────────────────────────────────────────────
select_three <- function(idx) {
  n <- length(idx)
  if (n == 0) return(integer(0))
  if (n <= 3) return(idx)
  c(idx[1], idx[ceiling(n / 2)], idx[n])
}

make_panel <- function(data, zone_label, min_fo_label, n = TOP_N) {
  top    <- data %>% arrange(desc(impact_score)) %>% slice_head(n = n)
  bottom <- data %>% arrange(impact_score)       %>% slice_head(n = n)
  plot_data <- bind_rows(
    bottom %>% mutate(.src = "bottom"),
    top    %>% mutate(.src = "top")
  ) %>%
    distinct(playerid, .keep_all = TRUE) %>%
    arrange(impact_score) %>%
    mutate(
      label     = factor(as.character(playerid), levels = as.character(playerid)),
      bar_fill  = ifelse(impact_score >= 0, "#00b894", "#8B1A1A"),
      txt_hjust = ifelse(impact_score >= 0, -0.1, 1.1)
    )

  divider_y <- sum(plot_data$.src == "bottom") + 0.5

  # 6 annotated players: top/mid/bottom of each half
  annot_idx <- unique(c(
    select_three(which(plot_data$.src == "bottom")),
    select_three(which(plot_data$.src == "top"))
  ))
  fo_annot <- plot_data[annot_idx, ] %>%
    filter(abs(impact_score) >= 0.001) %>%
    mutate(fo_label = sprintf("%.0f", 1 / abs(impact_score)))

  p <- ggplot(plot_data, aes(x = impact_score, y = label)) +
    geom_col(aes(fill = bar_fill), width = 0.72, show.legend = FALSE) +
    geom_hline(yintercept = divider_y, color = "gray50", linewidth = 0.6, linetype = "dashed") +
    geom_vline(xintercept = 0, color = "gray30", linewidth = 0.5) +
    scale_fill_identity() +
    scale_x_continuous(expand = expansion(mult = c(0.22, 0.22))) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      text               = element_text(face = "bold", color = "gray10"),
      panel.grid.major.y = element_blank()
    )

  p <- p + geom_text(
    data  = plot_data,
    aes(label = sprintf("%.0f%% win", win_pct), hjust = txt_hjust),
    size  = 3.2, color = "black", fontface = "bold", inherit.aes = TRUE
  )

  if (nrow(fo_annot) > 0) {
    fo_pos <- filter(fo_annot, impact_score >= 0)
    fo_neg <- filter(fo_annot, impact_score <  0)
    if (nrow(fo_pos) > 0)
      p <- p + geom_text(data = fo_pos,
        aes(x = impact_score / 2, y = label, label = fo_label),
        size = 3.0, color = "black", fontface = "bold", hjust = 0.5, inherit.aes = FALSE)
    if (nrow(fo_neg) > 0)
      p <- p + geom_text(data = fo_neg,
        aes(x = impact_score / 2, y = label, label = fo_label),
        size = 3.0, color = "white", fontface = "bold", hjust = 0.5, inherit.aes = FALSE)
  }
  p
}

p_oz <- make_panel(lb_oz, "Offensive Zone", MIN_FO_OZ)
p_nz <- make_panel(lb_nz, "Neutral Zone",   MIN_FO_NZ)
p_dz <- make_panel(lb_dz, "Defensive Zone", MIN_FO_DZ)

# Combined — all three zones side by side
ggsave("faceoff_impact_leaderboard_10s.png",
       p_oz | p_nz | p_dz,
       width = 18, height = 10, dpi = 150, bg = "white")
message("Saved: faceoff_impact_leaderboard_10s.png")

# Individual zone files
for (info in list(
  list(plot = p_oz, file = "faceoff_impact_leaderboard_oz_10s.png"),
  list(plot = p_nz, file = "faceoff_impact_leaderboard_nz_10s.png"),
  list(plot = p_dz, file = "faceoff_impact_leaderboard_dz_10s.png")
)) {
  ggsave(info$file, info$plot,
         width = 8, height = 10, dpi = 150, bg = "white")
  message("Saved: ", info$file)
}
