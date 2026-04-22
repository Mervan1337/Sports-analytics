rm(list = ls())
library(tidyverse)

# 1. Läs in data
df_raw <- read.csv("post_faceoff_events_10s.csv", stringsAsFactors = FALSE)

# 2. MASTER COUNT (Beräknas nu efter filtret för exakt matchning)
# Vi definierar facit längre ner

# 3. Registry - Identifiera varje lags startpunkt och roll
faceoff_registry <- df_raw %>%
  filter(eventname == "faceoff") %>%
  arrange(gameid, compiledgametime) %>%
  group_by(gameid) %>%
  mutate(
    join_id = ifelse(!is.na(lead(faceoff_sequence_id)) & (lead(faceoff_sequence_id) == faceoff_sequence_id + 1),
                     lead(faceoff_sequence_id), 
                     faceoff_sequence_id)
  ) %>%
  ungroup() %>%
  # --- VALIDERINGSSTEG: Behåll endast kompletta par (Lag A & B) ---
  group_by(gameid, join_id) %>%
  filter(n() == 2) %>% 
  ungroup() %>%
  # ---------------------------------------------------------------
mutate(
  my_zon = case_when(
    xadjcoord > 25   ~ "1. Offensiv Zon",
    xadjcoord > 5    ~ "2. Neutral Zon (Off)",
    xadjcoord > -5   ~ "3. Mitt-tekning",
    xadjcoord > -25  ~ "4. Neutral Zon (Def)",
    TRUE             ~ "5. Defensiv Zon"
  ),
  my_role = case_when(
    my_zon %in% c("1. Offensiv Zon", "2. Neutral Zon (Off)") ~ "Anfallande Lag",
    my_zon %in% c("4. Neutral Zon (Def)", "5. Defensiv Zon") ~ "Försvarande Lag",
    TRUE ~ ifelse(outcome == "successful", "Anfallande Lag", "Försvarande Lag")
  ),
  # UPPDATERAD: Hjälpvariabel för leverage-viktning med tre nivåer
  zone_type = case_when(
    abs(xadjcoord) > 25 ~ "Endzone",
    abs(xadjcoord) > 5  ~ "Neutral_Side",
    TRUE                ~ "Center"
  )
) %>%
  select(gameid, faceoff_sequence_id, join_id, teamid, my_zon, my_role, zone_type, period, scoredifferential, manpowersituation, outcome)

# Uppdaterad FACIT efter tvättning
expected_unique_fos <- nrow(faceoff_registry) / 2

cat("\n========================================================\n")
cat("FACIT (Efter tvättning):", expected_unique_fos, "validerade fysiska tekningar.\n")
cat("========================================================\n\n")

# 4. Event Stream (Inkluderar nu xG-kolumnen)
event_stream <- df_raw %>%
  select(gameid, faceoff_sequence_id, eventname, outcome, teamid, xadjcoord, teaminpossession, xg_allattempts) %>%
  rename(
    ev_name = eventname, 
    ev_outcome = outcome, 
    ev_team = teamid, 
    ev_x = xadjcoord,
    ev_possession = teaminpossession,
    ev_xg = xg_allattempts
  )

# 4.5 Beräkna Netto-xG per sekvens (Reward-motorn)
xg_by_sequence <- faceoff_registry %>%
  inner_join(event_stream, by = c("gameid", "join_id" = "faceoff_sequence_id"), relationship = "many-to-many") %>%
  filter(ev_name %in% c("shot", "goal")) %>%
  group_by(gameid, faceoff_sequence_id) %>%
  summarise(
    xg_winner = sum(ev_xg[ev_team == teamid[outcome == "successful"]], na.rm = TRUE),
    xg_loser  = sum(ev_xg[ev_team != teamid[outcome == "successful"]], na.rm = TRUE),
    .groups = "drop"
  )

# 5. Analysera utfall
analysis_data <- faceoff_registry %>%
  inner_join(event_stream, by = c("gameid", "join_id" = "faceoff_sequence_id"), relationship = "many-to-many") %>%
  group_by(gameid, faceoff_sequence_id, teamid, my_zon, my_role, zone_type) %>%
  summarise(
    i_made_goal    = any(ev_name == "goal" & ev_outcome == "successful" & ev_team == first(teamid)),
    opp_made_goal  = any(ev_name == "goal" & ev_outcome == "successful" & ev_team != first(teamid)),
    i_made_shot    = any(ev_name == "shot" & ev_outcome == "successful" & ev_team == first(teamid)),
    opp_made_shot  = any(ev_name == "shot" & ev_outcome == "successful" & ev_team != first(teamid)),
    
    i_attempt_shot = any(ev_name == "shot" & ev_team == first(teamid)),
    opp_attempt_shot = any(ev_name == "shot" & ev_team != first(teamid)),
    
    final_x = last(ev_x),
    last_possession_team = last(ev_possession),
    
    period = first(period),
    score_diff = first(scoredifferential),
    manpower = first(manpowersituation),
    outcome_val = first(outcome),
    .groups = "drop"
  ) %>%
  mutate(
    i_have_puck = ifelse(!is.na(last_possession_team) & last_possession_team == teamid, TRUE, FALSE),
    movement = case_when(
      final_x > 25  ~ "Offensiv Zon",
      final_x < -25 ~ "Defensiv Zon",
      TRUE          ~ "Mittzon"
    ),
    total_utfall = case_when(
      i_made_goal      ~ "01. Mål (Egna)",
      opp_made_goal    ~ "11. Mål (Insläppt)",
      i_made_shot      ~ "02. Avslut på mål (Egna)",
      opp_made_shot    ~ "10. Avslut på mål (Emot)",
      i_attempt_shot   ~ "03. Skottförsök (Egna - Miss/Block)",
      opp_attempt_shot ~ "09. Skottförsök (Emot - Miss/Block)",
      i_have_puck & movement == "Offensiv Zon" ~ "04. Innehav i Offensiv Zon",
      !i_have_puck & movement == "Offensiv Zon" ~ "05. Utan puck i Offensiv Zon",
      i_have_puck & movement == "Mittzon"      ~ "06. Innehav i Mittzon",
      !i_have_puck & movement == "Mittzon"     ~ "07. Utan puck i Mittzon",
      i_have_puck & movement == "Defensiv Zon" ~ "08. Innehav i Defensiv Zon",
      !i_have_puck & movement == "Defensiv Zon" ~ "09. Utan puck i Defensiv Zon",
      TRUE ~ "12. Odefinierat"
    )
  )

# 6. Beräkna FIS - UPPDATERAD LOGIK (Nu med ny Zon-trappa)
fis_results <- analysis_data %>%
  left_join(xg_by_sequence, by = c("gameid", "faceoff_sequence_id")) %>%
  mutate(
    xg_winner = replace_na(xg_winner, 0),
    xg_loser  = replace_na(xg_loser, 0),
    
    reward = ifelse(outcome_val == "successful", 
                    xg_winner - xg_loser, 
                    xg_loser - xg_winner),
    
    score_weight = 1 / (1 + abs(score_diff)),
    period_weight = case_when(period == 1 ~ 0.6, period == 2 ~ 0.8, period == 3 ~ 1.0, period >= 4 ~ 1.5, TRUE ~ 1.0),
    manpower_weight = ifelse(manpower == "evenStrength", 1.0, 1.3),
    
    # UPPDATERAD: Ny hierarkisk zon-viktning
    zone_weight = case_when(
      zone_type == "Endzone"      ~ 1.3,
      zone_type == "Neutral_Side" ~ 1.1,
      TRUE                        ~ 1.0
    ),
    
    leverage = score_weight * period_weight * manpower_weight * zone_weight,
    fis_score = reward * leverage
  )

# --- UTSKRIFTER ---

cat("\n--- TABELL 1: ZON IMPACT (FIS baserat på Netto-xG) ---\n")
zon_impact <- fis_results %>%
  group_by(my_zon) %>%
  summarise(
    Avg_FIS = round(mean(fis_score), 4), 
    Antal_Fysiska_Tekningar = n() / 2, 
    .groups = "drop"
  )
print(zon_impact)

cat("\n--- TABELL 3: FINAL REPORT (Logisk Rangordning) ---\n")
final_report <- fis_results %>%
  group_by(my_zon, my_role, total_utfall) %>%
  summarise(Antal_Tekningar = n(), .groups = "drop") %>%
  group_by(my_zon, my_role) %>%
  mutate(Procent = round(Antal_Tekningar / sum(Antal_Tekningar) * 100, 2)) %>%
  arrange(my_zon, my_role, total_utfall)
print(final_report, n = Inf)

cat("\n========================================================\n")
cat("SLUTKONTROLL: Summan i Tabell 1 är:", sum(zon_impact$Antal_Fysiska_Tekningar), " (Ska vara", expected_unique_fos, ")\n")
cat("========================================================\n")
