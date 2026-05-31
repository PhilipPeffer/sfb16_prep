suppressPackageStartupMessages(library(tidyverse))

weekly <- readRDS("data/sfb16_weekly_scores.rds")

# ── Weekly ranks (used for top-n counting) ───────────────────────────────────
weekly <- weekly |>
  group_by(week) |>
  mutate(weekly_overall_rank = rank(-sfb16_fpts, ties.method = "min")) |>
  ungroup() |>
  group_by(week, position) |>
  mutate(weekly_pos_rank = rank(-sfb16_fpts, ties.method = "min")) |>
  ungroup() |>
  mutate(
    top20_overall = weekly_overall_rank <= 20,
    top10_pos     = weekly_pos_rank     <= 10
  )

# ── Weekly scores pivoted wide (week_1 … week_18) ────────────────────────────
week_cols <- paste0("week_", 1:18)

weekly_wide <- weekly |>
  select(player_id, week, sfb16_fpts) |>
  pivot_wider(names_from = week, values_from = sfb16_fpts, names_prefix = "week_") |>
  select(player_id, any_of(week_cols))

# ── Season totals ─────────────────────────────────────────────────────────────
season <- weekly |>
  arrange(player_id, week) |>
  group_by(player_id) |>
  summarize(
    player_name         = last(player_name),
    position            = last(position),
    team                = last(team),
    total_sfb16_fpts    = sum(sfb16_fpts),
    mean_sfb16_ppg     = mean(sfb16_fpts),
    median_sfb16_ppg   = median(sfb16_fpts),
    total_ppr_fpts      = sum(fantasy_points_ppr),
    mean_ppr_ppg        = mean(fantasy_points_ppr),
    median_ppr_ppg      = median(fantasy_points_ppr),
    games_played        = n(),
    top20_overall_weeks = sum(top20_overall),
    top10_pos_weeks     = sum(top10_pos),
    .groups = "drop"
  ) |>
  mutate(
    total_sfb16_rank = rank(-total_sfb16_fpts, ties.method = "min"),
    mean_sfb16_ppg_rank = rank(-mean_sfb16_ppg, ties.method = "min"),
    med_sfb16_ppg_rank = rank(-median_sfb16_ppg, ties.method = "min"),
    total_ppr_rank = rank(-total_ppr_fpts, ties.method = "min"),
    mean_ppr_ppg_rank = rank(-mean_ppr_ppg, ties.method = "min"),
    med_ppr_ppg_rank = rank(-median_ppr_ppg, ties.method = "min")
  ) |>
  group_by(position) |>
  mutate(position_rank = rank(-total_sfb16_fpts, ties.method = "min")) |>
  ungroup() |>
  left_join(weekly_wide, by = "player_id") |>
  select(
    total_sfb16_rank, total_ppr_rank, position_rank,
    mean_sfb16_ppg_rank, med_sfb16_ppg_rank,
    mean_ppr_ppg_rank, med_ppr_ppg_rank,
    player_name, player_id, position, team,
    total_sfb16_fpts, mean_sfb16_ppg, median_sfb16_ppg,
    total_ppr_fpts, mean_ppr_ppg, median_ppr_ppg, games_played,
    top20_overall_weeks, top10_pos_weeks,
    
    any_of(week_cols)
  ) |>
  arrange(total_sfb16_rank)

# ── Save ──────────────────────────────────────────────────────────────────────
saveRDS(season, "data/sfb16_season_totals.rds")
write_csv(season, "data/sfb16_season_totals.csv")

message("Done. Players: ", nrow(season), " | Columns: ", ncol(season))

# ── Quick preview ─────────────────────────────────────────────────────────────
cat("\n=== Top 20 overall ===\n")
season |>
  select(total_sfb16_rank, position_rank, player_name, position, team,
         total_sfb16_fpts, games_played, top20_overall_weeks, top10_pos_weeks) |>
  slice_head(n = 20) |>
  print(n = 20)
