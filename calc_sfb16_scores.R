if (!requireNamespace("nflreadr", quietly = TRUE)) renv::install("nflreadr")
if (!requireNamespace("tidyverse", quietly = TRUE)) renv::install("tidyverse")

library(nflreadr)
library(tidyverse)

# ── Player stats (2025 regular season) ───────────────────────────────────────
player_stats <- load_player_stats(seasons = 2025) |>
  filter(season_type == "REG")

# ── Play-by-play (2025 regular season) ───────────────────────────────────────
pbp <- load_pbp(seasons = 2025) |>
  filter(season_type == "REG")

# ── Long-play bonus counts from PBP ──────────────────────────────────────────

pass_40plus <- pbp |>
  filter(pass == 1, yards_gained >= 40, !is.na(passer_player_id)) |>
  count(passer_player_id, week, name = "n_pass_40plus")

rush_40plus <- pbp |>
  filter(rush == 1, yards_gained >= 40, !is.na(rusher_player_id)) |>
  count(rusher_player_id, week, name = "n_rush_40plus")

rec_20plus <- pbp |>
  filter(pass == 1, complete_pass == 1, !is.na(receiver_player_id),
         !is.na(receiving_yards), receiving_yards >= 20) |>
  count(receiver_player_id, week, name = "n_rec_20plus")

# ── Join PBP bonuses onto player stats ───────────────────────────────────────
scores <- player_stats |>
  left_join(pass_40plus, by = c("player_id" = "passer_player_id", "week")) |>
  left_join(rush_40plus, by = c("player_id" = "rusher_player_id", "week")) |>
  left_join(rec_20plus,  by = c("player_id" = "receiver_player_id", "week")) |>
  replace_na(list(n_pass_40plus = 0L, n_rush_40plus = 0L, n_rec_20plus = 0L))

# ── Calculate SFB16 fantasy points ───────────────────────────────────────────
scores <- scores |>
  mutate(
    .rush_rec_yds = coalesce(rushing_yards, 0) + coalesce(receiving_yards, 0),
    .is_te        = position == "TE",

    pts_pass_yds    = coalesce(passing_yards, 0) * 0.04,
    pts_pass_tds    = coalesce(passing_tds, 0) * 6,
    pts_pass_2pt    = coalesce(passing_2pt_conversions, 0) * 2,

    pts_rush_yds    = coalesce(rushing_yards, 0) * 0.1,
    pts_rush_tds    = coalesce(rushing_tds, 0) * 6,
    pts_rush_2pt    = coalesce(rushing_2pt_conversions, 0) * 2,

    pts_rec_yds     = coalesce(receiving_yards, 0) * 0.1,
    pts_rec_tds     = coalesce(receiving_tds, 0) * 6,
    pts_rec_2pt     = coalesce(receiving_2pt_conversions, 0) * 2,
    pts_receptions  = coalesce(receptions, 0) * 0.5,

    # rushing + receiving first downs, 0.5 pts each for all positions
    pts_first_downs = (coalesce(rushing_first_downs, 0) + coalesce(receiving_first_downs, 0)) * 0.5,

    # TE premium: +1 per reception and +1 per first down (rush or rec)
    pts_te_rec_bonus = if_else(.is_te, coalesce(receptions, 0) * 1.0, 0),
    pts_te_fd_bonus  = if_else(.is_te,
      (coalesce(rushing_first_downs, 0) + coalesce(receiving_first_downs, 0)) * 1.0, 0),

    # Passing yard milestones — stack: 400+ earns both bonuses (+20 total)
    pts_pass_300    = if_else(coalesce(passing_yards, 0) >= 300, 10, 0),
    pts_pass_400    = if_else(coalesce(passing_yards, 0) >= 400, 10, 0),

    # Combined rush/rec milestones — stack: 200+ earns both bonuses (+20 total)
    pts_rr_100      = if_else(.rush_rec_yds >= 100, 10, 0),
    pts_rr_200      = if_else(.rush_rec_yds >= 200, 10, 0),

    # Long-play bonuses from PBP
    pts_pass_40plus = n_pass_40plus * 10,
    pts_rush_40plus = n_rush_40plus * 10,
    pts_rec_20plus  = n_rec_20plus  * 10,

    sfb16_fpts =
      pts_pass_yds    + pts_pass_tds    + pts_pass_2pt    +
      pts_rush_yds    + pts_rush_tds    + pts_rush_2pt    +
      pts_rec_yds     + pts_rec_tds     + pts_rec_2pt     +
      pts_receptions  + pts_first_downs +
      pts_te_rec_bonus + pts_te_fd_bonus +
      pts_pass_300    + pts_pass_400    +
      pts_rr_100      + pts_rr_200      +
      pts_pass_40plus + pts_rush_40plus + pts_rec_20plus
  ) |>
  select(-.rush_rec_yds, -.is_te)

# ── Save ─────────────────────────────────────────────────────────────────────
dir.create("data", showWarnings = FALSE)
saveRDS(scores, "data/sfb16_weekly_scores.rds")
write_csv(scores, "data/sfb16_weekly_scores.csv")

message("Done. Rows: ", nrow(scores), " | Weeks covered: ", n_distinct(scores$week))
