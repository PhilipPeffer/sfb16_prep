suppressPackageStartupMessages(library(tidyverse))
library(httr)
library(jsonlite)

# ── Fetch Tradyr projections ───────────────────────────────────────────────────
resp <- httr::GET("https://api.tradyr.app/api/sfb/adp-projection?season=2026&limit=400")
httr::stop_for_status(resp)

raw <- jsonlite::fromJSON(
  httr::content(resp, as = "text", encoding = "UTF-8"),
  simplifyDataFrame = TRUE,
  flatten = TRUE
)

# ── Parse and select fields ────────────────────────────────────────────────────
tradyr <- as_tibble(raw$players) |>
  select(
    playerKey, playerId, name, position,
    projMikeClayBasePts, projFantasyProsBasePts, projSleeperBasePts,
    tradyrIsRookie, lastSeasonSfb
  ) |>
  filter(position %in% c("QB", "RB", "WR", "TE")) |>
  mutate(
    tradyrIsRookie = as.logical(tradyrIsRookie),
    lastSeasonSfb  = coalesce(as.numeric(lastSeasonSfb), 0)
  )

# ── Weighted projected season totals ──────────────────────────────────────────
# Pre-bonus projection blends the three BasePts sources. Weights are renormalized
# across whichever sources are available for each player (0 or NA = missing).
weighted_proj <- function(vals, weights) {
  available <- !is.na(vals) & vals > 0
  if (!any(available)) return(0)
  w <- weights[available]
  sum(vals[available] * w / sum(w))
}

base_weights <- c(0.375, 0.375, 0.25)   # MikeClay, FantasyPros, Sleeper

tradyr <- tradyr |>
  rowwise() |>
  mutate(
    pre_bonus_proj_pts = weighted_proj(
      c(projMikeClayBasePts, projFantasyProsBasePts, projSleeperBasePts),
      base_weights
    )
  ) |>
  ungroup()

# ── Positional-tier bonus ──────────────────────────────────────────────────────
# Rank players within position by pre-bonus points, then add a fixed bonus based
# on the tier the player falls into. Rank thresholds below are cumulative upper
# bounds (e.g. QB "<= 12" means ranks 7-12). WR's lowest bucket (37-60) extends
# open-ended to 61+.
tier_bonus_fn <- function(position, rank) {
  case_when(
    position == "QB" & rank <=  6 ~ 178.33,
    position == "QB" & rank <= 12 ~ 134.00,
    position == "QB"             ~  89.00,
    position == "RB" & rank <=  6 ~ 157.00,
    position == "RB" & rank <= 12 ~ 103.00,
    position == "RB" & rank <= 24 ~  72.50,
    position == "RB" & rank <= 36 ~  45.17,
    position == "RB"             ~  26.08,
    position == "TE" & rank <=  6 ~ 147.67,
    position == "TE" & rank <= 12 ~  82.67,
    position == "TE"             ~  58.00,
    position == "WR" & rank <=  6 ~ 298.00,
    position == "WR" & rank <= 12 ~ 229.67,
    position == "WR" & rank <= 24 ~ 176.33,
    position == "WR" & rank <= 36 ~ 144.00,
    position == "WR"             ~ 104.33,
    TRUE ~ 0
  )
}

tradyr <- tradyr |>
  group_by(position) |>
  mutate(pos_rank_prebonus = min_rank(desc(pre_bonus_proj_pts))) |>
  ungroup() |>
  mutate(tier_bonus = tier_bonus_fn(position, pos_rank_prebonus))

# ── Final projection ───────────────────────────────────────────────────────────
# Add the tier bonus, then blend in last season's SFB total for veterans. When a
# veteran has no last-season data (lastSeasonSfb == 0, e.g. missed the season),
# keep the full projection instead of applying the 15% haircut.
tradyr <- tradyr |>
  mutate(
    proj_pts   = pre_bonus_proj_pts + tier_bonus,
    proj_pts   = if_else(tradyrIsRookie | lastSeasonSfb == 0,
                         proj_pts,
                         0.85 * proj_pts + 0.15 * lastSeasonSfb),
    weekly_pts = proj_pts / 17
  )

# ── VOLS / VORP / VBD ─────────────────────────────────────────────────────────
# League structure: 12 teams, 20 roster spots each (10 starters + 10 bench)
#   Starters: 2 QB/superflex + 8 RB/WR/TE flex = 10
#   VOLS QB threshold:   2 starters × 12 teams = 24th QB
#   VOLS flex threshold: 8 starters × 12 teams = 96th RB/WR/TE
#   VORP replacement:   20 roster spots × 12 teams = 240th player overall

tradyr <- tradyr |>
  mutate(is_qb = position == "QB")

# Last starter / replacement players selected by position from the sorted list.
# Using slice(min(threshold, n())) is tie-robust (min_rank can skip an exact
# boundary rank, leaving filter(rank == threshold) empty).
QB_LS   <- tradyr |> filter(is_qb)  |> arrange(desc(proj_pts)) |> slice(min(2 * 12, n()))
Flex_LS <- tradyr |> filter(!is_qb) |> arrange(desc(proj_pts)) |> slice(min(8 * 12, n()))
LB      <- tradyr |> arrange(desc(proj_pts)) |> slice(min(20 * 12, n()))

tradyr <- tradyr |>
  mutate(
    VOLS     = if_else(is_qb,
                       proj_pts - QB_LS$proj_pts,
                       proj_pts - Flex_LS$proj_pts),
    VORP     = proj_pts - LB$proj_pts,
    VBD      = VOLS + VORP,
    vbd_rank = min_rank(desc(VBD))
  ) |>
  select(-is_qb) |>
  arrange(vbd_rank)

# ── Fetch mock ADP ────────────────────────────────────────────────────────────
resp_adp <- httr::GET("https://api.tradyr.app/api/sfb/mock-adp?force=1")
httr::stop_for_status(resp_adp)

raw_adp <- jsonlite::fromJSON(
  httr::content(resp_adp, as = "text", encoding = "UTF-8"),
  simplifyDataFrame = TRUE,
  flatten = TRUE
)

mock_adp <- as_tibble(raw_adp$players) |>
  select(playerId, adp) |>
  mutate(playerId = as.character(playerId))

# ── Join ADP and calculate value vs ADP ───────────────────────────────────────
# value_vs_adp > 0 means player is ranked higher by VBD than draft consensus
tradyr <- tradyr |>
  mutate(playerId = as.character(playerId)) |>
  left_join(mock_adp, by = "playerId") |>
  mutate(value_vs_adp = adp - vbd_rank)

# ── VBD tiers ─────────────────────────────────────────────────────────────────
# Players sorted by descending VBD; gap to next player computed from VBD values.
# A new tier starts when the drop to the next player exceeds 2x the local
# median gap (rolling window of ±5 neighbours).
assign_tiers <- function(vbd, window = 5) {
  ord  <- order(-vbd)
  gaps <- abs(diff(vbd[ord]))
  if (length(gaps) == 0) return(rep(1L, length(vbd)))

  local_median_gap <- map_dbl(seq_along(gaps), \(i) {
    idx <- max(1L, i - window):min(length(gaps), i + window)
    median(gaps[idx])
  })

  tier_break <- gaps > 2 * local_median_gap   # TRUE where a new tier starts
  tiers      <- cumsum(c(TRUE, tier_break))    # tier 1 for first player

  tiers[order(ord)]                            # back to original row order
}

tradyr <- tradyr |>
  mutate(tier = assign_tiers(VBD)) |>
  group_by(position) |>
  mutate(pos_tier = assign_tiers(VBD, window = 2)) |>
  ungroup() |>
  arrange(vbd_rank)

# ── Save output ────────────────────────────────────────────────────────────────
saveRDS(tradyr, "data/tradyr_vbd.rds")
write_csv(tradyr, "data/tradyr_vbd.csv")

cat("Done.", nrow(tradyr), "players written to data/tradyr_vbd.rds and data/tradyr_vbd.csv\n")
