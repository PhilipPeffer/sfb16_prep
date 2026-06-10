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
  rename(meanBonusCredit = bonusCredit.mean) |>
  select(
    playerKey, playerId, name, position,
    projCompositePts, projSleeperPts, projFantasyProsPts,
    projMikeClayPts, projTradyrPts,
    tradyrIsRookie, lastSeasonSfb, meanBonusCredit
  ) |>
  filter(position %in% c("QB", "RB", "WR", "TE")) |>
  mutate(
    tradyrIsRookie  = as.logical(tradyrIsRookie),
    lastSeasonSfb   = coalesce(as.numeric(lastSeasonSfb), 0),
    meanBonusCredit = coalesce(as.numeric(meanBonusCredit), 0)
  )

# ── Weighted projected season totals ──────────────────────────────────────────
# Weights are renormalized across whichever sources are available for each player
# (0 or NA treated as missing). bonus credit is always additive on top.
weighted_proj <- function(vals, weights) {
  available <- !is.na(vals) & vals > 0
  if (!any(available)) return(NA_real_)
  w <- weights[available]
  sum(vals[available] * w / sum(w))
}

veteran_weights <- c(0.30, 0.25, 0.21, 0.19, 0.05)
rookie_weights  <- c(0.316, 0.263, 0.221, 0.2)

tradyr <- tradyr |>
  rowwise() |>
  mutate(
    proj_pts = weighted_proj(
      if (tradyrIsRookie)
        c(projTradyrPts, projMikeClayPts, projFantasyProsPts, projSleeperPts)
      else
        c(projTradyrPts, projMikeClayPts, projFantasyProsPts, projSleeperPts, (lastSeasonSfb - meanBonusCredit)),
      if (tradyrIsRookie) rookie_weights else veteran_weights
    ) + meanBonusCredit,
    weekly_pts = proj_pts/17
  ) |>
  ungroup()

# ── VOLS / VOPR / VBD ─────────────────────────────────────────────────────────
# League structure: 12 teams, 20 roster spots each (10 starters + 10 bench)
#   Starters: 2 QB/superflex + 8 RB/WR/TE flex = 10
#   VOLS QB threshold:   2 starters × 12 teams = 24th QB
#   VOLS flex threshold: 8 starters × 12 teams = 96th RB/WR/TE
#   VOPR replacement:   20 roster spots × 12 teams = 240th player overall

tradyr <- tradyr |>
  mutate(is_qb = position == "QB") |>
  group_by(is_qb) |>
  mutate(pos_rank = min_rank(desc(proj_pts))) |>
  ungroup()

QB_LS   <- tradyr |> filter(is_qb,  pos_rank == 2 * 12)
Flex_LS <- tradyr |> filter(!is_qb, pos_rank == 8 * 12)
LB      <- tradyr |> arrange(desc(proj_pts)) |> slice(min(20 * 12, n()))

tradyr <- tradyr |>
  mutate(
    VOLS     = if_else(is_qb,
                       proj_pts - QB_LS$proj_pts,
                       proj_pts - Flex_LS$proj_pts),
    VOPR     = proj_pts - LB$proj_pts,
    VBD      = VOLS + VOPR,
    vbd_rank = min_rank(desc(VBD))
  ) |>
  select(-is_qb, -pos_rank) |>
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
# Players sorted by vbd_rank; gap to next player computed from VBD values.
# A new tier starts when the drop to the next player exceeds 2x the local
# median gap (rolling window of ±5 neighbours).
vbd_sorted <- tradyr |> arrange(vbd_rank) |> pull(VBD)
gaps        <- abs(diff(vbd_sorted))          # drop from player i to i+1
window      <- 5

local_median_gap <- map_dbl(seq_along(gaps), \(i) {
  idx <- max(1L, i - window):min(length(gaps), i + window)
  median(gaps[idx])
})

tier_break <- gaps > 2 * local_median_gap     # TRUE where a new tier starts
tier       <- cumsum(c(TRUE, tier_break))      # tier 1 for first player

tradyr <- tradyr |>
  arrange(vbd_rank) |>
  mutate(tier = tier)

# ── Save output ────────────────────────────────────────────────────────────────
saveRDS(tradyr, "data/tradyr_vbd.rds")
write_csv(tradyr, "data/tradyr_vbd.csv")

cat("Done.", nrow(tradyr), "players written to data/tradyr_vbd.rds and data/tradyr_vbd.csv\n")
