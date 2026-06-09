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
    playerKey, name, position,
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
# Non-rookie: weights sum to 1.000 across 5 projection sources + bonus credit
# Rookie:     weights sum to 1.000 across 4 sources (no lastSeasonSfb) + bonus credit
tradyr <- tradyr |>
  mutate(
    proj_pts = if_else(
      tradyrIsRookie,
      0.333 * projTradyrPts +
        0.271 * projMikeClayPts +
        0.208 * projFantasyProsPts +
        0.188 * projSleeperPts +
        meanBonusCredit,
      0.302 * projTradyrPts +
        0.245 * projMikeClayPts +
        0.189 * projFantasyProsPts +
        0.170 * projSleeperPts +
        0.094 * lastSeasonSfb +
        meanBonusCredit
    )
  )

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

# ── Save output ────────────────────────────────────────────────────────────────
saveRDS(tradyr, "data/tradyr_vbd.rds")
write_csv(tradyr, "data/tradyr_vbd.csv")

cat("Done.", nrow(tradyr), "players written to data/tradyr_vbd.rds and data/tradyr_vbd.csv\n")
