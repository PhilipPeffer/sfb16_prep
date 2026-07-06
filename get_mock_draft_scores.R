suppressPackageStartupMessages(library(tidyverse))
library(httr)
library(jsonlite)

# ── Config ────────────────────────────────────────────────────────────────────
GAMES      <- 17                         # season length used by calc_tradyr_vbd.R
MY_TEAM    <- "828350705524928512"       # my Sleeper user id
PROJ_FILE  <- "data/tradyr_vbd.rds"
IDS_FILE   <- "mock_drafts.txt"
OUT_DIR    <- "data/mock_drafts"         # one CSV per draft
MY_SUMMARY <- "data/my_team_summary.csv" # my team's results across all drafts
PICKS_URL  <- "https://api.sleeper.app/v1/draft/%s/picks"

# ── Load weighted projections ─────────────────────────────────────────────────
if (!file.exists(PROJ_FILE)) {
  stop("Projections file '", PROJ_FILE, "' not found. Run calc_tradyr_vbd.R first.")
}

proj_raw <- readRDS(PROJ_FILE)
proj <- proj_raw |>
  transmute(playerId = as.character(playerId), proj_position = position, proj_pts)

# Baseline for undrafted/unprojected players: the first replacement player's
# projected total (240th player overall, where VORP == 0 in tradyr_vbd.rds).
BASELINE_PTS <- proj_raw |>
  slice_min(abs(VORP), n = 1, with_ties = FALSE) |>
  pull(proj_pts)

# ── Optimal SFB16 starting line-up: best 2 QB + best 8 RB/WR/TE ────────────────
optimal_starter_total <- function(position, proj_pts) {
  df  <- tibble(position, proj_pts)
  qb  <- df |> filter(position == "QB") |> slice_max(proj_pts, n = 2, with_ties = FALSE)
  flx <- df |> filter(position %in% c("RB", "WR", "TE")) |>
    slice_max(proj_pts, n = 8, with_ties = FALSE)
  sum(qb$proj_pts) + sum(flx$proj_pts)
}

# ── Score a single draft ──────────────────────────────────────────────────────
process_draft <- function(draft_id) {
  resp <- httr::GET(sprintf(PICKS_URL, draft_id))
  httr::stop_for_status(resp)
  picks <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    flatten = TRUE
  )

  if (length(picks) == 0 || nrow(picks) == 0) {
    warning("Draft ", draft_id, " returned no picks; skipping.", call. = FALSE)
    return(invisible(NULL))
  }

  picks <- as_tibble(picks) |>
    transmute(
      draft_slot,
      picked_by      = coalesce(as.character(picked_by), ""),
      player_id      = as.character(player_id),
      meta_position  = .data[["metadata.position"]]
    ) |>
    left_join(proj, by = c("player_id" = "playerId")) |>
    mutate(
      position    = coalesce(proj_position, meta_position),
      is_baseline = is.na(proj_pts),
      proj_pts    = coalesce(proj_pts, BASELINE_PTS)
    )

  missing <- picks |> filter(is_baseline)
  if (nrow(missing) > 0) {
    warning("Draft ", draft_id, ": ", nrow(missing),
            " drafted player(s) had no projection (assigned baseline ",
            round(BASELINE_PTS, 1), " pts): ",
            paste(unique(missing$player_id), collapse = ", "), call. = FALSE)
  }

  teams <- picks |>
    group_by(draft_slot) |>
    summarise(
      picked_by     = first(picked_by),
      n_players     = n(),
      team_total    = sum(proj_pts),
      starter_total = optimal_starter_total(position, proj_pts),
      .groups       = "drop"
    ) |>
    mutate(
      draft_id     = as.character(draft_id),
      is_my_team   = picked_by == MY_TEAM,
      starter_ppg  = starter_total / GAMES,
      team_ppg     = team_total / GAMES,
      starter_rank = min_rank(desc(starter_total)),
      team_rank    = min_rank(desc(team_total))
    ) |>
    select(draft_id, draft_slot, picked_by, is_my_team, n_players,
           starter_total, starter_ppg, team_total, team_ppg,
           starter_rank, team_rank) |>
    arrange(team_rank)

  out_file <- file.path(OUT_DIR, paste0("draft_", draft_id, ".csv"))
  write_csv(teams, out_file)

  cat("\n=== Draft ", draft_id, " ===\n", sep = "")
  teams |>
    mutate(across(c(starter_total, starter_ppg, team_total, team_ppg), \(x) round(x, 1))) |>
    as.data.frame() |>
    print(row.names = FALSE)

  best_s <- teams |> slice_max(starter_total, n = 1, with_ties = FALSE)
  best_t <- teams |> slice_max(team_total,    n = 1, with_ties = FALSE)
  cat("Best starters: slot ", best_s$draft_slot, " (", round(best_s$starter_total, 1),
      " pts)", if (best_s$is_my_team) "  <-- my team" else "", "\n", sep = "")
  cat("Best team:     slot ", best_t$draft_slot, " (", round(best_t$team_total, 1),
      " pts)", if (best_t$is_my_team) "  <-- my team" else "", "\n", sep = "")
  cat("Wrote", out_file, "\n")

  invisible(teams)
}

# ── Main ──────────────────────────────────────────────────────────────────────
if (!file.exists(IDS_FILE)) {
  stop("Draft id list '", IDS_FILE, "' not found. Add one Sleeper draft id per line.")
}

draft_ids <- readLines(IDS_FILE, warn = FALSE) |>
  str_squish() |>
  discard(~ .x == "" || str_starts(.x, "#"))

if (length(draft_ids) == 0) stop("No draft ids found in ", IDS_FILE)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Incremental: only fetch/compute drafts without an existing CSV.
for (id in draft_ids) {
  out_file <- file.path(OUT_DIR, paste0("draft_", id, ".csv"))
  if (file.exists(out_file)) {
    cat("Skipping draft", id, "- already computed (", out_file, ")\n")
    next
  }
  process_draft(id)
}

# ── Rebuild my-team summary from all per-draft CSVs listed in mock_drafts.txt ──
my_rows <- map(draft_ids, \(id) {
  f <- file.path(OUT_DIR, paste0("draft_", id, ".csv"))
  if (!file.exists(f)) return(NULL)
  read_csv(f, show_col_types = FALSE, col_types = cols(.default = "?",
           draft_id = "c", picked_by = "c")) |>
    filter(picked_by == MY_TEAM) |>
    select(draft_id, n_players, starter_total, starter_ppg,
           team_total, team_ppg, starter_rank, team_rank)
}) |>
  compact() |>
  bind_rows()

if (nrow(my_rows) > 0) {
  write_csv(my_rows, MY_SUMMARY)
  cat("\nWrote", MY_SUMMARY, "with", nrow(my_rows), "draft(s) for my team.\n")
} else {
  warning("My team (", MY_TEAM, ") not found in any draft results.", call. = FALSE)
}
