suppressPackageStartupMessages(library(tidyverse))

GAMES     <- 17                       # season length used by calc_tradyr_vbd.R
PROJ_FILE <- "data/tradyr_vbd.rds"
DRAFT_DIR <- "mock_drafts"
OUT_CSV   <- "data/mock_draft_eval.csv"

# ── Load weighted projections ─────────────────────────────────────────────────
if (!file.exists(PROJ_FILE)) {
  stop("Projections file '", PROJ_FILE, "' not found. Run calc_tradyr_vbd.R first.")
}

proj <- readRDS(PROJ_FILE) |>
  select(name, proj_pts, weekly_pts) |>
  mutate(name_key = str_squish(str_to_lower(name)))

# ── Find mock draft files ─────────────────────────────────────────────────────
files <- list.files(DRAFT_DIR, pattern = "\\.txt$", full.names = TRUE)
if (length(files) == 0) {
  stop("No .txt line-up files found in '", DRAFT_DIR, "/'. ",
       "Add one player name per line, 20 lines per team.")
}

# ── Score each line-up ────────────────────────────────────────────────────────
unmatched   <- tibble(name = character(), file = character())
team_results <- list()

for (f in files) {
  team <- tools::file_path_sans_ext(basename(f))

  players <- readLines(f, warn = FALSE) |>
    str_squish() |>
    discard(~ .x == "")

  if (length(players) != 20) {
    warning("Line-up '", team, "' has ", length(players),
            " players (expected 20). Scoring first 10 as starters and ",
            "all ", length(players), " as the team.", call. = FALSE)
  }

  lineup <- tibble(name = players) |>
    mutate(slot_order = row_number(),
           name_key   = str_squish(str_to_lower(name))) |>
    left_join(proj, by = "name_key", suffix = c("_input", ""))

  miss <- lineup |> filter(is.na(proj_pts))
  if (nrow(miss) > 0) {
    unmatched <- bind_rows(unmatched, tibble(name = miss$name_input, file = basename(f)))
  }

  starters <- lineup |> slice_head(n = 10)

  team_results[[team]] <- tibble(
    team          = team,
    n_players     = nrow(lineup),
    starter_total = sum(starters$proj_pts),
    starter_ppg   = sum(starters$proj_pts) / GAMES,
    team_total    = sum(lineup$proj_pts),
    team_ppg      = sum(lineup$proj_pts) / GAMES
  )
}

# ── Hard error on any unmatched names (all at once) ───────────────────────────
if (nrow(unmatched) > 0) {
  msg <- unmatched |>
    mutate(line = paste0("  - ", name, "  (", file, ")")) |>
    pull(line) |>
    paste(collapse = "\n")
  stop("Unmatched player names (fix spelling to match the 'name' column in ",
       "data/tradyr_vbd.csv):\n", msg, call. = FALSE)
}

# ── Rank and report ───────────────────────────────────────────────────────────
results <- bind_rows(team_results) |>
  mutate(
    starter_rank = min_rank(desc(starter_total)),
    team_rank    = min_rank(desc(team_total))
  ) |>
  arrange(starter_rank)

write_csv(results, OUT_CSV)

display <- results |>
  transmute(
    team,
    n_players,
    starter_total = round(starter_total, 1),
    starter_ppg   = round(starter_ppg, 1),
    team_total    = round(team_total, 1),
    team_ppg      = round(team_ppg, 1),
    starter_rank,
    team_rank
  )

cat("\n=== Mock Draft Evaluation (", nrow(results), " teams) ===\n\n", sep = "")
print(as.data.frame(display), row.names = FALSE)

best_starter <- results |> slice_max(starter_total, n = 1, with_ties = FALSE)
best_team    <- results |> slice_max(team_total,    n = 1, with_ties = FALSE)

cat("\nBest starting line-up: ", best_starter$team,
    "  (", round(best_starter$starter_total, 1), " pts, ",
    round(best_starter$starter_ppg, 1), " ppg)\n", sep = "")
cat("Best whole team:       ", best_team$team,
    "  (", round(best_team$team_total, 1), " pts, ",
    round(best_team$team_ppg, 1), " ppg)\n\n", sep = "")

cat("Wrote", OUT_CSV, "\n")
