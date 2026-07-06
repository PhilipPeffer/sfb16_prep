suppressPackageStartupMessages(library(tidyverse))

# ── Reconstruct a weekly ADP/VBD history from git ──────────────────────────────
# Each "daily VBD update" commit of data/tradyr_vbd.csv is a day's snapshot.
# This walks those commits, pulls each version out of git, and assembles a tidy
# long history (one row per player per snapshot date). Re-runnable anytime; the
# git history is the source of truth.

TRACKED_FILE <- "data/tradyr_vbd.csv"
OUT_FILE     <- "data/vbd_history.rds"

# Columns kept from every snapshot. any_of() tolerates schema drift across old
# commits (e.g. the VOPR->VORP rename, or snapshots predating tier/pos_tier).
KEEP_COLS <- c("playerKey", "name", "position", "adp", "vbd_rank",
               "VBD", "weekly_pts", "proj_pts", "tier", "pos_tier")

# (sha, ISO commit date) for every commit that touched the tracked file, oldest
# first so the last commit on a given day wins the de-dup below.
log_lines <- system2("git",
  c("log", "--reverse", "--format=%H %cI", "--", TRACKED_FILE),
  stdout = TRUE)

if (length(log_lines) == 0) {
  stop("No git history found for ", TRACKED_FILE)
}

commits <- tibble(raw = log_lines) |>
  separate(raw, into = c("sha", "commit_iso"), sep = " ", extra = "merge") |>
  mutate(snapshot_date = as.Date(substr(commit_iso, 1, 10)))

read_snapshot <- function(sha, snapshot_date) {
  txt <- tryCatch(
    system2("git", c("show", paste0(sha, ":", TRACKED_FILE)),
            stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  if (length(txt) == 0) return(NULL)

  df <- tryCatch(
    suppressWarnings(read_csv(paste(txt, collapse = "\n"), show_col_types = FALSE)),
    error = function(e) NULL
  )
  if (is.null(df) || !"playerKey" %in% names(df)) return(NULL)

  df |>
    select(any_of(KEEP_COLS)) |>
    mutate(snapshot_date = snapshot_date)
}

history <- map2(commits$sha, commits$snapshot_date, read_snapshot) |>
  compact() |>
  bind_rows()

# Keep the last snapshot per calendar day (commits are oldest-first, so slice the
# final row per date/player).
history <- history |>
  group_by(snapshot_date, playerKey) |>
  slice_tail(n = 1) |>
  ungroup() |>
  arrange(snapshot_date, vbd_rank)

saveRDS(history, OUT_FILE)

cat("Wrote", OUT_FILE, "-",
    nrow(history), "rows across",
    length(unique(history$snapshot_date)), "snapshot dates (",
    format(min(history$snapshot_date)), "to",
    format(max(history$snapshot_date)), ")\n")
