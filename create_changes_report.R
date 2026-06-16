suppressPackageStartupMessages(library(tidyverse))

OUT_FILE  <- "reports/changes_report.html"
NEW_FILE  <- "data/tradyr_vbd.rds"
PREV_FILE <- "data/tradyr_vbd_prev.rds"

# ── Helpers ───────────────────────────────────────────────────────────────────
fmt_num <- function(x, digits = 1, signed = FALSE) {
  fmt <- paste0("%", if (signed) "+" else "", ".", digits, "f")
  if_else(is.na(x), "–", sprintf(fmt, x))
}

esc_html <- function(x) {
  x |>
    str_replace_all("&", "&amp;") |>
    str_replace_all("<", "&lt;") |>
    str_replace_all(">", "&gt;")
}

# ── Load data ─────────────────────────────────────────────────────────────────
new_data <- readRDS(NEW_FILE)

if (!file.exists(PREV_FILE)) {
  html <- paste0(
    '<!DOCTYPE html><html><head><meta charset="utf-8">',
    '<title>Changes Report — ', Sys.Date(), '</title>',
    '<style>body{font:10pt Arial,sans-serif;margin:1in}</style>',
    '</head><body>',
    '<h1>SFB16 Daily Changes Report — ', format(Sys.Date(), "%B %d, %Y"), '</h1>',
    '<p>No previous snapshot found. This is the first run. ',
    "Tomorrow’s report will show changes from today’s data.</p>",
    '</body></html>'
  )
  dir.create("reports", showWarnings = FALSE)
  writeLines(enc2utf8(html), OUT_FILE, useBytes = TRUE)
  cat("No previous snapshot — stub report written to", OUT_FILE, "\n")
  quit(status = 0)
}

prev_data <- readRDS(PREV_FILE)

# ── Compute deltas ────────────────────────────────────────────────────────────
joined <- full_join(
  new_data  |> select(playerKey, name, position,
                       vbd_rank, VBD, weekly_pts, adp, tier, pos_tier),
  prev_data |> select(playerKey, name, position,
                       vbd_rank, VBD, weekly_pts, adp, tier, pos_tier),
  by     = "playerKey",
  suffix = c("_new", "_prev")
)

new_players <- joined |> filter(is.na(vbd_rank_prev))
dropped     <- joined |> filter(is.na(vbd_rank_new))
both        <- joined |>
  filter(!is.na(vbd_rank_prev), !is.na(vbd_rank_new)) |>
  mutate(
    delta_vbd_rank   = vbd_rank_prev - vbd_rank_new,   # positive = moved up
    delta_weekly     = weekly_pts_new - weekly_pts_prev,
    delta_adp        = adp_prev - adp_new,              # positive = ADP improved
    tier_changed     = tier_new     != tier_prev,
    pos_tier_changed = pos_tier_new != pos_tier_prev
  )

# ── CSS ───────────────────────────────────────────────────────────────────────
CSS <- '
body { font: 9pt Arial, sans-serif; margin: 0.5in; }
h1   { font-size: 14pt; margin: 0 0 4px; }
h2   { font-size: 11pt; border-bottom: 2px solid #000; margin: 20px 0 4px; }
.subtitle { color: #555; font-size: 8pt; margin: 0 0 14px; }
table { border-collapse: collapse; width: 100%; margin-bottom: 8px; }
th, td { padding: 2px 6px; white-space: nowrap; }
th { text-align: left; border-bottom: 1px solid #888; font-size: 8pt; }
td.num { text-align: right; font-variant-numeric: tabular-nums; }
tbody tr:nth-child(even) td { background: #f5f5f5; }
.up   { color: #1a7a1a; font-weight: bold; }
.down { color: #b00020; font-weight: bold; }
.summary-grid { display: flex; gap: 16px; flex-wrap: wrap; margin-bottom: 12px; }
.summary-box  { border: 1px solid #ccc; border-radius: 4px;
                padding: 8px 14px; min-width: 110px; }
.summary-box .val { font-size: 20pt; font-weight: bold; line-height: 1.1; }
.summary-box .lbl { font-size: 7.5pt; color: #555; }
'

# ── Section builders ──────────────────────────────────────────────────────────
build_rank_table <- function(df, direction, n = 20) {
  d <- df |>
    arrange(if (direction == "up") desc(delta_vbd_rank) else delta_vbd_rank) |>
    slice_head(n = n) |>
    filter(abs(delta_vbd_rank) >= 1)

  if (nrow(d) == 0) return("<p><em>None.</em></p>")

  rows <- d |>
    mutate(row_html = sprintf(
      '<tr><td>%s</td><td>%s</td><td class="num %s">%s</td><td class="num">%s</td><td class="num">%s</td></tr>',
      esc_html(name_new), position_new,
      if_else(delta_vbd_rank > 0, "up", "down"),
      fmt_num(delta_vbd_rank, 0, signed = TRUE),
      fmt_num(vbd_rank_prev, 0),
      fmt_num(vbd_rank_new,  0)
    )) |>
    pull(row_html)

  paste0(
    '<table><thead><tr><th>Player</th><th>Pos</th><th>Δ Rank</th>',
    '<th>Prev Rank</th><th>New Rank</th></tr></thead><tbody>\n',
    paste(rows, collapse = "\n"), '\n</tbody></table>'
  )
}

build_proj_table <- function(df, n = 20) {
  d <- df |>
    arrange(desc(abs(delta_weekly))) |>
    slice_head(n = n) |>
    filter(abs(delta_weekly) >= 0.05)

  if (nrow(d) == 0) return("<p><em>None.</em></p>")

  rows <- d |>
    mutate(row_html = sprintf(
      '<tr><td>%s</td><td>%s</td><td class="num %s">%s</td><td class="num">%s</td><td class="num">%s</td></tr>',
      esc_html(name_new), position_new,
      if_else(delta_weekly > 0, "up", "down"),
      fmt_num(delta_weekly, 2, signed = TRUE),
      fmt_num(weekly_pts_prev, 1),
      fmt_num(weekly_pts_new,  1)
    )) |>
    pull(row_html)

  paste0(
    '<table><thead><tr><th>Player</th><th>Pos</th><th>Δ Pts/Wk</th>',
    '<th>Prev</th><th>New</th></tr></thead><tbody>\n',
    paste(rows, collapse = "\n"), '\n</tbody></table>'
  )
}

build_adp_table <- function(df, n = 20) {
  d <- df |>
    filter(!is.na(adp_new), !is.na(adp_prev)) |>
    arrange(desc(abs(delta_adp))) |>
    slice_head(n = n) |>
    filter(abs(delta_adp) >= 1)

  if (nrow(d) == 0) return("<p><em>None.</em></p>")

  rows <- d |>
    mutate(row_html = sprintf(
      '<tr><td>%s</td><td>%s</td><td class="num %s">%s</td><td class="num">%s</td><td class="num">%s</td></tr>',
      esc_html(name_new), position_new,
      if_else(delta_adp > 0, "up", "down"),
      fmt_num(delta_adp, 1, signed = TRUE),
      fmt_num(adp_prev, 1),
      fmt_num(adp_new,  1)
    )) |>
    pull(row_html)

  paste0(
    '<table><thead><tr><th>Player</th><th>Pos</th><th>Δ ADP</th>',
    '<th>Prev ADP</th><th>New ADP</th></tr></thead><tbody>\n',
    paste(rows, collapse = "\n"), '\n</tbody></table>'
  )
}

build_tier_table <- function(df) {
  d <- df |>
    filter(pos_tier_changed) |>
    arrange(vbd_rank_new)

  if (nrow(d) == 0) return("<p><em>No tier changes.</em></p>")

  rows <- d |>
    mutate(row_html = sprintf(
      '<tr><td>%s</td><td>%s</td><td class="num">%s → %s</td><td class="num">%s → %s</td></tr>',
      esc_html(name_new), position_new,
      tier_prev, tier_new,
      pos_tier_prev, pos_tier_new
    )) |>
    pull(row_html)

  paste0(
    '<table><thead><tr><th>Player</th><th>Pos</th><th>Tier</th>',
    '<th>Pos Tier</th></tr></thead><tbody>\n',
    paste(rows, collapse = "\n"), '\n</tbody></table>'
  )
}

build_simple_table <- function(df, is_new) {
  if (nrow(df) == 0) return("<p><em>None.</em></p>")

  name_col <- if (is_new) "name_new" else "name_prev"
  pos_col  <- if (is_new) "position_new" else "position_prev"

  rows <- df |>
    mutate(row_html = sprintf(
      '<tr><td>%s</td><td>%s</td></tr>',
      esc_html(.data[[name_col]]),
      .data[[pos_col]]
    )) |>
    pull(row_html)

  paste0(
    '<table><thead><tr><th>Player</th><th>Pos</th></tr></thead><tbody>\n',
    paste(rows, collapse = "\n"), '\n</tbody></table>'
  )
}

# ── Summary counts ────────────────────────────────────────────────────────────
n_movers_5  <- sum(abs(both$delta_vbd_rank) >= 5,  na.rm = TRUE)
n_movers_10 <- sum(abs(both$delta_vbd_rank) >= 10, na.rm = TRUE)
n_tier_chg  <- sum(both$pos_tier_changed, na.rm = TRUE)

summary_boxes <- paste0(
  '<div class="summary-grid">',
  sprintf('<div class="summary-box"><div class="val">%d</div><div class="lbl">Total players</div></div>', nrow(new_data)),
  sprintf('<div class="summary-box"><div class="val">%d</div><div class="lbl">Moved ≥5 VBD spots</div></div>', n_movers_5),
  sprintf('<div class="summary-box"><div class="val">%d</div><div class="lbl">Moved ≥10 VBD spots</div></div>', n_movers_10),
  sprintf('<div class="summary-box"><div class="val">%d</div><div class="lbl">Tier changes</div></div>', n_tier_chg),
  sprintf('<div class="summary-box"><div class="val">%d</div><div class="lbl">New players</div></div>', nrow(new_players)),
  sprintf('<div class="summary-box"><div class="val">%d</div><div class="lbl">Dropped players</div></div>', nrow(dropped)),
  '</div>'
)

# ── Assemble HTML ─────────────────────────────────────────────────────────────
html <- paste0(
  '<!DOCTYPE html><html><head><meta charset="utf-8">',
  '<title>SFB16 Changes Report — ', Sys.Date(), '</title>',
  '<style>', CSS, '</style></head><body>',
  '<h1>SFB16 Daily Changes Report</h1>',
  '<p class="subtitle">Generated ', format(Sys.Date(), "%B %d, %Y"),
  ' &bull; Comparing current data to previous snapshot</p>',
  summary_boxes,
  '<h2>VBD Rank Risers (top 20)</h2>',
  build_rank_table(both, "up"),
  '<h2>VBD Rank Fallers (top 20)</h2>',
  build_rank_table(both, "down"),
  '<h2>Projection Changes — Pts/Wk (top 20)</h2>',
  build_proj_table(both),
  '<h2>ADP Changes (top 20)</h2>',
  build_adp_table(both),
  '<h2>Positional Tier Changes</h2>',
  build_tier_table(both),
  '<h2>New Players</h2>',
  build_simple_table(new_players, is_new = TRUE),
  '<h2>Dropped Players</h2>',
  build_simple_table(dropped, is_new = FALSE),
  '</body></html>'
)

dir.create("reports", showWarnings = FALSE)
writeLines(enc2utf8(html), OUT_FILE, useBytes = TRUE)
cat("Changes report written to", OUT_FILE, "\n")
