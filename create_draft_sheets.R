suppressPackageStartupMessages(library(tidyverse))

# ── Config ────────────────────────────────────────────────────────────────────
# Per-position caps sized so each sheet fits one printed letter page.
# If caps grow much beyond this, add `break-before: page` to the WR section.
POS_CAPS <- c(QB = 30, RB = 60, WR = 75, TE = 35)
OUT_DIR  <- "reports"

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

prep_position <- function(df, pos, sort_by) {
  df <- df |> filter(position == pos)
  df <- switch(sort_by,
    vbd = df |> arrange(desc(VBD)),
    adp = df |> arrange(is.na(adp), adp)
  )
  df |> slice_head(n = POS_CAPS[[pos]])
}

build_section_html <- function(df, pos) {
  rows <- df |>
    mutate(
      rn         = row_number(),
      tier_break = coalesce(tier != lag(tier), FALSE),
      row_html   = sprintf(
        '<tr%s><td class="num">%d</td><td class="name">%s</td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td><td class="num">%d</td><td class="num">%d</td></tr>',
        if_else(tier_break, ' class="tier-break"', ""),
        rn,
        esc_html(name),
        fmt_num(weekly_pts, 1),
        fmt_num(VBD, 0),
        fmt_num(adp, 1),
        fmt_num(value_vs_adp, 0, signed = TRUE),
        tier,
        pos_tier
      )
    ) |>
    pull(row_html)

  paste0(
    '<section class="pos-section">\n',
    "<h2>", pos, "</h2>\n",
    '<table>\n<thead><tr><th>#</th><th>Player</th><th>Pts/Wk</th><th>VBD</th><th>ADP</th><th>+/-ADP</th><th>Tier</th><th>PTier</th></tr></thead>\n<tbody>\n',
    paste(rows, collapse = "\n"),
    "\n</tbody>\n</table>\n</section>\n"
  )
}

CSS <- '
@page { size: letter; margin: 0.35in; }
* { box-sizing: border-box; }
body { font: 7.5pt/1.25 Arial, sans-serif; margin: 0; }
h1 { font-size: 11pt; margin: 0 0 2px; }
.subtitle { font-size: 7pt; color: #555; margin: 0 0 6px; }
.grid { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 10px; align-items: start; }
table { width: 100%; border-collapse: collapse; }
td, th { padding: 0.5px 3px; white-space: nowrap; }
th { text-align: left; border-bottom: 1px solid #000; font-size: 6.5pt; }
td.num { text-align: right; font-variant-numeric: tabular-nums; }
td.name { overflow: hidden; text-overflow: ellipsis; max-width: 1.4in; }
h2 { font-size: 9pt; margin: 4px 0 1px; border-bottom: 2px solid #000; }
tr { break-inside: avoid; page-break-inside: avoid; }
.pos-section h2 { break-after: avoid; }
tr.tier-break td { border-top: 1.5px solid #444; }
tbody tr:nth-child(even) td { background: #f2f2f2; print-color-adjust: exact; -webkit-print-color-adjust: exact; }
'

build_sheet <- function(df, sort_by, title, out_file) {
  col1 <- paste0(
    build_section_html(prep_position(df, "QB", sort_by), "QB"),
    build_section_html(prep_position(df, "TE", sort_by), "TE")
  )
  col2 <- build_section_html(prep_position(df, "RB", sort_by), "RB")
  col3 <- build_section_html(prep_position(df, "WR", sort_by), "WR")

  html <- paste0(
    "<!DOCTYPE html>\n<html>\n<head>\n",
    '<meta charset="utf-8">\n',
    "<title>", esc_html(title), "</title>\n",
    "<style>", CSS, "</style>\n",
    "</head>\n<body>\n",
    "<h1>", esc_html(title), "</h1>\n",
    '<p class="subtitle">Generated ', format(Sys.Date(), "%B %d, %Y"),
    " &bull; Pts/Wk = projected SFB16 points per week &bull; +/-ADP = ADP minus VBD rank (positive = value)</p>\n",
    '<div class="grid">\n',
    '<div class="col">\n', col1, "</div>\n",
    '<div class="col">\n', col2, "</div>\n",
    '<div class="col">\n', col3, "</div>\n",
    "</div>\n</body>\n</html>"
  )

  writeLines(enc2utf8(html), out_file, useBytes = TRUE)
  cat("Wrote", out_file, "\n")
}

# ── Main ──────────────────────────────────────────────────────────────────────
vbd <- readRDS("data/tradyr_vbd.rds")
dir.create(OUT_DIR, showWarnings = FALSE)

build_sheet(vbd, "vbd", "SFB16 Draft Sheet — VBD order",
            file.path(OUT_DIR, "draft_sheet_vbd.html"))
build_sheet(vbd, "adp", "SFB16 Draft Sheet — ADP order",
            file.path(OUT_DIR, "draft_sheet_adp.html"))
