suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(plotly)
  library(tidyverse)
})

# ── Load data once at startup ──────────────────────────────────────────────────
vbd          <- readRDS("data/tradyr_vbd.rds")
season_tot   <- readRDS("data/sfb16_season_totals.rds")
weekly_stats <- readRDS("data/sfb16_weekly_scores.rds")

history <- if (file.exists("data/vbd_history.rds")) {
  readRDS("data/vbd_history.rds")
} else {
  warning("data/vbd_history.rds not found - run build_vbd_history.R. ",
          "Risers/Fallers will be empty.")
  tibble(snapshot_date = as.Date(character()), playerKey = character(),
         name = character(), position = character(), adp = double(),
         vbd_rank = integer(), VBD = double(), weekly_pts = double(),
         proj_pts = double(), tier = integer(), pos_tier = integer())
}

snapshot_dates <- sort(unique(history$snapshot_date))
POSITIONS      <- c("QB", "RB", "WR", "TE")

# Default comparison window: latest vs the closest snapshot >= 7 days earlier.
default_to   <- if (length(snapshot_dates)) max(snapshot_dates) else NA
default_from <- if (length(snapshot_dates) > 1) {
  earlier <- snapshot_dates[snapshot_dates <= default_to - 7]
  if (length(earlier)) max(earlier) else min(snapshot_dates)
} else default_to

pos_pill <- function(inputId, selected = POSITIONS) {
  checkboxGroupInput(inputId, "Positions", choices = POSITIONS,
                     selected = selected, inline = TRUE)
}

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = "SFB16 Draft Dashboard",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  # ── Tab 1: Risers & Fallers ──
  nav_panel(
    "Risers & Fallers",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("rf_from", "Compare from",
                    choices = as.character(snapshot_dates),
                    selected = as.character(default_from)),
        selectInput("rf_to", "Compare to",
                    choices = as.character(snapshot_dates),
                    selected = as.character(default_to)),
        radioButtons("rf_metric", "Metric",
                     choices = c("VBD rank" = "vbd", "ADP" = "adp"),
                     selected = "vbd"),
        pos_pill("rf_pos"),
        sliderInput("rf_topn", "Show top N", min = 5, max = 40, value = 15)
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Risers"),   DTOutput("tbl_risers")),
        card(card_header("Fallers"),  DTOutput("tbl_fallers"))
      ),
      card(card_header("Biggest movers"), plotlyOutput("plot_movers", height = "420px"))
    )
  ),

  # ── Tab 2: Projections Explorer ──
  nav_panel(
    "Projections",
    layout_sidebar(
      sidebar = sidebar(
        pos_pill("pe_pos"),
        checkboxInput("pe_rookie", "Rookies only", FALSE),
        textInput("pe_search", "Search name", ""),
        selectInput("pe_x", "Scatter X",
                    choices = c("adp", "vbd_rank", "proj_pts", "VBD"),
                    selected = "adp"),
        selectInput("pe_y", "Scatter Y",
                    choices = c("proj_pts", "VBD", "weekly_pts", "value_vs_adp"),
                    selected = "proj_pts"),
        helpText("Click a table row to see a player's projection breakdown.")
      ),
      card(card_header("Projection scatter"),
           plotlyOutput("pe_scatter", height = "380px")),
      layout_columns(
        col_widths = c(8, 4),
        card(card_header("Players"), DTOutput("pe_table")),
        card(card_header("Projection breakdown"),
             plotlyOutput("pe_breakdown", height = "360px"))
      )
    )
  ),

  # ── Tab 3: Last Season ──
  nav_panel(
    "Last Season",
    layout_sidebar(
      sidebar = sidebar(
        pos_pill("ls_pos"),
        checkboxInput("ls_matched", "Only players in this year's pool", TRUE),
        helpText("Joined by name; rookies won't have last-season stats.")
      ),
      card(card_header("2025 season totals"), DTOutput("ls_table")),
      card(card_header("Weekly SFB16 points"),
           plotlyOutput("ls_weekly", height = "380px"))
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ---- Tab 1: risers/fallers ----
  deltas <- reactive({
    req(input$rf_from, input$rf_to, nrow(history) > 0)
    from_d <- as.Date(input$rf_from)
    to_d   <- as.Date(input$rf_to)

    from_snap <- history |> filter(snapshot_date == from_d)
    to_snap   <- history |> filter(snapshot_date == to_d)
    validate(need(nrow(from_snap) > 0 && nrow(to_snap) > 0,
                  "No data for the selected dates."))

    inner_join(
      from_snap |> select(playerKey, name, position, adp, vbd_rank),
      to_snap   |> select(playerKey, adp, vbd_rank),
      by = "playerKey", suffix = c("_from", "_to")
    ) |>
      filter(position %in% input$rf_pos) |>
      mutate(
        delta_vbd = vbd_rank_from - vbd_rank_to,   # positive = rose
        delta_adp = adp_from - adp_to,             # positive = ADP improved
        delta     = if (input$rf_metric == "vbd") delta_vbd else delta_adp
      )
  })

  mover_table <- function(rising) {
    d <- deltas()
    d <- if (rising) arrange(d, desc(delta)) else arrange(d, delta)
    d |>
      filter(if (rising) delta > 0 else delta < 0) |>
      head(input$rf_topn) |>
      transmute(
        Player = name, Pos = position,
        `Δ VBD rank` = delta_vbd,
        `Δ ADP` = round(delta_adp, 1),
        `VBD (to)` = vbd_rank_to,
        `ADP (to)` = round(adp_to, 1)
      )
  }

  output$tbl_risers  <- renderDT(datatable(mover_table(TRUE),
    rownames = FALSE, options = list(dom = "tp", pageLength = 15)))
  output$tbl_fallers <- renderDT(datatable(mover_table(FALSE),
    rownames = FALSE, options = list(dom = "tp", pageLength = 15)))

  output$plot_movers <- renderPlotly({
    d <- deltas()
    validate(need(nrow(d) > 0, "No movers for this selection."))
    top <- d |> arrange(desc(abs(delta))) |> head(input$rf_topn) |>
      mutate(name = fct_reorder(name, delta),
             dir = if_else(delta >= 0, "Riser", "Faller"))
    metric_lbl <- if (input$rf_metric == "vbd") "Δ VBD rank" else "Δ ADP"
    p <- ggplot(top, aes(delta, name, fill = dir,
                         text = paste0(name, "<br>", metric_lbl, ": ", round(delta, 1)))) +
      geom_col() +
      scale_fill_manual(values = c(Riser = "#2c7fb8", Faller = "#d95f5f")) +
      labs(x = metric_lbl, y = NULL, fill = NULL) +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })

  # ---- Tab 2: projections ----
  pe_data <- reactive({
    d <- vbd |> filter(position %in% input$pe_pos)
    if (isTRUE(input$pe_rookie)) d <- d |> filter(tradyrIsRookie)
    if (nzchar(input$pe_search)) {
      d <- d |> filter(str_detect(str_to_lower(name),
                                  fixed(str_to_lower(input$pe_search))))
    }
    d
  })

  output$pe_table <- renderDT({
    pe_data() |>
      transmute(name, position, proj_pts = round(proj_pts, 1),
                weekly_pts = round(weekly_pts, 1), VBD = round(VBD, 0),
                VOLS = round(VOLS, 0), VORP = round(VORP, 0),
                adp = round(adp, 1), value_vs_adp = round(value_vs_adp, 0),
                tier, pos_tier) |>
      datatable(rownames = FALSE, selection = "single",
                options = list(pageLength = 15, order = list(list(2, "desc"))))
  })

  output$pe_scatter <- renderPlotly({
    d <- pe_data()
    validate(need(nrow(d) > 0, "No players match the filters."))
    p <- ggplot(d, aes(.data[[input$pe_x]], .data[[input$pe_y]],
                       color = position, text = name)) +
      geom_point(alpha = 0.75) +
      labs(x = input$pe_x, y = input$pe_y, color = NULL) +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })

  output$pe_breakdown <- renderPlotly({
    sel <- input$pe_table_rows_selected
    validate(need(length(sel) > 0, "Select a player in the table."))
    p <- pe_data()[sel, ]
    parts <- tibble(
      component = c("MikeClay (base)", "FantasyPros (base)", "Sleeper (base)",
                    "Tier bonus", "Pre-bonus proj", "Final proj"),
      value = c(p$projMikeClayBasePts, p$projFantasyProsBasePts,
                p$projSleeperBasePts, p$tier_bonus,
                p$pre_bonus_proj_pts, p$proj_pts)
    ) |> mutate(component = fct_inorder(component))
    gg <- ggplot(parts, aes(value, fct_rev(component),
                            text = paste0(component, ": ", round(value, 1)))) +
      geom_col(fill = "#2c7fb8") +
      labs(title = p$name, x = "Points", y = NULL) +
      theme_minimal()
    ggplotly(gg, tooltip = "text")
  })

  # ---- Tab 3: last season ----
  ls_join <- reactive({
    pool_names <- vbd |> distinct(name, position, tradyrIsRookie)
    st <- season_tot |> filter(position %in% input$ls_pos)
    j <- st |>
      left_join(pool_names, by = c("player_display_name" = "name"),
                suffix = c("", "_pool"))
    if (isTRUE(input$ls_matched)) {
      j <- j |> semi_join(pool_names, by = c("player_display_name" = "name"))
    }
    j
  })

  output$ls_table <- renderDT({
    ls_join() |>
      transmute(Player = player_display_name, Pos = position,
                `SFB16 total` = round(total_sfb16_fpts, 1),
                `SFB16 PPG` = round(mean_sfb16_ppg, 1),
                GP = games_played, `Pos rank` = position_rank,
                `Top-20 wks` = top20_overall_weeks,
                `Top-10 pos wks` = top10_pos_weeks) |>
      arrange(desc(`SFB16 total`)) |>
      datatable(rownames = FALSE, selection = "single",
                options = list(pageLength = 15))
  })

  output$ls_weekly <- renderPlotly({
    sel <- input$ls_table_rows_selected
    validate(need(length(sel) > 0, "Select a player to see weekly points."))
    tbl <- ls_join() |> arrange(desc(total_sfb16_fpts))
    player <- tbl$player_display_name[sel]

    wk <- weekly_stats |>
      filter(player_display_name == player) |>
      arrange(week)
    validate(need(nrow(wk) > 0, "No weekly data for this player."))

    p <- ggplot(wk, aes(week, sfb16_fpts,
                        text = paste0("Wk ", week, ": ", round(sfb16_fpts, 1)))) +
      geom_line(color = "#2c7fb8", group = 1) +
      geom_point(color = "#2c7fb8") +
      scale_x_continuous(breaks = seq(1, 18, 1)) +
      labs(title = player, x = "Week", y = "SFB16 points") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)
