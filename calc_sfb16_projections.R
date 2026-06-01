suppressPackageStartupMessages(library(tidyverse))
library(nflreadr)

season <- readRDS("data/sfb16_season_totals.rds")

calc_proj_pts <- function(stats, pos) {
  proj <- read_csv(paste0("data/FantasyPros_Fantasy_Football_Projections_", pos, ".csv"), skip_empty_rows=TRUE)
  proj <- proj |> slice(c(-1, -n()))
  
  m <- lm(total_sfb16_fpts ~ total_ppr_fpts, data=(stats |> filter(position == pos)))
  r <- summary(m)$adj.r.squared
  print(paste0(pos, " R-squared: ", r))
  
  y <- predict(m, newdata=(proj |> select(FPTS) |> rename(total_ppr_fpts = FPTS)))
  return(bind_cols(Player=proj$Player, Pos=pos, ppr_proj=proj$FPTS, sfb16_fpts_proj=y))
}

positions <- c("QB", "RB", "WR", "TE")

dfs = list()
for(pos in positions) {
  dfs[[pos]] = calc_proj_pts(season, pos)
}
sfb16_proj <- bind_rows(dfs)

saveRDS(sfb16_proj, "data/sfb16_projections.rds")
write_csv(sfb16_proj, "data/sfb16_projections.csv")

# proj_ranks <- try({ # prevents cran errors
#   load_ff_rankings("draft")
# })
# proj_ranks <- proj_ranks |>
#   filter(page_type == "redraft-overall")
#
# m <- lm(total_sfb16_rank ~ total_ppr_rank, data=(season |> filter(position == "QB")))
# r <- summary(m)$adj.r.squared
# 
# qb_proj_ranks <- proj_ranks |>
#   filter(pos == "QB") |>
#   arrange(ecr) |>
#   select(c(player, ecr))
# y <- predict(m, newdata=(qb_proj_ranks |> select(ecr) |> rename(total_ppr_rank = ecr)))
# bind_cols(qb_proj_ranks, sfb16_pred=y)
#rmse <- sqrt(mean(m$residuals^2))