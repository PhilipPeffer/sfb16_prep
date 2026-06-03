suppressPackageStartupMessages(library(tidyverse))
library(nflreadr)

season <- readRDS("data/sfb16_season_totals.rds")

# calculate projected sfb16 points
# 1. make position-specific linear model of ppr pts to sfb16 pts from 2025 data
# 2. use the models to make predictions on FantasyPros ppr projections
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
sfb16_proj <- bind_rows(dfs) |> arrange(desc(sfb16_fpts_proj))

## Calculate Value Over Last Starter (VOLS) and Value Over Replacement Player (VORP)
sfb16_proj <- sfb16_proj |>
  mutate(QB = if_else(Pos == "QB", TRUE, FALSE)) |>
  group_by(QB) |>
  mutate(vols_rank = min_rank(desc(sfb16_fpts_proj))) |>
  ungroup()

# Last starting QB (assuming 2 per team)
QB_LS <- sfb16_proj |> filter(QB == TRUE, vols_rank == 2*12)
print("Last QB Starter:")
print(QB_LS)

# Last starting non-QB (assuming 8 per team)
Flex_LS <- sfb16_proj |> filter(QB == FALSE, vols_rank == 8*12)
print("Last Non-QB Starter:")
print(Flex_LS)

# # Last bench QB player (assuming 1 per team)
# QB_LB <- sfb16_proj |> filter(QB == TRUE, vbd_rank == 3*12)
# print("Last QB on bench:")
# print(QB_LB)
# 
# # Last bench non-QB player (assuming 9 per team)
# Flex_LB <- sfb16_proj |> filter(QB == FALSE, vbd_rank == 17*12)
# print("Last non-QB on bench:")
# print(Flex_LB)

# Last bench player
LB <- sfb16_proj |> arrange(desc(sfb16_fpts_proj)) |> slice(20*12)
print("Last player on bench:")
print(LB)

# sfb16_proj <- sfb16_proj |>
#   mutate(VOLS = if_else(QB == TRUE,
#                         sfb16_fpts_proj - QB_LS$sfb16_fpts_proj,
#                         sfb16_fpts_proj - Flex_LS$sfb16_fpts_proj),
#          VORP = if_else(QB == TRUE,
#                         sfb16_fpts_proj - QB_LB$sfb16_fpts_proj,
#                         sfb16_fpts_proj - Flex_LB$sfb16_fpts_proj),
#          VBD = VOLS + VORP,
#          vbd_rank = min_rank(desc(VBD))
#   ) |>
#   arrange(vbd_rank)

sfb16_proj <- sfb16_proj |>
  mutate(VOLS = if_else(QB == TRUE,
                        sfb16_fpts_proj - QB_LS$sfb16_fpts_proj,
                        sfb16_fpts_proj - Flex_LS$sfb16_fpts_proj),
         VORP = sfb16_fpts_proj - LB$sfb16_fpts_proj,
         VBD = VOLS + VORP,
         vbd_rank = min_rank(desc(VBD))
  ) |>
  arrange(vbd_rank) |>
  select(-vols_rank)

saveRDS(sfb16_proj, "data/sfb16_projections.rds")
write_csv(sfb16_proj, "data/sfb16_projections.csv")