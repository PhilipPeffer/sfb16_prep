suppressPackageStartupMessages(library(tidyverse))
library(nflreadr)

#read_csv()

season <- readRDS("data/sfb16_season_totals.rds")

proj_ranks <- try({ # prevents cran errors
  load_ff_rankings("draft")
})
proj_ranks <- proj_ranks |>
  filter(page_type == "redraft-overall")

m <- lm(total_sfb16_fpts ~ total_ppr_fpts, data=(season |> filter(position == "QB")))
r <- summary(m)$adj.r.squared

m <- lm(total_sfb16_rank ~ total_ppr_rank, data=(season |> filter(position == "QB")))
r <- summary(m)$adj.r.squared

qb_proj_ranks <- proj_ranks |>
  filter(pos == "QB") |>
  arrange(ecr) |>
  select(c(player, ecr))
y <- predict(m, newdata=(qb_proj_ranks |> select(ecr) |> rename(total_ppr_rank = ecr)))
bind_cols(qb_proj_ranks, sfb16_pred=y)
#rmse <- sqrt(mean(m$residuals^2))