## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)

## ----center-------------------------------------------------------------------
library(tidyILD)
d <- ild_simulate(n_id = 5, n_obs_per = 8, seed = 1)
x <- ild_prepare(d, id = "id", time = "time")
x <- ild_center(x, y)
# New columns: y_bp (person mean), y_wp (deviation from person mean)
head(x[, c("id", "y", "y_bp", "y_wp")])

## ----lag----------------------------------------------------------------------
d <- ild_simulate(n_id = 3, n_obs_per = 6, irregular = TRUE, seed = 2)
x <- ild_prepare(d, id = "id", time = "time")
x <- ild_lag(x, y, mode = "gap_aware", max_gap = 4000)
# Compare: .ild_dt (interval) and y_lag1 (NA after large gaps)
x[, c(".ild_id", ".ild_dt", "y", "y_lag1")]

## ----spacing------------------------------------------------------------------
ild_summary(x)$spacing
ild_spacing_class(x)

