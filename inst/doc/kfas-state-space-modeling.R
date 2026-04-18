## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
has_kfas <- requireNamespace("KFAS", quietly = TRUE)

## ----example, eval = has_kfas-------------------------------------------------
library(tidyILD)
set.seed(1)
d <- ild_simulate(n_id = 1, n_obs_per = 60, seed = 42)
x <- ild_prepare(d, id = "id", time = "time")
x <- ild_center(x, y)
fit <- suppressWarnings(
  ild_kfas(x, outcome = "y", state_spec = "local_level", time_units = "sim_steps")
)
b <- ild_diagnose(fit)
class(b)
ild_autoplot(b, section = "residual", type = "acf")

