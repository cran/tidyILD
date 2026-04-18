## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)

## ----export-pattern-----------------------------------------------------------
library(tidyILD)
set.seed(1)
d <- ild_simulate(n_id = 8, n_obs_per = 10, seed = 1)
d$x <- rnorm(nrow(d))
x <- ild_prepare(d, id = "id", time = "time")
x <- ild_center(x, y, x)
x <- ild_lag(x, dplyr::all_of(c("y", "x")), n = 1L, mode = "gap_aware")

meta <- ild_meta(x)
meta$ild_id
# Plain data frame for any external package:
dat <- as.data.frame(x)
head(dat[, c(meta$ild_id, ".ild_time_num", "y", "y_wp", "y_bp", "x_lag1")])

## ----stub-dynamite, eval = FALSE----------------------------------------------
# # library(dynamite)
# # After building `dat` from an ILD object (see above):
# # - Map id/time/outcome columns to dynamite's expected data layout.
# # - Specify channels, lags, and priors per package documentation.
# # Example placeholder only (not valid without a real dynamite specification):
# # fit_dyn <- dynamite::dynamite(
# #   dformula = <your dformula>,
# #   data = dat,
# #   ...
# # )

## ----stub-pgee, eval = FALSE--------------------------------------------------
# # library(PGEE)
# # Penalized GEE expects a long data frame with id, repeated outcome, and a matrix
# # or formula interface for high-dimensional covariates — see ?PGEE::PGEE.
# # Use `dat` from tidyILD after centering/lagging so covariates align with your estimand.
# # fit_pgee <- PGEE::PGEE(<formula or design>, data = dat, ...)

## ----stub-lavaan, eval = FALSE------------------------------------------------
# # library(lavaan)
# # Dynamic SEM is model-syntax-specific; export `dat` and define your model
# # in lavaan's longitudinal / DSEM extensions. tidyILD does not generate lavaan syntax.

## ----session-info, echo = FALSE-----------------------------------------------
sessionInfo()

