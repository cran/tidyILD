## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----recipe-random-lag-slope--------------------------------------------------
# library(tidyILD)
# set.seed(1)
# d <- ild_simulate(n_id = 15, n_obs_per = 12, seed = 1)
# d$x <- rnorm(nrow(d))
# x <- ild_prepare(d, id = "id", time = "time")
# x <- ild_lag(x, x, n = 1L, mode = "gap_aware")
# tpl <- ild_brms_dynamics_formula("y", "x_lag1", id_var = "id")
# tpl$formula
# tpl$notes
# # Short chains for illustration only:
# # fit <- ild_brms(tpl$formula, data = x, iter = 500, chains = 2, refresh = 0)
# # ild_tidy(fit)

## ----recipe-two-lags----------------------------------------------------------
# x2 <- ild_panel_lag_prepare(x, c("x", "y"), n = c(1L, 1L), mode = "gap_aware")
# names(x2$data)
# # Example fixed structure only (not run):
# # f2 <- y ~ x_lag1 + y_lag1 + (1 | id)
# # fit2 <- ild_brms(f2, data = x2$data, iter = 500, chains = 2, refresh = 0)

## ----recipe-mvbind------------------------------------------------------------
# # Not run — requires brms; illustration only:
# # library(brms)
# # f_mv <- bf(mvbind(mood, stress) ~ mood_lag1 + stress_lag1 + (1 | id)) + set_rescor(TRUE)
# # fit_mv <- brm(f_mv, data = x2$data, chains = 2, iter = 500, refresh = 0)
# # Build mood_lag1 / stress_lag1 with ild_lag() or ild_panel_lag_prepare() first.

## ----session-info, echo = FALSE-----------------------------------------------
# sessionInfo()

