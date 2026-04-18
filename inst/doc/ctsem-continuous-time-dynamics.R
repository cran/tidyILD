## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----eval = FALSE-------------------------------------------------------------
# library(tidyILD)
# 
# d <- ild_simulate(n_id = 1, n_obs_per = 60, seed = 501)
# x <- ild_prepare(d, id = "id", time = "time")
# x <- ild_center(x, y)
# 
# fit_ct <- ild_ctsem(
#   data = x,
#   outcome = "y",
#   model_type = "stanct",
#   chains = 1,
#   iter = 400
# )
# 
# fit_ct
# td <- ild_tidy(fit_ct)
# ag <- ild_augment(fit_ct)
# dg <- ild_diagnose(fit_ct)

## ----eval = FALSE-------------------------------------------------------------
# ild_autoplot(fit_ct, type = "fitted_vs_actual")
# ild_autoplot(fit_ct, type = "residual_time")
# ild_autoplot(dg, section = "fit", type = "convergence")
# ild_autoplot(dg, section = "residual", type = "acf")

