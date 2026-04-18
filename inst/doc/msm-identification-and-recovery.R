## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----eval = FALSE-------------------------------------------------------------
# library(tidyILD)
# 
# d <- ild_msm_simulate_scenario(n_id = 100, n_obs_per = 12, true_ate = 0.5, seed = 101)
# d <- ild_center(d, y)
# 
# hist_spec <- ild_msm_history_spec(vars = c("stress", "trt"), lags = 1:2)
# d <- ild_build_msm_history(d, hist_spec)
# 
# estimand <- ild_msm_estimand(type = "ate", regime = "static", treatment = "trt")
# 
# fit_obj <- ild_msm_fit(
#   estimand = estimand,
#   data = d,
#   outcome_formula = y ~ y_bp + y_wp + stress + trt + (1 | id),
#   history = ~ stress_lag1 + trt_lag1,
#   predictors_censor = "stress",
#   inference = "bootstrap",
#   n_boot = 200,
#   strict_inference = FALSE
# )
# 
# fit_obj
# fit_obj$inference$status
# fit_obj$inference$reason

## ----eval = FALSE-------------------------------------------------------------
# rec <- ild_msm_recovery(
#   n_sim = 100,
#   n_id = 120,
#   n_obs_per = 12,
#   true_ate = 0.5,
#   n_boot = 200,
#   inference = "bootstrap",
#   seed = 1001,
#   censoring = TRUE
# )
# 
# rec$summary
# rec$summary_by_scenario

## ----eval = FALSE-------------------------------------------------------------
# grid <- tibble::tibble(
#   scenario_id = c("baseline", "positivity_stress", "misspecified_treatment"),
#   positivity_stress = c(1, 1.8, 1),
#   misspec_treatment_model = c(FALSE, FALSE, TRUE)
# )
# 
# rec_grid <- ild_msm_recovery(
#   n_sim = 50,
#   n_id = 120,
#   n_obs_per = 12,
#   true_ate = 0.5,
#   n_boot = 200,
#   inference = "bootstrap",
#   scenario_grid = grid,
#   seed = 1101
# )
# 
# rec_grid$summary_by_scenario

