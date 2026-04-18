## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)

## ----prepare------------------------------------------------------------------
library(tidyILD)
# Simulate simple ILD
d <- ild_simulate(n_id = 10, n_obs_per = 12, irregular = TRUE, seed = 42)
# Prepare: encode time structure and add .ild_* columns
x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 7200)

## ----summary------------------------------------------------------------------
ild_summary(x)
ild_spacing_class(x)

## ----center_lag---------------------------------------------------------------
x <- ild_center(x, y)
x <- ild_lag(x, y, mode = "gap_aware", max_gap = 7200)

## ----lme_no_ar1---------------------------------------------------------------
fit0 <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)

## ----lme_ar1------------------------------------------------------------------
fit1 <- ild_lme(y ~ 1, data = x, ar1 = TRUE, correlation_class = "CAR1")

## ----diagnostics--------------------------------------------------------------
diag <- ild_diagnostics(fit1, data = x)
names(diag)  # meta, data, stats
names(plot_ild_diagnostics(diag))  # plot names for requested types
# Pooled residual ACF (tibble)
head(diag$stats$acf$pooled)
# By-id ACF when by_id = TRUE: one tibble per person
names(diag$stats$acf$by_id)
head(diag$stats$acf$by_id[[1]])

## ----plot_trajectory, fig.alt = "Trajectory plot"-----------------------------
ild_plot(x, type = "trajectory", var = "y", max_ids = 5)

## ----plot_fitted, fig.alt = "Fitted vs observed"------------------------------
ild_plot(fit1, type = "fitted")

## ----msm_weights, eval = FALSE------------------------------------------------
# # Example skeleton (not run in the vignette build)
# x2 <- ild_simulate(n_id = 12, n_obs_per = 10, seed = 1)
# x2$stress <- rnorm(nrow(x2))
# x2$trt <- rbinom(nrow(x2), 1L, 0.45)
# x2 <- ild_prepare(x2, id = "id", time = "time")
# x2 <- ild_center(x2, y)
# x2 <- ild_iptw_weights(x2, treatment = "trt", predictors = "stress")
# # Sequential A_t: x2 <- ild_lag(x2, stress); x2 <- ild_lag(x2, trt); ...
# # x2 <- ild_iptw_msm_weights(x2, treatment = "trt", history = ~ stress_lag1 + trt_lag1)
# x2 <- ild_ipcw_weights(x2, predictors = "stress")
# x2 <- ild_joint_msm_weights(x2)
# fit_msm <- ild_lme(y ~ y_bp + y_wp + stress + (1 | id), data = x2,
#   ar1 = FALSE, warn_no_ar1 = FALSE, warn_uncentered = FALSE)
# fit_msm_w <- ild_ipw_refit(fit_msm, data = x2)

## ----msm_bootstrap, eval = requireNamespace("lme4", quietly = TRUE)-----------
set.seed(3)
xb <- ild_simulate(n_id = 10, n_obs_per = 5, seed = 3)
xb$stress <- rnorm(nrow(xb))
xb <- ild_prepare(xb, id = "id", time = "time")
xb <- ild_center(xb, y)
xb$.ipw <- runif(nrow(xb), 0.85, 1.15)
fb <- ild_lme(y ~ y_bp + y_wp + stress + (1 | id), data = xb,
  ar1 = FALSE, warn_no_ar1 = FALSE, warn_uncentered = FALSE)
fwb <- ild_ipw_refit(fb, data = xb, weights = ".ipw")
bs_fixed <- ild_msm_bootstrap(fwb, n_boot = 20L, weight_policy = "fixed_weights", seed = 3)
tidy_ild_msm_bootstrap(bs_fixed)
# reestimate_weights: weights_fn must return ILD with the weight column, e.g. re-run IPTW pipeline:
bs_re <- ild_msm_bootstrap(fwb, n_boot = 12L, weight_policy = "reestimate_weights",
  seed = 4, weights_fn = function(d) { d$.ipw <- runif(nrow(d), 0.85, 1.15); d })

## ----reproducibility----------------------------------------------------------
# Optional: build a manifest with scenario and seed, then bundle the fit for saving
manifest <- ild_manifest(seed = 42, scenario = ild_summary(x), include_session = FALSE)
bundle <- ild_bundle(fit1, manifest = manifest, label = "model_ar1")
# saveRDS(bundle, "run.rds")  # one file with result + manifest + label
names(bundle)

