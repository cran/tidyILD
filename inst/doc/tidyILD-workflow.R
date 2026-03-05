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

## ----reproducibility----------------------------------------------------------
# Optional: build a manifest with scenario and seed, then bundle the fit for saving
manifest <- ild_manifest(seed = 42, scenario = ild_summary(x), include_session = FALSE)
bundle <- ild_bundle(fit1, manifest = manifest, label = "model_ar1")
# saveRDS(bundle, "run.rds")  # one file with result + manifest + label
names(bundle)

