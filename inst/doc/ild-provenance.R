## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)

## ----prepare------------------------------------------------------------------
library(tidyILD)
set.seed(1)
d <- ild_simulate(n_id = 8, n_obs_per = 10, irregular = TRUE, seed = 42)
x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 7200)

## ----center_lag---------------------------------------------------------------
x <- ild_center(x, y)
x <- ild_lag(x, y, n = 1, mode = "gap_aware", max_gap = 7200)

## ----fit----------------------------------------------------------------------
fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)

## ----diag---------------------------------------------------------------------
diag <- ild_diagnostics(fit, type = c("residual_acf", "qq"))

## ----history------------------------------------------------------------------
ild_history(x)

## ----history_fit--------------------------------------------------------------
ild_history(fit)

## ----methods------------------------------------------------------------------
ild_methods(fit)

## ----methods_robust, eval = requireNamespace("clubSandwich", quietly = TRUE)----
ild_methods(fit, robust_se = "CR2")

## ----report-------------------------------------------------------------------
r <- ild_report(fit)
names(r)
r$meta
r$methods
r$model_table

## ----export-------------------------------------------------------------------
tmp <- tempfile(fileext = ".json")
ild_export_provenance(fit, tmp, format = "json")
readLines(tmp, n = 20)

## ----report_export------------------------------------------------------------
tmp2 <- tempfile(fileext = ".yaml")
r2 <- ild_report(fit, export_provenance_path = tmp2)
r2$provenance_export_path

## ----compare_setup------------------------------------------------------------
x2 <- ild_prepare(d, id = "id", time = "time", gap_threshold = 3600)
x2 <- ild_center(x2, y)
x2 <- ild_lag(x2, y, n = 1, mode = "index")
fit2 <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x2, ar1 = FALSE, warn_no_ar1 = FALSE)

## ----compare------------------------------------------------------------------
cmp <- ild_compare_pipelines(fit, fit2)
cmp

