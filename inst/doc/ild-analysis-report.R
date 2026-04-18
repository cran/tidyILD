## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
fit_ar1 <- NULL

## ----fit----------------------------------------------------------------------
library(tidyILD)
d <- ild_simulate(n_id = 8, n_obs_per = 10, irregular = TRUE, seed = 101)
x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 7200)
x <- ild_center(x, y)
fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)

## ----tidy---------------------------------------------------------------------
tidy_ild_model(fit)

## ----tidy_robust, eval = requireNamespace("clubSandwich", quietly = TRUE)-----
tidy_ild_model(fit, se = "robust", robust_type = "CR2")

## ----fitted_plot, fig.alt = "Fitted vs observed"------------------------------
ild_plot(fit, type = "fitted")

## ----diag---------------------------------------------------------------------
diag <- ild_diagnostics(fit, type = c("residual_acf", "qq"))
diag

## ----diag_plots, fig.alt = "Residual ACF and Q-Q", fig.show = "hold"----------
plots <- plot_ild_diagnostics(diag)
plots$residual_acf
plots$qq

## ----fit_ar1------------------------------------------------------------------
fit_ar1 <- tryCatch(
  ild_lme(y ~ y_bp + y_wp, data = x, random = ~ 1 | id, ar1 = TRUE),
  error = function(e) NULL
)
if (!is.null(fit_ar1)) {
  tidy_ild_model(fit_ar1)
} else {
  message("AR1 fit did not converge on this run; use ild_lme(..., ar1 = TRUE) with your data.")
}

## ----diag_ar1, eval = !is.null(fit_ar1)---------------------------------------
# if (!is.null(fit_ar1)) {
#   diag_ar1 <- ild_diagnostics(fit_ar1, type = c("residual_acf", "qq"))
#   diag_ar1
# }

## ----tvem---------------------------------------------------------------------
set.seed(101)
d2 <- ild_simulate(n_id = 6, n_obs_per = 12, seed = 101)
d2$x <- rnorm(nrow(d2))
x2 <- ild_prepare(d2, id = "id", time = "time")
tv <- ild_tvem(x2, "y", "x", k = 5, re_id = TRUE)
ild_tvem_plot(tv)

