## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
# Optional: set `cache = TRUE` on slow chunks when developing locally (see chunks below).

## ----recovery-run, cache = FALSE----------------------------------------------
library(tidyILD)
library(dplyr)
library(ggplot2)

truth <- 0.35
n_sim <- 200L

set.seed(2026)
res <- ild_power(
  formula = y ~ x + (1 | id),
  n_sim = n_sim,
  n_id = 25L,
  n_obs_per = 12L,
  effect_size = truth,
  seed = 2026L,
  return_sims = TRUE,
  verbose = FALSE
)

sim <- res$sim_results |> dplyr::filter(converged)

## ----recovery-metrics---------------------------------------------------------
ild_recovery_metrics(res$sim_results, truth = truth, level = 0.95)

## ----recovery-plot, fig.alt = "Histogram of simulation estimates of the fixed effect", cache = FALSE----
ggplot2::ggplot(sim, ggplot2::aes(x = estimate)) +
  ggplot2::geom_histogram(fill = "steelblue", color = "white", bins = 20) +
  ggplot2::geom_vline(xintercept = truth, linetype = 2, linewidth = 0.8) +
  ggplot2::labs(
    x = "Estimated coefficient for x",
    y = "Count",
    title = "Sampling distribution of fixed-effect estimates",
    subtitle = sprintf("True beta = %s (vertical line)", truth)
  )

## ----power-link---------------------------------------------------------------
tibble::tibble(
  power = res$power,
  n_reject = res$n_reject,
  n_converged = res$n_converged,
  alpha = res$alpha
)

## ----varcorr-snapshot, eval = requireNamespace("lme4", quietly = TRUE)--------
set.seed(1)
d_one <- ild_simulate(n_id = 30L, n_obs_per = 15L, seed = 99)
d_one$x <- rnorm(nrow(d_one))
d_one$y <- d_one$y + 0.3 * d_one$x
prep_one <- ild_prepare(d_one, id = "id", time = "time")
fit_one <- ild_lme(y ~ x + (1 | id), prep_one, ar1 = FALSE, warn_no_ar1 = FALSE)
lme4::VarCorr(fit_one)

## ----ar1-illustration, eval = requireNamespace("nlme", quietly = TRUE)--------
set.seed(2)
d_ar <- ild_simulate(n_id = 20L, n_obs_per = 14L, ar1 = 0.35, wp_effect = 0.5, seed = 2)
d_ar$x <- rnorm(nrow(d_ar))
d_ar$y <- d_ar$y + 0.25 * d_ar$x
prep_ar <- ild_prepare(d_ar, id = "id", time = "time")
fit_ar <- tryCatch(
  ild_lme(y ~ x, prep_ar, ar1 = TRUE, random = ~ 1 | id, warn_no_ar1 = FALSE),
  error = function(e) NULL
)
if (!is.null(fit_ar)) summary(fit_ar) else "nlme fit failed on this platform (skip)"

## ----session-info, echo = FALSE-----------------------------------------------
sessionInfo()

