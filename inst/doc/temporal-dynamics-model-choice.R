## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)

## ----minimal-crosslag---------------------------------------------------------
library(tidyILD)
set.seed(1)
d <- ild_simulate(n_id = 12, n_obs_per = 10, seed = 1)
x <- ild_prepare(d, id = "id", time = "time")
out <- ild_crosslag(x, y, y, lag = 1L, ar1 = FALSE, warn_no_ar1 = FALSE)
out$lag_term[, c("term", "estimate", "std_error")]

## ----minimal-ar1, eval = requireNamespace("nlme", quietly = TRUE)-------------
x2 <- ild_center(x, y)
fit_ar <- tryCatch(
  ild_lme(y ~ y_bp + y_wp, data = x2, ar1 = TRUE, warn_no_ar1 = FALSE, warn_uncentered = FALSE),
  error = function(e) NULL
)
if (!is.null(fit_ar)) {
  print(fit_ar)
} else {
  "nlme fit skipped on this platform"
}

## ----minimal-tvem-------------------------------------------------------------
x3 <- ild_simulate(n_id = 10, n_obs_per = 15, seed = 2)
x3$x <- rnorm(nrow(x3))
x3 <- ild_prepare(x3, id = "id", time = "time")
tv <- ild_tvem(x3, "y", "x", k = 5, re_id = TRUE)
summary(tv)

## ----session-info, echo = FALSE-----------------------------------------------
sessionInfo()

