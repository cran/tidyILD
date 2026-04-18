## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----plot-examples------------------------------------------------------------
library(tidyILD)
set.seed(1)
d <- ild_simulate(n_id = 24, n_obs_per = 10, seed = 1)
d$cluster <- rep(LETTERS[1:3], length.out = nrow(d))
x <- ild_prepare(d, id = "id", time = "time")
ild_spaghetti(x, var = "y", facet_by = "cluster", max_ids = 12L)

fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE, warn_uncentered = FALSE)
ild_plot_predicted_trajectory(fit, time_var = ".ild_seq", max_ids = 8L, facet_by = "cluster")

## ----partial-effects-template, eval = FALSE-----------------------------------
# # install.packages(c("marginaleffects", "ggeffects"))  # if needed
# x <- ild_center(x, x)
# fit <- ild_lme(y ~ x_wp + x_bp + (1 | id), data = x, warn_uncentered = FALSE)
# # Example (syntax may vary by package version):
# # marginaleffects::plot_predictions(fit, condition = "x_wp")
# # marginaleffects::plot_predictions(fit, condition = "x_bp")
# # ggeffects::ggpredict(fit, terms = "x_wp")  # then plot() or ggplot2 layer

