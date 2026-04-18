## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = requireNamespace("lme4", quietly = TRUE)--------------------------
library(tidyILD)
d <- ild_simulate(n_id = 20, n_obs_per = 10, seed = 7)
x <- ild_prepare(d, id = "id", time = "time")
x <- ild_center(x, y)
fit <- ild_lme(y ~ y_wp + y_bp + (1 | id), data = x)
h <- ild_heterogeneity(fit)
print(h$summary)
head(ild_tidy(h))

## ----eval = FALSE-------------------------------------------------------------
# ild_autoplot(bundle, section = "fit", type = "heterogeneity", term = "y_wp")

## ----eval = FALSE-------------------------------------------------------------
# ild_heterogeneity_stratified(
#   y ~ y_wp + (y_wp | id),
#   data = x,
#   subgroup = "cohort",
#   min_n_id = 8L
# )

