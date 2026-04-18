## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----pattern------------------------------------------------------------------
library(tidyILD)
set.seed(11)
d <- ild_simulate(n_id = 25, n_obs_per = 12, seed = 11)
d$stress <- rnorm(nrow(d))
d$mood <- d$y
miss_i <- sample(nrow(d), 45)
d$mood[miss_i] <- NA
x <- ild_prepare(d, id = "id", time = "time")
mp <- ild_missing_pattern(x, vars = c("mood", "stress"), outcome = "mood")
mp$summary
head(mp$by_id, 3)

## ----compliance---------------------------------------------------------------
cm <- ild_missing_compliance(x, outcome = "mood", expected_occasions = 12L)
summary(cm$pct_nonmissing_outcome)

## ----cohort-hazard------------------------------------------------------------
coh <- ild_missing_cohort(x, outcome = "mood", plot = FALSE)
head(coh$by_occasion)
head(ild_missing_hazard_first(x, outcome = "mood"))

## ----report-------------------------------------------------------------------
rpt <- ild_missingness_report(
  x,
  outcome = "mood",
  predictors = "stress",
  fit_missing_model = TRUE,
  random = FALSE,
  cohort_plot = FALSE
)
names(rpt)
rpt$snippets["overview"]

## ----ipw-template, eval = FALSE-----------------------------------------------
# mm <- ild_missing_model(x, outcome = "mood", predictors = c("stress"), random = TRUE)
# x_w <- ild_ipw_weights(x, mm, stabilize = TRUE)
# fit_w <- ild_ipw_refit(mood ~ stress + (1 | id), data = x_w, weights = ".ipw")

## ----cc-vs-full, eval = FALSE-------------------------------------------------
# x_cc <- dplyr::filter(x, !is.na(mood))
# fit_full <- ild_lme(mood ~ stress + (1 | id), data = x, warn_uncentered = FALSE)
# fit_cc <- ild_lme(mood ~ stress + (1 | id), data = x_cc, warn_uncentered = FALSE)

