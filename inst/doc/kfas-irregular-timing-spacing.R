## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
has_kfas <- requireNamespace("KFAS", quietly = TRUE)

## ----spacing_demo, eval = TRUE------------------------------------------------
library(tidyILD)
set.seed(3)
d <- ild_simulate(n_id = 1, n_obs_per = 50, irregular = TRUE, seed = 11)
x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 7200)
ild_summary(x)$summary
ild_spacing_class(x)

## ----kfas_irregular, eval = has_kfas------------------------------------------
fit <- suppressWarnings(
  ild_kfas(
    x,
    outcome = "y",
    state_spec = "local_level",
    time_units = "seconds",
    irregular_time = TRUE
  )
)
diag <- ild_diagnose(fit)
# When spacing is irregular-ish and IQR/median interval ratio > 0.75, expect
# GR_KFAS_HIGH_IRREGULARITY_FOR_DISCRETE_TIME among triggered guardrails:
diag$guardrails[, c("rule_id", "message")]

