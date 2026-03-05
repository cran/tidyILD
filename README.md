# tidyILD

Tidyverse-native toolkit for **intensive longitudinal data (ILD)**.  
Author: **Alex Litovchenko**.

## Install

```r
remotes::install_github("alitovchenko/tidyILD")
```

## Quick start

```r
library(tidyILD)

# Prepare: validate time structure, add .ild_* columns and metadata
d <- data.frame(
  id = rep(1:3, each = 5),
  time = rep(as.POSIXct(0:4 * 3600, origin = "1970-01-01"), 3),
  mood = rnorm(15)
)
x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 7200)

# Inspect (summary tibble + list)
ild_summary(x)

# Within-between decomposition
x <- ild_center(x, mood)

# Spacing-aware lags (max_gap from metadata if omitted)
x <- ild_lag(x, mood, mode = "gap_aware", max_gap = 7200)

# Missingness (summary tibble + plot + by_id)
ild_missing_pattern(x, vars = "mood")

# Fit and report: tidy fixed effects, fitted vs observed, residual ACF + QQ
fit <- ild_lme(mood ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
tidy_ild_model(fit)
ild_plot(fit, type = "fitted")
diag <- ild_diagnostics(fit); diag; plot_ild_diagnostics(diag)
```

## Pipeline

- `ild_prepare()` — encode longitudinal structure, spacing, gaps
- `ild_summary()` — one-shot summary
- `ild_center()` — person-mean centering (WP/BP)
- `ild_lag()` — index, gap-aware, or time-window lags (supports `lubridate::hours(2)` etc.)
- `ild_decomposition()` — WP/BP variance and ratio; optional WP vs BP density plot
- `ild_check_lags()` — lag validity (valid/invalid, pct_invalid, lag order)
- `ild_crosslag()` — one-call cross-lag: lag predictor, check lags, fit outcome ~ lag
- `ild_acf()` — ACF on a variable or on residuals (pre-model check for AR1)
- `ild_spacing_class()` — regular-ish vs irregular-ish
- `ild_spacing()` — spacing diagnostics (median/IQR, large gaps %, CV) and AR1/CAR1 recommendation
- `ild_design_check()` — aggregate spacing, WP/BP, missingness, and recommendations
- `ild_center_plot()` — standalone WP vs BP density plot
- `ild_missing_pattern()` — missingness by person/variable
- `ild_missing_bias()` — test if missingness is associated with a predictor (informative missingness)
- `ild_align()` — align secondary stream (e.g. wearables) to primary ILD within a time window
- `ild_lme()` — mixed-effects model (lmer or nlme with AR1/CAR1)
- `ild_person_model()` — fit model per person (N-of-1); `ild_person_distribution()` — plot distribution of estimates
- `ild_diagnostics()` — residual ACF, residuals vs fitted/time (use `print()` for summary)
- `plot_ild_diagnostics()` — build diagnostic plots from an `ild_diagnostics` object
- `ild_plot()` — trajectory, heatmap, gaps, fitted vs observed, residual ACF
- `ild_heatmap()`, `ild_spaghetti()` — aliases for heatmap and trajectory plots
- `ild_circadian()` — variable by hour of day (when time is POSIXct)
- `augment_ild_model()` — tibble with .ild_id, .ild_time, outcome, .fitted, .resid
- `tidy_ild_model()` — fixed-effect table (estimate, SE, CI, p) for both engines
- `ild_simulate()` — simulated ILD (n_id, n_time/n_obs_per, ar1, wp_effect, bp_effect, irregular)
- `ild_power()` — simulation-based power for a fixed effect (ild_simulate → ild_lme → effect recovery)
- `ema_example` — built-in dataset (`data(ema_example)`)
- **broom**: Load `broom.mixed` for `tidy(fit)` and `augment(fit)` on `ild_lme` fits.

## Vignettes

- **From raw data to model**: full pipeline with `ild_prepare()` through `ild_lme()` and `ild_plot()`.
- **Short analysis report**: fit, tidy fixed effects, fitted vs observed, residual ACF and Q-Q.
- **Within-between decomposition and irregular spacing**: centering and gap-aware lags.
- **Glossary and quick-start**: function glossary and checklist.

## pkgdown site

Documentation and vignettes are built with [pkgdown](https://pkgdown.r-lib.org/). From the package root: `pkgdown::build_site()`. Config: `_pkgdown.yml`.

## License

MIT.
