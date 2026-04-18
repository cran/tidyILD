# tidyILD

A **reproducible, tidyverse-style framework** for intensive longitudinal data (ILD) analysis in R, with built-in methodological safeguards, **provenance tracking**, and reporting tools.  
Author: **Alex Litovchenko**.

## Install

```r
remotes::install_github("alitovchenko/tidyILD")
```

## Backend validation benchmarks (developers / CI)

From a **source checkout** of the package (directory containing `DESCRIPTION`), optional cross-backend simulation benchmarks can be run locally:

```bash
Rscript scripts/run-backend-validation-benchmarks.R --tier smoke --out-dir /tmp/bench
Rscript scripts/check-backend-validation-thresholds.R \
  --summary /tmp/bench/benchmark_summary.csv \
  --thresholds inst/benchmarks/thresholds-smoke.json \
  --out /tmp/bench/benchmark_checks.csv
```

This uses `pkgload::load_all()` when available. **Tiers:** `smoke` (fast), `nightly`, `full` — see `inst/dev/BACKEND_VALIDATION_BENCHMARK_CONTRACT.md`. GitHub Actions workflow: `.github/workflows/backend-validation-benchmarks.yml` (scheduled + manual dispatch; artifacts uploaded).

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
ild_plot_predicted_trajectory(fit, time_var = ".ild_seq") # observed + fitted vs time
diag <- ild_diagnostics(fit); diag; plot_ild_diagnostics(diag)
bundle <- ild_diagnose(fit) # diagnostics bundle; use ild_autoplot(bundle, ...)
```

## Pipeline

- `ild_prepare()` — encode longitudinal structure, spacing, gaps
- `ild_summary()` — one-shot summary
- `ild_center()` — person-mean centering (WP/BP)
- `ild_lag()` — index, gap-aware, or time-window lags (supports `lubridate::hours(2)` etc.)
- `ild_decomposition()` — WP/BP variance and ratio; optional WP vs BP density plot
- `ild_check_lags()` — lag validity (valid/invalid, pct_invalid, lag order)
- `ild_panel_lag_prepare()` — several `ild_lag()` columns + one lag audit table
- `ild_crosslag()` — one-call cross-lag: lag predictor, check lags, fit outcome ~ lag
- `ild_compare_fits()` — AIC/BIC/nobs table for a list of models (optional guardrail counts)
- `ild_brms_dynamics_formula()` — template `brms` formula for random lag slopes (does not fit)
- `ild_acf()` — ACF on a variable or on residuals (pre-model check for AR1)
- `ild_spacing_class()` — regular-ish vs irregular-ish
- `ild_spacing()` — spacing diagnostics (median/IQR, large gaps %, CV) and AR1/CAR1 recommendation
- `ild_design_check()` — aggregate spacing, WP/BP, missingness, and recommendations
- `ild_center_plot()` — standalone WP vs BP density plot
- `ild_missing_pattern()` — missingness by person/variable
- `ild_missing_bias()` — test if missingness is associated with a predictor (informative missingness)
- `ild_align()` — align secondary stream (e.g. wearables) to primary ILD within a time window
- `ild_lme()` — mixed-effects model (lmer or nlme with AR1/CAR1)
- `ild_robust_se()` — cluster-robust variance (clubSandwich); `tidy_ild_model(fit, se = "robust")` for robust SE/CI/p
- `ild_missing_model()` — model missingness from covariates; `ild_ipw_weights()` and `ild_ipw_refit()` for IPW sensitivity
- `ild_iptw_weights()`, `ild_iptw_msm_weights()`, `ild_ipcw_weights()`, `ild_joint_msm_weights()` — treatment/censoring/joint MSM weight pipelines
- `ild_msm_estimand()` + `ild_msm_fit()` — estimand-first MSM runner with explicit inference capability status and `strict_inference`
- `ild_msm_bootstrap()` / `tidy_ild_msm_bootstrap()` — cluster bootstrap inference for weighted `lmer` (fixed or re-estimated weights)
- `ild_msm_balance()`, `ild_ipw_ess()`, `ild_msm_overlap_plot()` — weighted balance, effective sample size, and overlap diagnostics
- `ild_msm_diagnose()` + `ild_msm_contrast_over_time()` — one-call diagnostics bridge and time-indexed post-fit contrasts
- `ild_msm_simulate_scenario()` + `ild_msm_recovery()` — causal simulation and recovery harness with scenario-grid validation
- `ild_ctsem()` — continuous-time latent-dynamics backend with `ild_tidy()`, `ild_augment()`, `ild_diagnose()`, and `ild_autoplot()`
- `ild_tvem()` — time-varying effects (GAM); `ild_tvem_plot()` for the coefficient curve
- **Temporal model choice:** `vignette("temporal-dynamics-model-choice", package = "tidyILD")` (lags vs AR vs TVEM vs state-space); `vignette("brms-dynamics-recipes", package = "tidyILD")` for Bayesian templates
- `ild_person_model()` — fit model per person (N-of-1); `ild_person_distribution()` — plot distribution of estimates
- `ild_diagnostics()` — residual ACF, residuals vs fitted/time (use `print()` for summary)
- `plot_ild_diagnostics()` — build diagnostic plots from an `ild_diagnostics` object
- `ild_plot()` — trajectory, heatmap, gaps, missingness, fitted vs observed, **predicted trajectory** (observed + fitted vs time), residual ACF; optional **`facet_by`** for panels (e.g. cluster)
- `ild_plot_predicted_trajectory()` — convenience wrapper for predicted trajectory vs `time_var`
- `ild_heatmap()`, `ild_spaghetti()` — aliases; pass **`facet_by`** through to `ggplot2::facet_wrap()`
- `ild_diagnose()` — assemble **`ild_diagnostics_bundle`** (data, design, fit, residual, …); **`ild_autoplot(bundle, ...)`** for sectioned plots
- `ild_circadian()` — variable by hour of day (when time is POSIXct)
- `augment_ild_model()` / `ild_augment()` — tibble per `ild_augment_schema()`: `.outcome`, `.fitted`, `.resid`, `.resid_std` (Pearson when available), `engine`, `model_class`, etc.
- `tidy_ild_model()` — fixed-effect table (estimate, SE, CI, p); use `se = "robust"` for cluster-robust inference
- `ild_simulate()` — simulated ILD (n_id, n_time/n_obs_per, ar1, wp_effect, bp_effect, irregular)
- `ild_power()` — simulation-based power for a fixed effect (ild_simulate → ild_lme → effect recovery)
- `ema_example` — built-in dataset (`data(ema_example)`)
- **broom**: Load `broom.mixed` for `tidy(fit)` and `augment(fit)` on `ild_lme` fits.

## Vignettes

- **Specialist backends** (`vignette("ild-specialist-backends", package = "tidyILD")`): when to move beyond `lme4` / default tidyILD fits; handoffs to dynamite, PGEE, DSEM, and multivariate workflows; export patterns after `ild_prepare()` / `ild_center()` / `ild_lag()`.
- **Visualization in tidyILD**: index of plots, bundle sections, `facet_by`, predicted trajectories, and partial-effects templates (`marginaleffects` / `ggeffects` on `_wp` / `_bp`).
- **From raw data to model**: full pipeline with `ild_prepare()` through `ild_lme()` and `ild_plot()`.
- **MSM identification and recovery**: assumptions, estimand-first MSM workflow, strict/degraded inference behavior, and scenario-grid recovery checks.
- **Continuous-time dynamics with ctsem**: `ild_ctsem()` workflow, diagnostics, and guardrails for ctsem fits.
- **Short analysis report**: fit, tidy fixed effects, fitted vs observed, residual ACF and Q-Q.
- **Within-between decomposition and irregular spacing**: centering and gap-aware lags.
- **Glossary and quick-start**: function glossary and checklist.

## pkgdown site

Documentation and vignettes are built with [pkgdown](https://pkgdown.r-lib.org/). From the package root: `pkgdown::build_site()`. Config: `_pkgdown.yml`.

## Version tags

Release tags follow `vMAJOR.MINOR.PATCH` and are listed in version order with:

```sh
git tag -l 'v*' --sort=version:refname
```

Current tags: **v0.0.1**, **v0.2.0**, **v0.3.0** (each annotated message is `tidyILD <version>`).

## License

MIT.
