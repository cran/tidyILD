# tidyILD 0.3.0

## Provenance and reporting

- **Provenance schema:** Added `schema_version = "1"` and `object_type` to data and analysis provenance for future evolution. Step records include `step_id`.
- **ild_report()** now returns a stable schema: `meta` (n_obs, n_id, engine), `methods`, `model_table`, `diagnostics_summary`, `provenance`, `provenance_export_path`.
- **ild_methods()** is richer: row counts (N persons, n observations) when available, explicit IPW/weighted-estimation wording, diagnostics sentence reflects requested types (autocorrelation, Q-Q, etc.). New optional argument `robust_se` so methods text can mention cluster-robust SEs when used.
- **Provenance vignette:** *Reproducible ILD workflows with tidyILD provenance* (prepare, center, lag, fit, diagnostics, ild_history, ild_methods, ild_report, export, ild_compare_pipelines).
- **Analysis provenance** attached to `ild_crosslag()` and `ild_ipw_refit()` return values.
- **Package description** updated to "A reproducible, tidyverse-style framework for intensive longitudinal data analysis in R, with built-in methodological safeguards, provenance tracking, and reporting tools."

# tidyILD 0.2.0

This release builds on four pillars: **methodological safeguards**, **modeling breadth**, **provenance tracking**, and **reporting tools**.

## Safeguards

- **Robust uncertainty:** `ild_robust_se()` provides cluster-robust variance estimators (CR0/CR2/CR3) via clubSandwich. `tidy_ild_model()` gains `se = "robust"` and `robust_type`; CIs and p-values use Wald normal approximation when using robust SEs.
- **Missingness and IPW:** `ild_missing_model()` fits missingness models (glm/glmer); `ild_ipw_weights()` generates inverse probability weights (stabilized or not, optional trim); `ild_ipw_refit()` refits ild_lme with IPW (lmer only). Documented as diagnostic/sensitivity tooling, not a full MNAR solution.
- **Design and diagnostics:** `ild_design_check()` aggregates spacing, WP/BP decomposition, and missingness with recommendations. `ild_spacing()` reports interval stats and AR1/CAR1 recommendation. `ild_missing_bias()` tests whether missingness is associated with a predictor. `ild_center_plot()` for WP vs BP density. WP/BP safeguard warning in `ild_lme()` when predictors vary at both levels (suggests `ild_center()`).

## Modeling breadth

- **Time-varying effects:** `ild_tvem()` fits time-varying effect models using mgcv; `ild_tvem_plot()` visualizes the coefficient curve with confidence band.
- **Power analysis:** `ild_power()` for simulation-based power of a fixed effect (ild_simulate -> ild_lme -> effect recovery); supports lmer and nlme.
- **Cross-lag and person-level:** `ild_crosslag()` one-call pipeline (ild_lag -> ild_check_lags -> ild_lme). `ild_person_model()` and `ild_person_distribution()` for per-person fits and estimate distribution.
- **Multi-stream alignment:** `ild_align()` aligns a secondary stream to primary ILD within a time window (e.g. self-report + wearables).
- **Visualization:** `ild_heatmap()`, `ild_spaghetti()`, `ild_circadian()` (time-of-day patterns when time is POSIXct).

## Provenance

- **Data and analysis provenance:** Preprocessing steps (ild_prepare, ild_center, ild_lag, ild_align, ild_ipw_weights) and analysis steps (ild_lme, ild_diagnostics, ild_tvem, ild_power, ild_missing_model, ild_crosslag, ild_ipw_refit) are recorded with a stable schema (version, schema_version, object_type, step_id, args, outputs).
- **ild_provenance()** and **ild_history()** return or print the recorded steps. **ild_export_provenance()** writes provenance to JSON or YAML for reproducibility supplements and archiving.
- **ild_compare_pipelines()** compares two ILD or model objects and reports differing steps and arguments.

## Reporting

- **ild_methods()** generates a methods-style narrative from provenance (row counts, AR1/CAR1, created variables, IPW and robust SE when specified). Optional `robust_se` argument for cluster-robust SE mention.
- **ild_report()** returns a standardized list: `meta` (n_obs, n_id, engine), `methods`, `model_table`, `diagnostics_summary`, `provenance`, `provenance_export_path`. Optional provenance export in one call.
- New vignette: *Reproducible ILD workflows with tidyILD provenance* (ild_history, ild_methods, ild_report, export, compare).

## Other improvements

- **Model tidiers:** `augment_ild_model()` and `tidy_ild_model()` with consistent columns across lmer/nlme; S3 print methods.
- **Documentation:** Package described as a reproducible, tidyverse-style framework with safeguards, provenance, and reporting. Vignettes use `set.seed()` / `seed` for determinism; optional-package examples (e.g. clubSandwich) use `eval = requireNamespace(...)`. `ild_power()` examples kept small (n_sim = 25).

# tidyILD 0.0.1 (initial release)

- Initial CRAN release.
- Pipeline: `ild_prepare()`, `ild_summary()`, `ild_center()`, `ild_lag()` (index, gap-aware, time-window), `ild_spacing_class()`, `ild_missing_pattern()`, `ild_check_lags()`.
- Modeling: `ild_lme()` (lmer or nlme with AR1/CAR1), `ild_diagnostics()`, `ild_plot()` (trajectory, gaps, missingness, fitted, residual ACF).
- Utilities: `ild_simulate()`, `ild_manifest()` / `ild_bundle()` for reproducibility, broom integration for `ild_lme` fits.
- Data: `ema_example` dataset.
- Vignettes: workflow, within-between decomposition and spacing, glossary and quick-start.
