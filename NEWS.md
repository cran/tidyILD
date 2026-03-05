# tidyILD 0.2.0

- **Power analysis:** `ild_power()` for simulation-based power of a fixed effect (ild_simulate -> ild_lme -> Wald rejection). Uses small n_sim in examples; lmerMod inference via Wald z-approximation when p-values not from backend.
- **Design and diagnostics:** `ild_design_check()` aggregates spacing, WP/BP decomposition, missingness; `ild_spacing()` reports interval stats and AR1/CAR1 recommendation; `ild_missing_bias()` tests informative missingness; `ild_center_plot()` for WP/BP density plot.
- **Cross-lag and person-level:** `ild_crosslag()` (ild_lag + ild_check_lags + ild_lme); `ild_person_model()` and `ild_person_distribution()` for per-person fits and estimate distribution.
- **Visualization:** `ild_heatmap()`, `ild_spaghetti()` (aliases); `ild_circadian()` for time-of-day; `ild_align()` for multi-stream alignment within a time window.
- **Model tidiers:** `augment_ild_model()`, `tidy_ild_model()` with consistent columns across lmer/nlme; S3 print methods for diagnostics and tidy model.
- **Documentation:** Package help and vignettes updated; pkgdown site; analysis-report vignette. Examples use `set.seed()` for determinism; ild_power examples kept small (n_sim = 25).

# tidyILD 0.0.1 (initial release)

- Initial CRAN release.
- Pipeline: `ild_prepare()`, `ild_summary()`, `ild_center()`, `ild_lag()` (index, gap-aware, time-window), `ild_spacing_class()`, `ild_missing_pattern()`, `ild_check_lags()`.
- Modeling: `ild_lme()` (lmer or nlme with AR1/CAR1), `ild_diagnostics()`, `ild_plot()` (trajectory, gaps, missingness, fitted, residual ACF).
- Utilities: `ild_simulate()`, `ild_manifest()` / `ild_bundle()` for reproducibility, broom integration for `ild_lme` fits.
- Data: `ema_example` dataset.
- Vignettes: workflow, within-between decomposition and spacing, glossary and quick-start.
