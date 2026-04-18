# tidyILD 0.4.0

## Specialist backends and handoff documentation

- **New vignette:** `vignette("ild-specialist-backends", package = "tidyILD")` — when to move beyond the default tidyILD stack, with a contract (what tidyILD owns vs specialist tools), a decision table (multivariate feedback, p >> n, latent/DSEM, nonlinear/TVP, HD confounding, correlated outcomes), a runnable **export** pattern after `ild_prepare()` / `ild_center()` / `ild_lag()`, and `eval = FALSE` code stubs for **dynamite**, **PGEE**, and **lavaan** (no new Suggests).
- **`temporal-dynamics-model-choice`:** feature-map rows for **multivariate lag systems / feedback** and **high-dimensional predictors (p >> n)** with a pointer to the new specialist vignette.
- **`brms-dynamics-recipes`:** Recipe 3 — multivariate outcomes with `mvbind()` + `set_rescor(TRUE)` sketch, with caveats on residual coupling vs full DSEM / dynamite.
- **Navigation:** `_pkgdown.yml` gains a "Beyond the default stack" articles section; `README.md` adds a Vignettes bullet; `R/package.R` (`?tidyILD`) adds a `@details` paragraph, a vignette list entry, and a `@seealso` link.

## Dependency hygiene

- **`DESCRIPTION`:** `Depends: R (>= 4.1.0)` to match package code that uses the native pipe `|>` and lambda `\(...)` (R 4.1+). Previously `R (>= 4.0.0)`.

# tidyILD 0.3.0

## Visualization layer (index, facets, predicted trajectories)

- **Vignette:** `vignette("visualization-in-tidyILD", package = "tidyILD")` maps common questions to `ild_plot()`, `ild_autoplot()`, and backend helpers; documents `facet_by`, `ild_plot_predicted_trajectory()`, and `eval = FALSE` templates for **marginaleffects** / **ggeffects** on `x_wp` / `x_bp`.
- **`ild_plot()`:** optional **`facet_by`** for `trajectory`, `heatmap`, `gaps`, and new type **`predicted_trajectory`** (observed and fitted lines vs `time_var`). **`ild_spaghetti()`** and **`ild_heatmap()`** pass **`facet_by`** through.
- **`ild_plot_predicted_trajectory()`:** convenience wrapper for the predicted-trajectory plot.
- **pkgdown:** **Visualization** reference group; article listed under workflows. **`?tidyILD`:** **Visualization** section.

## Missingness workflow (diagnostics and reporting)

- **Vignette:** `vignette("ild-missingness-workflow", package = "tidyILD")` — MAR/MNAR context, `ild_missing_pattern()`, compliance, cohort/hazard summaries, IPW template, limits of tidyILD.
- **`ild_missing_compliance()`:** person-level `%` observed, longest observed run, monotone-missing flag, optional `expected_occasions`.
- **`ild_missing_cohort()`**, **`ild_missing_hazard_first()`:** cohort fraction observed by `.ild_seq`; discrete hazard of first missing row among at-risk occasions.
- **`ild_missingness_report()`:** orchestrates compliance, pattern (with optional `outcome` join), cohort, hazard, optional **`ild_missing_model()`**, late-dropout flag, text **`snippets`**.
- **`ild_missing_pattern()`:** new arguments **`outcome`**, **`expected_occasions`** to enrich **`by_id`** via **`ild_missing_compliance()`**.
- **pkgdown / `?tidyILD`:** missingness functions grouped under reference; workflow article listed.

## Temporal dynamics and model choice

- **Vignettes:** `vignette("temporal-dynamics-model-choice", package = "tidyILD")` (decision axes, feature map, minimal examples); `vignette("brms-dynamics-recipes", package = "tidyILD")` (copy-paste `ild_brms` templates, `eval = FALSE` on CRAN).
- **`ild_panel_lag_prepare()`:** build multiple lag columns with one `ild_check_lags()` audit and provenance step.
- **`ild_compare_fits()`:** compare a named list of fits on AIC/BIC/`nobs` with optional parallel `guardrail_bundles` for triggered-rule counts.
- **`ild_brms_dynamics_formula()`:** suggested formula list for person-varying lag slopes (identification/priors still user responsibility).
- **Guardrails:** `GR_LAG_MEAN_STRONG_RESIDUAL_ACF_NO_AR`, `GR_INDEX_LAG_IRREGULAR_SPACING` (contextual evaluation from `ild_diagnose()`).
- **Benchmark harness:** scenario `mlm_lag_x` (recovery of `x_lag1` coefficient, lme4); manifest version 2; contract updated in `inst/dev/BACKEND_VALIDATION_BENCHMARK_CONTRACT.md`.
- **pkgdown / cross-links:** workflow and KFAS choosing-backend vignettes point to temporal-dynamics article.

## Cross-backend validation harness (CI / developers)

- **Benchmark contract:** `inst/dev/BACKEND_VALIDATION_BENCHMARK_CONTRACT.md` — scenario matrix, tiers (`smoke`, `nightly`, `full`), metric and artifact schema (`benchmark_raw.csv`, `benchmark_summary.csv`, `benchmark_metadata.json`).
- **Harness helpers:** `tests/testthat/helper-backend-validation-harness.R` — `harness_run_benchmark()`, summarization, JSON threshold evaluation (optional **jsonlite**).
- **Scripts:** `scripts/run-backend-validation-benchmarks.R`, `scripts/check-backend-validation-thresholds.R`; default thresholds in `inst/benchmarks/thresholds-smoke.json` and `thresholds-nightly.json`.
- **GitHub Actions:** `.github/workflows/backend-validation-benchmarks.yml` (manual + nightly schedule; artifact upload). Does not replace `R-CMD-check`.
- **Tests:** `tests/testthat/test-backend-validation-harness.R` for schema and threshold logic.
- **Docs:** README “Backend validation benchmarks”; developer contracts in `inst/dev/DEVELOPER_CONTRACTS.md`; vignette `vignette("benchmark-simulation-recovery")` cross-link.

## Random slope heterogeneity interpretation

- **`ild_heterogeneity()`**: partial-pooling summaries for **`lmerMod`**, **`lme`**, and **`brmsfit`** (from **`ild_brms()`**): long-format person-level **`ranef` / `coef`**-style tables with conditional SEs/CIs where available; **`summary`** with **`prop_gt_zero`**, quantiles, optional **`threshold`** + **`scale`** (`"raw"`, `"sd_x"`, `"sd_y"`); joins **`VarCorr`** SDs for **`lmer`** when names align.
- **`ild_tidy.ild_heterogeneity()`**, **`ild_autoplot.ild_heterogeneity()`** (caterpillar and histogram), **`print.ild_heterogeneity()`**.
- **`ild_heterogeneity_stratified()`**: descriptive refits by **`subgroup`** with **`min_n_id`** guardrail; not a formal variance-difference test.
- **Diagnostics:** **`ild_diagnose()`** populates **`fit$heterogeneity`** when extraction succeeds; **`ild_autoplot(bundle, section = "fit", type = "heterogeneity", term = ...)`**.
- **Guardrails:** **`GR_RE_SLOPE_VARIANCE_VERSUS_RESIDUAL_LOW`**, **`GR_PERSON_SPECIFIC_SLOPES_EMPIRICALLY_TIGHT`** (heuristic interpretation aids).
- **Vignette:** `vignette("heterogeneity-interpretation", package = "tidyILD")`. Developer contract: **`fit$heterogeneity`** in **`inst/dev/DEVELOPER_CONTRACTS.md`**.

## MSM bootstrap inference (weighted `lmer`)

- **`ild_msm_bootstrap()`**: person-level cluster bootstrap for **`lmer`** fits after **`ild_ipw_refit()`** (or formula + ILD + weight column); **`weight_policy`** **`fixed_weights`** vs **`reestimate_weights`** with user **`weights_fn`**. **`tidy_ild_msm_bootstrap()`** returns **`ild_tidy_schema`** rows with **`interval_type = "bootstrap_percentile"`**. Umbrella topic **`?ild_msm_inference`** (bootstrap vs sandwich vs Bayes guidance). Provenance step **`ild_msm_bootstrap`**.

## MSM balance, ESS, overlap

- **`ild_msm_balance()`**: weighted **standardized mean differences** (binary treatment) for named covariates; optional **by-occasion** stratification. **`ild_ipw_ess()`**: Kish effective sample size from a weight column (pooled or by occasion).
- **`ild_msm_overlap_plot()`**: **propensity overlap** densities (pooled IPTW via **`attr(x, "ild_iptw_fit")`**, or sequential MSM via **`attr(x, "ild_iptw_msm_fits")`**).
- **`ild_diagnose(..., balance = TRUE, balance_treatment, balance_covariates, ...)`** adds **`causal$balance`** (table + ESS). Guardrails **`GR_MSM_BALANCE_SMD_HIGH`** (|SMD| > 0.25) and **`GR_MSM_ESS_LOW`** (ESS vs **N**). **`ild_autoplot(bundle, section = "causal", type = "overlap", treatment = ...)`**.

## Estimand-first, history builder, and recovery harness (MSM v1)

- **`ild_msm_estimand()`** + **`ild_msm_fit()`**: estimand-first runner (v1 scope: static-regime ATE) that executes history/weighting/refit and optional robust/bootstrap inference with provenance.
- **`ild_msm_history_spec()`** + **`ild_build_msm_history()`**: declarative history construction on top of `ild_lag()` with deterministic lag-column manifest.
- **`ild_msm_simulate_scenario()`** + **`ild_msm_recovery()`**: causal DGP and recovery harness reporting bias, RMSE, CI coverage, and positivity stress summaries.
- **New vignette:** `vignette("msm-identification-and-recovery", package = "tidyILD")` for explicit identification assumptions and recovery workflow.

## MSM remaining-gaps closeout (v1.1 hardening)

- **`ild_msm_fit()` inference capability model:** explicit `inference$status` (`ok`/`degraded`/`unsupported`), machine-readable `reason`, user-facing `message`, unified `inference$summary` schema (`term`, `terms`, `estimate`, `std_error`, `conf_low`, `conf_high`, `interval_type`, `method`), and `strict_inference` fail-fast mode instead of silent degradation.
- **Bootstrap capability signaling:** `ild_msm_fit(..., inference = "bootstrap")` now distinguishes full success vs partial/no-success replicate paths and records degraded/unsupported status codes.
- **Estimand schema v1.1 (backward compatible):** `ild_msm_estimand()` supports structured regime specs, `target_time`, explicit contrast metadata, and validation gates for placeholder estimand classes (`att`) and dynamic-rule scaffolding.
- **Dynamic regime + contrasts:** `ild_msm_fit()` evaluates deterministic dynamic rules and records explicit degraded status when full dynamic weighting is not yet implemented; `ild_msm_contrast_over_time()` computes time-indexed contrasts with target-time support and tidy-schema-aligned output.
- **Diagnostics bridge:** `ild_msm_diagnose()` provides one-call dispatch from `ild_msm_fit` outputs into the diagnostics bundle API.
- **Recovery scenario grid + diagnostics:** `ild_msm_recovery()` accepts scenario grids (positivity stress, misspecification toggles, censoring severity) and reports richer per-scenario metrics (failure/degradation rates, ESS quantiles, overlap-extreme rate, interval-calibration gap, modal inference method) with fast + extended test tiers.

## IPTW / IPCW / joint MSM weights

- **`ild_iptw_msm_weights()`**: **sequential** MSM IPTW for time-varying binary `A_t`—per-occasion `glm`, stabilized factors, cumulative `.ipw_treat` within person; attributes `ild_iptw_msm_fits` / `ild_iptw_msm_numerator_fits`. Contrasts with pooled **`ild_iptw_weights()`**.
- **`ild_iptw_weights()`**: logistic **pooled** treatment propensity weights (`.ipw_treat`); binary treatment only in this version.
- **`ild_ipcw_weights()`**: discrete-time **IPCW** for **monotone dropout** (`.ipw_censor`), pooled logistic on an internal `drop_next` indicator.
- **`ild_joint_msm_weights()`**: multiplies `.ipw_treat` × `.ipw_censor` into `.ipw` (optional mean-1 scaling and trimming) for **`ild_ipw_refit()`** / **`ild_diagnose()`**.
- **Diagnostics:** `fill_diagnostics_causal()` summarizes component weights; **`ild_autoplot(..., section = "causal")`** facets joint vs. IPTW vs. IPCW when present; new guardrail **`GR_MSM_COMPONENT_WEIGHTS_UNSTABLE`** (same max/min ratio idea as joint IPW).
- **`ild_ipw_helpers.R`**: shared trim / attribute restore used by IPW and MSM weight steps.

## KFAS backend (experimental)

- **`ild_kfas()`** (optional **KFAS**): single-subject Gaussian **local level** state-space models; **`ild_tidy()`**, **`ild_augment()`**, **`ild_tidy_states()`**, **`ild_diagnose()`**, **`ild_autoplot()`** methods. **`ild_augment()`** uses **`alphahat`** when **`muhat`** is absent (common under diffuse initialization), so **`.fitted`** and residuals align with the smoothed signal. **`ild_autoplot(ild_diagnostics_bundle)`** when **`meta$engine == "KFAS"`** uses KFAS-specific panels: **`predictive`** + **`forecast`** / **`errors`** (not PPC); **`fit`** + **`convergence`** (MLE text summary); **`residual`** + **`acf`** / **`qq`** / **`fitted`**. Direct **`ild_plot_states()`**, **`ild_plot_filtered_vs_smoothed()`**, **`ild_plot_forecast()`** for discoverability. **`ild_methods()`** / **`ild_report()`** describe KFAS provenance (state spec, family, smoothing, fit context, optional **`fitSSM`** dots). See **`inst/dev/KFAS_V1_BACKEND.md`** for the full v1 design and roadmap (trend, AR(1), regression+state, pooling).
- **Tests:** **`tests/testthat/helper-kfas-fixtures.R`** (DGP + bundle expectations); **`tests/testthat/test-kfas-extended.R`** — contract depth, local-level **recovery** (variances + smoothed state), **missingness** (documented row dropping + guardrail), extra **guardrails** (high irregularity, many NA segments); **skipped** placeholders for **`local_trend`** / **`ar1_state`** / **`regression_local_level`** recovery and **`GR_KFAS_STATE_DIMENSION_HIGH_FOR_N`** until multi-state specs ship.
- **`ild_diagnose()`** for **`ild_fit_kfas`** centers **state-space residual diagnostics**: standardized innovations (with `rstandard` / `v`–`F` fallback), ACF, normal QQ correlation, outlier flags, Ljung–Box; rich **`meta`**, **`data`**, **`design`**, **`fit`**, **`predictive`**, **`warnings`**; plus **KFAS-specific guardrails** (`GR_KFAS_*` in **`guardrail_registry()`**). Use **`fit_context = "independent_series_per_id"`** when stacking independent single-ID fits (triggers heterogeneity guardrail).

## ctsem backend (first-class wrapper, v1)

- **`ild_ctsem()`**: first-class continuous-time latent-dynamics wrapper with tidyILD provenance and ILD metadata attachment.
- **S3 backend parity:** `ild_tidy.ild_fit_ctsem()`, `ild_augment.ild_fit_ctsem()`, `ild_diagnose.ild_fit_ctsem()`, and `ild_autoplot.ild_fit_ctsem()` integrate ctsem fits into tidy/augment/diagnostics workflows.
- **Diagnostics bundle integration:** ctsem fits now populate `ild_diagnostics_bundle` with residual/predictive summaries and section-first autoplot compatibility.
- **ctsem guardrails:** added `GR_CTSEM_NONCONVERGENCE`, `GR_CTSEM_UNSTABLE_DYNAMICS`, and `GR_CTSEM_SHORT_SERIES_FOR_COMPLEX_DYNAMICS` with deterministic trigger coverage.
- **New vignette:** `vignette("ctsem-continuous-time-dynamics", package = "tidyILD")`.

## Guardrails (identity & reporting)

- **`print()`** on **`ild_diagnostics_bundle`** prints a **Summary** after slots: counts for warnings/guardrails, highest guardrail severity, and up to five **`rule_id`** values when guardrails are present.
- **`ild_report()`** enriches **`diagnostics_summary`** with **`guardrails_narrative`** and a **`guardrails`** list (`n`, `max_severity`, `rule_ids`); when guardrails fired, **`methods_with_guardrails`** combines methods text with the guardrail sentence.
- **`ild_methods()`** accepts optional **`bundle`** (typically `ild_diagnose(fit)`) to append methodological cautions when guardrails triggered.

## Documentation

- **Simulation benchmark (`ild_recovery_metrics()`, `vignette("benchmark-simulation-recovery", package = "tidyILD")`):** **`ild_recovery_metrics()`** summarizes bias, RMSE, and nominal Wald coverage from **`ild_power(..., return_sims = TRUE)$sim_results`**. The vignette documents the **`ild_simulate()`** DGP, fixed-effect recovery vs empirical **power**, **`sessionInfo`**, optional **knitr** chunk caching for heavy runs, and sections on variance-component snapshots, AR(1) DGP vs residual correlation, and pointers to **brms** / **KFAS**. Illustrative scope: Gaussian **`ild_lme()`** path through **`ild_power()`**, not exhaustive validation of all backends. **GitHub Actions** runs **`R CMD check`** on push/PR.
- **CI / `R CMD check` portability:** **`BH`** and **`RcppEigen`** in **`Suggests`** so **rstan**/**brms** tests can compile Stan models on Linux CI (Boost and Eigen headers). **`ild_augment.brmsfit`** uses **`stats::fitted()`** (S3 dispatch to **brms**). KFAS paths use **`stats::predict`** / **`stats::rstandard`** (instead of non-exported **`KFAS::`** names). ASCII-only diagnostics roxygen strings; **`?ild_plot_filtered_vs_smoothed`** usage matches the function signature.
- **`ild_fit()`** unified entry point: **`backend = "brms"`** dispatches to **`ild_brms()`** (with **`prior`** / **`prior_template`**); **`correlation_class`** other than **`"auto"`** errors for brms (nlme-only). State-space: use **`ild_kfas()`** (not via **`ild_fit()`**). For **`brms::brm()`’s Stan **`backend`** argument, use **`ild_brms()`** directly (name clash with **`ild_fit(..., backend =)`**).
- **Tsibble interoperability (Phase 1):** When **`ild_prepare()`** receives a **`tsibble`** **`tbl_ts`** (one key + index omitted, or explicit **`id`** / **`time`** while still a **`tbl_ts`**), metadata is stored in **`attr(x, "tidyILD")$tsibble`** and read via **`ild_tsibble_meta()`**; **`ild_spacing`** may include a **`tsibble`** sublist contrasting declared vs empirical intervals. **`ild_as_tsibble()`** uses stored **`is_regular`** for a best-effort round-trip. User-facing walkthrough: **`vignette("tsibble-interoperability", package = "tidyILD")`**. See also **`?ild_prepare`**, **`?ild_tsibble_meta`**, **`inst/dev/DEVELOPER_CONTRACTS.md`** §5.
- **KFAS conceptual vignettes:** `vignette("kfas-state-space-modeling", package = "tidyILD")` (state-space basics, filtered vs smoothed in ILD language, explicit v1 trust boundaries: discrete-time, not ctsem-like CT dynamics, not multilevel latent pooling); `vignette("kfas-irregular-timing-spacing", package = "tidyILD")` (spacing diagnostics first; KFAS does not “solve” irregular timing in a continuous-time sense); `vignette("kfas-choosing-backend", package = "tidyILD")` (decision framing: lme vs brms vs KFAS). **pkgdown:** `_pkgdown.yml` lists these three under **Articles** in reading order before other vignettes.
- **`ild_diagnose()` / `ild_diagnostics_bundle`:** sections are **denser by default**: `data` includes **`cohort`**, **`obs_per_id`**, **`outcome_summaries`**, **`missingness_rates`**; `design` includes **`flags`**, **`time_coverage`**, **`occasion_imbalance`** (mirrors `obs_per_id`); `fit` includes **`reml`**, **`optimizer`**, **`theta_summary`**, **`X_rank`**, **`residual_correlation`** (frequentist/brms); **`predictive`** adds **`n`**, **`mean_residual`**, **`max_abs_error`**; **`missingness`** is always structured (`note`, `pct_na_by_var`). Optional **`missing_model`** / **`missing_model_predictors`** / **`causal_detail`** gate **`ild_missing_model()`** and extra IPW quantiles.
- **`ild_autoplot()`** for **`ild_diagnostics_bundle`** is **section-first**: use `section` and `type` (e.g. `residual` + `acf` / `qq` / `fitted`; `fit` + `convergence`; `predictive` + `ppc` for brms; `data` + `missingness`; `design` + `coverage`; `causal` + `weights`). With `section = "residual"` and `type = NULL`, legacy bundles still return the full named list from `plot_ild_diagnostics()` when `residual$legacy_ild_diagnostics` is present. **`ild_diagnose()`** sets `attr(bundle, "ild_fit")` and `attr(bundle, "ild_data")` for plotting.
- **`?ild_diagnostics_utilities`**: umbrella help topic describing **standalone** use and **bundle section** roles for `ild_design_check()`, `ild_missing_pattern()`, `ild_missing_model()`, and `ild_ipw_weights()` alongside `ild_diagnose()` / `ild_diagnostics_bundle`.
- **Developer package standards:** normative spec in **`inst/dev/DEVELOPER_CONTRACTS.md`** and vignette **`vignette("developer-contracts", package = "tidyILD")`** (diagnostics bundle sections, tidy/augment semantics, backend adapter checklist).
- **Contract regression tests** (`tests/testthat/test-contract-regression.R`, `tests/testthat/helper-contract-fixtures.R`): seeded scenarios assert bundle section density, expected guardrail `rule_id`s, **`ild_tidy` / `ild_augment`** columns, **`ild_autoplot`** routes, and guardrail-aware **`ild_methods(..., bundle)`** (see developer contracts).

## `ild_tidy()` schema migration

- **`tidy_ild_model()`** and **`ild_tidy.brmsfit()`** now emit **all** columns from **`ild_tidy_schema()`**: `conf_low` / `conf_high` (replacing **`ci_low`** / **`ci_high`**), plus **`component`** (currently `"fixed"` for coefficient rows), **`effect_level`** (conservative: `"population"` for intercept, `"within"` / `"between"` for `_wp` / `_bp` terms, `"cross_level"` for interactions with both, `"unknown"` otherwise), **`statistic`**, **`interval_type`** (`"Wald"` frequentist, `"quantile"` brms), **`engine`**, **`model_class`**. Optional columns are included (`NA` for frequentist; **brms** fills **rhat** / **ESS** when `intervals = TRUE`).
- Internal helpers in **`ild_tidy_helpers.R`**; see **`vignette("developer-contracts", package = "tidyILD")`** for semantics.

## `ild_augment()` schema migration

- **`augment_ild_model()`** and **`ild_augment.brmsfit()`** emit all **`ild_augment_schema()`** required columns: **`.outcome`** (canonical observed vector; formula-named response column removed), **`.resid_std`** (Pearson-type via `residuals(..., type = "pearson")` when length-matched; otherwise **`NA`**), **`engine`**, **`model_class`**, plus optional columns (mostly **`NA`** until used).
- **brms:** posterior interval columns are **`.fitted_lower`** / **`.fitted_upper`** (replacing **`.fitted_q2.5`** / **`.fitted_q97.5`**).
- Helpers in **`ild_augment_helpers.R`**.

## Guardrails (analysis safety layer)

- **`guardrail_registry()`**: catalog of methodological rules (`rule_id`, `section`, `severity`, default messages). **`ild_diagnose()`** populates **`ild_diagnostics_bundle$guardrails`** with **triggered** rules only, using the canonical columns above. Rules cover mixed WP/BP predictors, irregular spacing without residual correlation, high gap rates, late missingness concentration, singular fits, poor mixing / low ESS (Bayesian), and unstable IPW weights.

## Contracts (Phase 1 — specifications only)

- **`ild_diagnostics_bundle()`**: canonical class for engine-agnostic diagnostics with fixed slots: `meta`, `data`, `design`, `fit`, `residual`, `predictive`, `missingness`, `causal`, `warnings`, `guardrails`, `summary_text` (see `?ild_diagnostics_bundle`). Populated by engines in a later phase.
- **`ild_tidy_schema()`** / **`ild_augment_schema()`**: documented required and optional column names; **`ild_tidy()`** and **`ild_augment()`** outputs match these contracts in this release.

## Provenance and reporting

- **Provenance schema:** Added `schema_version = "1"` and `object_type` to data and analysis provenance for future evolution. Step records include `step_id`.
- **ild_report()** now returns a stable schema: `meta` (n_obs, n_id, engine), `methods`, `model_table`, `diagnostics_summary`, `provenance`, `provenance_export_path`.
- **ild_methods()** is richer: row counts (N persons, n observations) when available, explicit IPW/weighted-estimation wording, diagnostics sentence reflects requested types (autocorrelation, Q-Q, etc.). New optional argument `robust_se` so methods text can mention cluster-robust SEs when used.
- **Provenance vignette:** *Reproducible ILD workflows with tidyILD provenance* (prepare, center, lag, fit, diagnostics, ild_history, ild_methods, ild_report, export, ild_compare_pipelines).
- **Analysis provenance** attached to `ild_crosslag()` and `ild_ipw_refit()` return values.
- **Package description** updated to "A reproducible, tidyverse-style framework for intensive longitudinal data analysis in R, with built-in methodological safeguards, provenance tracking, and reporting tools."

## Unified API and tsibble

- **ild_lme()** analysis provenance records `backend` (`"lme4"` / `"nlme"`), `fit_engine` (`"lmer"` / `"lme"`), and `backend_version` (installed package version).
- **ild_fit()** wrapper to choose `backend = "lme4"` or `"nlme"` (same as `ild_lme()` with `ar1` implied).
- **ild_tidy()**, **ild_augment()**, **ild_autoplot()** S3 generics dispatching to `tidy_ild_model()`, `augment_ild_model()`, and `plot_ild_diagnostics()` / `ild_plot()` as appropriate.
- **ild_as_tsibble()** converts an ILD object to a `tbl_ts` (optional **tsibble** in Suggests). **ild_prepare()** with both `id` and `time` omitted can take a `tbl_ts` when `tsibble` is installed (single key + index inferred).

## Bayesian backend (brms)

- **ild_brms()** fits Bayesian mixed models via **brms**, with **ild_prior_ild()** templates (`default`, `weakly_informative`, `minimal_shrinkage`). Attributes **ild_posterior** (sampler settings, prior summary, divergences) and **ild_provenance** are attached.
- **ild_tidy()** / **ild_augment()** methods for `brmsfit` (posterior means, 95% intervals, R-hat, ESS; augmented fitted intervals as `.fitted_lower` / `.fitted_upper`).
- **ild_diagnose()** always returns **`ild_diagnostics_bundle`** for `lmerMod`, `lme`, and `brmsfit` (unified slots: data, design, fit, residual, predictive, missingness, causal, warnings, guardrails, `summary_text`). Frequentist residual plots use **`residual$legacy_ild_diagnostics`** with **`plot_ild_diagnostics()`** or **`ild_autoplot(bundle, section = "residual")`**. Call **`ild_diagnostics()`** directly if you only need the legacy object. **ild_methods()** and **ild_report()** describe priors, chains, warmup, and sampler controls for `ild_brms` fits.

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
