# tidyILD developer contracts (package standards)

**Status:** Normative for new backends and API evolution. User-facing summaries live in `?ild_diagnostics_bundle`, `?ild_tidy_schema`, and `?ild_augment_schema`; this document is the **operational** specification for implementers.

**Version:** Align with **tidyILD** package version; schema constants live in `R/ild_diagnostics_bundle.R`, `R/ild_schema_tidy_augment.R`, and `R/ild_guardrail_registry.R`.

---

## 1. Diagnostics bundle standard (`ild_diagnostics_bundle`)

### 1.1 Object-level contract

- **Class:** `c("ild_diagnostics_bundle", "list")`.
- **Top-level names (fixed order, non-optional):**  
  `meta`, `data`, `design`, `fit`, `residual`, `predictive`, `missingness`, `causal`, `warnings`, `guardrails`, `summary_text`  
  (constant `ILD_DIAGNOSTICS_BUNDLE_SLOTS` in source).
- **Validation:** `validate_ild_diagnostics_bundle()` — list names must match exactly; `warnings` and `guardrails` must be tibbles; `guardrails` must include the columns in §1.10; `summary_text` must be character.
- **NULL sections:** Any of `meta` … `causal` may be `NULL` if not applicable for an engine or run. **`warnings`** and **`guardrails`** are never `NULL` (use empty tibbles).

### 1.2 Section: `meta`

| Field | Required | Type | Semantics |
|-------|----------|------|-----------|
| `engine` | Recommended | character | Backend id: `"lmer"`, `"lme"`, `"brms"`, or future ids (e.g. `"kfas"`). |
| `n_obs`, `n_id` | Recommended | integer | Rows and distinct persons on the ILD used for diagnostics. |
| `ar1` | Optional | logical | Whether residual AR1/CAR1 was used (frequentist path). |
| `brms_types` | Optional | character | Subset of diagnostics requested for brms (`convergence`, `sampler`, `ppc`). |

Additional named entries are allowed if documented per engine.

### 1.3 Section: `data`

**Purpose:** Observation-level and cohort-level **data** quality (spacing, gaps, global missingness pattern).

| Field | Required | Semantics |
|-------|----------|------------|
| `summary` | Recommended | One-row tibble from `ild_summary()$summary` (e.g. `n_id`, `n_obs`, `prop_gap`, `median_dt_sec`, `iqr_dt_sec`). |
| `spacing_class` | Recommended | `"regular-ish"` or `"irregular-ish"` (`ild_spacing_class()`). |
| `n_gaps`, `pct_gap` | Recommended | Gap counts / percent of intervals flagged as gaps. |
| `median_dt_sec`, `iqr_dt_sec` | Optional | Timing dispersion. |
| `time_range` | Optional | Numeric range of `.ild_time_num` or equivalent. |
| `cohort` | Recommended | `list(n_id, n_obs, n_intervals)` aligned with `ild_summary`. |
| `obs_per_id` | Recommended | `list(min, median, max, sd)` observation counts per person (occasion imbalance). |
| `outcome_summaries` | Optional | Tibble per outcome column: `mean`, `sd`, `min`, `max`, `pct_na`, `n` (when model response/predictors passed to `ild_diagnose`). |
| `missingness_rates` | Optional | Tibble `variable`, `pct_na` for those columns. |
| `missing_pattern` | Optional | List: `summary`, `overall`, `n_complete` from `ild_missing_pattern()` (global columns). |

### 1.4 Section: `design`

**Purpose:** **Design** structure (WP/BP, imbalance, design-time missingness).

| Field | Required | Semantics |
|-------|----------|------------|
| `ild_design_check` | Recommended | Full return value of `ild_design_check()` (embedded list object). |
| `spacing_class`, `recommendation` | Optional | Redundant copies acceptable for fast reporting. |
| `wp_bp` | Optional | Decomposition tibble or `NULL` if `vars` omitted. |
| `design_missingness` | Optional | Summaries bundled inside `ild_design_check` / design slice. |
| `flags` | Recommended | `list(has_wp_bp, spacing_class, irregular)` for fast checks. |
| `time_coverage` | Recommended | `list(min, max, span_sec)` from pooled timeline. |
| `occasion_imbalance` | Recommended | Same object as `data$obs_per_id` (mirrored for visibility). |

### 1.5 Section: `fit`

**Purpose:** **Estimation** diagnostics (convergence, singularity, MCMC).

**Frequentist `lmerMod`:** `engine`, `singular`, `converged`, `optimizer_messages`, `optinfo` (lmer); **`reml`**, **`optimizer`**, **`theta_summary`** (`length`/`min`/`max` of variance-component vector), **`X_rank`** (fixed-effects design rank from `qr(X)`), **`residual_correlation`** (`list(modeled, structure, note)` — lmer has no AR1/CAR1; `modeled` reflects `attr(ild_ar1)`).

**Frequentist `lme`:** above where applicable; **`apVar_ok`**; **`residual_correlation`** with **`class`** (`ild_correlation_class`) and **`coef_corStruct`** when present.

**Bayesian (`brmsfit`):** `engine`, `ild_posterior`, `convergence_table`, `max_rhat`, `n_divergent`, `n_max_treedepth_hits`; **`residual_correlation`** (`note` only; residual structure is model-specific).

**Optional `heterogeneity` (`lmerMod`, `lme`, `brmsfit`):** `list(available, reason, object)`.
When `available` is `TRUE`, `object` is an `ild_heterogeneity` result from [ild_heterogeneity()] (person-specific partial-pooling summaries).
When extraction fails (e.g. intercept-only fixed model without random effects, or parsing error), `available` is `FALSE` and `reason` is a short message.
`ild_autoplot(bundle, section = "fit", type = "heterogeneity", ...)` forwards `term` / `heterogeneity_type` to [ild_autoplot.ild_heterogeneity()].

### 1.6 Section: `residual`

**Purpose:** **Residual** behavior (ACF, Q-Q, vs time/fitted).

| Field | Required | Semantics |
|-------|----------|------------|
| `legacy_ild_diagnostics` | Optional | Full `ild_diagnostics()` object for frequentist engines (enables `plot_ild_diagnostics()` / `ild_autoplot(bundle, section = "residual", type = NULL)` multi-plot or short `type` names `acf` / `qq` / `fitted`). |
| `residual_sd`, `cor_observed_fitted` | Optional | Scalar summaries. |
| `engine` | Recommended | Engine id. |

Bayesian fits may omit `legacy_ild_diagnostics` and keep lighter summaries only.

### 1.7 Section: `predictive`

**Purpose:** **Predictive** checks (obs vs fitted, PPC).

| Content | Semantics |
|---------|-----------|
| Frequentist | `engine`, `n`, `mean_abs_error`, `rmse`, `mean_residual` (bias), `max_abs_error`, `cor_observed_fitted`. |
| brms | `ppc` from `ild_brms_ppc_summary()` when requested; same observation-level scalars as frequentist when `ild_augment` succeeds (`n`, MAE, RMSE, etc.). |

### 1.8 Section: `missingness`

**Purpose:** **Variable-level** missingness aligned to model terms (not duplicate of global `data$missing_pattern` when both exist).

| Field | Semantics |
|-------|------------|
| `note` | Set when no model variables; else `NULL`. |
| `summary`, `overall` | From `ild_missing_pattern(data, vars = predictors)` when predictors exist. |
| `pct_na_by_var` | Subset of `summary` (`variable`, `pct_na`) when available. |
| `missing_model_diagnostic` | Optional: `tidy`, `message`, `outcome`, `predictors` when `ild_diagnose(..., missing_model = TRUE)` runs `ild_missing_model()`. |

### 1.9 Section: `causal`

**Purpose:** **Causal / weighting** diagnostics (IPW, positivity).

Typical content: `columns_found`, `weight_summary` (`min`, `max`, `mean`) when `.ipw` or related columns exist on `ild_data`. Optional **`weight_detail`** (`quantiles`, `sum_w`) when `ild_diagnose(..., causal_detail = TRUE)`.

### 1.10 Tibbles: `warnings` and `guardrails`

**`warnings` (event log from software / sampler):**

| Column | Semantics |
|--------|-----------|
| `source` | e.g. `"lme4"`, `"stan"`, `"tidyILD"`. |
| `level` | e.g. `"warning"`, `"note"`. |
| `message` | Human-readable text. |
| `code` | Short machine id: e.g. `lmer_warning`, `divergent_transitions`. |

**`guardrails` (methodological rules; see `?guardrail_registry`):**

| Column | Semantics |
|--------|-----------|
| `rule_id` | Stable id (e.g. `GR_SINGULAR_RANDOM_EFFECTS`). |
| `section` | Bundle section the rule relates to (`fit`, `design`, `data`, …). |
| `severity` | e.g. `info`, `warning`. |
| `triggered` | Always `TRUE` for rows that appear (only triggered rules are stored). |
| `message`, `recommendation` | Run-specific or default text. |

**Surfacing guardrails (package identity):**

- **`print()` on `ild_diagnostics_bundle`:** After the slot listing, prints a **Summary** block: warning row count, guardrail row count, and (when `nrow(guardrails) > 0`) **highest severity** (`info` < `warning`) and up to **five** unique `rule_id` values. Helpers: `guardrail_severity_rank()` / `guardrail_max_severity()` in `R/ild_guardrail_registry.R`.
- **`ild_report()`:** On successful `ild_diagnose()`, `diagnostics_summary` includes **`guardrails_narrative`** (short string) and **`guardrails`** (`n`, `max_severity`, `rule_ids`). If any guardrails fired, **`methods_with_guardrails`** repeats the methods paragraph with the same guardrail sentence appended (equivalent to `ild_methods(fit, bundle = diagnose_result)`).
- **`ild_methods(..., bundle = NULL)`:** Optional `bundle` from `ild_diagnose(fit)`; when `nrow(bundle$guardrails) > 0`, appends one sentence after provenance: *Methodological cautions (tidyILD guardrails): …*
- **New rules:** Each new `rule_id` added to `ILD_GUARDRAIL_REGISTRY` must have a **deterministic regression test** that asserts the rule can still be triggered (see `tests/testthat/test-guardrails-triggers.R` and fit-level tests via `evaluate_guardrails_fit()` with a constructed `fit_diag` when full `ild_diagnose()` is impractical).

**Contract regression (semantics, not only shape):** `tests/testthat/helper-contract-fixtures.R` builds stable seeded scenarios (regular vs irregular spacing, IPW instability, late dropout with merged contextual guardrails, merged singular / brms fit-level guardrails). `tests/testthat/test-contract-regression.R` checks dense bundle sections, expected `rule_id`s, `ild_tidy` / `ild_augment` required columns, core `ild_autoplot()` routes, and guardrail-aware `ild_methods(..., bundle)` text. The brms scenario uses `skip_on_cran()` because it fits a small `brms` model.

### 1.11 `summary_text`

Character vector of short narrative lines for reporting (`ild_report`, printing). Not a substitute for structured sections.

---

## 2. Tidy output standard (`ild_tidy()`)

**Schema function:** `ild_tidy_schema()` — constants `ILD_TIDY_REQUIRED_COLS`, `ILD_TIDY_OPTIONAL_COLS`.

### 2.1 Required columns (implemented)

`term`, `component`, `effect_level`, `estimate`, `std_error`, `conf_low`, `conf_high`, `statistic`, `p_value`, `interval_type`, `engine`, `model_class`

**Frequentist (`tidy_ild_model`):** `interval_type` is `"Wald"`; `statistic` is the model-reported **t** (or **z**-ratio when `se = "robust"`). **brms:** `interval_type` is `"quantile"` (equal-tailed default intervals); `p_value` is `NA`; optional posterior columns filled when `intervals = TRUE`.

### 2.2 Optional columns

`rhat`, `ess_bulk`, `ess_tail`, `pd`, `rope_low`, `rope_high` (Bayesian / posterior summaries).

### 2.3 Semantics: `component`

Conservative vocabulary (extend with engine-specific values only if documented):

| Value | Meaning |
|-------|---------|
| `fixed` | Standard fixed-effect regression coefficients (current default for all exposed FE rows). |
| `random` | Random-effect variance / covariance summaries when exposed as rows. |
| `auxiliary` | Dispersion, residual variance, or residual-correlation parameters (not yet exposed for lme4/nlme in `ild_tidy`). |
| `scale` | Dispersion parameters (legacy alias in docs; prefer `auxiliary` where appropriate). |
| `correlation` | Correlation structure parameters (e.g. AR1). |
| `nonlinear` | Smooth / spline terms (e.g. mgcv-style). |

### 2.4 Semantics: `effect_level`

| Value | Meaning |
|-------|---------|
| `population` | Intercept / cohort-mean type estimands (`(Intercept)` rows). |
| `within` | Term name ends with `_wp` (within-person component). |
| `between` | Term name ends with `_bp` (between-person component). |
| `cross_level` | Interaction term involving both `_wp` and `_bp` substrings. |
| `unknown` | Coefficient rows that are not clearly classified (default for generic predictors). |
| `auxiliary` | Variance / sampler / state parameters (when such rows exist). |

### 2.5 Semantics: `interval_type`

| Value | Meaning |
|-------|---------|
| `Wald` | Symmetric interval from SE (normal or t as implemented). |
| `quantile` | Equal-tailed posterior quantiles. |
| `HPD` | Highest posterior density interval. |
| `normal` | Alias for large-sample normal approximation when distinct from `Wald`. |

---

## 3. Augment output standard (`ild_augment()`)

**Schema function:** `ild_augment_schema()` — `ILD_AUGMENT_REQUIRED_COLS`, `ILD_AUGMENT_OPTIONAL_COLS`.

### 3.1 Required columns (implemented)

`.ild_id`, `.ild_time`, `.outcome`, `.fitted`, `.resid`, `.resid_std`, `engine`, `model_class`

**`.resid_std` semantics (principled but sparse):** set from `residuals(fit, type = "pearson")` when the engine returns a numeric vector of the correct length; otherwise all `NA`. Do **not** populate with arbitrary z-scores of `.resid` in the same column (avoids mixing definitions). For **brms**, Pearson residuals are used when available; otherwise `NA`.

### 3.2 Optional columns

`.fitted_lower`, `.fitted_upper` (e.g. equal-tailed 95% for **brms** `ild_augment`), `.influence`, `.state`, `.state_lower`, `.state_upper` (e.g. latent states; often `NA` placeholders).

### 3.3 Reserved prefixes and names

| Pattern | Reserved for |
|---------|----------------|
| `.ild_*` | ILD system columns from `ild_prepare()` (do not overwrite for non-ILD semantics). |
| `.fitted*` | Fitted / predicted mean response. |
| `.resid*` | Residuals (raw or transformed). |
| `.state*` | Latent trajectory / random-effect line values when exposed per row. |

The observed response is always **`.outcome`** (no duplicate formula-named column in the augmented tibble).

---

## 4. Backend adapter checklist

**Skeleton:** `inst/dev/backend-adapter-template.R` — commented R patterns for `ild_tidy`, `ild_augment`, `ild_diagnose`, guardrails, provenance, tests, and docs (not sourced by the package; copy into `R/` when implementing).

Each new estimation backend (e.g. KFAS, ctsem) should ship:

### 4.1 S3 methods (operational generics)

| Generic | Requirement |
|---------|-------------|
| `ild_tidy()` | Return a tibble **aligned with `ild_tidy_schema()`** (required columns when feasible). |
| `ild_augment()` | Return a tibble **aligned with `ild_augment_schema()`**; attach `attr(..., "ild_data")` on the **model object** used by diagnostics. |
| `ild_diagnose()` | Return **`ild_diagnostics_bundle`** only; populate all relevant sections; use shared fillers/helpers where possible (`fill_diagnostics_*`, guardrail evaluation). |
| `ild_autoplot()` | For bundle: **section-first** routing (`section` + `type`); implement plotters that read bundle slots; call engine-specific code only inside plotters. Attach `attr(bundle, "ild_fit")` and `attr(bundle, "ild_data")` from `ild_diagnose()` so PPC, fitted, and missingness plots work without a second user argument. |

### 4.2 Data attachment

- Fitted object must carry **`attr(fit, "ild_data")`** (validated ILD) for `ild_augment` / `ild_diagnose`.

### 4.3 Provenance

- Attach **`attr(fit, "ild_provenance")`** via `ild_new_analysis_provenance()` (or equivalent) with `step` set to the fitting function name (e.g. `ild_kfas`), serializable `args`, and `outputs` summarizing key choices.

### 4.4 Tests (minimum)

- **Schema:** `expect_named()` / `expect_true(all(ild_tidy_schema()$required %in% names(...)))` for tidy output (or explicit waiver for transitional columns).
- **Bundle:** `expect_s3_class(..., "ild_diagnostics_bundle")`; `expect_no_error(validate_ild_diagnostics_bundle(...))` when constructed via `ild_diagnose()`.
- **Smoke:** one small simulated ILD dataset, one fitted model, `ild_tidy`, `ild_augment`, `ild_diagnose` run without error (use `skip_if_not_installed` for heavy deps).

### 4.5 Documentation

- Export methods documented on the same `@rdname` as existing engines where possible; describe engine-specific **fit** / **residual** slots in **Details**.

---

## 5. Tsibble interoperability

**Status:** Phase 1 — provenance on input; best-effort round-trip via `ild_as_tsibble()`.

- **Conceptual choice (keys):** ILD uses **one** subject identifier column. `tbl_ts` inputs must have **exactly one** key variable. Multiple keys (e.g. crossed factors) are **not** supported; reshape or paste into a single id before `ild_prepare()`.
- **Provenance:** When `data` is a `tbl_ts`, `ild_prepare()` stores metadata in `attr(x, "tidyILD")$tsibble` (key/index names, interval summary string, `is_regular`, tsibble version). The `tbl_ts` class is still dropped on output; this records formal time-series semantics for reporting and for `ild_as_tsibble()`.
- **Spacing:** `ild_spacing$tsibble` (when present) links declared tsibble regularity/interval text to empirical `.ild_dt` summaries; they may diverge after sorting, duplicate handling, or row drops.
- **Round-trip:** `ild_as_tsibble()` passes `regular =` from stored `is_regular` when available so `interval()` often matches the original for **unchanged** data.

---

## 6. Cross-backend validation benchmark harness

**Normative spec:** `inst/dev/BACKEND_VALIDATION_BENCHMARK_CONTRACT.md` (scenario IDs, tiers, metric columns, artifact layout).

**Purpose:** Run shared simulation scenarios across `lme4` / `nlme` / `brms` / KFAS / ctsem entry points (where installed), write `benchmark_raw.csv`, `benchmark_summary.csv`, and `benchmark_metadata.json`, and optionally gate regressions with JSON thresholds in `inst/benchmarks/thresholds-*.json`.

**Implementation (not exported API):**

| Location | Role |
|----------|------|
| `tests/testthat/helper-backend-validation-harness.R` | `harness_run_benchmark()`, metric extraction, summarization, threshold evaluation. |
| `scripts/run-backend-validation-benchmarks.R` | CLI runner (`--tier`, `--backends`, `--n-sim`, `--seed`, `--out-dir`). Expects package root + `pkgload::load_all()` or `devtools::load_all()`. |
| `scripts/check-backend-validation-thresholds.R` | Reads `benchmark_summary.csv` + thresholds JSON; writes `benchmark_checks.csv`; exit code 1 on hard failures. |
| `.github/workflows/backend-validation-benchmarks.yml` | Scheduled / manual CI; uploads artifacts. |

**When adding a new backend:** extend the scenario manifest and `harness_fit_one()` in the harness helper; document the scenario here and in the benchmark contract; add or adjust `skip_if_not_installed()` tests in `tests/testthat/test-backend-validation-harness.R`. Prefer **warn-only** thresholds for slow or fragile optional engines until metrics stabilize.

---

## 7. Temporal dynamics helpers and guardrails

**User-facing functions** (see `vignette("temporal-dynamics-model-choice", package = "tidyILD")`):

| Function | Role |
|----------|------|
| `ild_panel_lag_prepare()` | Multi-variable `ild_lag()` + single `ild_check_lags()`; provenance step `ild_panel_lag_prepare`. |
| `ild_compare_fits()` | Named list of fits → tibble (`aic`, `bic`, `n_obs`, `converged`, optional `n_guardrails`); **not** an automatic nested-model test. |
| `ild_brms_dynamics_formula()` | Returns a suggested `formula` + `notes` for `ild_brms()`; does not fit. |

**Guardrails** (registry + `evaluate_guardrails_contextual()`):

- `GR_LAG_MEAN_STRONG_RESIDUAL_ACF_NO_AR` — lag terms in mean, no residual AR, pooled residual ACF at lag 1 above a fixed threshold (heuristic).
- `GR_INDEX_LAG_IRREGULAR_SPACING` — `ild_spacing_class` irregular-ish and data provenance records `ild_lag(..., mode = "index")`.

---

## 8. Related files in the source tree

| Location | Role |
|----------|------|
| `inst/CONTRACTS.md` | Short user-facing mirror of slot/column names. |
| `inst/dev/backend-adapter-template.R` | Skeleton methods and checklist for new engines (copy-paste starting point). |
| `inst/dev/KFAS_V1_BACKEND.md` | KFAS / `ild_kfas()` v1 design (Gaussian local level first; full vocabulary staged). |
| `inst/dev/BACKEND_VALIDATION_BENCHMARK_CONTRACT.md` | Benchmark scenario matrix and artifact schema. |
| `R/ild_diagnostics_bundle.R` | Bundle constructor and validation. |
| `R/ild_schema_tidy_augment.R` | Tidy/augment column lists. |
| `R/ild_guardrail_registry.R` | Guardrail rule ids and evaluation helpers. |
| `R/ild_diagnose_fillers.R` | Reference implementations for section contents. |

This file is the **single extended normative spec** for developers; keep it updated when slots or schemas change.
