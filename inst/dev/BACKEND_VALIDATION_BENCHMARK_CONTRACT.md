# Cross-backend validation benchmark contract

This document defines the **scenario matrix**, **metric semantics**, **runtime tiers**, and **artifact schema** for the tidyILD backend validation harness (`scripts/run-backend-validation-benchmarks.R`, `tests/testthat/helper-backend-validation-harness.R`).

## Architecture alignment

- Entry points remain: `ild_fit()` for `lme4` / `nlme` / `brms`; `ild_kfas()` and `ild_ctsem()` are invoked only when the scenario type requires them.
- Outputs are validated against existing contracts where applicable: `ild_tidy()` / `ild_augment()` schemas, `ild_diagnostics_bundle` structure, `guardrail_registry()` rule IDs in diagnostic bundles.
- The harness lives under `tests/` and `scripts/`; it is **not** part of the exported user API unless promoted later.

## Runtime tiers

| Tier     | Intended use              | Typical `n_sim` | Scenarios                          |
|----------|---------------------------|-----------------|------------------------------------|
| `smoke`  | PR / manual quick check   | 2–5             | Fast MLM recovery + KFAS smoke     |
| `nightly`| Scheduled regression      | 10–30           | + nlme, brms, ctsem (optional)     |
| `full`   | Release / deep validation | 50+             | All scenarios, larger samples      |

Tiers are **not** CRAN-facing; they run in CI or locally with optional dependencies installed.

## Scenario IDs (v1)

| `scenario_id`        | `type`      | `tier_min` | Description |
|----------------------|------------|------------|-------------|
| `mlm_power_ri`       | `mlm_power`| `smoke`    | `ild_simulate` + Gaussian `x`, `y += effect * x`; estimand: coefficient of `x` (same DGP for all MLM backends). |
| `mlm_lag_x`         | `mlm_lag`  | `smoke`    | `x` with `ild_lag(..., gap_aware)`; outcome `y = beta * x_lag1 + noise`; estimand: coefficient of `x_lag1`; **lme4** only in the harness matrix. |
| `kfas_local_level` | `kfas`     | `smoke`    | Single-series ILD subset; `ild_kfas()` local level; metrics focus on **fit success** and variance auxiliaries (truth optional / `NA`). |
| `ctsem_univariate`  | `ctsem`    | `nightly`  | Single-ID continuous-time path; short Stan chains in CI; **warn-only** thresholds by default. |

Additional scenarios may append rows to the manifest in `helper-backend-validation-harness.R` and update this table.

## Backend eligibility

- `mlm_power_ri`: `lme4`, `nlme`, `brms` (each skipped if package unavailable or not requested via `--backends`).
- `mlm_lag_x`: `lme4` only.
- `kfas_local_level`: `kfas` only (requires **KFAS**).
- `ctsem_univariate`: `ctsem` only (requires **ctsem**, **rstan**; slow).

## Metric definitions (per replication)

- **truth**: Known estimand when defined (e.g. `effect_size` for `x`); `NA` for auxiliary-only KFAS smoke rows.
- **estimate**, **std_error**, **conf_low**, **conf_high**: From `ild_tidy()` / `tidy_ild_model()` for the target term when available; otherwise `NA`.
- **bias** = `estimate - truth` (if `truth` finite).
- **squared_error** = `(estimate - truth)^2`.
- **covered**: `truth` inside `[conf_low, conf_high]` when intervals exist; else `NA`.
- **calibration_gap** = `nominal_level - mean(covered)` aggregated at summary level (not per row when `covered` is NA).
- **converged**: Backend-specific heuristic (e.g. lme4 optimizer, brms Rhat heuristic, ctsem flag).
- **singular** (lme4): `lme4::isSingular` when applicable.
- **n_guardrails**: Row count in `bundle$guardrails` from `ild_diagnose()`.
- **elapsed_sec**: Wall time for fit + diagnostics slice used in harness.

## Artifact schema

### `benchmark_raw.csv`

Columns (minimum):

`run_id`, `git_sha`, `tier`, `scenario_id`, `backend`, `replicate_id`, `truth`, `estimate`, `bias`, `squared_error`, `std_error`, `ci_low`, `ci_high`, `covered`, `nominal_level`, `elapsed_sec`, `converged`, `singular`, `fit_error`, `n_guardrails`, `guardrail_rule_ids`, `skipped_reason`

### `benchmark_summary.csv`

Grouped by `tier`, `scenario_id`, `backend`:

`n_replicates`, `n_skipped`, `n_converged`, `convergence_rate`, `mean_bias`, `rmse`, `mean_coverage`, `calibration_gap`, `mean_elapsed_sec`, `mean_n_guardrails`

### `benchmark_metadata.json`

`package_version`, `r_version`, `platform`, `git_sha`, `seed_base`, `tier`, `backends_requested`, `backends_skipped`, `scenario_manifest_version`, `timestamp_utc`.

## Threshold checking

Threshold files live in `inst/benchmarks/thresholds-*.json`. The checker script evaluates **aggregates** in `benchmark_summary.csv` against:

- `min_coverage` (for scenarios with defined interval coverage)
- `max_convergence_failure_rate`
- optional `max_rmse`, `min_n_converged`

Optional backends (`brms`, `ctsem`) may use `"warn_only": true` so CI emits warnings without failing.

## Adding a new backend or scenario

1. Extend the scenario manifest and `tier_min` in the harness helper.
2. Implement a branch in `harness_fit_one()` and metric extraction if the estimand differs.
3. Document the scenario here and in `inst/dev/DEVELOPER_CONTRACTS.md`.
4. Add deterministic unit tests for schema and threshold logic in `test-backend-validation-harness.R`.
