# KFAS backend (tidyILD v1) — design specification

**Status:** Normative target for `ild_kfas()` and related methods. Implementation is incremental; see `NEWS.md` for what is shipped vs planned.

**Related:** `inst/dev/backend-adapter-template.R`, `inst/dev/DEVELOPER_CONTRACTS.md`, `?ild_tidy_schema`, `?ild_augment_schema`.

---

## Scope (v1)

Smaller than the full KFAS surface area. Focus on recurring ILD use cases:

- Latent level tracking over time; latent trend/change; AR(1)-style latent dynamics (staged).
- **Gaussian observation models first**; Poisson/binomial only if the object contract stays stable.
- Single-subject or stacked-by-subject workflows with clear semantics; **no pooled latent state across IDs in v1**.

### In scope for v1 (incremental delivery)

- Gaussian observation models.
- Local level (`SSMtrend(degree = 1)`).
- Local linear trend, AR(1) state, regression + state hybrids — **state_spec vocabulary reserved**; implementation may follow after local level is stable.
- Filtered and smoothed state extraction.
- One-step-ahead fitted values and prediction errors; standardized residuals via KFAS (`rstandard.KFS` when available).
- State-space residual diagnostics (bundle + autoplot hooks).
- Optional short-horizon forecasts (planned).
- Simulation-based state uncertainty where practical (planned).

### Out of scope for v1

- Pooled multi-subject latent states.
- Continuous-time unequal-interval modeling (reserve for ctsem later).

---

## User-facing API

### Primary: `ild_kfas()`

Opinionated entry point. Conceptual arguments:

| Argument | Role |
|----------|------|
| `data` | Prepared ILD (`validate_ild`) or data frame passed through `ild_prepare_state_space()` |
| `outcome` | Outcome column name (string) |
| `state_spec` | Controlled vocabulary (see below) |
| `observation_family` | `"gaussian"` in v1 |
| `time_units` | **Required explicit** declaration (e.g. `"hours"`, `"days"`) for provenance and spacing summaries |
| `irregular_time` | Logical / detection; warn when the state model assumes near-equal spacing but data are highly irregular |
| `smoother` | Pass through to `KFS()` smoothing |
| `forecast_horizon` | Optional short horizon (planned) |
| `...` | KFAS control (`fitSSM`, `KFS` args) |

### State spec vocabulary (v1)

Controlled labels (not free-form formulas initially) so tidyILD can generate guardrails, diagnostics, methods text, state labels, and tidy outputs:

- `local_level`
- `local_trend`
- `ar1_state`
- `regression_local_level`
- `regression_ar1_state`

**v1 implementation:** `local_level` only unless noted in `NEWS.md`.

### Pooling modes

- **`pool = "none"` (default):** one time series per fit — **one subject per call** in v1 (single distinct `.ild_id`).
- **Mode B (stacked per-ID fits):** wrapper loops over IDs; combined tidyILD object + per-ID subfits — **planned**; not required for first merge.

---

## Return object class and contents

Suggested class vector:

```r
c("ild_fit_kfas", "ild_fit_model", "ild_analysis")
```

**Contents (list or similar):**

| Slot | Description |
|------|-------------|
| `kfas_model` | Fitted `SSModel` after `fitSSM` |
| `kfs` | `KFS` output |
| `spec` | Standardized internal spec (from `ild_kfas_spec.R`) |
| `state_labels` | Names for states (for tidiers and reports) |
| `mapping` | Data mapping metadata (outcome column, time order, ID) |
| `schema_version` | Character, e.g. `"0.1"` |

**Attributes:**

- `attr(., "ild_data")` — ILD data used (single-subject slice)
- `attr(., "ild_provenance")` — via `ild_new_analysis_provenance(..., step = "ild_kfas", ...)`

---

## Data contract

KFAS requires a stricter temporal contract than mixed-model wrappers.

1. Input must be **`validate_ild` ILD** with valid `id` / `time` structure, **or** a raw data frame processed by internal `ild_prepare_state_space()`.
2. **Explicit `time_units`** stored in provenance and echoed in bundle `meta` when diagnosing.
3. Interval summaries and interval distributions belong in bundle `data` / `design` when `ild_diagnose()` is implemented for this engine.

---

## File layout (source)

| File | Role |
|------|------|
| `R/ild_kfas_spec.R` | Vocabulary + internal spec object |
| `R/ild_kfas_helpers.R` | `ild_prepare_state_space()`, spacing warnings, KFS extraction |
| `R/ild_kfas.R` | `ild_kfas()` — fit layer |
| `R/ild_kfas_tidiers.R` | `ild_tidy()`, `ild_tidy_states()` |
| `R/ild_kfas_augment.R` | `ild_augment()` |
| `R/ild_kfas_diagnose.R` | `ild_diagnose()` — bundle contract adapter |
| `R/ild_kfas_autoplot.R` | `ild_autoplot()` |

Internal layers: **specification → model build (`SSModel`) → `fitSSM` → `KFS` → extraction → contract adapter.**

---

## `ild_tidy()` (parameters)

Minimum: one row per estimated variance / hyperparameter where available.

**Required schema columns:** per `ild_tidy_schema()`.

**`component` values (KFAS-specific):**

- `observation` — regression coefficients (when regression spec is implemented)
- `state` — transition / state dynamics parameters
- `variance` — observation and state innovation variances
- `initial` — initial state parameters if exposed
- `auxiliary` — other quantities

**`effect_level`:** conservative defaults — variances and latent dynamics often `auxiliary`; `_wp` / `_bp` terms follow existing tidyILD rules when present.

**`interval_type`:** `"Wald"` if asymptotic intervals are returned; `"none"` if unavailable (document in `?ild_tidy` method).

---

## `ild_tidy_states()`

One row per time point and state summary (backend-specific). Does **not** replace `ild_tidy()` for parameters.

---

## `ild_augment()`

Must satisfy `ild_augment_schema()` required columns.

**KFAS-specific optional columns:** `.state`, `.state_lower`, `.state_upper` — use meaningfully; for multi-state models prefer **`ild_augment_states()`** (long format) instead of overloading columns.

**v1 preference:** single interpretable latent level → populate `.state` with the smoothed level when `state_spec = "local_level"`.

**`.resid_std`:** standardized **one-step-ahead prediction errors** from the state-space model (e.g. `rstandard(KFS)`), **not** arbitrary z-scores of raw residuals.

---

## Tests

- `skip_if_not_installed("KFAS")` for integration tests.
- Single-subject simulated series; assert `ild_tidy` / `ild_augment` column names, `validate_ild` on `ild_data`, and object class.

## Guardrails (KFAS-specific)

Registered in `guardrail_registry()` / `ILD_GUARDRAIL_REGISTRY` with `rule_id` prefix `GR_KFAS_*`:

| rule_id | Intent |
|---------|--------|
| `GR_KFAS_HIGH_IRREGULARITY_FOR_DISCRETE_TIME` | Irregular spacing vs discrete-time indexing |
| `GR_KFAS_SHORT_SERIES_FOR_STATE_SPACE` | Too few time points for the chosen state complexity |
| `GR_KFAS_STATE_DIMENSION_HIGH_FOR_N` | Latent dimension large relative to `n` |
| `GR_KFAS_DEGENERATE_VARIANCE_ESTIMATE` | `Q` or `H` near zero |
| `GR_KFAS_NONCONVERGENCE` | `fitSSM` optimization did not converge |
| `GR_KFAS_MANY_MISSING_OUTCOME_SEGMENTS` | Many NA runs in the outcome before row omission |
| `GR_KFAS_UNMODELED_BETWEEN_PERSON_HETEROGENEITY` | Set `fit_context = "independent_series_per_id"` when fitting the same template per person; not a pooled latent model |

Evaluation: `evaluate_guardrails_kfas()` in `R/ild_kfas_guardrails.R`, called from `ild_diagnose.ild_fit_kfas`.

---

## Changelog

Track implementation status in `NEWS.md` and bump `schema_version` when the stored object shape changes.
