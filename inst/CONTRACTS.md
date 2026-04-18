# tidyILD Phase 1 contracts (user-facing summary)

`ild_tidy()` and `ild_augment()` behavior is unchanged until later phases. **`ild_diagnose()`** returns **`ild_diagnostics_bundle`** for supported model classes (see `?ild_diagnose`).
Standalone utilities that also feed bundle sections are documented under **`?ild_diagnostics_utilities`**.
This file mirrors the R-level contracts: see `?ild_diagnostics_bundle`, `?ild_tidy_schema`, `?ild_augment_schema`.

**Developers:** the full normative specification (section fields, tidy/augment semantics, backend checklist) is **`inst/dev/DEVELOPER_CONTRACTS.md`** and the vignette **`vignette("developer-contracts", package = "tidyILD")`**.

## 1. `ild_diagnostics_bundle`

Fixed top-level slots (same names for every engine; content may be `NULL`):

| Slot | Role |
|------|------|
| `meta` | Run metadata, engine, dimensions |
| `data` | Spacing, gaps, compliance, missingness, distributions |
| `design` | Within/between variation, time coverage, occasions, imbalance |
| `fit` | Convergence, singularity, optimizer / MCMC state |
| `residual` | ACF, Q-Q, fitted vs observed, heteroskedasticity |
| `predictive` | CV, PPC, forecast error |
| `missingness` | IPW / imputation summaries when applicable |
| `causal` | Weights, positivity, causal diagnostics when applicable |
| `warnings` | Tibble (0+ rows): `source`, `level`, `message`, `code` |
| `guardrails` | Tibble (0+ rows): `rule_id`, `section`, `severity`, `triggered`, `message`, `recommendation` (see `?guardrail_registry`) |
| `summary_text` | Character vector |

## 2. `ild_tidy()` columns

**Required:** `term`, `component`, `effect_level`, `estimate`, `std_error`, `conf_low`, `conf_high`, `statistic`, `p_value`, `interval_type`, `engine`, `model_class`

**Optional:** `rhat`, `ess_bulk`, `ess_tail`, `pd`, `rope_low`, `rope_high`

## 3. `ild_augment()` columns

**Required:** `.ild_id`, `.ild_time`, `.outcome`, `.fitted`, `.resid`, `.resid_std`, `engine`, `model_class`

**Optional:** `.fitted_lower`, `.fitted_upper`, `.influence`, `.state`, `.state_lower`, `.state_upper`
