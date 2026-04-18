# Phase 1 column dictionaries for ild_tidy() and ild_augment() — contract only; outputs migrate later

#' Column dictionary for ild_tidy() outputs
#'
#' Minimum stable contract for parameter-level tables returned by \code{\link{ild_tidy}}.
#' Implementations (\code{\link{tidy_ild_model}}, \code{\link{ild_tidy.brmsfit}}) emit all
#' required columns; \code{conf_low} / \code{conf_high} replace legacy \code{ci_low} / \code{ci_high}.
#'
#' @return A list with \code{required} and \code{optional} character vectors.
#' @details
#' **Required columns:** \code{term}, \code{component}, \code{effect_level}, \code{estimate},
#' \code{std_error}, \code{conf_low}, \code{conf_high}, \code{statistic}, \code{p_value},
#' \code{interval_type}, \code{engine}, \code{model_class}.
#'
#' **Optional:** \code{rhat}, \code{ess_bulk}, \code{ess_tail}, \code{pd}, \code{rope_low}, \code{rope_high}.
#'
#' **\code{interval_type}:** Frequentist rows are typically \code{Wald} (model or robust; see \code{\link{tidy_ild_model}}).
#' Bootstrap output from \code{\link{tidy_ild_msm_bootstrap}} uses \code{bootstrap_percentile} (equal-tailed over replicate coefficients).
#' @seealso \code{\link{ild_augment_schema}}, \code{\link{ild_diagnostics_bundle}}.
#'   Full semantics for \code{component}, \code{effect_level}, and \code{interval_type}:
#'   \code{vignette("developer-contracts", package = "tidyILD")}.
#' @export
ild_tidy_schema <- function() {
  list(
    required = ILD_TIDY_REQUIRED_COLS,
    optional = ILD_TIDY_OPTIONAL_COLS
  )
}

#' @keywords internal
ILD_TIDY_REQUIRED_COLS <- c(
  "term",
  "component",
  "effect_level",
  "estimate",
  "std_error",
  "conf_low",
  "conf_high",
  "statistic",
  "p_value",
  "interval_type",
  "engine",
  "model_class"
)

ILD_TIDY_OPTIONAL_COLS <- c(
  "rhat",
  "ess_bulk",
  "ess_tail",
  "pd",
  "rope_low",
  "rope_high"
)

#' Column dictionary for \code{ild_augment()} outputs
#'
#' Defines the minimum stable contract for augmented observation-level tables.
#' \code{\link{augment_ild_model}} and \code{\link{ild_augment}} methods emit all required
#' columns; see \code{\link{augment_ild_model}} for \code{.resid_std} semantics.
#'
#' @details
#' **Required columns**
#' \tabular{ll}{
#'   \code{.ild_id} \tab Person identifier (from ILD) \cr
#'   \code{.ild_time} \tab Time variable \cr
#'   \code{.outcome} \tab Observed outcome (numeric vector aligned to rows) \cr
#'   \code{.fitted} \tab Fitted / posterior mean fitted value \cr
#'   \code{.resid} \tab Raw residual \cr
#'   \code{.resid_std} \tab Pearson-type residual when \code{residuals(fit, type = "pearson")} is available and length-matched; otherwise \code{NA} (not ad hoc z-scores) \cr
#'   \code{engine} \tab Character engine id \cr
#'   \code{model_class} \tab Model class string \cr
#' }
#'
#' **Optional:** \code{.fitted_lower}, \code{.fitted_upper}, \code{.influence},
#' \code{.state}, \code{.state_lower}, \code{.state_upper}.
#'
#' @return A list with \code{required} and \code{optional} character vectors.
#' @seealso \code{\link{ild_tidy_schema}}, \code{\link{ild_diagnostics_bundle}}.
#'   Reserved prefixes and migration notes: \code{vignette("developer-contracts", package = "tidyILD")}.
#' @export
ild_augment_schema <- function() {
  list(
    required = ILD_AUGMENT_REQUIRED_COLS,
    optional = ILD_AUGMENT_OPTIONAL_COLS
  )
}

#' @keywords internal
ILD_AUGMENT_REQUIRED_COLS <- c(
  ".ild_id",
  ".ild_time",
  ".outcome",
  ".fitted",
  ".resid",
  ".resid_std",
  "engine",
  "model_class"
)

ILD_AUGMENT_OPTIONAL_COLS <- c(
  ".fitted_lower",
  ".fitted_upper",
  ".influence",
  ".state",
  ".state_lower",
  ".state_upper"
)
