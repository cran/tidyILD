# Package-level documentation for ?tidyILD
# Do not add code; this file exists only for the roxygen block below.

#' tidyILD: Tidy Intensive Longitudinal Data Analysis
#'
#' @description
#' tidyILD is a reproducible, tidyverse-style framework for intensive longitudinal
#' data (ILD) analysis in R, with built-in methodological safeguards, provenance
#' tracking, and reporting tools. It supports ecological momentary assessment
#' (EMA) and diary studies with a tidy pipeline from raw data to mixed-effects
#' models: explicit time structure, within-between decomposition, spacing-aware
#' lags, and diagnostics. Use it when you have repeated measures per person over
#' time and want consistent handling of time, gaps, centering, and residual
#' correlation (AR1/CAR1).
#'
#' @details
#' All ILD structure (`.ild_*` columns and `ild_*` metadata) is created only
#' by \code{\link{ild_prepare}} (via the internal constructor). Downstream
#' functions expect data prepared with \code{ild_prepare()}. For the full
#' workflow and applications, see the vignettes.
#'
#' @section Getting started:
#' A minimal workflow: simulate or load data, prepare with
#' \code{\link{ild_prepare}}, inspect with \code{\link{ild_summary}}, apply
#' \code{\link{ild_center}} and \code{\link{ild_lag}}, fit with
#' \code{\link{ild_lme}}, then \code{\link{ild_diagnostics}} or
#' \code{\link{ild_plot}}. See the examples below.
#'
#' @section Function index by topic:
#' \describe{
#'   \item{Setup and validation}{\code{\link{ild_prepare}}, \code{\link{as_ild}},
#'     \code{\link{is_ild}}, \code{\link{validate_ild}}, \code{\link{ild_meta}}}
#'   \item{Summaries and inspection}{\code{\link{ild_summary}},
#'     \code{\link{ild_spacing_class}}, \code{\link{ild_spacing}},
#'     \code{\link{ild_design_check}}, \code{\link{ild_missing_pattern}},
#'     \code{\link{ild_missing_bias}}, \code{\link{ild_missing_model}},
#'     \code{\link{ild_ipw_weights}}, \code{\link{ild_ipw_refit}},
#'     \code{\link{ild_plot}} (types: trajectory, gaps, missingness)}
#'   \item{Within-person and lags}{\code{\link{ild_center}}, \code{\link{ild_center_plot}},
#'     \code{\link{ild_decomposition}}, \code{\link{ild_lag}}, \code{\link{ild_check_lags}},
#'     \code{\link{ild_crosslag}}, \code{\link{ild_align}}}
#'   \item{Modeling}{\code{\link{ild_lme}}, \code{\link{ild_person_model}},
#'     \code{\link{ild_tvem}} (time-varying effects)}
#'   \item{Diagnostics and visualization}{\code{\link{ild_acf}}, \code{\link{ild_diagnostics}},
#'     \code{\link{ild_plot}} (types: fitted, residual_acf), \code{\link{ild_heatmap}},
#'     \code{\link{ild_spaghetti}}, \code{\link{ild_circadian}}, \code{\link{ild_tvem_plot}}}
#'   \item{Provenance and methods}{\code{\link{ild_provenance}}, \code{\link{ild_history}},
#'     \code{\link{ild_methods}}, \code{\link{ild_report}}, \code{\link{ild_compare_pipelines}},
#'     \code{\link{ild_export_provenance}}}
#'   \item{Reproducibility}{\code{\link{ild_manifest}}, \code{\link{ild_bundle}}}
#'   \item{Utilities and data}{\code{\link{ild_simulate}}, \code{\link{ild_power}}, \code{\link{ema_example}}}
#'   \item{Person-level}{\code{\link{ild_person_model}}, \code{\link{ild_person_distribution}}}
#'   \item{Model tidiers}{\code{\link{augment_ild_model}}, \code{\link{tidy_ild_model}}
#'     (model or robust SE via \code{se = "robust"}), \code{\link{ild_robust_se}};
#'     \code{\link{tidy.ild_lme}}, \code{\link{augment.ild_lme}} (broom.mixed, see \code{\link{broom_ild_lme}})}
#' }
#'
#' @section Vignettes:
#' \code{browseVignettes("tidyILD")} lists all vignettes. Key entries:
#' \itemize{
#'   \item \emph{From raw data to model with tidyILD} --- Full pipeline:
#'     prepare, inspect, center, lag, fit, diagnose.
#'   \item \emph{Short analysis report} --- Fit, tidy fixed effects,
#'     fitted vs observed, residual ACF and Q-Q.
#'   \item \emph{Within-between decomposition and irregular spacing} ---
#'     Centering (BP/WP), gap-aware lags, spacing classification.
#'   \item \emph{Reproducible ILD workflows with tidyILD provenance} ---
#'     Inspect history, generate methods text, ild_report(), export and compare pipelines.
#'   \item \emph{Glossary and quick-start checklist} --- Table of main
#'     functions and a short checklist.
#' }
#'
#' @section Key concepts:
#' \itemize{
#'   \item \strong{ILD:} Intensive longitudinal data; many repeated
#'     measurements per person over time (e.g. EMA).
#'   \item \strong{Within-between decomposition:} \code{\link{ild_center}}
#'     adds \code{_bp} (person mean) and \code{_wp} (within-person deviation);
#'     use WP for within-person effects and BP for between-person or
#'     cross-level terms.
#'   \item \strong{Spacing-aware lags:} \code{\link{ild_lag}} supports
#'     \code{index}, \code{gap_aware} (NA when \code{gap > max_gap}), and
#'     \code{time_window}; avoids misalignment from assuming equal spacing.
#'   \item \strong{Residual correlation:} \code{\link{ild_lme}} can fit nlme
#'     with AR1 or CAR1 for residual autocorrelation;
#'     \code{\link{ild_spacing_class}} helps choose \code{regular-ish} vs
#'     \code{irregular-ish} spacing.
#'   \item \strong{Person-level:} \code{\link{ild_person_model}} fits models separately
#'     per participant; \code{\link{ild_person_distribution}} plots the distribution of
#'     estimates across persons (N-of-1 / idiographic).
#' }
#'
#' @author Alex Litovchenko \email{al4877@columbia.edu}
#'
#' @seealso
#' \code{\link{browseVignettes}} and \code{vignette(package = "tidyILD")} for
#' vignettes. Core entry points: \code{\link{ild_prepare}}, \code{\link{ild_lme}}.
#' Related packages: \pkg{lme4}, \pkg{nlme} (model backends),
#' \pkg{broom.mixed} (tidiers).
#'
#' @examples
#' library(tidyILD)
#' d <- ild_simulate(n_id = 10, n_obs_per = 12, irregular = TRUE, seed = 42)
#' x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 7200)
#' ild_summary(x)
#' x <- ild_center(x, y)
#' x <- ild_lag(x, y, mode = "gap_aware", max_gap = 7200)
#' fit <- ild_lme(y ~ 1, data = x, ar1 = TRUE, correlation_class = "CAR1")
#' ild_diagnostics(fit, data = x)
#' ild_plot(fit, type = "fitted")
#'
#' @docType package
#' @name tidyILD-package
NULL
