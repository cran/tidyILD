# KFAS state-space models for single-subject ILD (optional dependency: KFAS)

#' Fit a Gaussian state-space model via KFAS (single-subject ILD)
#'
#' Opinionated entry point for **local level** models in v1. Requires the
#' \pkg{KFAS} package. One distinct \code{.ild_id} per fit; subset your data first.
#'
#' @param data An object that passes [validate_ild()] (e.g. after [ild_prepare()]).
#' @param outcome Character name of the numeric outcome column.
#' @param state_spec Character; use \code{"local_level"} in v1. See
#'   \code{inst/dev/KFAS_V1_BACKEND.md} for the full controlled vocabulary.
#' @param observation_family Currently only \code{"gaussian"}.
#' @param time_units Character describing the time column (e.g. \code{"days"},
#'   \code{"hours"}). Stored in provenance; required for clear reporting.
#' @param irregular_time Logical; if \code{TRUE}, skips the irregular-spacing warning.
#' @param smoother Logical; passed to \code{KFS()} smoothing (default \code{TRUE}).
#' @param forecast_horizon Integer; when \code{> 0}, used by \code{\link{ild_plot_forecast}()}
#'   with \code{\link[stats]{predict}} on the fitted model and stored in \code{mapping$forecast_horizon}.
#' @param fit_context \code{"single_series"} (default) or \code{"independent_series_per_id"}
#'   when you have fit the **same** state-space template separately for multiple persons
#'   and must not interpret results as a pooled multilevel latent model. The latter
#'   triggers guardrail \code{GR_KFAS_UNMODELED_BETWEEN_PERSON_HETEROGENEITY}.
#' @param ... Passed to \code{KFAS::fitSSM()} (e.g. \code{method}, \code{inits}).
#' @return An object of class \code{c("ild_fit_kfas", "ild_fit_model", "ild_analysis")}
#'   with list slots \code{kfas_model}, \code{kfs}, \code{spec}, \code{state_labels},
#'   \code{mapping}, \code{schema_version}, \code{preprocessing}. Attributes \code{ild_data} and
#'   \code{ild_provenance} are set.
#' @seealso \code{inst/dev/KFAS_V1_BACKEND.md}, [ild_tidy()], [ild_augment()], [ild_diagnose()].
#'
#' \strong{Conceptual vignettes:}
#' \itemize{
#'   \item \href{../doc/kfas-state-space-modeling.html}{\code{vignette("kfas-state-space-modeling", package = "tidyILD")}}
#'   \item \href{../doc/kfas-irregular-timing-spacing.html}{\code{vignette("kfas-irregular-timing-spacing", package = "tidyILD")}}
#'   \item \href{../doc/kfas-choosing-backend.html}{\code{vignette("kfas-choosing-backend", package = "tidyILD")}}
#' }
#' @export
ild_kfas <- function(data,
                     outcome,
                     state_spec = "local_level",
                     observation_family = "gaussian",
                     time_units,
                     irregular_time = FALSE,
                     smoother = TRUE,
                     forecast_horizon = 0L,
                     fit_context = c("single_series", "independent_series_per_id"),
                     ...) {
  rlang::check_installed("KFAS", reason = "to use ild_kfas()")
  if (missing(time_units) || !nzchar(as.character(time_units)[1L])) {
    stop("time_units must be a non-empty string (e.g. 'days', 'hours').", call. = FALSE)
  }
  observation_family <- match.arg(observation_family, c("gaussian"))
  fit_context <- match.arg(fit_context)
  state_spec <- ild_kfas_normalize_state_spec(state_spec, implemented_only = TRUE)
  if (!isTRUE(irregular_time)) {
    ild_kfas_warn_irregular(data)
  }

  prep <- ild_kfas_prepare_ts(data, outcome)
  d <- prep$data
  y <- prep$y
  is_na_y <- is.na(y)
  n_na_segments <- ild_kfas_na_run_count(is_na_y)
  pct_outcome_na <- if (length(is_na_y) > 0L) mean(is_na_y) else NA_real_
  n_rows_input <- nrow(d)
  if (any(is_na_y)) {
    warning("Removing rows with NA in outcome for KFAS.", call. = FALSE)
    ok <- !is_na_y
    d <- d[ok, , drop = FALSE]
    y <- y[ok]
  }
  if (length(y) < 5L) {
    stop("Need at least 5 non-missing outcome values for ild_kfas().", call. = FALSE)
  }
  preprocessing <- list(
    n_rows_input = n_rows_input,
    n_outcome_na = sum(is_na_y),
    n_na_segments = n_na_segments,
    pct_outcome_na = pct_outcome_na,
    n_rows_used = nrow(d)
  )

  dots <- list(...)
  if (state_spec == "local_level") {
    # Local level: random walk + noise; variances estimated by ML
    # Formula must see unqualified SSMtrend() for stats::model.frame (KFAS requirement).
    SSMtrend <- KFAS::SSMtrend
    mod <- KFAS::SSModel(
      y ~ SSMtrend(degree = 1L, Q = list(matrix(NA_real_))),
      H = matrix(NA_real_)
    )
    vy <- stats::var(y, na.rm = TRUE)
    vd <- stats::var(diff(as.numeric(y)), na.rm = TRUE)
    if (!is.finite(vy) || vy <= 0) vy <- 1
    if (!is.finite(vd) || vd <= 0) vd <- vy
    inits <- c(log(vy), log(vd))
    fit_ssm <- do.call(
      KFAS::fitSSM,
      c(list(mod, inits = inits, method = "BFGS"), dots)
    )
    sm <- if (isTRUE(smoother)) "state" else "none"
    kfs <- KFAS::KFS(fit_ssm$model, smoothing = sm, filtering = "state")
  } else {
    stop("Unreachable: state_spec not implemented.", call. = FALSE)
  }

  spec <- ild_kfas_internal_spec(
    state_spec = state_spec,
    observation_family = observation_family,
    time_units = as.character(time_units)[1L],
    irregular_time = isTRUE(irregular_time),
    fit_context = fit_context,
    pool_mode = "none"
  )

  m_state <- tryCatch(ncol(kfs$alphahat), error = function(e) NA_integer_)
  diffuse_end <- tryCatch(as.integer(kfs$d), error = function(e) NA_integer_)
  state_labels <- c(level = "local_level")
  mapping <- list(
    outcome = outcome,
    n_time = nrow(d),
    id = prep$id_label,
    time_units = spec$time_units,
    state_dimension = m_state,
    observation_dimension = 1L,
    pool_mode = "none",
    smoother = isTRUE(smoother),
    diffuse_phase_end = diffuse_end,
    forecast_horizon = as.integer(forecast_horizon)
  )

  out <- list(
    kfas_model = fit_ssm$model,
    kfs = kfs,
    fit_ssm = fit_ssm,
    spec = spec,
    state_labels = state_labels,
    mapping = mapping,
    preprocessing = preprocessing,
    schema_version = ILD_KFAS_SCHEMA_VERSION
  )
  class(out) <- c("ild_fit_kfas", "ild_fit_model", "ild_analysis")

  attr(out, "ild_data") <- d
  attr(out, "ild_response") <- outcome
  attr(out, "ild_kfas_schema") <- list(version = ILD_KFAS_SCHEMA_VERSION)
  attr(out, "ild_provenance") <- ild_new_analysis_provenance(
    data,
    "ild_kfas",
    args = list(
      outcome = outcome,
      state_spec = state_spec,
      observation_family = observation_family,
      time_units = spec$time_units,
      irregular_time = irregular_time,
      smoother = smoother,
      kfs_smoothing = sm,
      kfs_filtering = "state",
      forecast_horizon = as.integer(forecast_horizon),
      fit_context = fit_context,
      state_labels = state_labels,
      fit_ssm_dots = dots
    ),
    outputs = list(
      n_obs = nrow(d),
      n_id = 1L,
      logLik = tryCatch(as.numeric(kfs$logLik), error = function(e) NA_real_),
      kfas_version = tryCatch(
        as.character(utils::packageVersion("KFAS")),
        error = function(e) NA_character_
      ),
      diffuse_phase_end = diffuse_end,
      pool_mode = "none"
    )
  )
  out
}
