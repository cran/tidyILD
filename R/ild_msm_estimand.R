# MSM estimand-first API (v1.1 scaffold)

#' Define an MSM estimand specification
#'
#' Creates a lightweight estimand object used by [ild_msm_fit()].
#' v1.1 preserves backward compatibility with v1 static ATE calls while adding
#' explicit slots for regime specification, time targeting, and contrasts.
#'
#' @param type Estimand type. Supports \code{"ate"} and placeholder \code{"att"}.
#' @param regime Regime class. Supports \code{"static"} and scaffold \code{"dynamic"}.
#' @param treatment Binary treatment column name.
#' @param time_var Occasion index column (default \code{".ild_seq"}).
#' @param contrast Optional contrast definition. Character labels are accepted.
#'   Lists may include \code{label}, \code{treated}, and \code{control}.
#' @param target_time Optional target occasion:
#'   \code{"all"} (default), \code{"final"}, or numeric vector.
#' @param regime_value For \code{regime = "static"}, assignment target (default \code{1}).
#' @param dynamic_rule For \code{regime = "dynamic"}, a deterministic rule:
#'   function taking \code{data} and returning binary assignment, or one of
#'   \code{"always_treat"}, \code{"never_treat"}, \code{"as_observed"}.
#' @return Object of class \code{ild_msm_estimand}.
#' @export
#' @examples
#' e <- ild_msm_estimand(treatment = "trt")
#' e
ild_msm_estimand <- function(type = c("ate", "att"),
                             regime = "static",
                             treatment,
                             time_var = ".ild_seq",
                             contrast = NULL,
                             target_time = "all",
                             regime_value = 1,
                             dynamic_rule = NULL) {
  type <- match.arg(type)
  regime_class <- if (is.list(regime) && !is.null(regime$class)) {
    as.character(regime$class)[1L]
  } else {
    match.arg(as.character(regime)[1L], c("static", "dynamic"))
  }
  if (missing(treatment) || !nzchar(as.character(treatment)[1L])) {
    stop("treatment must be provided as a non-empty column name.", call. = FALSE)
  }
  target_time <- .ild_msm_normalize_target_time(target_time)
  regime_spec <- .ild_msm_normalize_regime_spec(
    regime = regime,
    regime_value = regime_value,
    dynamic_rule = dynamic_rule
  )
  contrast_spec <- .ild_msm_normalize_contrast(contrast)
  out <- list(
    type = type,
    regime = regime_class,
    treatment = as.character(treatment)[1L],
    time_var = as.character(time_var)[1L],
    target_time = target_time,
    contrast = contrast_spec,
    regime_spec = regime_spec
  )
  class(out) <- c("ild_msm_estimand", "list")
  out
}

#' @keywords internal
#' @noRd
validate_ild_msm_estimand <- function(x) {
  if (!inherits(x, "ild_msm_estimand")) {
    stop("estimand must be from ild_msm_estimand().", call. = FALSE)
  }
  if (!x$type %in% c("ate", "att")) {
    stop("type must be one of \"ate\" or \"att\".", call. = FALSE)
  }
  if (!x$regime %in% c("static", "dynamic")) {
    stop("regime must be one of \"static\" or \"dynamic\".", call. = FALSE)
  }
  req <- c("treatment", "time_var")
  for (nm in req) {
    if (is.null(x[[nm]]) || !nzchar(as.character(x[[nm]])[1L])) {
      stop("estimand$", nm, " must be non-empty.", call. = FALSE)
    }
  }
  if (!is.null(x$target_time)) {
    if (is.character(x$target_time)) {
      if (!x$target_time %in% c("all", "final")) {
        stop("target_time string must be \"all\" or \"final\".", call. = FALSE)
      }
    } else {
      tt <- as.numeric(x$target_time)
      if (!length(tt) || any(!is.finite(tt))) {
        stop("Numeric target_time must contain finite values.", call. = FALSE)
      }
    }
  }
  if (is.null(x$regime_spec) || !is.list(x$regime_spec) || is.null(x$regime_spec$class)) {
    stop("estimand$regime_spec must be a structured regime specification.", call. = FALSE)
  }
  if (!identical(as.character(x$regime_spec$class)[1L], x$regime)) {
    stop("estimand$regime and estimand$regime_spec$class must agree.", call. = FALSE)
  }
  if (!is.null(x$contrast) && !is.list(x$contrast)) {
    stop("estimand$contrast must be NULL or a contrast list.", call. = FALSE)
  }
  if (is.list(x$contrast)) {
    if (!is.null(x$contrast$treated)) {
      tr <- as.numeric(x$contrast$treated)[1L]
      if (!is.finite(tr) || !tr %in% c(0, 1)) {
        stop("estimand$contrast$treated must be 0 or 1.", call. = FALSE)
      }
    }
    if (!is.null(x$contrast$control)) {
      ct <- as.numeric(x$contrast$control)[1L]
      if (!is.finite(ct) || !ct %in% c(0, 1)) {
        stop("estimand$contrast$control must be 0 or 1.", call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}

#' @export
print.ild_msm_estimand <- function(x, ...) {
  cat("MSM estimand\n")
  cat("  type: ", x$type, "\n", sep = "")
  cat("  regime: ", x$regime, "\n", sep = "")
  cat("  treatment: ", x$treatment, "\n", sep = "")
  cat("  time_var: ", x$time_var, "\n", sep = "")
  if (!is.null(x$target_time)) cat("  target_time: ", paste(x$target_time, collapse = ","), "\n", sep = "")
  if (!is.null(x$contrast) && !is.null(x$contrast$label)) cat("  contrast: ", x$contrast$label, "\n", sep = "")
  invisible(x)
}

#' @keywords internal
#' @noRd
.ild_msm_normalize_target_time <- function(target_time) {
  if (is.null(target_time)) return(NULL)
  if (is.character(target_time)) {
    if (length(target_time) > 1L) {
      target_time <- stats::na.omit(target_time)
      if (length(target_time) == 0L) {
        stop("target_time must be \"all\", \"final\", or numeric vector.", call. = FALSE)
      }
      target_time <- as.character(target_time)[1L]
    }
    val <- tolower(as.character(target_time)[1L])
    if (!val %in% c("all", "final")) {
      stop("target_time must be \"all\", \"final\", or numeric vector.", call. = FALSE)
    }
    return(val)
  }
  tt <- as.numeric(target_time)
  if (!length(tt) || any(!is.finite(tt))) {
    stop("Numeric target_time must contain finite values.", call. = FALSE)
  }
  tt
}

#' @keywords internal
#' @noRd
.ild_msm_normalize_regime_spec <- function(regime, regime_value, dynamic_rule) {
  if (is.list(regime)) {
    cls <- as.character(regime$class)[1L]
    if (!cls %in% c("static", "dynamic")) {
      stop("regime$class must be \"static\" or \"dynamic\".", call. = FALSE)
    }
    if (identical(cls, "static")) {
      rv <- as.numeric(if (is.null(regime$value)) regime_value else regime$value)[1L]
      if (!is.finite(rv) || !rv %in% c(0, 1)) {
        stop("regime$value for static regime must be 0 or 1.", call. = FALSE)
      }
      return(list(class = "static", value = as.integer(rv)))
    }
    rule <- if (is.null(regime$rule)) dynamic_rule else regime$rule
    if (is.null(rule)) {
      stop("regime$rule (or dynamic_rule) must be supplied for dynamic regime.", call. = FALSE)
    }
    if (is.character(rule)) {
      dr <- tolower(as.character(rule)[1L])
      if (!dr %in% c("always_treat", "never_treat", "as_observed")) {
        stop(
          "dynamic rule string must be one of \"always_treat\", \"never_treat\", \"as_observed\".",
          call. = FALSE
        )
      }
      return(list(class = "dynamic", rule = dr, deterministic = TRUE))
    }
    if (!is.function(rule)) {
      stop("dynamic regime rule must be a function or supported character value.", call. = FALSE)
    }
    return(list(class = "dynamic", rule = rule, deterministic = TRUE))
  }
  if (identical(regime, "static")) {
    rv <- as.numeric(regime_value)[1L]
    if (!is.finite(rv) || !rv %in% c(0, 1)) {
      stop("regime_value for static regime must be 0 or 1.", call. = FALSE)
    }
    return(list(class = "static", value = as.integer(rv)))
  }
  if (is.null(dynamic_rule)) {
    stop("dynamic_rule must be supplied when regime = \"dynamic\".", call. = FALSE)
  }
  if (is.character(dynamic_rule)) {
    dr <- tolower(as.character(dynamic_rule)[1L])
    if (!dr %in% c("always_treat", "never_treat", "as_observed")) {
      stop(
        "dynamic_rule string must be one of \"always_treat\", \"never_treat\", \"as_observed\".",
        call. = FALSE
      )
    }
    return(list(class = "dynamic", rule = dr, deterministic = TRUE))
  }
  if (is.function(dynamic_rule)) {
    return(list(class = "dynamic", rule = dynamic_rule, deterministic = TRUE))
  }
  stop("dynamic_rule must be a function or a supported character rule.", call. = FALSE)
}

#' @keywords internal
#' @noRd
.ild_msm_normalize_contrast <- function(contrast) {
  if (is.null(contrast)) {
    return(list(label = "A=1_vs_A=0", treated = 1L, control = 0L))
  }
  if (is.character(contrast)) {
    return(list(label = as.character(contrast)[1L], treated = 1L, control = 0L))
  }
  if (is.list(contrast)) {
    out <- contrast
    if (is.null(out$label)) out$label <- "custom_contrast"
    if (is.null(out$treated)) out$treated <- 1L
    if (is.null(out$control)) out$control <- 0L
    return(out)
  }
  stop("contrast must be NULL, character, or list.", call. = FALSE)
}
