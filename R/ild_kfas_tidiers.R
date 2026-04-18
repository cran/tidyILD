# Tidiers for ild_kfas objects (KFAS backend)

#' @rdname ild_tidy
#' @method ild_tidy ild_fit_kfas
#' @export
ild_tidy.ild_fit_kfas <- function(x, ...) {
  rlang::check_installed("KFAS", reason = "to use ild_tidy() for ild_kfas fits")
  fm <- x$kfas_model
  rows <- list()
  # Observation and state innovation variances (Gaussian local level)
  qv <- ild_kfas_qh_scalar(fm$Q)
  hv <- ild_kfas_qh_scalar(fm$H)
  if (is.finite(qv)) {
    rows[[length(rows) + 1L]] <- list(
      term = "level_variance",
      component = "variance",
      effect_level = "auxiliary",
      estimate = qv,
      std_error = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_,
      p_value = NA_real_,
      statistic = NA_real_,
      interval_type = "none"
    )
  }
  if (is.finite(hv)) {
    rows[[length(rows) + 1L]] <- list(
      term = "observation_variance",
      component = "variance",
      effect_level = "auxiliary",
      estimate = hv,
      std_error = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_,
      p_value = NA_real_,
      statistic = NA_real_,
      interval_type = "none"
    )
  }
  if (length(rows) == 0L) {
    return(
      ild_tidy_assemble(
        term = "(no_variance_extracted)",
        estimate = NA_real_,
        std_error = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_,
        p_value = NA_real_,
        statistic = NA_real_,
        interval_type = "none",
        engine = "KFAS",
        model_class = "ild_fit_kfas",
        component = "auxiliary",
        effect_level = "auxiliary",
        optional = NULL
      )
    )
  }
  n <- length(rows)
  term <- vapply(rows, function(z) z$term, character(1L))
  comp <- vapply(rows, function(z) z$component, character(1L))
  el <- vapply(rows, function(z) z$effect_level, character(1L))
  est <- vapply(rows, function(z) z$estimate, numeric(1L))
  se <- rep(NA_real_, n)
  cl <- rep(NA_real_, n)
  ch <- rep(NA_real_, n)
  pv <- rep(NA_real_, n)
  st <- rep(NA_real_, n)
  int <- vapply(rows, function(z) z$interval_type, character(1L))
  ild_tidy_assemble(
    term = term,
    estimate = est,
    std_error = se,
    conf_low = cl,
    conf_high = ch,
    p_value = pv,
    statistic = st,
    interval_type = int,
    engine = "KFAS",
    model_class = "ild_fit_kfas",
    component = comp,
    effect_level = el,
    optional = NULL
  )
}

#' Tidy per-time state summaries for ild_kfas
#'
#' Returns a tibble with one row per time index and columns for the smoothed
#' level (and optional variance). Separate from [ild_tidy()] which tidies
#' **parameters**.
#'
#' @param x An object from [ild_kfas()].
#' @param ... Reserved.
#' @return A \code{\link[tibble]{tibble}}.
#' @export
ild_tidy_states <- function(x, ...) {
  UseMethod("ild_tidy_states")
}

#' @export
ild_tidy_states.ild_fit_kfas <- function(x, ...) {
  rlang::check_installed("KFAS", reason = "to use ild_tidy_states()")
  kfs <- x$kfs
  ah <- kfs$alphahat
  V <- kfs$V
  n <- if (!is.null(ah) && is.matrix(ah)) nrow(ah) else 0L
  if (n == 0L) {
    return(tibble::tibble(
      time_index = integer(),
      state = character(),
      estimate = numeric(),
      std_error = numeric()
    ))
  }
  lev <- as.numeric(ah[, 1L])
  se <- rep(NA_real_, n)
  if (!is.null(V) && length(dim(V)) >= 2L) {
    # diagonal of state covariance per time (approximate)
    for (i in seq_len(n)) {
      se[i] <- tryCatch(sqrt(V[1L, 1L, i]), error = function(e) NA_real_)
    }
  }
  tibble::tibble(
    time_index = seq_len(n),
    state = rep("level", n),
    estimate = lev,
    std_error = se
  )
}
