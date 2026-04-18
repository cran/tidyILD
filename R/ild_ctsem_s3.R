# S3 methods for ild_ctsem backend

#' @rdname ild_tidy
#' @method ild_tidy ild_fit_ctsem
#' @export
ild_tidy.ild_fit_ctsem <- function(x, ...) {
  rlang::check_installed("ctsem", reason = "to use ild_tidy() for ild_ctsem fits")
  pars <- .ild_ctsem_extract_parameters(x$ct_fit)
  if (nrow(pars) == 0L) {
    return(
      ild_tidy_assemble(
        term = "(no_parameters_extracted)",
        estimate = NA_real_,
        std_error = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_,
        p_value = NA_real_,
        statistic = NA_real_,
        interval_type = "Wald",
        engine = "ctsem",
        model_class = "ild_fit_ctsem",
        component = "auxiliary",
        effect_level = "auxiliary",
        optional = NULL
      )
    )
  }
  z <- stats::qnorm(0.975)
  pars$std_error[!is.finite(pars$std_error)] <- NA_real_
  pars$statistic <- ifelse(
    is.finite(pars$estimate) & is.finite(pars$std_error) & pars$std_error > 0,
    pars$estimate / pars$std_error,
    NA_real_
  )
  pars$p_value <- ifelse(
    is.finite(pars$statistic),
    2 * (1 - stats::pnorm(abs(pars$statistic))),
    NA_real_
  )
  pars$conf_low <- ifelse(
    is.finite(pars$estimate) & is.finite(pars$std_error),
    pars$estimate - z * pars$std_error,
    NA_real_
  )
  pars$conf_high <- ifelse(
    is.finite(pars$estimate) & is.finite(pars$std_error),
    pars$estimate + z * pars$std_error,
    NA_real_
  )
  component <- vapply(pars$term, .ild_ctsem_component_from_term, character(1L))
  effect_level <- ifelse(component == "fixed", "population", "auxiliary")
  ild_tidy_assemble(
    term = pars$term,
    estimate = pars$estimate,
    std_error = pars$std_error,
    conf_low = pars$conf_low,
    conf_high = pars$conf_high,
    p_value = pars$p_value,
    statistic = pars$statistic,
    interval_type = rep("Wald", nrow(pars)),
    engine = "ctsem",
    model_class = "ild_fit_ctsem",
    component = component,
    effect_level = effect_level,
    optional = NULL
  )
}

#' @rdname ild_augment
#' @method ild_augment ild_fit_ctsem
#' @export
ild_augment.ild_fit_ctsem <- function(x, ...) {
  rlang::check_installed("ctsem", reason = "to use ild_augment() for ild_ctsem fits")
  data <- attr(x, "ild_data", exact = TRUE)
  if (is.null(data)) {
    stop("ild_augment() for ild_fit_ctsem requires attr(fit, \"ild_data\").", call. = FALSE)
  }
  validate_ild(data)
  resp <- attr(x, "ild_response", exact = TRUE)
  if (is.null(resp) || !resp %in% names(data)) {
    stop("ild_response attribute missing or not present in data; refit with ild_ctsem().", call. = FALSE)
  }
  n <- nrow(data)
  y <- suppressWarnings(as.numeric(data[[resp]]))
  if (length(y) != n) {
    stop("Outcome length does not match ILD rows after coercion; check missing outcome values.", call. = FALSE)
  }
  fit <- .ild_ctsem_extract_fitted(x$ct_fit, n_expected = n)
  if (length(fit) != n || !is.numeric(fit)) {
    fit <- rep(NA_real_, n)
  }
  resid <- rep(NA_real_, n)
  ok <- is.finite(y) & is.finite(fit)
  resid[ok] <- (y - fit)[ok]
  resid_std <- .ild_ctsem_extract_resid_std(x$ct_fit, n_expected = n)

  ild_augment_assemble(
    .ild_id = data[[".ild_id"]],
    .ild_time = data[[".ild_time"]],
    .outcome = y,
    .fitted = fit,
    .resid = resid,
    .resid_std = resid_std,
    engine = "ctsem",
    model_class = "ild_fit_ctsem",
    optional = NULL
  )
}

#' @rdname ild_autoplot
#' @method ild_autoplot ild_fit_ctsem
#' @param type For \code{ild_fit_ctsem}: \code{"fitted_vs_actual"} (default),
#'   \code{"residual_time"}, or \code{"qq"}.
#' @export
ild_autoplot.ild_fit_ctsem <- function(x, type = c("fitted_vs_actual", "residual_time", "qq"), ...) {
  type <- match.arg(type)
  ag <- ild_augment(x)
  if (type == "fitted_vs_actual") {
    return(
      ggplot2::ggplot(ag, ggplot2::aes(x = .data$.outcome, y = .data$.fitted)) +
        ggplot2::geom_point(alpha = 0.5) +
        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
        ggplot2::labs(title = "ctsem fitted vs observed", x = "Observed", y = "Fitted") +
        ggplot2::theme_minimal()
    )
  }
  if (type == "residual_time") {
    return(
      ggplot2::ggplot(ag, ggplot2::aes(x = .data$.ild_time, y = .data$.resid)) +
        ggplot2::geom_point(alpha = 0.5) +
        ggplot2::geom_hline(yintercept = 0, linetype = 2) +
        ggplot2::labs(title = "ctsem residuals over time", x = ".ild_time", y = "Residual") +
        ggplot2::theme_minimal()
    )
  }
  ggplot2::ggplot(ag, ggplot2::aes(sample = .data$.resid_std)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line() +
    ggplot2::labs(title = "ctsem standardized residual QQ plot", x = "Theoretical", y = "Sample") +
    ggplot2::theme_minimal()
}

#' @keywords internal
#' @noRd
.ild_ctsem_component_from_term <- function(term) {
  u <- toupper(as.character(term)[1L])
  if (grepl("DRIFT|CINT|T0MEAN|LAMBDA|BETA", u)) return("fixed")
  if (grepl("DIFF|MANIFESTVAR|T0VAR|COV|VAR", u)) return("auxiliary")
  "auxiliary"
}

#' @keywords internal
#' @noRd
.ild_ctsem_extract_parameters <- function(fit) {
  # Prefer coef() for stable names, then attempt SE extraction from summary.
  est <- tryCatch(stats::coef(fit), error = function(e) NULL)
  if (is.null(est)) {
    est <- tryCatch(fit$coefficients, error = function(e) NULL)
  }
  est <- suppressWarnings(as.numeric(unlist(est)))
  nm <- names(unlist(tryCatch(stats::coef(fit), error = function(e) fit$coefficients)))
  if (is.null(nm) || length(nm) != length(est)) {
    nm <- paste0("par_", seq_along(est))
  }
  if (!length(est)) {
    return(tibble::tibble(term = character(), estimate = numeric(), std_error = numeric()))
  }
  se_map <- .ild_ctsem_extract_se_map(fit)
  se <- unname(se_map[nm])
  se[is.na(se)] <- NA_real_
  tibble::tibble(term = nm, estimate = est, std_error = as.numeric(se))
}

#' @keywords internal
#' @noRd
.ild_ctsem_extract_se_map <- function(fit) {
  out <- numeric()
  sm <- tryCatch(summary(fit), error = function(e) NULL)
  if (is.null(sm)) return(out)
  mats <- list()
  if (is.matrix(sm) || is.data.frame(sm)) mats <- c(mats, list(sm))
  if (is.list(sm)) {
    for (nm in names(sm)) {
      obj <- sm[[nm]]
      if (is.matrix(obj) || is.data.frame(obj)) mats <- c(mats, list(obj))
    }
  }
  for (m in mats) {
    mm <- as.data.frame(m)
    cn <- tolower(names(mm))
    i_est <- match(TRUE, cn %in% c("estimate", "est"), nomatch = 0L)
    i_se <- match(TRUE, cn %in% c("std.error", "std.error.", "se", "std.error.est"), nomatch = 0L)
    if (i_est > 0L && i_se > 0L) {
      rn <- rownames(mm)
      if (!is.null(rn) && length(rn) == nrow(mm)) {
        tmp <- suppressWarnings(as.numeric(mm[[i_se]]))
        names(tmp) <- rn
        out <- tmp
        break
      }
    }
  }
  out
}

#' @keywords internal
#' @noRd
.ild_ctsem_extract_fitted <- function(fit, n_expected) {
  out <- rep(NA_real_, n_expected)
  f1 <- tryCatch(stats::fitted(fit), error = function(e) NULL)
  if (is.matrix(f1) || is.data.frame(f1)) {
    v <- tryCatch(suppressWarnings(as.numeric(f1[, 1L])), error = function(e) NULL)
    if (!is.null(v) && length(v) == n_expected) out <- v
  } else if (is.numeric(f1) && length(f1) == n_expected) {
    out <- suppressWarnings(as.numeric(f1))
  }
  if (all(!is.finite(out))) {
    # ctPredict is not always exported from ctsem; avoid ctsem::ctPredict (R CMD check).
    # Do not use return() inside tryCatch expr: it exits .ild_ctsem_extract_fitted() with NULL
    # instead of the intended length-n_expected vector (breaks ild_augment / tibble).
    pred <- tryCatch(
      {
        pred_raw <- NULL
        if (requireNamespace("ctsem", quietly = TRUE)) {
          fn <- get0("ctPredict", envir = asNamespace("ctsem"), inherits = FALSE, ifnotfound = NULL)
          if (is.function(fn)) {
            pred_raw <- fn(fit)
          }
        }
        pred_raw
      },
      error = function(e) NULL
    )
    if (!is.null(pred)) {
      # ctPredict shape varies by ctsem version; coercion can error (e.g. list-column y).
      v <- tryCatch(
        {
          vv <- NULL
          if (is.data.frame(pred) && "y" %in% names(pred)) {
            vv <- suppressWarnings(as.numeric(unlist(pred$y, recursive = FALSE, use.names = FALSE)))
          } else if (is.list(pred) && !is.null(pred$yhat)) {
            vv <- suppressWarnings(as.numeric(unlist(pred$yhat, recursive = FALSE, use.names = FALSE)))
          }
          if (!is.null(vv) && length(vv) == n_expected) vv else NULL
        },
        error = function(e) NULL
      )
      if (!is.null(v)) out <- v
    }
  }
  out
}

#' @keywords internal
#' @noRd
.ild_ctsem_extract_resid_std <- function(fit, n_expected) {
  out <- rep(NA_real_, n_expected)
  r <- tryCatch(stats::residuals(fit, type = "pearson"), error = function(e) NULL)
  if (is.matrix(r) || is.data.frame(r)) r <- r[, 1L]
  r <- suppressWarnings(as.numeric(r))
  if (is.numeric(r) && length(r) == n_expected) return(r)
  out
}
