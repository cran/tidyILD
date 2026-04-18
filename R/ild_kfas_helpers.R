# Helpers for KFAS backend: data prep, KFS extraction, warnings

#' Prepare data for KFAS: single-subject ILD, sorted by time
#'
#' Validates ILD, enforces one distinct \code{.ild_id}, orders rows by time sequence.
#'
#' @param data An object that passes [validate_ild()].
#' @param outcome Character; name of outcome column.
#' @keywords internal
#' @noRd
ild_kfas_prepare_ts <- function(data, outcome) {
  validate_ild(data)
  if (!outcome %in% names(data)) {
    stop("Outcome column '", outcome, "' not found in data.", call. = FALSE)
  }
  n_id <- dplyr::n_distinct(data[[".ild_id"]])
  if (n_id > 1L) {
    stop(
      "ild_kfas() v1 fits one subject at a time. Subset `data` to a single `.ild_id`.",
      call. = FALSE
    )
  }
  ord <- order(data[[".ild_seq"]], data[[".ild_time"]], seq_len(nrow(data)))
  d <- data[ord, , drop = FALSE]
  y <- d[[outcome]]
  if (!is.numeric(y)) {
    y <- suppressWarnings(as.numeric(y))
  }
  list(
    data = d,
    y = y,
    outcome = outcome,
    id_label = as.character(d[[".ild_id"]][1L])
  )
}

#' Irregular spacing warning for local models that assume equal intervals
#' @keywords internal
#' @noRd
ild_kfas_warn_irregular <- function(data) {
  sc <- tryCatch(ild_spacing_class(data), error = function(e) NA_character_)
  if (!is.na(sc) && sc == "irregular-ish") {
    warning(
      "Time spacing is irregular-ish; local level on a regular grid is most interpretable. ",
      "Consider alignment or continuous-time extensions (future backends).",
      call. = FALSE
    )
  }
  invisible(sc)
}

#' Extract smoothed / filtered signal and standardized innovations from KFS output
#' @keywords internal
#' @noRd
ild_kfas_extract_augment <- function(kfs, y, use_smoother = TRUE) {
  # Smoothed observation expectation (signal) when available.
  # KFS often omits muhat for diffuse init; for identity observation y_t = Z*alpha_t,
  # the first smoothed state column matches the signal when Z = (1).
  muhat <- kfs$muhat
  ny <- length(as.numeric(y))
  if (!is.null(muhat) && is.matrix(muhat)) {
    fit <- as.numeric(muhat[, 1L])
  } else {
    ah <- kfs$alphahat
    if (!is.null(ah) && is.matrix(ah) && nrow(ah) == ny) {
      fit <- as.numeric(ah[, 1L])
    } else {
      m <- kfs$m
      if (!is.null(m) && is.matrix(m) && nrow(m) == ny) {
        fit <- as.numeric(m[, 1L])
      } else {
        fit <- rep(NA_real_, ny)
      }
    }
  }
  resid <- as.numeric(y) - fit
  rs <- rep(NA_real_, length(y))
  list(fitted = fit, resid = resid, resid_std = rs)
}

#' Scalar extract from KFAS Q / H arrays (time-invariant)
#' @keywords internal
#' @noRd
ild_kfas_qh_scalar <- function(x) {
  d <- dim(x)
  if (is.null(d)) return(NA_real_)
  if (length(d) >= 3L) {
    return(tryCatch(as.numeric(x[1L, 1L, 1L]), error = function(e) NA_real_))
  }
  tryCatch(as.numeric(x[1L, 1L]), error = function(e) NA_real_)
}

#' First latent state (smoothed) for local level — column 1 of alphahat
#' @keywords internal
#' @noRd
ild_kfas_extract_level_state <- function(kfs) {
  ah <- kfs$alphahat
  if (!is.null(ah) && is.matrix(ah)) {
    return(list(mean = as.numeric(ah[, 1L])))
  }
  list(mean = NULL)
}

#' Rich residual / innovation diagnostics for KFS output (Gaussian)
#' @keywords internal
#' @noRd
ild_kfas_residual_diagnostics <- function(kfs, y) {
  rlang::check_installed("KFAS", reason = "KFAS residual diagnostics")
  innov <- tryCatch(as.numeric(stats::rstandard(kfs)), error = function(e) NULL)
  if (is.null(innov) || length(innov) == 0L) {
    # Fallback: scale one-step prediction errors by their variances (Gaussian)
    v <- tryCatch(as.numeric(kfs$v), error = function(e) NULL)
    Fv <- tryCatch(as.numeric(kfs$F), error = function(e) NULL)
    if (!is.null(v) && !is.null(Fv) && length(v) == length(Fv)) {
      innov <- ifelse(is.finite(Fv) & Fv > 0, v / sqrt(Fv), NA_real_)
    }
  }
  ny <- length(as.numeric(y))
  if (is.null(innov) || length(innov) == 0L) {
    return(list(
      standardized_innovations_summary = NULL,
      acf_innovations = NULL,
      qq = NULL,
      outlier_flags = NULL,
      ljung_box = NULL
    ))
  }
  if (length(innov) != ny) {
    m <- min(length(innov), ny)
    innov <- innov[seq_len(m)]
  }
  n <- length(innov)
  ok <- is.finite(innov)
  iv <- innov[ok]
  summ <- list(
    n = length(iv),
    mean = mean(iv, na.rm = TRUE),
    sd = stats::sd(iv, na.rm = TRUE),
    min = suppressWarnings(min(iv, na.rm = TRUE)),
    max = suppressWarnings(max(iv, na.rm = TRUE)),
    mae = mean(abs(iv - mean(iv, na.rm = TRUE)), na.rm = TRUE)
  )
  acf_obj <- tryCatch(
    stats::acf(iv, plot = FALSE, lag.max = min(20L, max(1L, length(iv) - 1L))),
    error = function(e) NULL
  )
  acf_list <- if (!is.null(acf_obj)) {
    la <- acf_obj$lag
    ac <- acf_obj$acf
    if (is.matrix(la)) la <- as.numeric(la[, 1L]) else as.numeric(la)
    if (is.matrix(ac)) ac <- as.numeric(ac[, 1L]) else as.numeric(ac)
    list(lag = la, acf = ac)
  } else {
    NULL
  }
  qq_cor <- tryCatch({
    qn <- stats::qqnorm(iv, plot = FALSE)
    stats::cor(qn$x, qn$y)
  }, error = function(e) NA_real_)
  thr <- 3.5
  outlier_idx <- which(abs(innov) > thr & is.finite(innov))
  lb <- tryCatch(
    stats::Box.test(iv, type = "Ljung-Box", lag = min(5L, max(1L, length(iv) - 1L))),
    error = function(e) NULL
  )
  lb_list <- if (!is.null(lb)) {
    list(statistic = unname(lb$statistic), p_value = unname(lb$p.value), df = lb$parameter)
  } else {
    NULL
  }
  list(
    standardized_innovations_summary = summ,
    acf_innovations = acf_list,
    qq = list(correlation_normal_scores = qq_cor),
    outlier_flags = list(threshold_abs = thr, indices = outlier_idx, n = length(outlier_idx)),
    ljung_box = lb_list
  )
}

#' Count distinct runs of NA in a logical vector
#' @keywords internal
#' @noRd
ild_kfas_na_run_count <- function(is_na) {
  if (length(is_na) == 0L) return(0L)
  r <- rle(is_na)
  as.integer(sum(r$values))
}
