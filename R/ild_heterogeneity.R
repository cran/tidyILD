# Heterogeneity interpretation for mixed models (partial pooling, BLUPs, summaries)
# See vignette("heterogeneity-interpretation", package = "tidyILD").

#' Heterogeneity and person-specific effects from mixed models
#'
#' Summarizes **conditional modes** (empirical Bayes / BLUP deviations) and
#' **person-specific coefficients** (\code{fixef + ranef}) for hierarchical models
#' fit with [ild_lme()] or [ild_brms()]. Outputs are labeled as **partial-pooling**
#' estimates; they are not the same as no-pooling separate regressions (see
#' [ild_person_model()]) or population-average fixed effects alone.
#'
#' @param fit A fitted \code{lmerMod}, \code{lme}, or \code{brmsfit} (from [ild_brms()]).
#' @param term Character vector of random-effect row names to restrict (e.g. \code{"x"} for slope).
#'   Default \code{NULL} includes all random-effect terms except optional filters.
#' @param group Name of the grouping factor in \code{ranef(fit)} to use (default first).
#' @param threshold Optional numeric; with \code{scale}, proportion of person-specific
#'   **total** coefficients exceeding this threshold is reported in \code{summary}.
#' @param scale If \code{threshold} is set: \code{"raw"} (default), \code{"sd_x"} (multiply
#'   threshold by residual SD of a focal predictor from model frame), or \code{"sd_y"}
#'   (multiply by SD of response). Only used when a single numeric predictor is found
#'   for \code{sd_x} / \code{sd_y} heuristics.
#' @param conf_level For \code{lmerMod}, Wald interval level for conditional modes (normal approx.).
#' @param n_draws For \code{brmsfit}, number of posterior draws for summaries (capped).
#' @param ... Reserved.
#'
#' @return An object of class \code{ild_heterogeneity}: a list with \code{meta}, \code{random_effects}
#'   (long tibble), \code{varcorr} (tibble), \code{summary} (per-term metrics), and \code{fit}.
#' @export
#' @seealso [ild_tidy.ild_heterogeneity()], [ild_autoplot.ild_heterogeneity()],
#'   [ild_heterogeneity_stratified()], [ild_person_model()]
ild_heterogeneity <- function(fit, ...) {
  UseMethod("ild_heterogeneity")
}

#' @export
ild_heterogeneity.default <- function(fit, ...) {
  stop(
    "No ild_heterogeneity method for ",
    paste(class(fit), collapse = ", "),
    call. = FALSE
  )
}

#' @rdname ild_heterogeneity
#' @export
ild_heterogeneity.lmerMod <- function(fit,
                                      term = NULL,
                                      group = NULL,
                                      threshold = NULL,
                                      scale = c("raw", "sd_x", "sd_y"),
                                      conf_level = 0.95,
                                      ...) {
  scale <- match.arg(scale)
  nr <- tryCatch(lme4::getME(fit, "n_rtrms"), error = function(e) 0L)
  if (is.null(nr) || nr < 1L) {
    stop("Model has no random effects; nothing to summarize for ild_heterogeneity().", call. = FALSE)
  }
  re <- tryCatch(lme4::ranef(fit, condVar = TRUE), error = function(e) NULL)
  co <- tryCatch(stats::coef(fit), error = function(e) NULL)
  if (is.null(re) || is.null(co)) {
    stop("Could not extract ranef/coef from lmer fit.", call. = FALSE)
  }
  gn <- names(re)
  if (is.null(gn) || length(gn) == 0L) {
    stop("Empty random-effects structure.", call. = FALSE)
  }
  if (!is.null(group)) {
    if (!group %in% gn) {
      stop("group '", group, "' not found in ranef names: ", paste(gn, collapse = ", "), call. = FALSE)
    }
    gn_use <- group
  } else {
    gn_use <- gn[1L]
  }

  mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  sd_y <- NA_real_
  sd_x_map <- list()
  if (!is.null(mf)) {
    resp <- tryCatch(stats::model.response(mf), error = function(e) NULL)
    if (!is.null(resp) && is.numeric(resp)) {
      sd_y <- stats::sd(resp, na.rm = TRUE)
    }
    for (nm in names(mf)) {
      v <- mf[[nm]]
      if (is.numeric(v)) {
        sd_x_map[[nm]] <- stats::sd(v, na.rm = TRUE)
      }
    }
  }

  rows <- list()
  for (g in gn_use) {
    R <- re[[g]]
    C <- co[[g]]
    if (is.null(R) || is.null(C)) next
    levs <- rownames(R)
    cn <- colnames(R)
    if (is.null(cn)) next
    pv <- attr(R, "postVar")
    z <- abs(stats::qnorm((1 - conf_level) / 2))
    for (ti in cn) {
      if (!is.null(term) && length(term) > 0L && !ti %in% term) next
      for (i in seq_along(levs)) {
        r_est <- as.numeric(R[levs[i], ti, drop = TRUE])
        t_est <- as.numeric(C[levs[i], ti, drop = TRUE])
        se <- NA_real_
        if (!is.null(pv) && length(dim(pv)) == 3L) {
          j <- match(ti, cn)
          if (!is.na(j) && j >= 1L && j <= dim(pv)[1L] && i <= dim(pv)[3L]) {
            vv <- pv[j, j, i]
            if (is.finite(vv) && vv >= 0) {
              se <- sqrt(vv)
            }
          }
        }
        lo <- if (is.finite(se)) t_est - z * se else NA_real_
        hi <- if (is.finite(se)) t_est + z * se else NA_real_
        rows[[length(rows) + 1L]] <- tibble::tibble(
          group = g,
          level_id = levs[i],
          term = ti,
          estimate_ranef = r_est,
          estimate_total = t_est,
          std_error = se,
          conf_low = lo,
          conf_high = hi,
          estimand = "conditional_mode_total"
        )
      }
    }
  }
  if (length(rows) == 0L) {
    stop("No random-effect rows extracted (check term filter).", call. = FALSE)
  }
  tbl <- dplyr::bind_rows(rows)

  vc_df <- tryCatch(
    as.data.frame(lme4::VarCorr(fit)),
    error = function(e) NULL
  )
  vc_tbl <- if (!is.null(vc_df)) {
    tibble::as_tibble(vc_df)
  } else {
    tibble::tibble()
  }

  thr_resolved <- .ild_heterogeneity_resolve_threshold(
    threshold, scale, tbl$term, sd_x_map, sd_y
  )

  summ <- .ild_heterogeneity_summary_table(tbl, vc_tbl, thr_resolved)
  meta <- list(
    engine = "lmer",
    class = "lmerMod",
    conf_level = conf_level,
    threshold = thr_resolved$threshold,
    scale = thr_resolved$scale_label,
    note = paste(
      "estimate_ranef: deviation from population; estimate_total: fixef + ranef (partial pooling).",
      "Intervals are conditional (empirical Bayes), not population CIs."
    )
  )
  structure(
    list(
      meta = meta,
      random_effects = tbl,
      varcorr = vc_tbl,
      summary = summ,
      fit = fit
    ),
    class = c("ild_heterogeneity", "list")
  )
}

#' @rdname ild_heterogeneity
#' @export
ild_heterogeneity.lme <- function(fit,
                                  term = NULL,
                                  group = NULL,
                                  threshold = NULL,
                                  scale = c("raw", "sd_x", "sd_y"),
                                  conf_level = 0.95,
                                  ...) {
  scale <- match.arg(scale)
  if (!inherits(fit, "lme")) {
    stop("Expected nlme::lme fit.", call. = FALSE)
  }
  re <- tryCatch(nlme::ranef(fit), error = function(e) NULL)
  co <- tryCatch(stats::coef(fit), error = function(e) NULL)
  if (is.null(re) || is.null(co)) {
    stop("Could not extract ranef/coef from lme fit.", call. = FALSE)
  }
  mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  sd_y <- NA_real_
  sd_x_map <- list()
  if (!is.null(mf)) {
    resp <- tryCatch(stats::model.response(mf), error = function(e) NULL)
    if (!is.null(resp) && is.numeric(resp)) {
      sd_y <- stats::sd(resp, na.rm = TRUE)
    }
    for (nm in names(mf)) {
      v <- mf[[nm]]
      if (is.numeric(v)) {
        sd_x_map[[nm]] <- stats::sd(v, na.rm = TRUE)
      }
    }
  }

  rows <- list()
  if (is.data.frame(re) && is.data.frame(co)) {
    if (!is.null(term) && length(term) > 0L) {
      cn <- intersect(term, colnames(re))
      if (length(cn) == 0L) {
        stop("No matching term columns in ranef() for lme.", call. = FALSE)
      }
    } else {
      cn <- colnames(re)
    }
    levs <- rownames(re)
    if (is.null(levs)) {
      levs <- as.character(seq_len(nrow(re)))
    }
    grp <- if (!is.null(group)) group else "id"
    for (ti in cn) {
      for (i in seq_along(levs)) {
        r_est <- as.numeric(re[levs[i], ti, drop = TRUE])
        t_est <- as.numeric(co[levs[i], ti, drop = TRUE])
        rows[[length(rows) + 1L]] <- tibble::tibble(
          group = grp,
          level_id = levs[i],
          term = ti,
          estimate_ranef = r_est,
          estimate_total = t_est,
          std_error = NA_real_,
          conf_low = NA_real_,
          conf_high = NA_real_,
          estimand = "conditional_mode_total"
        )
      }
    }
  } else if (is.list(co) && !is.data.frame(co)) {
    for (nm in names(co)) {
      if (!is.null(group) && !identical(nm, group)) next
      cm <- co[[nm]]
      if (!is.matrix(cm) && !is.data.frame(cm)) next
      cn <- colnames(cm)
      if (is.null(cn)) next
      levs <- rownames(cm)
      if (is.null(levs)) levs <- as.character(seq_len(nrow(cm)))
      for (ti in cn) {
        if (!is.null(term) && length(term) > 0L && !ti %in% term) next
        for (i in seq_along(levs)) {
          t_est <- as.numeric(cm[levs[i], ti, drop = TRUE])
          r_est <- NA_real_
          if (is.data.frame(re) || is.matrix(re)) {
            if (ti %in% colnames(re) && levs[i] %in% rownames(re)) {
              r_est <- as.numeric(re[levs[i], ti, drop = TRUE])
            }
          }
          rows[[length(rows) + 1L]] <- tibble::tibble(
            group = nm,
            level_id = levs[i],
            term = ti,
            estimate_ranef = r_est,
            estimate_total = t_est,
            std_error = NA_real_,
            conf_low = NA_real_,
            conf_high = NA_real_,
            estimand = "conditional_mode_total"
          )
        }
      }
    }
  }
  if (length(rows) == 0L) {
    stop("Could not reshape lme random effects into long format.", call. = FALSE)
  }
  tbl <- dplyr::bind_rows(rows)
  vc_df <- tryCatch(nlme::VarCorr(fit), error = function(e) NULL)
  vc_tbl <- if (!is.null(vc_df)) {
    tibble::tibble(note = "See nlme::VarCorr(fit) raw table", raw = list(vc_df))
  } else {
    tibble::tibble()
  }
  thr_resolved <- .ild_heterogeneity_resolve_threshold(
    threshold, scale, tbl$term, sd_x_map, sd_y
  )
  summ <- .ild_heterogeneity_summary_table(tbl, tibble::tibble(), thr_resolved)
  meta <- list(
    engine = "lme",
    class = "lme",
    conf_level = conf_level,
    threshold = thr_resolved$threshold,
    scale = thr_resolved$scale_label,
    note = paste(
      "nlme: per-level std_error/confidence for random effects not computed here;",
      "use lme4 for conditional variances or Bayesian brms for posterior intervals."
    )
  )
  structure(
    list(
      meta = meta,
      random_effects = tbl,
      varcorr = vc_tbl,
      summary = summ,
      fit = fit
    ),
    class = c("ild_heterogeneity", "list")
  )
}

#' @keywords internal
#' @noRd
.ild_heterogeneity_resolve_threshold <- function(threshold, scale, term_vec, sd_x_map, sd_y) {
  scale_label <- scale
  if (is.null(threshold)) {
    return(list(threshold = NULL, scale_label = NA_character_, effective = NULL))
  }
  th <- as.numeric(threshold)[1L]
  if (!is.finite(th)) {
    return(list(threshold = NULL, scale_label = NA_character_, effective = NULL))
  }
  eff <- th
  if (identical(scale, "sd_y") && is.finite(sd_y) && sd_y > 0) {
    eff <- th * sd_y
    scale_label <- sprintf("sd_y (x %.4g)", sd_y)
  } else if (identical(scale, "sd_x")) {
    terms_u <- unique(as.character(term_vec))
    terms_u <- terms_u[!terms_u %in% c("(Intercept)", "Intercept")]
    sdx <- NA_real_
    for (t in terms_u) {
      if (!is.null(sd_x_map[[t]])) {
        sdx <- sd_x_map[[t]]
        break
      }
    }
    if (is.finite(sdx) && sdx > 0) {
      eff <- th * sdx
      scale_label <- sprintf("sd_x (x %.4g)", sdx)
    } else {
      scale_label <- "raw (sd_x unavailable)"
    }
  }
  list(threshold = th, scale_label = scale_label, effective = eff)
}

#' @keywords internal
#' @noRd
.ild_heterogeneity_summary_table <- function(tbl, vc_tbl, thr) {
  eff_thr <- thr$effective
  summ <- tbl |>
    dplyr::group_by(.data$group, .data$term) |>
    dplyr::summarise(
      n_levels = dplyr::n(),
      mean_total = mean(.data$estimate_total, na.rm = TRUE),
      sd_total = stats::sd(.data$estimate_total, na.rm = TRUE),
      prop_gt_zero = mean(.data$estimate_total > 0, na.rm = TRUE),
      q25 = stats::quantile(.data$estimate_total, 0.25, na.rm = TRUE, names = FALSE, type = 7),
      q50 = stats::median(.data$estimate_total, na.rm = TRUE),
      q75 = stats::quantile(.data$estimate_total, 0.75, na.rm = TRUE, names = FALSE, type = 7),
      prop_gt_threshold = if (is.null(eff_thr) || !is.finite(eff_thr)) {
        NA_real_
      } else {
        mean(.data$estimate_total > eff_thr, na.rm = TRUE)
      },
      .groups = "drop"
    )
  if (nrow(vc_tbl) > 0L && all(c("grp", "var1", "sdcor") %in% names(vc_tbl))) {
    vc2 <- vc_tbl
    if ("var2" %in% names(vc2)) {
      vc2 <- vc2 |>
        dplyr::filter(is.na(.data$var2) | as.character(.data$var2) == "")
    }
    vc2 <- vc2 |>
      dplyr::select("grp", "var1", "sdcor") |>
      dplyr::rename(varcorr_sdcor = "sdcor", group = "grp", term = "var1")
    summ <- dplyr::left_join(summ, vc2, by = c("group", "term"))
  } else {
    summ$varcorr_sdcor <- NA_real_
  }
  summ
}

#' @rdname ild_heterogeneity
#' @param formula,data Passed to [ild_lme()] within each subgroup.
#' @param subgroup Character name of a column in \code{data} defining subgroups.
#' @param min_n_id Minimum number of distinct persons required in a subgroup.
#' @param ... Passed to [ild_lme()].
#'
#' @return A tibble with one row per successful subgroup fit, including key columns from
#'   \code{ild_heterogeneity()$summary} flattened (\code{subgroup_level}, \code{term}, etc.).
#'   This is a **descriptive** comparison across refits, not a formal test of variance heterogeneity.
#' @export
ild_heterogeneity_stratified <- function(formula,
                                         data,
                                         subgroup,
                                         min_n_id = 5L,
                                         ...) {
  validate_ild(data)
  if (!subgroup %in% names(data)) {
    stop("subgroup column '", subgroup, "' not found in data.", call. = FALSE)
  }
  min_n_id <- as.integer(min_n_id)[1L]
  levs <- unique(data[[subgroup]])
  out_rows <- list()
  for (lev in levs) {
    sub <- data[data[[subgroup]] == lev, , drop = FALSE]
    n_id <- length(unique(sub[[".ild_id"]]))
    if (n_id < min_n_id) {
      next
    }
    fit <- tryCatch(
      ild_lme(formula, data = sub, ...),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      next
    }
    het <- tryCatch(ild_heterogeneity(fit), error = function(e) NULL)
    if (is.null(het)) {
      next
    }
    s <- het$summary
    s$subgroup_level <- lev
    s$n_id_subgroup <- n_id
    out_rows[[length(out_rows) + 1L]] <- s
  }
  if (length(out_rows) == 0L) {
    return(tibble::tibble(
      subgroup_level = character(),
      term = character(),
      note = character()
    ))
  }
  dplyr::bind_rows(out_rows)
}

#' @describeIn ild_heterogeneity Tidy long table of person-level random effects (for \code{ild_heterogeneity} objects).
#' @param x An \code{ild_heterogeneity} object.
#' @export
ild_tidy.ild_heterogeneity <- function(x, ...) {
  if (!inherits(x, "ild_heterogeneity")) {
    stop("Expected ild_heterogeneity object.", call. = FALSE)
  }
  x$random_effects
}

#' @describeIn ild_heterogeneity Caterpillar or histogram of person-specific effects.
#' @param type \code{"caterpillar"} (default) or \code{"histogram"}.
#' @param term Which random-effect term to plot (required if multiple).
#' @export
ild_autoplot.ild_heterogeneity <- function(x, type = c("caterpillar", "histogram"), term = NULL, ...) {
  if (!inherits(x, "ild_heterogeneity")) {
    stop("Expected ild_heterogeneity object.", call. = FALSE)
  }
  type <- match.arg(type)
  d <- x$random_effects
  terms_u <- unique(d$term)
  if (is.null(term)) {
    term <- if (length(terms_u) == 1L) {
      terms_u[1L]
    } else {
      non_int <- terms_u[!terms_u %in% c("(Intercept)", "Intercept")]
      if (length(non_int) > 0L) {
        non_int[1L]
      } else {
        terms_u[1L]
      }
    }
  }
  if (!term %in% terms_u) {
    stop("term '", term, "' not found; available: ", paste(terms_u, collapse = ", "), call. = FALSE)
  }
  sub <- d[d$term == term, , drop = FALSE]
  sub <- sub[order(sub$estimate_total), , drop = FALSE]
  sub$level_ord <- factor(sub$level_id, levels = sub$level_id)
  if (type == "caterpillar") {
    ggplot2::ggplot(sub, ggplot2::aes(x = .data$level_ord, y = .data$estimate_total)) +
      ggplot2::geom_point() +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = .data$conf_low,
          ymax = .data$conf_high
        ),
        width = 0,
        na.rm = TRUE
      ) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2, alpha = 0.6) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = NULL,
        y = "Person-specific coefficient (partial pooling)",
        title = paste0("Caterpillar: ", term)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 6))
  } else {
    ggplot2::ggplot(sub, ggplot2::aes(x = .data$estimate_total)) +
      ggplot2::geom_histogram(bins = min(30L, max(5L, nrow(sub) %/% 2)), fill = "gray70", color = "gray40") +
      ggplot2::geom_vline(xintercept = 0, linetype = 2, alpha = 0.6) +
      ggplot2::labs(
        x = "Person-specific coefficient",
        y = "Count",
        title = paste0("Distribution: ", term, " (partial pooling)")
      ) +
      ggplot2::theme_minimal()
  }
}

#' @export
print.ild_heterogeneity <- function(x, ...) {
  cat("ild_heterogeneity:", x$meta$engine, "\n")
  cat(x$meta$note, "\n\n")
  print(x$summary, n = min(20L, nrow(x$summary)))
  invisible(x)
}

#' @rdname ild_heterogeneity
#' @export
ild_heterogeneity.brmsfit <- function(fit,
                                      term = NULL,
                                      group = NULL,
                                      threshold = NULL,
                                      scale = c("raw", "sd_x", "sd_y"),
                                      n_draws = 500L,
                                      ...) {
  rlang::check_installed("brms")
  if (is.null(attr(fit, "ild_data", exact = TRUE))) {
    stop(
      "ild_heterogeneity() for brmsfit requires ild_brms() (ild_data attribute missing).",
      call. = FALSE
    )
  }
  scale <- match.arg(scale)
  n_draws <- as.integer(n_draws)[1L]
  co <- tryCatch(stats::coef(fit, summary = TRUE), error = function(e) NULL)
  if (is.null(co)) {
    stop("Could not extract coef(fit, summary = TRUE) from brms fit.", call. = FALSE)
  }
  gn <- names(co)
  if (is.null(gn) || length(gn) == 0L) {
    stop("Empty coef() list from brms fit.", call. = FALSE)
  }
  if (!is.null(group)) {
    if (!group %in% gn) {
      stop("group '", group, "' not in coef() names: ", paste(gn, collapse = ", "), call. = FALSE)
    }
    gn_use <- group
  } else {
    gn_use <- gn[1L]
  }
  data <- attr(fit, "ild_data", exact = TRUE)
  mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  sd_y <- NA_real_
  sd_x_map <- list()
  if (!is.null(mf)) {
    resp <- tryCatch(stats::model.response(mf), error = function(e) NULL)
    if (!is.null(resp) && is.numeric(resp)) {
      sd_y <- stats::sd(resp, na.rm = TRUE)
    }
    for (nm in names(mf)) {
      v <- mf[[nm]]
      if (is.numeric(v)) {
        sd_x_map[[nm]] <- stats::sd(v, na.rm = TRUE)
      }
    }
  }
  rows <- list()
  for (g in gn_use) {
    arr <- co[[g]]
    if (is.null(arr)) next
    d <- dim(arr)
    # brms::coef(summary=TRUE): dim = c(n_level, n_stat, n_coef) (see dimnames order)
    dn2 <- dimnames(arr)[[2L]]
    dn3 <- dimnames(arr)[[3L]]
    if (length(d) == 3L && !is.null(dn2) && !is.null(dn3)) {
      i_est <- match("Estimate", dn2)
      i_se <- match("Est.Error", dn2)
      if (is.na(i_se)) {
        w <- grep("Est.Error", dn2, fixed = TRUE)
        i_se <- if (length(w)) w[1L] else NA_integer_
      }
      i_lo <- match("Q2.5", dn2)
      if (is.na(i_lo)) {
        w <- grep("2.5%", dn2, fixed = TRUE)
        if (length(w) == 0L) w <- grep("Q2.5", dn2, fixed = TRUE)
        i_lo <- if (length(w)) w[1L] else NA_integer_
      }
      i_hi <- match("Q97.5", dn2)
      if (is.na(i_hi)) {
        w <- grep("97.5%", dn2, fixed = TRUE)
        if (length(w) == 0L) w <- grep("Q97.5", dn2, fixed = TRUE)
        i_hi <- if (length(w)) w[1L] else NA_integer_
      }
      if (is.na(i_est)) next
      levs <- dimnames(arr)[[1L]]
      tns <- dn3
      if (is.null(levs) || is.null(tns)) next
      for (ti in seq_along(tns)) {
        tnm <- tns[ti]
        if (!is.null(term) && length(term) > 0L && !tnm %in% term) next
        for (i in seq_along(levs)) {
          t_est <- as.numeric(arr[i, i_est, ti])
          se <- if (!is.na(i_se)) as.numeric(arr[i, i_se, ti]) else NA_real_
          lo <- if (!is.na(i_lo)) as.numeric(arr[i, i_lo, ti]) else NA_real_
          hi <- if (!is.na(i_hi)) as.numeric(arr[i, i_hi, ti]) else NA_real_
          rows[[length(rows) + 1L]] <- tibble::tibble(
            group = g,
            level_id = levs[i],
            term = tnm,
            estimate_ranef = NA_real_,
            estimate_total = t_est,
            std_error = se,
            conf_low = lo,
            conf_high = hi,
            estimand = "posterior_summary_total"
          )
        }
      }
    }
  }
  if (length(rows) == 0L) {
    stop("Could not parse brms coef(summary) arrays for heterogeneity.", call. = FALSE)
  }
  tbl <- dplyr::bind_rows(rows)
  thr_resolved <- .ild_heterogeneity_resolve_threshold(
    threshold, scale, tbl$term, sd_x_map, sd_y
  )
  summ <- .ild_heterogeneity_summary_table(tbl, tibble::tibble(), thr_resolved)
  meta <- list(
    engine = "brms",
    class = "brmsfit",
    conf_level = NA_real_,
    threshold = thr_resolved$threshold,
    scale = thr_resolved$scale_label,
    n_draws = n_draws,
    note = paste(
      "Person-specific coefficients from brms::coef(summary=TRUE) (posterior means/median-aligned summaries).",
      "Not equivalent to lme4 BLUPs; intervals are posterior quantiles."
    )
  )
  structure(
    list(
      meta = meta,
      random_effects = tbl,
      varcorr = tibble::tibble(),
      summary = summ,
      fit = fit
    ),
    class = c("ild_heterogeneity", "list")
  )
}

#' @keywords internal
#' @noRd
fill_diagnostics_heterogeneity <- function(fit) {
  out <- list(available = FALSE, reason = NA_character_, object = NULL)
  if (!inherits(fit, c("lmerMod", "lme", "brmsfit"))) {
    out$reason <- "unsupported fit class"
    return(out)
  }
  h <- tryCatch(
    ild_heterogeneity(fit),
    error = function(e) {
      out$reason <<- conditionMessage(e)
      NULL
    }
  )
  if (!is.null(h)) {
    out$available <- TRUE
    out$object <- h
    out$reason <- NA_character_
  }
  out
}
