# MSM / IPW inference: cluster bootstrap for weighted lmer fits

#' @title MSM and IPW inference for weighted mixed models
#' @name ild_msm_inference
#' @description
#' After [ild_ipw_refit()] (or any \code{lmer(..., weights =)} with \code{attr(fit, "ild_data")}),
#' uncertainty for MSM-style **estimands** should not rely on default \code{lmer} standard errors.
#' tidyILD supports:
#' \itemize{
#'   \item \strong{Cluster bootstrap} — [ild_msm_bootstrap()] resamples persons (clusters),
#'     refits the weighted model, and collects fixed effects; use
#'     \code{weight_policy = "reestimate_weights"} and a \code{weights_fn} to re-fit propensity
#'     weights on each replicate when that matches your estimand (computationally heavier;
#'     preferred for many MSM workflows). \code{weight_policy = "fixed_weights"} keeps the
#'     weights from the resampled rows (fast approximation; ignores first-stage uncertainty).
#'   \item \strong{Cluster-robust sandwich Wald} — [ild_robust_se()] / [tidy_ild_model(..., se = "robust")]
#'     apply \pkg{clubSandwich} at the **person** level on the **weighted outcome model only**; they
#'     do **not** account for estimation error in IPW/IPCW weights unless you treat weights as fixed.
#'   \item \strong{Bayesian} — Full joint Bayesian MSM is not built in. See [ild_brms()] for
#'     hierarchical outcome models; combining external IPW row weights with Stan is delicate;
#'     prefer explicit joint/sensitivity designs and specialized causal packages for Bayes MSM.
#' }
#' @seealso [ild_msm_bootstrap()], [tidy_ild_msm_bootstrap()], [ild_ipw_refit()],
#'   [ild_robust_se()], [tidy_ild_model()], [ild_brms()], [ild_msm_estimand()],
#'   [ild_msm_fit()]
#' @keywords internal
NULL

#' Cluster bootstrap inference for weighted \code{lmer} (MSM / IPW sensitivity)
#'
#' Resamples **clusters** (persons) with replacement, subsets \code{attr(fit, "ild_data")}
#' (or \code{data}), optionally re-estimates weights, and refits \code{\link[lme4]{lmer}} with
#' the same formula. Collects fixed effects across replicates for bootstrap standard errors and
#' percentile confidence intervals.
#'
#' @param fit A \code{lmerMod} from [ild_ipw_refit()] or [ild_lme()] with \code{attr(fit, "ild_data")}.
#'   Ignored if \code{formula} and \code{data} are supplied.
#' @param formula Optional. If \code{fit} is \code{NULL}, mixed-model formula for refitting.
#' @param data Optional ILD object when \code{fit} is \code{NULL}; must contain \code{weights_col}.
#' @param weights_col Name of the row-level weight column in each bootstrap dataset (default \code{".ipw"}).
#' @param n_boot Number of bootstrap replicates.
#' @param weight_policy \code{"fixed_weights"} (subset rows; weights unchanged within replicated rows)
#'   or \code{"reestimate_weights"} (apply \code{weights_fn} to each resampled dataset before fitting).
#' @param weights_fn Function \code{function(x)} taking ILD \code{x} and returning ILD with
#'   \code{weights_col} present. Required when \code{weight_policy = "reestimate_weights"}.
#'   Ignored for \code{"fixed_weights"}.
#' @param cluster \code{"id"} (default: cluster by \code{ild_meta(data)$ild_id}) or \code{"data"}.
#' @param cluster_vec When \code{cluster = "data"}, length-\code{nrow(data)} vector of cluster IDs
#'   aligned to \code{ild_data} rows.
#' @param coef_fun \code{function(fit)} returning named numeric vector of estimands (default \code{lme4::fixef}).
#' @param seed Optional integer passed to \code{set.seed()} before resampling.
#' @param verbose Logical. If \code{TRUE}, warn on failed refits.
#' @param ... Passed to \code{lme4::lmer()} on each successful replicate (e.g. \code{control}).
#'   Do not pass \code{weights} here; use \code{weights_col} on the ILD data.
#' @return An object of class \code{ild_msm_bootstrap}: \code{replicates} (matrix \code{n_boot} x \code{p}),
#'   \code{estimate} (point estimates from \code{fit}), \code{bootstrap_se}, \code{conf_low}, \code{conf_high},
#'   \code{term_names}, \code{n_boot}, \code{n_success}, \code{n_cluster}, metadata, and \code{fit} (original).
#'   Use [tidy_ild_msm_bootstrap()] for an \code{\link{ild_tidy_schema}} tibble.
#' @seealso [ild_msm_inference], [ild_ipw_refit()], [tidy_ild_msm_bootstrap()]
#' @export
#' @examples
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   set.seed(5001)
#'   d <- ild_simulate(n_id = 12, n_obs_per = 6, seed = 5001)
#'   d$stress <- rnorm(nrow(d))
#'   d <- ild_prepare(d, id = "id", time = "time")
#'   d <- ild_center(d, y)
#'   d$.ipw <- runif(nrow(d), 0.8, 1.2)
#'   f0 <- ild_lme(y ~ y_bp + y_wp + stress + (1 | id), data = d, ar1 = FALSE,
#'     warn_no_ar1 = FALSE, warn_uncentered = FALSE)
#'   fw <- ild_ipw_refit(f0, data = d, weights = ".ipw")
#'   b <- ild_msm_bootstrap(fw, n_boot = 15L, weight_policy = "fixed_weights", seed = 2)
#'   print(b)
#'   tidy_ild_msm_bootstrap(b)
#' }
ild_msm_bootstrap <- function(fit = NULL,
                              formula = NULL,
                              data = NULL,
                              weights_col = ".ipw",
                              n_boot = 200L,
                              weight_policy = c("fixed_weights", "reestimate_weights"),
                              weights_fn = NULL,
                              cluster = c("id", "data"),
                              cluster_vec = NULL,
                              coef_fun = function(f) lme4::fixef(f),
                              seed = NULL,
                              verbose = FALSE,
                              ...) {
  weight_policy <- match.arg(weight_policy)
  cluster <- match.arg(cluster)
  if (weight_policy == "reestimate_weights" && is.null(weights_fn)) {
    stop("weights_fn must be supplied when weight_policy = \"reestimate_weights\".", call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)

  if (!is.null(fit)) {
    if (!inherits(fit, "lmerMod")) {
      stop("fit must be a lmerMod from ild_ipw_refit() or ild_lme().", call. = FALSE)
    }
    formula <- stats::formula(fit)
    ild_data <- attr(fit, "ild_data", exact = TRUE)
    if (is.null(ild_data)) {
      stop("fit must have attr(fit, \"ild_data\") (use ild_ipw_refit() / ild_lme()).", call. = FALSE)
    }
    validate_ild(ild_data)
  } else {
    if (is.null(formula) || is.null(data)) {
      stop("Supply either fit, or both formula and data.", call. = FALSE)
    }
    validate_ild(data)
    ild_data <- data
    fit <- NULL
  }

  if (!weights_col %in% names(ild_data)) {
    stop("weights_col '", weights_col, "' not found in ILD data.", call. = FALSE)
  }

  meta <- ild_meta(ild_data)
  cl_vec <- if (cluster == "id") {
    if (is.null(meta$ild_id) || !meta$ild_id %in% names(ild_data)) {
      stop("ild_data must contain id column from ild_meta().", call. = FALSE)
    }
    ild_data[[meta$ild_id]]
  } else {
    if (is.null(cluster_vec)) stop("When cluster = \"data\", supply cluster_vec.", call. = FALSE)
    if (length(cluster_vec) != nrow(ild_data)) {
      stop("cluster_vec length must match nrow(ild_data).", call. = FALSE)
    }
    cluster_vec
  }

  ucl <- unique(as.vector(cl_vec))
  n_cl <- length(ucl)
  if (n_cl < 2L && n_boot > 1L) {
    warning("Fewer than 2 clusters; bootstrap is degenerate.", call. = FALSE)
  }

  lmer_dots <- list(...)
  beta_point <- if (!is.null(fit)) {
    coef_fun(fit)
  } else {
    w0 <- ild_data[[weights_col]]
    ftmp <- do.call(
      lme4::lmer,
      c(list(formula = formula, data = ild_data, weights = w0), lmer_dots)
    )
    attr(ftmp, "ild_data") <- ild_data
    fit <- ftmp
    coef_fun(fit)
  }

  p_names <- names(beta_point)
  if (length(p_names) == 0L) stop("Could not determine coefficient names from fit.", call. = FALSE)

  n_boot <- as.integer(n_boot)[1L]
  if (!is.finite(n_boot) || n_boot < 1L) stop("n_boot must be a positive integer.", call. = FALSE)

  reps <- matrix(NA_real_, nrow = n_boot, ncol = length(p_names), dimnames = list(NULL, p_names))
  n_ok <- 0L

  for (b in seq_len(n_boot)) {
    sid <- sample(ucl, size = n_cl, replace = TRUE)
    idx <- unlist(lapply(sid, function(s) which(cl_vec == s)), use.names = FALSE)
    if (length(idx) == 0L) next
    data_b <- ild_data[idx, , drop = FALSE]
    attrs <- attributes(ild_data)
    data_b <- .ild_ipw_restore_attrs(data_b, attrs)

    if (weight_policy == "reestimate_weights") {
      data_b <- weights_fn(data_b)
      validate_ild(data_b)
      if (!weights_col %in% names(data_b)) {
        stop("weights_fn must return data with weights column '", weights_col, "'.", call. = FALSE)
      }
    }

    w_vec <- data_b[[weights_col]]
    fit_b <- tryCatch(
      do.call(
        lme4::lmer,
        c(list(formula = formula, data = data_b, weights = w_vec), lmer_dots)
      ),
      error = function(e) {
        if (verbose) {
          warning(
            "Bootstrap replicate ", b, " failed to fit: ", conditionMessage(e),
            call. = FALSE
          )
        }
        NULL
      }
    )
    if (is.null(fit_b)) next
    cb <- coef_fun(fit_b)
    vec <- rep(NA_real_, length(p_names))
    names(vec) <- p_names
    nm <- names(cb)
    vec[nm] <- as.numeric(unname(cb[nm]))
    reps[b, ] <- vec
    n_ok <- n_ok + 1L
  }

  q <- 0.025
  boot_se <- apply(reps, 2L, stats::sd, na.rm = TRUE)
  c_lo <- apply(reps, 2L, stats::quantile, probs = q, na.rm = TRUE, type = 7)
  c_hi <- apply(reps, 2L, stats::quantile, probs = 1 - q, na.rm = TRUE, type = 7)

  cluster_name <- if (cluster == "id") meta$ild_id else "user"

  out <- list(
    replicates = reps,
    estimate = stats::setNames(as.numeric(beta_point[p_names]), p_names),
    term_names = p_names,
    bootstrap_se = boot_se,
    conf_low = c_lo,
    conf_high = c_hi,
    n_boot = n_boot,
    n_success = n_ok,
    n_cluster = n_cl,
    weight_policy = weight_policy,
    weights_col = weights_col,
    cluster_name = cluster_name,
    formula = formula,
    fit = fit
  )
  class(out) <- c("ild_msm_bootstrap", "list")

  attr(out, "ild_provenance") <- ild_new_analysis_provenance(
    if (!is.null(fit)) fit else ild_data,
    "ild_msm_bootstrap",
    list(
      weights_col = weights_col,
      n_boot = n_boot,
      weight_policy = weight_policy,
      cluster = cluster
    ),
    list(n_success = n_ok, n_cluster = n_cl)
  )
  out
}

#' @export
print.ild_msm_bootstrap <- function(x, ...) {
  cat("tidyILD MSM bootstrap (weighted lmer)\n")
  cat("  replicates: ", x$n_boot, " (successful: ", x$n_success, ")\n", sep = "")
  cat("  clusters: ", x$n_cluster, " (by ", x$cluster_name, ")\n", sep = "")
  cat("  weight_policy: ", x$weight_policy, "\n", sep = "")
  cat("  weights_col: ", x$weights_col, "\n", sep = "")
  invisible(x)
}

#' Tidy fixed effects from \code{ild_msm_bootstrap}
#'
#' Returns a tibble matching \code{\link{ild_tidy_schema}} with \code{interval_type}
#' \code{bootstrap_percentile} (equal-tailed over replicate coefficients).
#'
#' @param x Object from [ild_msm_bootstrap()].
#' @param conf_level Numeric. Used for implied tail quantiles (default 0.95).
#' @param ... Unused.
#' @return A tibble; see \code{\link{ild_tidy_schema}}.
#' @seealso [ild_msm_bootstrap()], [ild_msm_inference]
#' @export
tidy_ild_msm_bootstrap <- function(x, conf_level = 0.95, ...) {
  if (!inherits(x, "ild_msm_bootstrap")) {
    stop("x must be from ild_msm_bootstrap().", call. = FALSE)
  }
  q <- (1 - conf_level) / 2
  term <- x$term_names
  est <- as.numeric(x$estimate)
  se <- as.numeric(x$bootstrap_se)
  # Recompute CIs for conf_level
  reps <- x$replicates
  c_lo <- apply(reps, 2L, stats::quantile, probs = q, na.rm = TRUE, type = 7)
  c_hi <- apply(reps, 2L, stats::quantile, probs = 1 - q, na.rm = TRUE, type = 7)
  pval <- rep(NA_real_, length(term))
  stat <- rep(NA_real_, length(term))
  ok <- is.finite(se) & se > 0
  stat[ok] <- est[ok] / se[ok]
  pval[ok] <- 2 * (1 - stats::pnorm(abs(stat[ok])))
  mc <- if (!is.null(x$fit)) ild_tidy_model_class_string(x$fit) else "lmerMod"
  ild_tidy_assemble(
    term = term,
    estimate = est,
    std_error = se,
    conf_low = c_lo,
    conf_high = c_hi,
    p_value = pval,
    statistic = stat,
    interval_type = rep("bootstrap_percentile", length(term)),
    engine = "lmer",
    model_class = mc,
    component = rep("fixed", length(term)),
    effect_level = NULL,
    optional = NULL
  )
}
