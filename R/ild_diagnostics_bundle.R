# Canonical diagnostics bundle (Phase 1 contract -- structure only; engines populate later)

#' Top-level slot names for an ILD diagnostics bundle (internal contract)
#' @keywords internal
#' @noRd
ILD_DIAGNOSTICS_BUNDLE_SLOTS <- c(
  "meta",
  "data",
  "design",
  "fit",
  "residual",
  "predictive",
  "missingness",
  "causal",
  "warnings",
  "guardrails",
  "summary_text"
)

#' Canonical ILD diagnostics bundle
#'
#' The bundle has identical top-level \strong{slot names} for every estimation backend.
#' Content differs by engine; unavailable sections are \code{NULL}.
#' \code{warnings} and \code{guardrails} are always tibbles (possibly 0 rows).
#' Guardrails use the canonical schema documented in \code{\link{guardrail_registry}}:
#' \code{rule_id}, \code{section}, \code{severity}, \code{triggered}, \code{message},
#' \code{recommendation} (only triggered rules are stored as rows).
#'
#' Several standalone functions **feed** these slots when you use \code{\link{ild_diagnose}};
#' see \code{\link{ild_diagnostics_utilities}} (\code{\link{ild_design_check}},
#' \code{\link{ild_missing_pattern}}, \code{\link{ild_missing_model}}, \code{\link{ild_ipw_weights}}).
#'
#' Bundles returned by \code{\link{ild_diagnose}} also set \code{attr(bundle, "ild_fit")} and
#' \code{attr(bundle, "ild_data")} for \code{\link{ild_autoplot}}; re-run \code{ild_diagnose} if
#' these are missing (e.g. after loading a saved bundle without the fitted object).
#'
#' The \code{\link{print}} method lists each slot and prints a short \strong{summary} line:
#' number of warning rows, number of guardrail rows, highest guardrail severity
#' (\code{info} < \code{warning}), and up to five triggered \code{rule_id} values.
#'
#' @section Slots:
#' \describe{
#'   \item{\code{meta}}{List or \code{NULL}: run ids, engine, dimensions.}
#'   \item{\code{data}}{List or \code{NULL}: spacing, gaps, compliance, missingness, distributions.}
#'   \item{\code{design}}{List or \code{NULL}: within/between variation, time coverage, occasions, imbalance.}
#'   \item{\code{fit}}{List or \code{NULL}: convergence, singularity, optimizer / MCMC diagnostics.}
#'   \item{\code{residual}}{List or \code{NULL}: ACF, Q-Q, fitted vs observed, heteroskedasticity.}
#'   \item{\code{predictive}}{List or \code{NULL}: CV, PPC, forecast error.}
#'   \item{\code{missingness}}{List or \code{NULL}: IPW / imputation summaries when applicable.}
#'   \item{\code{causal}}{List or \code{NULL}: weights, positivity, causal diagnostics when applicable.}
#'   \item{\code{warnings}}{Tibble: structured warnings.}
#'   \item{\code{guardrails}}{Tibble: methodological guardrails (see \code{\link{guardrail_registry}}).}
#'   \item{\code{summary_text}}{Character: short narrative summary.}
#' }
#'
#' @param meta,data,design,fit,residual,predictive,missingness,causal Optional sections
#'   (list or structured object, or \code{NULL}).
#' @param warnings,guardrails Tibbles (default empty).
#' @param summary_text Character vector (default empty).
#' @return An object of class \code{ild_diagnostics_bundle}.
#' @seealso \code{\link{ild_diagnose}}, \code{\link{guardrail_registry}},
#'   \code{\link{ild_diagnostics_utilities}},
#'   \code{\link{ild_tidy_schema}}, \code{\link{ild_augment_schema}}.
#'   Per-section field semantics: \code{vignette("developer-contracts", package = "tidyILD")}.
#' @export
ild_diagnostics_bundle <- function(meta = NULL,
                                 data = NULL,
                                 design = NULL,
                                 fit = NULL,
                                 residual = NULL,
                                 predictive = NULL,
                                 missingness = NULL,
                                 causal = NULL,
                                 warnings = NULL,
                                 guardrails = NULL,
                                 summary_text = character()) {
  if (is.null(warnings)) warnings <- tibble::tibble()
  if (is.null(guardrails)) guardrails <- guardrails_empty_tibble()
  if (!tibble::is_tibble(warnings)) {
    warnings <- tibble::as_tibble(warnings)
  }
  if (!tibble::is_tibble(guardrails)) {
    guardrails <- tibble::as_tibble(guardrails)
  }
  if (!is.character(summary_text)) summary_text <- as.character(summary_text)
  out <- list(
    meta = meta,
    data = data,
    design = design,
    fit = fit,
    residual = residual,
    predictive = predictive,
    missingness = missingness,
    causal = causal,
    warnings = warnings,
    guardrails = guardrails,
    summary_text = summary_text
  )
  validate_ild_diagnostics_bundle(out)
  class(out) <- c("ild_diagnostics_bundle", "list")
  out
}

#' @rdname ild_diagnostics_bundle
#' @export
new_ild_diagnostics_bundle <- ild_diagnostics_bundle

#' @rdname ild_diagnostics_bundle
#' @return \code{is_ild_diagnostics_bundle}: logical.
#' @param x Object to test.
#' @export
is_ild_diagnostics_bundle <- function(x) {
  inherits(x, "ild_diagnostics_bundle")
}

#' @keywords internal
#' @noRd
validate_ild_diagnostics_bundle <- function(x) {
  if (!is.list(x)) stop("Bundle must be a list.", call. = FALSE)
  nm <- names(x)
  if (!identical(nm, ILD_DIAGNOSTICS_BUNDLE_SLOTS)) {
    stop(
      "Diagnostics bundle must have exactly these names in order: ",
      paste(ILD_DIAGNOSTICS_BUNDLE_SLOTS, collapse = ", "),
      call. = FALSE
    )
  }
  if (!tibble::is_tibble(x$warnings)) stop("Bundle $warnings must be a tibble.", call. = FALSE)
  if (!tibble::is_tibble(x$guardrails)) stop("Bundle $guardrails must be a tibble.", call. = FALSE)
  gr_req <- c("rule_id", "section", "severity", "triggered", "message", "recommendation")
  if (!all(gr_req %in% names(x$guardrails))) {
    stop(
      "Bundle $guardrails must have columns: ",
      paste(gr_req, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.character(x$summary_text)) stop("Bundle $summary_text must be character.", call. = FALSE)
  invisible(x)
}

#' @export
print.ild_diagnostics_bundle <- function(x, ...) {
  cat("ILD diagnostics bundle\n")
  for (s in ILD_DIAGNOSTICS_BUNDLE_SLOTS) {
    if (s %in% c("warnings", "guardrails")) {
      n <- nrow(x[[s]])
      cat("  ", s, ": tibble [", n, " x ", ncol(x[[s]]), "]\n", sep = "")
      next
    }
    if (s == "summary_text") {
      cat("  summary_text: ", length(x[[s]]), " string(s)\n", sep = "")
      next
    }
    v <- x[[s]]
    if (is.null(v)) {
      cat("  ", s, ": NULL\n", sep = "")
    } else {
      cat("  ", s, ": <non-null>\n", sep = "")
    }
  }
  nw <- nrow(x$warnings)
  ng <- nrow(x$guardrails)
  cat("Summary:\n")
  cat("  warnings: ", nw, " row(s)\n", sep = "")
  cat("  guardrails: ", ng, " row(s)", sep = "")
  if (ng > 0L) {
    max_sev <- guardrail_max_severity(x$guardrails$severity)
    rids <- unique(as.character(x$guardrails$rule_id))
    show <- rids[seq_len(min(length(rids), 5L))]
    cat(
      "; highest severity: ",
      if (!is.na(max_sev) && nzchar(max_sev)) max_sev else "-",
      "\n  key rule_id(s): ",
      if (length(show) > 0L) paste(show, collapse = ", ") else "-",
      "\n",
      sep = ""
    )
  } else {
    cat("\n")
  }
  invisible(x)
}
