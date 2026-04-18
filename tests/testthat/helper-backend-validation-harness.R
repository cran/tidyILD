# Cross-backend validation harness — loaded by testthat and sourced by CI scripts.
# Depends on tidyILD being loaded (library / load_all). See inst/dev/BACKEND_VALIDATION_BENCHMARK_CONTRACT.md.

#' @keywords internal
harness_tier_rank <- function(tier) {
  tier <- match.arg(tier, c("smoke", "nightly", "full"))
  switch(tier, smoke = 1L, nightly = 2L, full = 3L)
}

#' @keywords internal
harness_scenario_in_tier <- function(scenario_tier_min, requested_tier) {
  harness_tier_rank(requested_tier) >= harness_tier_rank(scenario_tier_min)
}

#' Scenario manifest (v1) — columns used by runner
#' @keywords internal
harness_scenario_manifest <- function() {
  tibble::tibble(
    scenario_id = c("mlm_power_ri", "kfas_local_level", "ctsem_univariate", "mlm_lag_x"),
    scenario_type = c("mlm_power", "kfas", "ctsem", "mlm_lag"),
    tier_min = c("smoke", "smoke", "nightly", "smoke"),
    mlm_n_id = c(18L, NA_integer_, NA_integer_, NA_integer_),
    mlm_n_obs_per = c(10L, NA_integer_, NA_integer_, NA_integer_),
    mlm_effect_size = c(0.35, NA_real_, NA_real_, NA_real_),
    mlm_test_term = c("x", NA_character_, NA_character_, NA_character_),
    kfas_n_id = c(NA_integer_, 1L, NA_integer_, NA_integer_),
    kfas_n_obs_per = c(NA_integer_, 36L, NA_integer_, NA_integer_),
    ctsem_n_obs_per = c(NA_integer_, NA_integer_, 42L, NA_integer_),
    mlm_lag_n_id = c(NA_integer_, NA_integer_, NA_integer_, 20L),
    mlm_lag_n_obs_per = c(NA_integer_, NA_integer_, NA_integer_, 10L),
    mlm_lag_beta = c(NA_real_, NA_real_, NA_real_, 0.4),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
harness_default_backends_for_tier <- function(tier) {
  tier <- match.arg(tier, c("smoke", "nightly", "full"))
  if (tier == "smoke") {
    return(c("lme4", "kfas"))
  }
  if (tier == "nightly") {
    return(c("lme4", "nlme", "brms", "kfas", "ctsem"))
  }
  c("lme4", "nlme", "brms", "kfas", "ctsem")
}

#' @keywords internal
harness_backend_installed <- function(backend) {
  backend <- match.arg(backend, c("lme4", "nlme", "brms", "kfas", "ctsem"), several.ok = TRUE)
  vapply(backend, function(b) {
    switch(b,
      lme4 = requireNamespace("lme4", quietly = TRUE),
      nlme = requireNamespace("nlme", quietly = TRUE),
      brms = requireNamespace("brms", quietly = TRUE) &&
        (requireNamespace("rstan", quietly = TRUE) || requireNamespace("cmdstanr", quietly = TRUE)),
      kfas = requireNamespace("KFAS", quietly = TRUE),
      ctsem = requireNamespace("ctsem", quietly = TRUE) && requireNamespace("rstan", quietly = TRUE),
      FALSE
    )
  }, logical(1L))
}

#' @keywords internal
harness_expand_runs <- function(manifest, tier, backends_requested) {
  rows <- list()
  for (i in seq_len(nrow(manifest))) {
    sid <- manifest$scenario_id[i]
    stype <- manifest$scenario_type[i]
    tmin <- manifest$tier_min[i]
    if (!harness_scenario_in_tier(tmin, tier)) {
      next
    }
    bes <- switch(stype,
      mlm_power = intersect(backends_requested, c("lme4", "nlme", "brms")),
      mlm_lag = intersect(backends_requested, "lme4"),
      kfas = if ("kfas" %in% backends_requested) "kfas" else character(),
      ctsem = if ("ctsem" %in% backends_requested) "ctsem" else character(),
      character()
    )
    for (b in bes) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        scenario_id = sid,
        scenario_type = stype,
        backend = b
      )
    }
  }
  if (length(rows) == 0L) {
    return(tibble::tibble(
      scenario_id = character(),
      scenario_type = character(),
      backend = character()
    ))
  }
  dplyr::bind_rows(rows)
}

#' @keywords internal
harness_generate_mlm_power_data <- function(seed,
                                            n_id,
                                            n_obs_per,
                                            effect_size,
                                            test_term = "x") {
  set.seed(as.integer(seed))
  d <- ild_simulate(
    n_id = n_id,
    n_obs_per = n_obs_per,
    seed = as.integer(seed)
  )
  tt <- as.character(test_term)[1L]
  d[[tt]] <- stats::rnorm(nrow(d))
  d$y <- d$y + as.numeric(effect_size) * d[[tt]]
  ild_prepare(d, id = "id", time = "time")
}

#' @keywords internal
harness_generate_kfas_data <- function(seed, n_id, n_obs_per) {
  d <- ild_simulate(
    n_id = as.integer(n_id),
    n_obs_per = as.integer(n_obs_per),
    seed = as.integer(seed)
  )
  ild_prepare(d, id = "id", time = "time")
}

#' @keywords internal
harness_generate_mlm_lag_data <- function(seed, n_id, n_obs_per, beta_lag) {
  set.seed(as.integer(seed))
  d <- ild_simulate(
    n_id = as.integer(n_id),
    n_obs_per = as.integer(n_obs_per),
    seed = as.integer(seed)
  )
  d$x <- stats::rnorm(nrow(d))
  prep <- ild_prepare(d, id = "id", time = "time")
  prep2 <- ild_lag(prep, x, n = 1L, mode = "gap_aware")
  b <- as.numeric(beta_lag)[1L]
  prep2$y <- b * prep2$x_lag1 + stats::rnorm(nrow(prep2), sd = 0.75)
  prep2
}

#' @keywords internal
harness_generate_ctsem_data <- function(seed, n_obs_per) {
  d <- ild_simulate(
    n_id = 1L,
    n_obs_per = as.integer(n_obs_per),
    irregular = TRUE,
    seed = as.integer(seed)
  )
  x <- ild_prepare(d, id = "id", time = "time")
  ild_center(x, y)
}

#' @keywords internal
harness_resolve_tidy_row <- function(td, target_term) {
  if (is.null(td) || nrow(td) == 0L) {
    return(NULL)
  }
  if (target_term %in% td$term) {
    return(td[td$term == target_term, , drop = FALSE][1L, ])
  }
  pat <- paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", target_term), "$")
  hit <- td$term[grepl(pat, td$term)]
  if (length(hit) >= 1L) {
    return(td[td$term == hit[1L], , drop = FALSE][1L, ])
  }
  NULL
}

#' @keywords internal
harness_wald_interval <- function(est, se, level = 0.95) {
  if (!is.finite(est) || !is.finite(se) || se <= 0) {
    return(c(NA_real_, NA_real_))
  }
  z <- stats::qnorm(1 - (1 - level) / 2)
  c(est - z * se, est + z * se)
}

#' @keywords internal
harness_converged_heuristic <- function(fit, backend) {
  if (is.null(fit)) {
    return(FALSE)
  }
  if (backend == "lme4") {
    if (inherits(fit, "lmerMod")) {
      conv <- fit@optinfo$conv$lme4
      if (!is.null(conv$code) && is.numeric(conv$code)) {
        return(as.integer(conv$code) == 0L)
      }
    }
    return(TRUE)
  }
  if (backend == "nlme") {
    return(TRUE)
  }
  if (backend == "brms") {
    if (!requireNamespace("brms", quietly = TRUE)) {
      return(FALSE)
    }
    rh <- tryCatch(max(brms::rhat(fit), na.rm = TRUE), error = function(e) NA_real_)
    return(is.finite(rh) && rh < 1.08)
  }
  if (backend == "kfas") {
    return(inherits(fit, "ild_fit_kfas"))
  }
  if (backend == "ctsem") {
    return(inherits(fit, "ild_fit_ctsem"))
  }
  TRUE
}

#' @keywords internal
harness_singular_heuristic <- function(fit, backend) {
  if (backend != "lme4" || !inherits(fit, "lmerMod")) {
    return(NA)
  }
  if (!requireNamespace("lme4", quietly = TRUE)) {
    return(NA)
  }
  isTRUE(lme4::isSingular(fit))
}

#' Collect guardrail summary from diagnostics bundle
#' @keywords internal
collect_diagnostics_signals <- function(bundle) {
  if (is.null(bundle) || !inherits(bundle, "ild_diagnostics_bundle")) {
    return(list(
      n_guardrails = 0L,
      guardrail_rule_ids = ""
    ))
  }
  gr <- bundle$guardrails
  if (is.null(gr) || !tibble::is_tibble(gr) || nrow(gr) == 0L) {
    return(list(n_guardrails = 0L, guardrail_rule_ids = ""))
  }
  ids <- gr$rule_id
  ids <- ids[!is.na(ids) & nzchar(as.character(ids))]
  list(
    n_guardrails = nrow(gr),
    guardrail_rule_ids = paste(unique(as.character(ids)), collapse = ";")
  )
}

#' Fit one case; returns list(fit, error_message, elapsed_sec)
#' @keywords internal
harness_fit_one <- function(backend,
                            scenario_id,
                            scenario_type,
                            data,
                            manifest_row,
                            brms_iter = 200L,
                            brms_chains = 1L,
                            brms_warmup = 100L,
                            ctsem_iter = 150L,
                            ctsem_chains = 1L) {
  fit <- NULL
  err <- NA_character_
  t0 <- proc.time()[[3L]]
  if (backend == "lme4" && scenario_type == "mlm_power") {
    tt <- manifest_row$mlm_test_term
    ff <- stats::as.formula(paste("y ~", tt, "+ (1 | id)"))
    fit <- tryCatch(
      suppressWarnings(ild_lme(
        ff,
        data = data,
        ar1 = FALSE,
        warn_no_ar1 = FALSE,
        warn_uncentered = FALSE
      )),
      error = function(e) {
        err <<- conditionMessage(e)
        NULL
      }
    )
  } else if (backend == "lme4" && scenario_type == "mlm_lag") {
    fit <- tryCatch(
      suppressWarnings(ild_lme(
        y ~ x_lag1 + (1 | id),
        data = data,
        ar1 = FALSE,
        warn_no_ar1 = FALSE,
        warn_uncentered = FALSE
      )),
      error = function(e) {
        err <<- conditionMessage(e)
        NULL
      }
    )
  } else if (backend == "nlme" && scenario_type == "mlm_power") {
    tt <- manifest_row$mlm_test_term
    ff <- stats::as.formula(paste("y ~", tt))
    fit <- tryCatch(
      suppressWarnings(ild_lme(
        ff,
        data = data,
        ar1 = TRUE,
        warn_no_ar1 = FALSE,
        warn_uncentered = FALSE
      )),
      error = function(e) {
        err <<- conditionMessage(e)
        NULL
      }
    )
  } else if (backend == "brms" && scenario_type == "mlm_power") {
    tt <- manifest_row$mlm_test_term
    ff <- stats::as.formula(paste("y ~", tt, "+ (1 | id)"))
    fit <- tryCatch(
      suppressWarnings(ild_brms(
        ff,
        data = data,
        warn_uncentered = FALSE,
        iter = brms_iter,
        chains = brms_chains,
        warmup = brms_warmup,
        refresh = 0,
        seed = 1L
      )),
      error = function(e) {
        err <<- conditionMessage(e)
        NULL
      }
    )
  } else if (backend == "kfas" && scenario_type == "kfas") {
    fit <- tryCatch(
      suppressWarnings(ild_kfas(
        data = data,
        outcome = "y",
        time_units = "hours",
        irregular_time = TRUE
      )),
      error = function(e) {
        err <<- conditionMessage(e)
        NULL
      }
    )
  } else if (backend == "ctsem" && scenario_type == "ctsem") {
    fit <- tryCatch(
      suppressWarnings(ild_ctsem(
        data = data,
        outcome = "y",
        model_type = "stanct",
        iter = ctsem_iter,
        chains = ctsem_chains
      )),
      error = function(e) {
        err <<- conditionMessage(e)
        NULL
      }
    )
  }
  elapsed <- proc.time()[[3L]] - t0
  list(fit = fit, error_message = err, elapsed_sec = as.numeric(elapsed))
}

#' One raw benchmark row
#' @keywords internal
harness_extract_metrics_row <- function(fit,
                                        backend,
                                        scenario_type,
                                        truth,
                                        test_term,
                                        nominal_level = 0.95,
                                        elapsed_sec = NA_real_,
                                        fit_error = NA_character_) {
  skipped_reason <- ""
  fe_ok <- length(fit_error) > 0L && !is.na(fit_error[1L]) && nzchar(as.character(fit_error[1L]))
  if (fe_ok) {
    return(tibble::tibble(
      truth = truth,
      estimate = NA_real_,
      bias = NA_real_,
      squared_error = NA_real_,
      std_error = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      covered = NA,
      nominal_level = nominal_level,
      elapsed_sec = elapsed_sec,
      converged = FALSE,
      singular = NA,
      fit_error = as.character(fit_error[1L]),
      n_guardrails = 0L,
      guardrail_rule_ids = "",
      skipped_reason = ""
    ))
  }
  if (is.null(fit)) {
    return(tibble::tibble(
      truth = truth,
      estimate = NA_real_,
      bias = NA_real_,
      squared_error = NA_real_,
      std_error = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      covered = NA,
      nominal_level = nominal_level,
      elapsed_sec = elapsed_sec,
      converged = FALSE,
      singular = harness_singular_heuristic(fit, backend),
      fit_error = if (is.na(fit_error)) "null fit" else fit_error,
      n_guardrails = 0L,
      guardrail_rule_ids = "",
      skipped_reason = ""
    ))
  }

  data <- attr(fit, "ild_data", exact = TRUE)
  bundle <- tryCatch(
    suppressWarnings(ild_diagnose(fit, data = data, type = "qq")),
    error = function(e) NULL
  )
  sig <- collect_diagnostics_signals(bundle)

  td <- tryCatch(ild_tidy(fit), error = function(e) NULL)
  row <- NULL
  if (scenario_type %in% c("mlm_power", "mlm_lag")) {
    row <- harness_resolve_tidy_row(td, test_term)
  } else if (scenario_type == "kfas") {
    row <- harness_resolve_tidy_row(td, test_term %||% "observation_variance")
  } else if (scenario_type == "ctsem") {
    row <- if (!is.null(td) && nrow(td) > 0L) td[1L, , drop = FALSE] else NULL
  }

  est <- NA_real_
  se <- NA_real_
  lo <- NA_real_
  hi <- NA_real_
  if (!is.null(row)) {
    est <- as.numeric(row$estimate)[1L]
    se <- as.numeric(row$std_error)[1L]
    lo <- as.numeric(row$conf_low)[1L]
    hi <- as.numeric(row$conf_high)[1L]
  }
  if (scenario_type %in% c("mlm_power", "mlm_lag") && is.finite(se) && (!is.finite(lo) || !is.finite(hi))) {
    w <- harness_wald_interval(est, se, nominal_level)
    lo <- w[1L]
    hi <- w[2L]
  }

  conv <- harness_converged_heuristic(fit, backend)
  sing <- harness_singular_heuristic(fit, backend)
  covered <- NA
  bias <- NA_real_
  sqe <- NA_real_
  if (is.finite(truth) && is.finite(est)) {
    bias <- est - truth
    sqe <- bias^2
    if (is.finite(lo) && is.finite(hi)) {
      covered <- truth >= lo & truth <= hi
    }
  }

  tibble::tibble(
    truth = truth,
    estimate = est,
    bias = bias,
    squared_error = sqe,
    std_error = se,
    ci_low = lo,
    ci_high = hi,
    covered = covered,
    nominal_level = nominal_level,
    elapsed_sec = elapsed_sec,
    converged = conv,
    singular = sing,
    fit_error = NA_character_,
    n_guardrails = sig$n_guardrails,
    guardrail_rule_ids = sig$guardrail_rule_ids,
    skipped_reason = skipped_reason
  )
}

#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Summarize raw benchmark tibble
#' @keywords internal
summarize_validation_metrics <- function(raw_df) {
  if (nrow(raw_df) == 0L) {
    return(raw_df)
  }
  raw_df <- tibble::as_tibble(raw_df)
  raw_df$has_skip <- !is.na(raw_df$skipped_reason) & nzchar(as.character(raw_df$skipped_reason))
  raw_df$install_skipped <- raw_df$skipped_reason == "backend_not_installed"
  dplyr::group_by(raw_df, .data$tier, .data$scenario_id, .data$backend) %>%
    dplyr::summarise(
      n_replicates = dplyr::n(),
      n_skipped = sum(.data$has_skip, na.rm = TRUE),
      n_install_skipped = sum(.data$install_skipped, na.rm = TRUE),
      n_ran = sum(!.data$install_skipped, na.rm = TRUE),
      n_converged = sum(.data$converged %in% TRUE & !.data$install_skipped, na.rm = TRUE),
      convergence_rate = ifelse(
        sum(!.data$install_skipped) > 0L,
        mean(.data$converged[!.data$install_skipped] %in% TRUE, na.rm = TRUE),
        NA_real_
      ),
      mean_bias = mean(.data$bias[!.data$install_skipped], na.rm = TRUE),
      rmse = sqrt(mean(.data$squared_error[!.data$install_skipped], na.rm = TRUE)),
      mean_coverage = ifelse(
        sum(!is.na(.data$covered[!.data$install_skipped])) > 0L,
        mean(.data$covered[!.data$install_skipped] %in% TRUE, na.rm = TRUE),
        NA_real_
      ),
      n_coverage_defined = sum(!is.na(.data$covered[!.data$install_skipped])),
      calibration_gap = ifelse(
        sum(!is.na(.data$covered[!.data$install_skipped])) > 0L,
        dplyr::first(.data$nominal_level) -
          mean(.data$covered[!.data$install_skipped] %in% TRUE, na.rm = TRUE),
        NA_real_
      ),
      mean_elapsed_sec = mean(.data$elapsed_sec[!.data$install_skipped], na.rm = TRUE),
      mean_n_guardrails = mean(.data$n_guardrails[!.data$install_skipped], na.rm = TRUE),
      .groups = "drop"
    )
}

#' Parse --key value / --key=value CLI args
#' @keywords internal
harness_parse_cli_args <- function(args = NULL) {
  if (is.null(args)) {
    args <- commandArgs(trailingOnly = TRUE)
  }
  out <- list(
    tier = "smoke",
    backends = NULL,
    n_sim = 3L,
    seed = 20260406L,
    out_dir = file.path(getwd(), "benchmark_artifacts"),
    run_id = format(Sys.time(), "bench-%Y%m%d-%H%M%S"),
    git_sha = NA_character_,
    brms_iter = 200L,
    brms_chains = 1L,
    ctsem_iter = 150L,
    ctsem_chains = 1L
  )
  if (length(args) == 0L) {
    return(out)
  }
  i <- 1L
  while (i <= length(args)) {
    a <- args[i]
    if (grepl("^--tier=", a)) {
      out$tier <- sub("^--tier=", "", a)
    } else if (a == "--tier" && i < length(args)) {
      out$tier <- args[i + 1L]
      i <- i + 1L
    } else if (grepl("^--backends=", a)) {
      out$backends <- strsplit(sub("^--backends=", "", a), ",", fixed = TRUE)[[1L]]
    } else if (a == "--backends" && i < length(args)) {
      out$backends <- strsplit(args[i + 1L], ",", fixed = TRUE)[[1L]]
      i <- i + 1L
    } else if (grepl("^--n-sim=", a)) {
      out$n_sim <- as.integer(sub("^--n-sim=", "", a))
    } else if (a == "--n-sim" && i < length(args)) {
      out$n_sim <- as.integer(args[i + 1L])
      i <- i + 1L
    } else if (grepl("^--seed=", a)) {
      out$seed <- as.integer(sub("^--seed=", "", a))
    } else if (a == "--seed" && i < length(args)) {
      out$seed <- as.integer(args[i + 1L])
      i <- i + 1L
    } else if (grepl("^--out-dir=", a)) {
      out$out_dir <- sub("^--out-dir=", "", a)
    } else if (a == "--out-dir" && i < length(args)) {
      out$out_dir <- args[i + 1L]
      i <- i + 1L
    } else if (grepl("^--run-id=", a)) {
      out$run_id <- sub("^--run-id=", "", a)
    } else if (a == "--run-id" && i < length(args)) {
      out$run_id <- args[i + 1L]
      i <- i + 1L
    } else if (grepl("^--git-sha=", a)) {
      out$git_sha <- sub("^--git-sha=", "", a)
    } else if (a == "--git-sha" && i < length(args)) {
      out$git_sha <- args[i + 1L]
      i <- i + 1L
    }
    i <- i + 1L
  }
  if (is.null(out$backends) || length(out$backends) == 0L) {
    out$backends <- harness_default_backends_for_tier(out$tier)
  }
  out
}

#' Run full benchmark matrix; returns list(raw, summary, metadata, skipped_backends)
#' @keywords internal
harness_run_benchmark <- function(tier = c("smoke", "nightly", "full"),
                                    backends = NULL,
                                    n_sim = 3L,
                                    seed = 20260406L,
                                    run_id = NULL,
                                    git_sha = NA_character_,
                                    brms_iter = 200L,
                                    brms_chains = 1L,
                                    ctsem_iter = 150L,
                                    ctsem_chains = 1L) {
  tier <- match.arg(tier)
  if (is.null(backends)) {
    backends <- harness_default_backends_for_tier(tier)
  }
  if (is.null(run_id)) {
    run_id <- format(Sys.time(), "bench-%Y%m%d-%H%M%S")
  }
  manifest <- harness_scenario_manifest()
  grid <- harness_expand_runs(manifest, tier, backends)
  skipped_backends <- backends[!harness_backend_installed(backends)]
  rows <- list()
  if (nrow(grid) == 0L) {
    raw <- tibble::tibble()
  } else {
    rep_idx <- 0L
    for (r in seq_len(nrow(grid))) {
      sid <- grid$scenario_id[r]
      st <- grid$scenario_type[r]
      be <- grid$backend[r]
      mrow <- manifest[manifest$scenario_id == sid, , drop = FALSE][1L, ]
      if (!harness_backend_installed(be)) {
        for (k in seq_len(n_sim)) {
          rep_idx <- rep_idx + 1L
          rows[[length(rows) + 1L]] <- tibble::tibble(
            run_id = run_id,
            git_sha = git_sha %||% NA_character_,
            tier = tier,
            scenario_id = sid,
            backend = be,
            replicate_id = k,
            truth = NA_real_,
            estimate = NA_real_,
            bias = NA_real_,
            squared_error = NA_real_,
            std_error = NA_real_,
            ci_low = NA_real_,
            ci_high = NA_real_,
            covered = NA,
            nominal_level = 0.95,
            elapsed_sec = NA_real_,
            converged = FALSE,
            singular = NA,
            fit_error = NA_character_,
            n_guardrails = 0L,
            guardrail_rule_ids = "",
            skipped_reason = "backend_not_installed"
          )
        }
        next
      }
      for (k in seq_len(n_sim)) {
        rep_idx <- rep_idx + 1L
        rep_seed <- as.integer(seed) + rep_idx
        data <- NULL
        truth <- NA_real_
        tt <- "x"
        if (st == "mlm_power") {
          data <- harness_generate_mlm_power_data(
            rep_seed,
            mrow$mlm_n_id,
            mrow$mlm_n_obs_per,
            mrow$mlm_effect_size,
            mrow$mlm_test_term
          )
          truth <- as.numeric(mrow$mlm_effect_size)
          tt <- as.character(mrow$mlm_test_term)
        } else if (st == "kfas") {
          data <- harness_generate_kfas_data(
            rep_seed,
            mrow$kfas_n_id,
            mrow$kfas_n_obs_per
          )
          truth <- NA_real_
          tt <- "observation_variance"
        } else if (st == "ctsem") {
          data <- harness_generate_ctsem_data(rep_seed, mrow$ctsem_n_obs_per)
          truth <- NA_real_
          tt <- NA_character_
        } else if (st == "mlm_lag") {
          data <- harness_generate_mlm_lag_data(
            rep_seed,
            mrow$mlm_lag_n_id,
            mrow$mlm_lag_n_obs_per,
            mrow$mlm_lag_beta
          )
          truth <- as.numeric(mrow$mlm_lag_beta)
          tt <- "x_lag1"
        }
        fo <- harness_fit_one(
          be, sid, st, data, mrow,
          brms_iter = brms_iter,
          brms_chains = brms_chains,
          brms_warmup = max(50L, brms_iter %/% 2L),
          ctsem_iter = ctsem_iter,
          ctsem_chains = ctsem_chains
        )
        err <- fo$error_message
        if (!is.na(err) && nzchar(err)) {
          mr <- harness_extract_metrics_row(
            NULL, be, st, truth, tt,
            elapsed_sec = fo$elapsed_sec,
            fit_error = err
          )
        } else {
          mr <- harness_extract_metrics_row(
            fo$fit, be, st, truth, tt,
            elapsed_sec = fo$elapsed_sec
          )
        }
        rows[[length(rows) + 1L]] <- tibble::tibble(
          run_id = run_id,
          git_sha = git_sha %||% NA_character_,
          tier = tier,
          scenario_id = sid,
          backend = be,
          replicate_id = k
        ) %>%
          dplyr::bind_cols(mr)
      }
    }
    raw <- dplyr::bind_rows(rows)
  }
  summ <- if (nrow(raw) > 0L) summarize_validation_metrics(raw) else tibble::tibble()
  meta <- list(
    package_version = as.character(utils::packageVersion("tidyILD")),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    platform = R.version$platform,
    git_sha = git_sha,
    seed_base = seed,
    tier = tier,
    backends_requested = backends,
    backends_skipped_install = skipped_backends,
    scenario_manifest_version = 2L,
    timestamp_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  list(raw = raw, summary = summ, metadata = meta, skipped_backends = skipped_backends)
}

#' Read thresholds JSON (requires jsonlite)
#' @keywords internal
harness_read_thresholds_json <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite required to read thresholds (Suggests).", call. = FALSE)
  }
  jsonlite::read_json(path, simplifyVector = FALSE)
}

#' Evaluate summary against thresholds; returns tibble of check rows
#' @keywords internal
harness_evaluate_thresholds <- function(summary_df,
                                        thresholds_list,
                                        warn_only_backends = c("brms", "ctsem")) {
  if (nrow(summary_df) == 0L) {
    return(tibble::tibble(
      scenario_id = character(),
      backend = character(),
      check = character(),
      status = character(),
      detail = character()
    ))
  }
  defs <- thresholds_list$defaults %||% list()
  by_be <- thresholds_list$by_backend %||% list()
  by_sc <- thresholds_list$by_scenario %||% list()
  min_cov <- as.numeric(defs$min_coverage %||% 0.65)
  max_fail <- as.numeric(defs$max_convergence_failure_rate %||% 0.5)
  min_conv_n <- as.integer(defs$min_n_converged %||% 1L)

  out <- list()
  for (i in seq_len(nrow(summary_df))) {
    sid <- summary_df$scenario_id[i]
    be <- summary_df$backend[i]
    sc_row <- summary_df[i, , drop = FALSE]
    be_cfg <- by_be[[be]] %||% list()
    sc_cfg <- by_sc[[sid]] %||% list()
    warn_only <- isTRUE(be_cfg$warn_only) || isTRUE(sc_cfg$warn_only) ||
      be %in% warn_only_backends
    min_cov_i <- as.numeric(sc_cfg$min_coverage %||% be_cfg$min_coverage %||% min_cov)
    max_fail_i <- as.numeric(
      sc_cfg$max_convergence_failure_rate %||%
        be_cfg$max_convergence_failure_rate %||% max_fail
    )
    min_conv_i <- as.integer(sc_cfg$min_n_converged %||% be_cfg$min_n_converged %||% min_conv_n)

    n_ran <- sc_row$n_ran
    if (!is.finite(n_ran) || n_ran <= 0L) {
      out[[length(out) + 1L]] <- tibble::tibble(
        scenario_id = sid,
        backend = be,
        check = "convergence",
        status = "pass",
        detail = "no runs (backend not installed or filtered)"
      )
      next
    }

    fail_rate <- 1 - as.numeric(sc_row$convergence_rate)
    if (!is.finite(fail_rate)) {
      fail_rate <- 1
    }

    st_conv <- if (sc_row$n_converged < min_conv_i) {
      "fail"
    } else if (fail_rate > max_fail_i) {
      "fail"
    } else {
      "pass"
    }
    detail_conv <- sprintf(
      "n_converged=%g, rate=%.3f, max_fail=%.3f",
      sc_row$n_converged, sc_row$convergence_rate, max_fail_i
    )
    out[[length(out) + 1L]] <- tibble::tibble(
      scenario_id = sid,
      backend = be,
      check = "convergence",
      status = if (warn_only && st_conv == "fail") "warn" else st_conv,
      detail = detail_conv
    )

    n_cov <- sc_row$n_coverage_defined
    if (is.finite(n_cov) && n_cov > 0L && is.finite(sc_row$mean_coverage)) {
      st_cov <- if (sc_row$mean_coverage < min_cov_i) "fail" else "pass"
      out[[length(out) + 1L]] <- tibble::tibble(
        scenario_id = sid,
        backend = be,
        check = "coverage",
        status = if (warn_only && st_cov == "fail") "warn" else st_cov,
        detail = sprintf("mean_cov=%.3f min=%.3f", sc_row$mean_coverage, min_cov_i)
      )
    }
  }
  dplyr::bind_rows(out)
}

#' Exit status helper: 0 pass, 1 fail
#' @keywords internal
harness_threshold_exit_status <- function(checks_df) {
  if (nrow(checks_df) == 0L) {
    return(0L)
  }
  if (any(checks_df$status == "fail")) {
    return(1L)
  }
  0L
}
