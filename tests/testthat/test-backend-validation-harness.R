test_that("harness scenario manifest is stable", {
  m <- harness_scenario_manifest()
  expect_true(all(c("scenario_id", "scenario_type", "tier_min") %in% names(m)))
  expect_true("mlm_power_ri" %in% m$scenario_id)
  expect_true("kfas_local_level" %in% m$scenario_id)
})

test_that("harness_expand_runs respects tier and backends", {
  m <- harness_scenario_manifest()
  g <- harness_expand_runs(m, "smoke", c("lme4", "kfas"))
  expect_true(all(g$backend %in% c("lme4", "kfas")))
  expect_true(any(g$scenario_id == "mlm_power_ri" & g$backend == "lme4"))
  expect_true(any(g$scenario_id == "mlm_lag_x" & g$backend == "lme4"))
  expect_true(any(g$scenario_id == "kfas_local_level" & g$backend == "kfas"))
  g2 <- harness_expand_runs(m, "smoke", "lme4")
  expect_false(any(g2$backend == "kfas"))
  g3 <- harness_expand_runs(m, "nightly", c("ctsem"))
  expect_true(any(g3$scenario_id == "ctsem_univariate"))
})

test_that("harness_parse_cli_args parses flags", {
  a <- harness_parse_cli_args(c("--tier", "nightly", "--n-sim", "5", "--seed", "1"))
  expect_equal(a$tier, "nightly")
  expect_equal(a$n_sim, 5L)
  expect_equal(a$seed, 1L)
})

test_that("summarize_validation_metrics and thresholds are deterministic", {
  est <- c(0.3, 0.4, 0.36)
  truth <- 0.35
  bias <- est - truth
  raw <- tibble::tibble(
    run_id = "r1",
    git_sha = "abc",
    tier = "smoke",
    scenario_id = "mlm_power_ri",
    backend = "lme4",
    replicate_id = 1:3,
    truth = truth,
    estimate = est,
    bias = bias,
    squared_error = bias^2,
    std_error = c(0.1, 0.1, 0.1),
    ci_low = est - 0.2,
    ci_high = est + 0.2,
    covered = c(TRUE, TRUE, TRUE),
    nominal_level = 0.95,
    elapsed_sec = c(1, 1, 1),
    converged = TRUE,
    singular = FALSE,
    fit_error = NA_character_,
    n_guardrails = 0L,
    guardrail_rule_ids = "",
    skipped_reason = ""
  )
  s <- summarize_validation_metrics(raw)
  expect_equal(nrow(s), 1L)
  expect_true(is.finite(s$convergence_rate))
  expect_true(s$mean_coverage <= 1)

  thr <- list(
    defaults = list(min_coverage = 0.5, max_convergence_failure_rate = 0.5, min_n_converged = 1L),
    by_backend = list(),
    by_scenario = list()
  )
  chk <- harness_evaluate_thresholds(s, thr)
  expect_true(all(chk$status %in% c("pass", "warn", "fail")))
  expect_equal(harness_threshold_exit_status(chk), 0L)
})

test_that("harness_evaluate_thresholds fails on bad convergence", {
  s <- tibble::tibble(
    tier = "smoke",
    scenario_id = "mlm_power_ri",
    backend = "lme4",
    n_replicates = 5L,
    n_skipped = 0L,
    n_install_skipped = 0L,
    n_ran = 5L,
    n_converged = 0L,
    convergence_rate = 0,
    mean_bias = NA_real_,
    rmse = NA_real_,
    mean_coverage = NA_real_,
    n_coverage_defined = 0L,
    calibration_gap = NA_real_,
    mean_elapsed_sec = 1,
    mean_n_guardrails = 0
  )
  thr <- list(
    defaults = list(min_coverage = 0.5, max_convergence_failure_rate = 0.1, min_n_converged = 1L),
    by_backend = list(),
    by_scenario = list()
  )
  chk <- harness_evaluate_thresholds(s, thr)
  expect_true(any(chk$status == "fail"))
  expect_equal(harness_threshold_exit_status(chk), 1L)
})

test_that("harness smoke benchmark runs for lme4 when available", {
  res <- harness_run_benchmark(
    tier = "smoke",
    backends = "lme4",
    n_sim = 2L,
    seed = 4242L,
    run_id = "test-smoke",
    git_sha = "testsha"
  )
  expect_true(nrow(res$raw) >= 2L)
  expect_true(all(c("estimate", "converged", "scenario_id", "backend") %in% names(res$raw)))
  expect_true(all(res$raw$backend == "lme4"))
})

test_that("harness_read_thresholds_json reads package thresholds", {
  skip_if_not_installed("jsonlite")
  p <- system.file("benchmarks", "thresholds-smoke.json", package = "tidyILD")
  skip_if(!nzchar(p), "installed package has no inst/benchmarks (use load_all)")
  thr <- harness_read_thresholds_json(p)
  expect_type(thr, "list")
  expect_true(!is.null(thr$defaults))
})
