# Stable KFAS test fixtures: DGPs, named seeds, shared expectations
# Loaded automatically by testthat.

# Chosen so the Gaussian local-level DGP does not land on a pathological ML optimum.
KFAS_TEST_SEED_RECOVERY <- 42L
KFAS_TEST_SEED_CONTRACT <- 9001L

#' Simulate Gaussian local level + noise; return [ild_prepare()]d data and true level.
#' Use moderate \code{Q}, \code{H} (e.g. 1) so ML variance recovery is numerically stable.
#' @keywords internal
kfas_dgp_local_level <- function(n, Q = 1, H = 1, seed) {
  set.seed(seed)
  Q <- as.numeric(Q)[1L]
  H <- as.numeric(H)[1L]
  alpha <- numeric(n)
  alpha[1L] <- stats::rnorm(1L, 0, sqrt(Q))
  if (n >= 2L) {
    for (t in 2L:n) {
      alpha[t] <- alpha[t - 1L] + stats::rnorm(1L, 0, sqrt(Q))
    }
  }
  y <- alpha + stats::rnorm(n, 0, sqrt(H))
  raw <- data.frame(
    id = 1L,
    time = seq_len(n),
    y = y,
    stringsAsFactors = FALSE
  )
  list(
    data = ild_prepare(raw, id = "id", time = "time"),
    true_level = alpha,
    Q = Q,
    H = H
  )
}

#' Expect core KFAS bundle slots used by autoplot and reports.
#' @keywords internal
expect_kfas_bundle_core <- function(b) {
  expect_s3_class(b, "ild_diagnostics_bundle")
  expect_no_error(validate_ild_diagnostics_bundle(b))
  expect_identical(b$meta$engine, "KFAS")
  expect_true(is.list(b$fit))
  expect_true(!is.null(b$fit$engine))
  expect_true("log_likelihood" %in% names(b$fit))
  expect_true(is.list(b$fit$optimizer))
  # Residual ACF list shape (when innovations computed)
  ac <- b$residual$acf
  if (!is.null(ac) && is.list(ac)) {
    expect_true(!is.null(ac$lag))
    expect_true(!is.null(ac$acf))
    expect_true(length(ac$lag) == length(ac$acf))
  }
}

#' Single-subject ILD with highly variable observation intervals (high IQR/median on .ild_dt).
#' @keywords internal
kfas_fixture_irregular_intervals <- function(n = 55L, seed = 91001L) {
  set.seed(seed)
  gaps <- rep(c(1, 10), length.out = max(1L, n - 1L))
  tvec <- cumsum(c(0, gaps))
  y <- stats::rnorm(length(tvec))
  raw <- data.frame(id = 1L, time = tvec, y = y, stringsAsFactors = FALSE)
  ild_prepare(raw, id = "id", time = "time")
}

#' Outcome with several NA runs and enough NA share to trigger missing-segment guardrail.
#' @keywords internal
kfas_fixture_segmented_na_outcome <- function(seed = 91002L) {
  set.seed(seed)
  n <- 50L
  y <- stats::rnorm(n)
  # Four separate NA runs; 8 missing -> 16% > 10%
  y[c(3L, 12L, 13L, 22L, 23L, 24L, 38L, 45L)] <- NA
  raw <- data.frame(id = 1L, time = seq_len(n), y = y, stringsAsFactors = FALSE)
  ild_prepare(raw, id = "id", time = "time")
}
