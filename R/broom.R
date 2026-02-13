#' Tidy and augment ild_lme fits with broom.mixed
#'
#' These S3 methods delegate to [broom.mixed::tidy()] and [broom.mixed::augment()]
#' on the underlying model object so that ild_lme fits work in tidy workflows.
#' Package \pkg{broom.mixed} must be attached (e.g. \code{library(broom.mixed)}).
#'
#' @param x A fitted model from [ild_lme()].
#' @param ... Passed to \code{broom.mixed::tidy()} or \code{broom.mixed::augment()}.
#' @return Same as the corresponding broom.mixed method.
#' @name broom_ild_lme
NULL

#' @rdname broom_ild_lme
#' @export
tidy.ild_lme <- function(x, ...) {
  if (!requireNamespace("broom.mixed", quietly = TRUE)) {
    stop("Package 'broom.mixed' is required for tidy.ild_lme(). Install it and run library(broom.mixed).", call. = FALSE)
  }
  class(x) <- setdiff(class(x), "ild_lme")
  broom.mixed::tidy(x, ...)
}

#' @rdname broom_ild_lme
#' @export
augment.ild_lme <- function(x, ...) {
  if (!requireNamespace("broom.mixed", quietly = TRUE)) {
    stop("Package 'broom.mixed' is required for augment.ild_lme(). Install it and run library(broom.mixed).", call. = FALSE)
  }
  class(x) <- setdiff(class(x), "ild_lme")
  broom.mixed::augment(x, ...)
}
