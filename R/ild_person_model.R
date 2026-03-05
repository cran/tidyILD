#' Fit a model separately per person (N-of-1 / idiographic)
#'
#' Splits the ILD by person and fits the same formula (e.g. \code{lm}) within each.
#' Returns a tibble of person-level estimates for teaching, N-of-1 analysis, or
#' inspecting heterogeneity. Use [ild_person_distribution()] to visualize the
#' distribution of estimates across persons.
#'
#' @param formula A formula (e.g. \code{y ~ x}). Used for each person's \code{lm}.
#' @param data An ILD object (see [is_ild()]).
#' @param method Character. Currently only \code{"lm"} (default).
#' @param min_obs Integer. Minimum observations per person to fit (default 2).
#'   Persons with fewer are omitted or get NA rows.
#' @return A tibble with columns \code{.ild_id} (or the id column name from metadata),
#'   \code{term}, \code{estimate}, \code{std_error}, \code{p_value}, and optionally
#'   \code{sigma}, \code{n_obs}. One row per person per term (long format).
#' @export
#' @examples
#' d <- ild_simulate(n_id = 5, n_obs_per = 8, seed = 1)
#' x <- ild_prepare(d, id = "id", time = "time")
#' pm <- ild_person_model(y ~ 1, x)
#' ild_person_distribution(pm, term = "(Intercept)")
ild_person_model <- function(formula, data, method = c("lm"), min_obs = 2L) {
  validate_ild(data)
  method <- match.arg(method)
  min_obs <- as.integer(min_obs)[1L]
  if (min_obs < 1L) min_obs <- 1L
  id_col <- ".ild_id"
  ids <- unique(data[[id_col]])
  out_rows <- list()
  for (i in seq_along(ids)) {
    id_val <- ids[i]
    sub <- data[data[[id_col]] == id_val, ]
    if (nrow(sub) < min_obs) next
    fit <- tryCatch(stats::lm(formula, data = sub), error = function(e) NULL)
    if (is.null(fit)) next
    co <- summary(fit)$coefficients
    if (is.null(co) || nrow(co) == 0) next
    for (j in seq_len(nrow(co))) {
      out_rows[[length(out_rows) + 1L]] <- list(
        .ild_id = id_val,
        term = rownames(co)[j],
        estimate = co[j, "Estimate"],
        std_error = co[j, "Std. Error"],
        p_value = co[j, "Pr(>|t|)"],
        sigma = stats::sigma(fit),
        n_obs = nrow(sub)
      )
    }
  }
  if (length(out_rows) == 0) {
    return(tibble::tibble(
      .ild_id = data[[id_col]][0],
      term = character(),
      estimate = double(),
      std_error = double(),
      p_value = double(),
      sigma = double(),
      n_obs = integer()
    ))
  }
  tibble::as_tibble(do.call(rbind, lapply(out_rows, as.data.frame, stringsAsFactors = FALSE)))
}

#' Plot distribution of person-level estimates from ild_person_model
#'
#' Draws a histogram or density of the selected term's estimates across persons.
#' Useful to visualize heterogeneity (e.g. distribution of slopes or intercepts).
#'
#' @param person_fit Tibble returned by [ild_person_model()] (columns \code{term}, \code{estimate}, etc.).
#' @param term Character. Which term to plot (e.g. \code{"(Intercept)"} or a covariate name).
#'   If \code{NULL}, the first term in the table is used.
#' @param type Character. \code{"histogram"} (default) or \code{"density"}.
#' @return A ggplot object.
#' @export
ild_person_distribution <- function(person_fit, term = NULL, type = c("histogram", "density")) {
  type <- match.arg(type)
  if (!inherits(person_fit, "data.frame") || !"estimate" %in% names(person_fit)) {
    stop("person_fit must be a tibble from ild_person_model() with an 'estimate' column.", call. = FALSE)
  }
  if (!"term" %in% names(person_fit)) {
    stop("person_fit must have a 'term' column.", call. = FALSE)
  }
  terms_avail <- unique(person_fit$term)
  if (length(terms_avail) == 0) stop("No terms in person_fit.", call. = FALSE)
  if (is.null(term)) term <- terms_avail[1]
  if (!term %in% terms_avail) stop("term '", term, "' not found in person_fit.", call. = FALSE)
  sub <- person_fit[person_fit$term == term, ]
  est <- sub$estimate[!is.na(sub$estimate)]
  if (length(est) == 0) stop("No non-NA estimates for term '", term, "'.", call. = FALSE)
  df <- data.frame(estimate = est)
  if (type == "histogram") {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$estimate)) +
      ggplot2::geom_histogram(bins = min(30L, max(5L, length(est) %/% 2)), fill = "gray70", color = "gray40") +
      ggplot2::labs(x = "Estimate", y = "Count", title = paste0("Distribution of ", term, " (by person)")) +
      ggplot2::theme_minimal()
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$estimate)) +
      ggplot2::geom_density(fill = "gray70", alpha = 0.6) +
      ggplot2::labs(x = "Estimate", y = "Density", title = paste0("Distribution of ", term, " (by person)")) +
      ggplot2::theme_minimal()
  }
  p
}
