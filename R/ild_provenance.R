# Provenance helpers: attr(x, "tidyILD")$provenance (data) and attr(obj, "ild_provenance") (analysis)
# Uses .TIDYILD_ATTR from ild-class.R (Collate: ild_provenance after ild-class)

# Attribute name for analysis provenance on model/diagnostics/power/etc. objects
.ILD_PROVENANCE_ATTR <- "ild_provenance"

#' Append a step to the ILD provenance (internal)
#'
#' Ensures \code{attr(x, "tidyILD")$provenance} exists, then appends one step
#' record. If the tidyILD bundle is missing, builds a minimal one from
#' \code{ild_*} attributes. Modifies \code{x} in place by setting the attribute.
#'
#' @param x An ILD object (see [is_ild()]).
#' @param step Character; step name (e.g. \code{"ild_prepare"}, \code{"ild_center"}).
#' @param args Named list of user-facing arguments (serializable).
#' @param outputs Optional named list of step outputs (e.g. \code{created}, \code{n_id}).
#' @return \code{x} (invisibly), with \code{attr(x, "tidyILD")$provenance$steps} updated.
#' @noRd
ild_add_step <- function(x, step, args, outputs = NULL) {
  bundle <- attr(x, .TIDYILD_ATTR, exact = TRUE)
  if (is.null(bundle)) {
    bundle <- list(
      id_col = attr(x, "ild_id", exact = TRUE),
      time_col = attr(x, "ild_time", exact = TRUE),
      tz = "UTC",
      gap_threshold = attr(x, "ild_gap_threshold", exact = TRUE),
      created = NA,
      spacing = attr(x, "ild_spacing", exact = TRUE)
    )
  }
  if (is.null(bundle$provenance)) {
    pv <- tryCatch(
      as.character(utils::packageVersion("tidyILD")),
      error = function(e) "0.0.0"
    )
    bundle$provenance <- list(version = pv, schema_version = "1", object_type = "ild_data", steps = list())
  }
  step_id <- as.character(length(bundle$provenance$steps) + 1L)
  step_record <- list(
    step_id = step_id,
    step = step,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    args = args,
    outputs = outputs
  )
  bundle$provenance$steps <- c(bundle$provenance$steps, list(step_record))
  attr(x, .TIDYILD_ATTR) <- bundle
  invisible(x)
}

#' Flatten provenance into an ordered list of steps (internal)
#' Data steps first (from source_data_provenance or prov$steps), then analysis_steps.
#' @param prov Return value of ild_provenance(x).
#' @return List of step records (each with step, args, outputs; step_id if present).
#' @noRd
ild_flatten_provenance <- function(prov) {
  if (is.null(prov)) return(list())
  out <- list()
  if (!is.null(prov$source_data_provenance) && length(prov$source_data_provenance$steps) > 0) {
    out <- c(out, prov$source_data_provenance$steps)
  }
  if (!is.null(prov$steps) && length(prov$steps) > 0) {
    out <- c(out, prov$steps)
  }
  if (!is.null(prov$analysis_steps) && length(prov$analysis_steps) > 0) {
    out <- c(out, prov$analysis_steps)
  }
  out
}

#' Get raw provenance object (internal)
#' @param x An ILD object.
#' @return \code{attr(x, "tidyILD")$provenance} or \code{NULL}.
#' @noRd
ild_get_history <- function(x) {
  bundle <- attr(x, .TIDYILD_ATTR, exact = TRUE)
  if (is.null(bundle)) return(NULL)
  bundle$provenance
}

#' Map analysis step name to object_type (internal)
#' @noRd
ild_provenance_object_type <- function(step) {
  switch(
    step,
    ild_lme = "ild_model",
    ild_brms = "ild_model",
    ild_tvem = "ild_model",
    ild_crosslag = "ild_model",
    ild_panel_lag_prepare = "ild_data",
    ild_ipw_refit = "ild_model",
    ild_msm_bootstrap = "ild_msm_bootstrap",
    ild_msm_fit = "ild_msm_fit",
    ild_msm_recovery = "ild_msm_recovery",
    ild_ctsem = "ild_model",
    ild_kfas = "ild_model",
    ild_diagnostics = "ild_diagnostics",
    ild_power = "ild_power",
    ild_missing_model = "ild_missingness",
    "ild_other"
  )
}

#' Build analysis provenance for a non-data object (internal)
#'
#' Used by ild_lme(), ild_diagnostics(), ild_tvem(), ild_power(), ild_missing_model()
#' to attach a provenance record to the returned fit/list. Structure:
#' \code{list(version, object_type, source_data_provenance, analysis_steps)}.
#'
#' @param source ILD data (for data provenance snapshot), or an object with
#'   \code{attr(., "ild_provenance")} (e.g. fit from ild_lme), or \code{NULL}.
#' @param step Character; analysis step name (e.g. \code{"ild_lme"}, \code{"ild_diagnostics"}).
#' @param args Named list of serializable arguments.
#' @param outputs Optional named list of step outputs.
#' @return List with \code{version}, \code{object_type}, \code{source_data_provenance}, \code{analysis_steps}.
#' @noRd
ild_new_analysis_provenance <- function(source = NULL, step, args, outputs = NULL) {
  pv <- tryCatch(
    as.character(utils::packageVersion("tidyILD")),
    error = function(e) "0.0.0"
  )
  object_type <- ild_provenance_object_type(step)
  step_id <- "1"
  if (!is.null(source)) {
    existing <- attr(source, .ILD_PROVENANCE_ATTR, exact = TRUE)
    if (!is.null(existing) && is.list(existing) && length(existing$analysis_steps) > 0) {
      step_id <- as.character(length(existing$analysis_steps) + 1L)
    }
  }
  step_record <- list(
    step_id = step_id,
    step = step,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    args = args,
    outputs = outputs
  )
  schema_version <- "1"
  if (is.null(source)) {
    return(list(
      version = pv,
      schema_version = schema_version,
      object_type = object_type,
      source_data_provenance = NULL,
      analysis_steps = list(step_record)
    ))
  }
  existing <- attr(source, .ILD_PROVENANCE_ATTR, exact = TRUE)
  if (!is.null(existing) && is.list(existing)) {
    return(list(
      version = pv,
      schema_version = schema_version,
      object_type = object_type,
      source_data_provenance = existing$source_data_provenance,
      analysis_steps = c(existing$analysis_steps, list(step_record))
    ))
  }
  data_prov <- ild_get_history(source)
  list(
    version = pv,
    schema_version = schema_version,
    object_type = object_type,
    source_data_provenance = data_prov,
    analysis_steps = list(step_record)
  )
}

#' Return the raw provenance object
#'
#' For ILD data: returns \code{attr(x, "tidyILD")$provenance} (version + steps from
#' preprocessing). For analysis objects (e.g. fits from [ild_lme()], [ild_diagnostics()],
#' [ild_tvem()], [ild_power()], [ild_missing_model()]): returns \code{attr(x, "ild_provenance")},
#' which has \code{source_data_provenance} (snapshot of data provenance) and
#' \code{analysis_steps} (list of analysis step records).
#'
#' @param x An ILD object (see [is_ild()]) or an analysis object with \code{ild_provenance} attribute.
#' @return For data: list with \code{version} and \code{steps}. For analysis: list with
#'   \code{version}, \code{source_data_provenance}, \code{analysis_steps}. \code{NULL} if none.
#' @export
ild_provenance <- function(x) {
  ap <- attr(x, .ILD_PROVENANCE_ATTR, exact = TRUE)
  if (!is.null(ap)) return(ap)
  ild_get_history(x)
}

#' Print a human-readable log of preprocessing or analysis steps
#'
#' For ILD data, displays data provenance steps. For analysis objects (e.g. from
#' [ild_lme()], [ild_diagnostics()]), displays source data provenance (if any)
#' and analysis steps. Use [ild_provenance()] to get the raw structured object.
#'
#' @param x An ILD object (see [is_ild()]) or an analysis object with \code{ild_provenance}.
#' @return The provenance object (from [ild_provenance()]) invisibly, or a message if none.
#' @export
ild_history <- function(x) {
  prov <- ild_provenance(x)
  if (is.null(prov)) {
    message("No provenance recorded for this object.")
    return(invisible(NULL))
  }
  if (!is.null(prov$analysis_steps)) {
    cat("ILD analysis provenance (tidyILD ", prov$version, ")\n", sep = "")
    if (!is.null(prov$source_data_provenance) && length(prov$source_data_provenance$steps) > 0) {
      cat("  [Source data steps: ", length(prov$source_data_provenance$steps), "]\n", sep = "")
    }
    for (i in seq_along(prov$analysis_steps)) {
      s <- prov$analysis_steps[[i]]
      cat("  ", i, ". ", s$step, " @ ", s$timestamp, "\n", sep = "")
      if (length(s$args) > 0) {
        cat("      args: ", paste(names(s$args), collapse = ", "), "\n", sep = "")
      }
      if (length(s$outputs) > 0 && !is.null(s$outputs)) {
        cat("      outputs: ", paste(names(s$outputs), collapse = ", "), "\n", sep = "")
      }
    }
    return(invisible(prov))
  }
  cat("ILD provenance (tidyILD ", prov$version, ")\n", sep = "")
  for (i in seq_along(prov$steps)) {
    s <- prov$steps[[i]]
    cat("  ", i, ". ", s$step, " @ ", s$timestamp, "\n", sep = "")
    if (length(s$args) > 0) {
      cat("      args: ", paste(names(s$args), collapse = ", "), "\n", sep = "")
    }
    if (length(s$outputs) > 0 && !is.null(s$outputs)) {
      cat("      outputs: ", paste(names(s$outputs), collapse = ", "), "\n", sep = "")
    }
  }
  invisible(prov)
}

#' Export provenance to a JSON or YAML file
#'
#' Writes the full provenance structure (data or analysis) to a file for
#' reproducibility supplements, preregistration appendices, or lab archiving.
#' Requires the \pkg{jsonlite} package for JSON or \pkg{yaml} for YAML.
#'
#' @param x An ILD object or an analysis object with [ild_provenance()].
#' @param path Character. File path to write (e.g. \code{"analysis_provenance.json"}).
#' @param format Character. \code{"auto"} (default) infers from file extension
#'   (\code{.json} -> JSON, \code{.yaml} or \code{.yml} -> YAML). Use \code{"json"}
#'   or \code{"yaml"} to force a format.
#' @return The \code{path} invisibly, after writing the file.
#' @export
ild_export_provenance <- function(x, path, format = c("auto", "json", "yaml")) {
  format <- match.arg(format)
  if (missing(path) || is.null(path) || !is.character(path) || length(path) != 1L || is.na(path) || nchar(path) == 0) {
    stop("path must be a non-empty character string (file path).", call. = FALSE)
  }
  prov <- ild_provenance(x)
  if (is.null(prov)) {
    stop("Object has no provenance to export.", call. = FALSE)
  }
  if (format == "auto") {
    ext <- tolower(sub("^.*\\.([^.]+)$", "\\1", path))
    if (ext %in% c("yml", "yaml")) format <- "yaml" else format <- "json"
  }
  if (format == "json") {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package 'jsonlite' is required for JSON export. Install with install.packages(\"jsonlite\").", call. = FALSE)
    }
    txt <- jsonlite::toJSON(prov, auto_unbox = TRUE, pretty = TRUE, null = "null")
    writeLines(txt, path, useBytes = TRUE)
  } else {
    if (!requireNamespace("yaml", quietly = TRUE)) {
      stop("Package 'yaml' is required for YAML export. Install with install.packages(\"yaml\").", call. = FALSE)
    }
    txt <- yaml::as.yaml(prov)
    writeLines(txt, path, useBytes = TRUE)
  }
  invisible(path)
}
