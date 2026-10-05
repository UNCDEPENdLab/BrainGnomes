#' Collect a study QC inventory from existing diagnostics
#'
#' Assemble a read-only snapshot of acquisitions, expected native derivatives,
#' workflow attempts, output-manifest checks, and available QC evidence. No
#' image processing, human review, or inclusion/exclusion decisions are run.
#'
#' @param input A project configuration object, YAML file, or project directory.
#' @param subjects Optional data frame with character `sub_id` and optional
#'   `ses_id` columns. Adds expected participants/sessions, including those whose
#'   data have not arrived. Discovered and tracked subjects are always retained.
#' @param refresh Query the scheduler through [inspect_project()] for active
#'   jobs. Defaults to `FALSE`; the snapshot then uses recorded database states.
#' @return A `bg_qc_inventory` list with `metadata`, `acquisitions`, `inventory`
#'   (one row per derivative/stage/stream), `workflow`, `metrics`, `evidence`,
#'   `checks`, `jobs`, `attempts`, `manifest_files`, `active`, `reconciliation`,
#'   and `issues`. Missing diagnostics are explicit; an available file is not
#'   evidence of scientific quality or a successful producing attempt.
#' @details Expected postprocessing paths are resolved from existing fMRIPrep
#'   inputs and the current stream configuration; ROI targets follow those
#'   postprocessing paths. Before upstream inputs exist, configured work remains
#'   visible in `workflow`, without guessing output spaces or echo combinations.
#'   `acquisitions` inventories raw BOLD files separately, including each echo.
#'   Historical jobs and exact manifest file associations remain available.
#'   Subject/session workflow states provide context rather than assigning a
#'   producing job to every acquisition. File-manifest comparisons use size and
#'   modification time, not a content hash. Diagnostics have their own file
#'   timestamps and can outlive the derivative they describe.
#' @examples
#' \dontrun{
#' qc <- collect_qc_inventory("/path/to/project")
#' subset(qc$inventory, output_status == "missing")
#' write_qc_inventory(qc, "/path/to/qc-snapshot")
#' render_qc_dashboard(qc, "/path/to/qc-dashboard")
#' }
#' @export
collect_qc_inventory <- function(input = getwd(), subjects = NULL, refresh = FALSE) {
  started_at <- qc_time(Sys.time())
  scfg <- project_config_from_input(input)
  checkmate::assert_data_frame(subjects, null.ok = TRUE)
  inspection <- inspect_project(scfg, refresh = refresh)
  state <- new.env(parent = emptyenv())
  state$issues <- list()
  state$evidence <- list()
  state$metrics <- list()
  state$checks <- list()
  state$regions <- list()

  # Scan known output roots once. Never read image arrays or invoke a pipeline.
  roots <- scfg$metadata
  raw <- qc_list_files(roots$bids_directory, "_bold\\.nii(\\.gz)?$")
  if (length(raw)) {
    # A BIDS root can contain derivatives/ and sourcedata/. Only subject folders
    # directly beneath the raw root contribute to the raw acquisition count.
    root_prefix <- paste0(sub("/+$", "", normalizePath(roots$bids_directory,
      winslash = "/", mustWork = TRUE)), "/")
    relative <- substring(raw, nchar(root_prefix) + 1L)
    raw <- raw[grepl("^sub-[A-Za-z0-9]+/", relative)]
  }
  preproc <- qc_list_files(roots$fmriprep_directory, "_bold\\.nii(\\.gz)?$")
  audits <- qc_list_files(roots$log_directory, "_postproc-validation\\.json$")
  acquisitions <- qc_entities(raw)
  acquisitions$source_file <- raw

  inventory <- qc_expected_derivatives(scfg, preproc, state)
  # MRIQC IQM JSONs are observed products with their own acquisition identities.
  # Their metrics remain distinct from fMRIPrep-derived motion summaries.
  for (path in qc_list_files(roots$mriqc_directory, "_bold\\.json$")) {
    inventory <- rbind(inventory, qc_derivative_row(path, "mriqc",
      NA_character_, NA_character_, NA_character_, NA_character_, "discovered"))
  }
  inventory$record_id <- sprintf("derivative-%06d", seq_len(nrow(inventory)))
  info <- file.info(inventory$derivative_file)
  inventory$output_status <- ifelse(is.na(info$size), "missing",
    ifelse(info$isdir | info$size == 0, "empty_or_invalid", "available"))
  inventory$size_bytes <- as.numeric(info$size)
  inventory$modified_at <- qc_time(info$mtime)
  inventory$validation_status <- rep("unavailable", nrow(inventory))
  inventory$diagnostics_status <- rep("unavailable", nrow(inventory))
  inventory$attention <- inventory$output_status != "available"
  # Shared destination paths can result from colliding stream configurations.
  # Preserve both expectations and expose the ambiguity instead of merging them.
  derivative_keys <- qc_path_key(inventory$derivative_file)
  shared <- duplicated(derivative_keys) | duplicated(derivative_keys, fromLast = TRUE)
  inventory$association_status <- ifelse(shared, "shared_destination", "unique_destination")
  inventory$attention <- inventory$attention | shared
  for (path in unique(inventory$derivative_file[shared])) {
    qc_issue(state, path, "Multiple configured records share this destination; stream/source attribution is ambiguous.")
  }

  for (i in seq_len(nrow(inventory))) {
    row <- inventory[i, , drop = FALSE]
    record <- row$record_id
    qc_add_evidence(state, record, "derivative", row$derivative_file)
    qc_add_evidence(state, record, "source", row$source_file)
    if (row$stage == "mriqc") {
      inventory$diagnostics_status[i] <- qc_collect_mriqc(state, record, row$derivative_file)
    } else if (row$stage == "fmriprep") {
      # Only report upstream FD from its original, headed fMRIPrep TSV. Native
      # confounds may have been filtered/demeaned and are not interchangeable.
      confounds <- qc_find_confounds(row$derivative_file)
      inventory$diagnostics_status[i] <- qc_collect_motion(state, record, confounds)
    } else if (row$stage == "postprocess") {
      audit_name <- sub("\\.nii(\\.gz)?$", "_postproc-validation.json",
        basename(row$derivative_file))
      candidates <- audits[basename(audits) == audit_name]
      inventory$validation_status[i] <- qc_collect_audit(
        state, record, candidates, row$derivative_file
      )
      censor <- get_censor_file(as.list(extract_bids_info(row$derivative_file)))
      inventory$diagnostics_status[i] <- qc_collect_censor(state, record, censor)
    } else if (row$stage == "extract_rois") {
      diagnostic <- sub("_(timeseries|connectivity)\\.tsv$", "_roidiagnostics.tsv",
        row$derivative_file)
      # Correlation entities occur only in connectivity filenames, not diagnostics.
      diagnostic <- sub("_(correlation|cor)-[^_]+", "", diagnostic)
      inventory$diagnostics_status[i] <- qc_collect_roi(state, record, diagnostic)
    }
    inventory$attention[i] <- inventory$attention[i] ||
      inventory$validation_status[i] %in% c("failed", "error", "invalid", "ambiguous") ||
      inventory$diagnostics_status[i] %in% c("invalid", "roi_loss")
  }

  # Attach upstream HTML at its actual subject/acquisition scope. Evidence is
  # linked by compatible entities, never by substring subject matching.
  reports <- c(qc_list_files(roots$fmriprep_directory, "\\.html$"),
    qc_list_files(roots$mriqc_directory, "\\.html$"))
  for (path in unique(reports)) {
    ids <- qc_entities(path)
    matched <- qc_compatible_rows(inventory, ids)
    if (!length(matched)) qc_add_evidence(state, NA_character_, "upstream_report", path)
    for (i in matched) qc_add_evidence(state, inventory$record_id[i], "upstream_report", path)
  }
  # Portable provenance is linked when present, without requiring its database
  # or assuming that an adjacent record necessarily describes current content.
  for (i in seq_len(nrow(inventory))) {
    sidecar <- sub("\\.nii(\\.gz)?$|\\.tsv$", ".json", inventory$derivative_file[i])
    if (file.exists(sidecar)) qc_add_evidence(state, inventory$record_id[i], "sidecar", sidecar)
  }

  workflow <- qc_workflow(scfg, inspection, acquisitions, inventory, subjects, state)
  inventory <- qc_attach_workflow(inventory, workflow)
  manifests <- qc_manifest_files(inspection$jobs, state)
  evidence <- qc_bind_rows(state$evidence, qc_empty_evidence())
  metrics <- qc_bind_rows(state$metrics, qc_empty_metrics())
  # Put convenient numeric summaries in the main TSV while retaining units,
  # source filenames, and denominators in the authoritative long metric table.
  metric_names <- c("mean_fd", "mriqc_fd_mean", "mriqc_tsnr", "mriqc_dvars_std", "mriqc_gcor",
    "censor_retained_percent", "censor_retained_frames", "roi_retained_percent",
    "roi_lost_count", "roi_mean_usable_percent")
  for (name in metric_names) {
    values <- metrics[metrics$metric == name, , drop = FALSE]
    inventory[[name]] <- values$value[match(inventory$record_id, values$record_id)]
  }
  checks <- qc_bind_rows(state$checks, qc_empty_checks())
  issues <- qc_bind_rows(state$issues, qc_empty_issues())
  regions <- qc_bind_rows(state$regions, qc_empty_regions())
  structure(list(
    metadata = list(schema_version = "brain-gnomes-qc-v1", snapshot_id = uuid::UUIDgenerate(),
      created_at = qc_time(Sys.time()), project = inspection$project,
      collection_started_at = started_at, tracking_retrieved_at = qc_time(inspection$retrieved_at),
      project_directory = inspection$project_directory,
      log_directory = inspection$log_directory,
      braingnomes_version = as.character(utils::packageVersion("BrainGnomes")),
      scheduler_refreshed = isTRUE(refresh),
      expectation_basis = "Current configuration and discovered inputs; workflow includes expected subjects."),
    acquisitions = acquisitions, inventory = inventory, workflow = workflow,
    metrics = metrics, regions = regions, evidence = unique(evidence), checks = checks,
    jobs = as.data.frame(inspection$jobs), attempts = inspection$attempts,
    manifest_files = manifests, active = inspection$active,
    reconciliation = inspection$reconciliation, issues = issues
  ), class = "bg_qc_inventory")
}

#' Format snapshot timestamps consistently, retaining missing values
#' @keywords internal
#' @noRd
qc_time <- function(x) {
  format(x, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
}

#' List files only beneath an explicitly configured directory
#' @keywords internal
#' @noRd
qc_list_files <- function(directory, pattern) {
  if (!checkmate::test_string(directory) || !dir.exists(directory)) return(character())
  normalizePath(sort(list.files(directory, pattern, recursive = TRUE,
    full.names = TRUE)), winslash = "/", mustWork = FALSE)
}

#' Normalize filesystem paths for identity comparisons
#'
#' @param path Character vector of filesystem paths.
#' @return Character vector with platform aliases and separators normalized.
#' @keywords internal
#' @noRd
qc_path_key <- function(path) {
  if (!length(path)) return(character())
  result <- as.character(path)
  valid <- !is.na(result) & nzchar(result)
  # Audits can describe failed outputs that do not exist yet. Resolve aliases
  # through existing ancestors just as the runtime configuration resolver does.
  result[valid] <- vapply(result[valid], normalize_project_path, character(1),
    USE.NAMES = FALSE)
  # Windows paths are case-insensitive even though string comparisons are not.
  if (.Platform$OS.type == "windows") result[valid] <- tolower(result[valid])
  result
}

#' Parse delimited BIDS identities without collapsing echo or acquisition labels
#' @keywords internal
#' @noRd
qc_entities <- function(paths) {
  tokens <- c(sub_id = "sub", ses_id = "ses", task = "task", acquisition = "acq",
    reconstruction = "rec", direction = "dir", run = "run", echo = "echo",
    part = "part", space = "space", resolution = "res", density = "den",
    hemisphere = "hemi", cohort = "cohort", description = "desc")
  result <- lapply(tokens, function(token) {
    vapply(basename(paths), function(path) {
      matched <- regmatches(path, regexec(paste0("(?:^|_)", token,
        "-([A-Za-z0-9]+)(?=_|\\.|$)"), path, perl = TRUE))[[1L]]
      if (length(matched) < 2L) NA_character_ else matched[2L]
    }, character(1), USE.NAMES = FALSE)
  })
  as.data.frame(result, stringsAsFactors = FALSE)
}

#' Bind typed rows while keeping the empty-table schema
#' @keywords internal
#' @noRd
qc_bind_rows <- function(rows, empty) {
  if (!length(rows)) return(empty)
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

#' Construct the evidence table schema
#' @keywords internal
#' @noRd
qc_empty_evidence <- function() {
  data.frame(record_id = character(), kind = character(), path = character(),
    exists = logical(), size_bytes = numeric(), modified_at = character())
}

#' Construct the metric table schema
#' @keywords internal
#' @noRd
qc_empty_metrics <- function() {
  data.frame(record_id = character(), metric = character(), value = numeric(),
    unit = character(), definition = character(), source_file = character(),
    n_observations = integer(), n_missing = integer())
}

#' Construct the validation-check table schema
#' @keywords internal
#' @noRd
qc_empty_checks <- function() {
  data.frame(record_id = character(), step = character(), status = character(),
    message = character(), source_file = character())
}

#' Construct the regional diagnostic table schema
#' @keywords internal
#' @noRd
qc_empty_regions <- function() {
  data.frame(record_id = character(), roi = character(), proportion_usable = numeric(),
    retained = logical(), exclusion_reason = character(), source_file = character())
}

#' Construct the collection-issue table schema
#' @keywords internal
#' @noRd
qc_empty_issues <- function() {
  data.frame(path = character(), message = character())
}

#' Record a collection problem without hiding the remaining study evidence
#' @keywords internal
#' @noRd
qc_issue <- function(state, path, message) {
  state$issues[[length(state$issues) + 1L]] <- data.frame(
    path = as.character(path), message = as.character(message))
  invisible(NULL)
}

#' Return a scalar character value or an explicit missing value
#' @keywords internal
#' @noRd
qc_scalar <- function(value) {
  if (length(value) != 1L || is.list(value) || is.na(value)) NA_character_ else as.character(value)
}

#' Read diagnostic JSON locally, recording malformed files in the inventory
#' @keywords internal
#' @noRd
qc_read_json <- function(path, state) {
  tryCatch(jsonlite::fromJSON(txt = paste(readLines(path, warn = FALSE), collapse = "\n"),
    simplifyVector = FALSE), error = function(e) {
      qc_issue(state, path, conditionMessage(e))
      NULL
    })
}

#' Register evidence, including paths that were expected but are absent
#' @keywords internal
#' @noRd
qc_add_evidence <- function(state, record, kind, path) {
  if (!checkmate::test_string(path)) return(invisible(NULL))
  info <- file.info(path)
  state$evidence[[length(state$evidence) + 1L]] <- data.frame(
    record_id = record, kind = kind, path = path,
    exists = !is.na(info$size) && !isTRUE(info$isdir),
    size_bytes = as.numeric(info$size), modified_at = qc_time(info$mtime))
  invisible(NULL)
}

#' Add a metric with its definition, units, denominator, and original file
#' @keywords internal
#' @noRd
qc_add_metric <- function(state, record, metric, value, unit, definition,
                          source, n, missing = 0L) {
  state$metrics[[length(state$metrics) + 1L]] <- data.frame(
    record_id = record, metric = metric, value = as.numeric(value), unit = unit,
    definition = definition, source_file = source,
    n_observations = as.integer(n), n_missing = as.integer(missing))
  invisible(NULL)
}

#' Resolve expected postprocessing and ROI filenames using production conventions
#' @keywords internal
#' @noRd
qc_expected_derivatives <- function(scfg, preproc, state) {
  rows <- list()
  # Every discovered fMRIPrep image remains visible, even when native stages
  # are disabled or select only a subset of its spaces/tasks.
  for (path in preproc) rows[[length(rows) + 1L]] <- qc_derivative_row(
    path, "fmriprep", NA_character_, NA_character_, NA_character_, NA_character_, "discovered")
  if (isTRUE(scfg$postprocess$enable)) for (stream in get_postprocess_stream_names(scfg)) {
    cfg <- scfg$postprocess[[stream]]
    if (!checkmate::test_string(cfg$bids_desc) ||
        !checkmate::test_string(scfg$metadata$postproc_directory)) {
      qc_issue(state, paste0("postprocess/", stream), "Cannot resolve output paths: missing bids_desc or postproc_directory.")
      next
    }
    specs <- cfg$input_regex
    if (is.null(specs)) specs <- "desc:preproc suffix:bold"
    patterns <- tryCatch(vapply(specs, construct_bids_regex, character(1)),
      error = function(e) { qc_issue(state, stream, conditionMessage(e)); character() })
    selected <- tryCatch(preproc[vapply(basename(preproc), function(path) {
      any(vapply(patterns, grepl, logical(1), x = path, perl = TRUE))
    }, logical(1))], error = function(e) {
      qc_issue(state, stream, conditionMessage(e)); character()
    })
    for (source in selected) {
      ids <- as.list(extract_bids_info(source))
      target_dir <- file.path(scfg$metadata$postproc_directory, paste0("sub-", ids$subject))
      if (!is.na(ids$session)) target_dir <- file.path(target_dir, paste0("ses-", ids$session))
      target <- construct_bids_filename(utils::modifyList(ids,
        list(directory = target_dir, description = cfg$bids_desc)), full.names = TRUE)
      rows[[length(rows) + 1L]] <- qc_derivative_row(target, "postprocess", stream,
        NA_character_, source, NA_character_, "configured_input")
      if (isTRUE(scfg$extract_rois$enable)) for (ex in get_extract_stream_names(scfg)) {
        ecfg <- scfg$extract_rois[[ex]]
        if (!stream %in% ecfg$input_streams) next
        if (!checkmate::test_string(scfg$metadata$rois_directory)) {
          qc_issue(state, ex, "Cannot resolve ROI output paths: missing rois_directory.")
          next
        }
        for (atlas in ecfg$atlases) {
          atlas_name <- sub("\\.nii(\\.gz)?$", "", basename(atlas))
          bids <- utils::modifyList(as.list(extract_bids_info(target)),
            list(rois = bids_camelcase(atlas_name), ext = ".tsv"))
          suffixes <- if (!isFALSE(ecfg$save_ts)) list(list(suffix = "timeseries")) else list()
          methods <- ecfg$correlation$method
          if (is.null(methods)) methods <- ecfg$cor_method
          for (method in setdiff(methods, "none")) suffixes[[length(suffixes) + 1L]] <- list(
            suffix = "connectivity", correlation = bids_camelcase(method))
          for (spec in suffixes) {
            output <- file.path(scfg$metadata$rois_directory, atlas_name,
              construct_bids_filename(utils::modifyList(bids, spec), full.names = FALSE))
            rows[[length(rows) + 1L]] <- qc_derivative_row(output, "extract_rois", ex,
              stream, target, atlas, "configured_input")
          }
        }
      }
    }
  }
  # Retain existing native outputs even when upstream images were archived or
  # streams were disabled. Unknown stream attribution remains explicit.
  known <- unlist(lapply(rows, `[[`, "derivative_file"), use.names = FALSE)
  observed_pp <- qc_list_files(scfg$metadata$postproc_directory,
    "_bold\\.nii(\\.gz)?$")
  observed_pp <- observed_pp[!qc_path_key(observed_pp) %in% qc_path_key(known)]
  for (path in observed_pp) {
    desc <- qc_entities(path)$description
    streams <- get_postprocess_stream_names(scfg)
    streams <- streams[vapply(streams, function(stream) {
      identical(desc, scfg$postprocess[[stream]]$bids_desc)
    }, logical(1))]
    if (!length(streams)) streams <- NA_character_
    for (stream in streams) rows[[length(rows) + 1L]] <- qc_derivative_row(path,
      "postprocess", stream, NA_character_, NA_character_, NA_character_, "discovered")
  }
  observed_roi <- qc_list_files(scfg$metadata$rois_directory,
    "_(timeseries|connectivity)\\.tsv$")
  observed_roi <- observed_roi[!qc_path_key(observed_roi) %in% qc_path_key(known)]
  for (path in observed_roi) rows[[length(rows) + 1L]] <- qc_derivative_row(path,
    "extract_rois", NA_character_, NA_character_, NA_character_, NA_character_, "discovered")
  empty <- qc_derivative_row(character(), character(), character(), character(),
    character(), character(), character())
  unique(qc_bind_rows(rows, empty))
}

#' Build a derivative row with a complete acquisition and spatial identity
#' @keywords internal
#' @noRd
qc_derivative_row <- function(path, stage, stream, input_stream, source, atlas, basis) {
  cbind(qc_entities(path), data.frame(stage = stage, stream = stream,
    input_stream = input_stream, atlas = atlas, source_file = source,
    derivative_file = path, expectation_basis = basis))
}

#' Match evidence only where every identity present in the evidence agrees
#' @keywords internal
#' @noRd
qc_compatible_rows <- function(rows, identity) {
  fields <- names(identity)[!is.na(identity[1L, ])]
  if (!"sub_id" %in% fields) return(integer())
  keep <- rep(TRUE, nrow(rows))
  for (field in fields) keep <- keep & !is.na(rows[[field]]) & rows[[field]] == identity[[field]]
  which(keep)
}

#' Locate an unambiguous original fMRIPrep confounds table
#' @keywords internal
#' @noRd
qc_find_confounds <- function(path) {
  bids <- as.list(extract_bids_info(path))
  bids$space <- bids$resolution <- bids$cohort <- bids$hemisphere <- NA_character_
  bids$description <- "confounds"
  bids$ext <- ".tsv"
  candidates <- vapply(c("timeseries", "regressors"), function(suffix) {
    construct_bids_filename(utils::modifyList(bids, list(suffix = suffix)), full.names = TRUE)
  }, character(1))
  extant <- candidates[file.exists(candidates)]
  if (length(extant)) extant[1L] else candidates[1L]
}

#' Read a headed TSV without changing identifiers or treating filenames as commands
#' @keywords internal
#' @noRd
qc_read_tsv <- function(path, state) {
  tryCatch(as.data.frame(data.table::fread(file = path, sep = "\t", header = TRUE, colClasses = "character",
    na.strings = c("n/a", "NA", ""), showProgress = FALSE)), error = function(e) {
      qc_issue(state, path, conditionMessage(e)); NULL
    })
}

#' Read known MRIQC IQMs without recomputing them or borrowing upstream thresholds
#' @keywords internal
#' @noRd
qc_collect_mriqc <- function(state, record, path) {
  qc_add_evidence(state, record, "mriqc_iqms", path)
  values <- qc_read_json(path, state)
  if (!is.list(values)) return("invalid")
  definitions <- c(fd_mean = "MRIQC-reported mean framewise displacement.",
    tsnr = "MRIQC-reported median temporal signal-to-noise ratio.",
    dvars_std = "MRIQC-reported standardized DVARS.",
    gcor = "MRIQC-reported global correlation.")
  count <- 0L
  for (name in intersect(names(definitions), names(values))) {
    value <- suppressWarnings(as.numeric(qc_scalar(values[[name]])))
    if (!is.finite(value)) next
    qc_add_metric(state, record, paste0("mriqc_", name), value,
      if (name == "fd_mean") "mm" else "dimensionless", definitions[[name]],
      path, NA_integer_, NA_integer_)
    count <- count + 1L
  }
  if (count) "available" else "unavailable"
}

#' Collect source FD and its explicit available/missing frame denominator
#' @keywords internal
#' @noRd
qc_collect_motion <- function(state, record, path) {
  qc_add_evidence(state, record, "fmriprep_confounds", path)
  if (!file.exists(path)) return("unavailable")
  df <- qc_read_tsv(path, state)
  if (is.null(df)) return("invalid")
  if (!"framewise_displacement" %in% names(df)) return("unavailable")
  fd <- suppressWarnings(as.numeric(df$framewise_displacement))
  good <- is.finite(fd)
  qc_add_metric(state, record, "mean_fd", if (any(good)) mean(fd[good]) else NA_real_,
    "mm", "Mean original fMRIPrep framewise_displacement over finite frames; no dashboard threshold applied.",
    path, sum(good), sum(!good))
  if (any(good)) "available" else "unavailable"
}

#' Collect censor-mask counts without claiming that censoring was executed
#' @keywords internal
#' @noRd
qc_collect_censor <- function(state, record, path) {
  qc_add_evidence(state, record, "censor_mask", path)
  if (!file.exists(path)) return("unavailable")
  values <- tryCatch(scan(path, what = numeric(), quiet = TRUE), error = function(e) NULL)
  if (!length(values) || anyNA(values) || any(!values %in% c(0, 1))) {
    qc_issue(state, path, "Censor mask must contain a nonempty binary vector (1 = retain).")
    return("invalid")
  }
  qc_add_metric(state, record, "censor_retained_percent", 100 * mean(values), "%",
    "Percent of entries equal to 1 in the saved censor mask; this does not establish whether volumes were removed.",
    path, length(values))
  qc_add_metric(state, record, "censor_retained_frames", sum(values), "frames",
    "Number of entries equal to 1 in the saved censor mask.", path, length(values))
  "available"
}

#' Collect native validation audits with explicit schema and target checks
#' @keywords internal
#' @noRd
qc_collect_audit <- function(state, record, candidates, target) {
  if (!length(candidates)) return("unavailable")
  matches <- list()
  for (path in candidates) {
    qc_add_evidence(state, record, "validation_audit", path)
    audit <- qc_read_json(path, state)
    if (!is.list(audit) || !identical(audit$schema_version, "postproc-validation-v1")) {
      qc_issue(state, path, "Unknown or invalid postprocessing validation schema.")
      next
    }
    intended <- qc_scalar(audit$intended_final_file)
    if (!is.na(intended) && identical(
        qc_path_key(intended), qc_path_key(target))) matches[[path]] <- audit
  }
  if (length(matches) != 1L) {
    qc_issue(state, target, "Validation audit does not identify exactly one matching target.")
    return(if (length(matches) > 1L) "ambiguous" else "invalid")
  }
  audit <- matches[[1L]]
  for (check in audit$checks) {
    if (!is.list(check)) next
    state$checks[[length(state$checks) + 1L]] <- data.frame(record_id = record,
      step = qc_scalar(check$step), status = qc_scalar(check$status),
      message = qc_scalar(check$message), source_file = names(matches)[1L])
  }
  status <- qc_scalar(audit$overall_status)
  if (is.na(status) || !status %in% c("passed", "failed", "error", "skipped", "not_run")) "invalid" else status
}

#' Collect per-ROI retention while preserving atlas-specific denominators
#' @keywords internal
#' @noRd
qc_collect_roi <- function(state, record, path) {
  qc_add_evidence(state, record, "roi_diagnostics", path)
  if (!file.exists(path)) return("unavailable")
  df <- qc_read_tsv(path, state)
  if (is.null(df) || !nrow(df) || !all(c("roi", "retained", "proportion_usable") %in% names(df))) {
    qc_issue(state, path, "ROI diagnostics require roi, retained, and proportion_usable columns.")
    return("invalid")
  }
  retained <- as.character(df$retained)
  if (anyNA(retained) || any(!retained %in% c("TRUE", "FALSE", "1", "0"))) {
    qc_issue(state, path, "ROI retained values must be non-missing logical values.")
    return("invalid")
  }
  retained <- retained %in% c("TRUE", "1")
  usable <- suppressWarnings(as.numeric(df$proportion_usable))
  if (any(!is.finite(usable)) || any(usable < 0 | usable > 1) || anyDuplicated(df$roi)) {
    qc_issue(state, path, "ROI diagnostics require unique labels and usable proportions in [0, 1].")
    return("invalid")
  }
  state$regions[[length(state$regions) + 1L]] <- data.frame(record_id = record,
    roi = df$roi, proportion_usable = usable, retained = retained,
    exclusion_reason = if ("exclusion_reason" %in% names(df)) df$exclusion_reason else NA_character_,
    source_file = path)
  qc_add_metric(state, record, "roi_retained_percent", 100 * mean(retained), "%",
    "Percent of atlas labels passing the extraction voxel-retention rule.", path, length(retained))
  qc_add_metric(state, record, "roi_lost_count", sum(!retained), "ROIs",
    "Atlas labels that failed the saved extraction voxel-retention rule.", path, length(retained))
  qc_add_metric(state, record, "roi_mean_usable_percent", 100 * mean(usable), "%",
    "Unweighted mean percent of usable atlas voxels across all labels, including failed labels.", path, length(retained))
  if (all(retained)) "available" else "roi_loss"
}

#' Join configured subject/stage expectations with recorded current workflow units
#' @keywords internal
#' @noRd
qc_workflow <- function(scfg, inspection, acquisitions, inventory, subjects, state) {
  candidates <- list(acquisitions[, c("sub_id", "ses_id"), drop = FALSE],
    inventory[, c("sub_id", "ses_id"), drop = FALSE],
    inspection$subject_stages[, c("sub_id", "ses_id"), drop = FALSE])
  bids_root <- scfg$metadata$bids_directory
  if (checkmate::test_string(bids_root) && dir.exists(bids_root)) {
    for (directory in list.dirs(bids_root, recursive = FALSE, full.names = TRUE)) {
      if (!grepl("^sub-[A-Za-z0-9]+$", basename(directory))) next
      sessions <- list.dirs(directory, recursive = FALSE, full.names = FALSE)
      sessions <- sessions[grepl("^ses-[A-Za-z0-9]+$", sessions)]
      candidates[[length(candidates) + 1L]] <- data.frame(
        sub_id = sub("^sub-", "", basename(directory)),
        ses_id = if (length(sessions)) sub("^ses-", "", sessions) else NA_character_)
    }
  }
  if (!is.null(subjects)) {
    if (!"sub_id" %in% names(subjects)) stop("subjects must contain sub_id.", call. = FALSE)
    if (!"ses_id" %in% names(subjects)) subjects$ses_id <- rep(NA_character_, nrow(subjects))
    candidates[[length(candidates) + 1L]] <- subjects[, c("sub_id", "ses_id"), drop = FALSE]
  }
  # Include planned scopes even when subjects have not yet produced any files.
  for (path in qc_list_files(scfg$metadata$log_directory, "^subjects\\.tsv$")) {
    scope <- qc_read_tsv(path, state)
    if (is.data.frame(scope) && "sub_id" %in% names(scope)) {
      if (!"ses_id" %in% names(scope)) scope$ses_id <- rep(NA_character_, nrow(scope))
      candidates[[length(candidates) + 1L]] <- scope[, c("sub_id", "ses_id"), drop = FALSE]
    }
  }
  people <- unique(do.call(rbind, candidates))
  people$sub_id <- sub("^sub-", "", as.character(people$sub_id))
  people$ses_id <- sub("^ses-", "", as.character(people$ses_id))
  people <- people[!is.na(people$sub_id) & nzchar(people$sub_id), , drop = FALSE]
  # A subject-wide tracking row is context, not an extra no-session acquisition.
  people <- people[!(is.na(people$ses_id) & people$sub_id %in%
    people$sub_id[!is.na(people$ses_id)]), , drop = FALSE]
  expected <- list()
  for (stage in status_spec(scfg)$steps) {
    streams <- switch(stage, postprocess = get_postprocess_stream_names(scfg),
      extract_rois = get_extract_stream_names(scfg), NA_character_)
    scope <- people
    if (!stage %in% c("postprocess", "extract_rois", "bids_conversion")) {
      scope$ses_id <- rep(NA_character_, nrow(scope))
      scope <- unique(scope)
    }
    for (stream in streams) if (nrow(scope)) expected[[length(expected) + 1L]] <- data.frame(
      scope, stage = stage, stream = stream, status = "NOT_TRACKED", run_id = NA_character_)
  }
  empty <- data.frame(sub_id = character(), ses_id = character(), stage = character(),
    stream = character(), status = character(), run_id = character())
  result <- qc_bind_rows(expected, empty)
  current <- inspection$attempts[inspection$attempts$is_current,
    c("sub_id", "ses_id", "stage", "stream", "status", "run_id"), drop = FALSE]
  # Keep each real work unit, including project-level setup/controller jobs.
  result <- result[!qc_workflow_key(result) %in% qc_workflow_key(current), , drop = FALSE]
  unique(rbind(result, current))
}

#' Construct a missing-aware workflow join key
#' @keywords internal
#' @noRd
qc_workflow_key <- function(df) {
  do.call(paste, c(lapply(df[c("sub_id", "ses_id", "stage", "stream")],
    function(x) ifelse(is.na(x), "", x)), sep = "|"))
}

#' Attach current workflow context without identifying a derivative's producer
#' @keywords internal
#' @noRd
qc_attach_workflow <- function(inventory, workflow) {
  inventory$workflow_status <- rep("UNAVAILABLE", nrow(inventory))
  inventory$workflow_run_id <- rep(NA_character_, nrow(inventory))
  inventory$workflow_scope <- rep(NA_character_, nrow(inventory))
  for (i in seq_len(nrow(inventory))) {
    row <- inventory[i, , drop = FALSE]
    matches <- which(!is.na(workflow$sub_id) & workflow$sub_id == row$sub_id &
      workflow$stage == row$stage &
      ifelse(is.na(workflow$stream), "", workflow$stream) == ifelse(is.na(row$stream), "", row$stream) &
      (is.na(workflow$ses_id) | (!is.na(row$ses_id) & workflow$ses_id == row$ses_id)))
    if (length(matches) == 1L) {
      unit <- workflow[matches, , drop = FALSE]
      inventory$workflow_status[i] <- unit$status
      inventory$workflow_run_id[i] <- unit$run_id
      inventory$workflow_scope[i] <- if (is.na(unit$ses_id)) "participant" else "participant_session"
    } else if (length(matches) > 1L) inventory$workflow_status[i] <- "MULTIPLE_UNITS"
  }
  inventory$attention <- inventory$attention |
    inventory$workflow_status %in% c("FAILED", "BLOCKED", "CANCELLED")
  inventory
}

#' Inspect exact output-manifest files without attributing them to newer attempts
#' @keywords internal
#' @noRd
qc_manifest_files <- function(jobs, state) {
  rows <- list()
  for (i in seq_len(nrow(jobs))) {
    job <- jobs[i, , drop = FALSE]
    if (is.na(job$output_manifest) || !nzchar(job$output_manifest)) next
    manifest <- tryCatch(jsonlite::fromJSON(job$output_manifest, simplifyVector = FALSE),
      error = function(e) NULL)
    if (!is.list(manifest) || !checkmate::test_string(manifest$output_dir)) {
      qc_issue(state, paste0("job:", job$job_id), "Invalid output manifest.")
      next
    }
    for (entry in manifest$files) {
      if (!is.list(entry) || !checkmate::test_string(entry$path) ||
          !checkmate::test_number(entry$size, lower = 0) ||
          !checkmate::test_number(entry$mtime)) {
        qc_issue(state, paste0("job:", job$job_id), "Output manifest entry lacks a valid path, size, or mtime.")
        next
      }
      path <- file.path(manifest$output_dir, entry$path)
      info <- file.info(path)
      match <- !is.na(info$size) && !isTRUE(info$isdir) &&
        isTRUE(all.equal(as.numeric(info$size), as.numeric(entry$size))) &&
        isTRUE(all.equal(as.numeric(info$mtime), as.numeric(entry$mtime), tolerance = 1e-10))
      rows[[length(rows) + 1L]] <- data.frame(job_id = job$job_id,
        run_id = job$sequence_id, attempt = job$attempt, contract_id = job$contract_id,
        stage = job$stage, stream = job$stream, is_current_attempt = job$is_current_attempt,
        path = path, recorded_size = as.numeric(entry$size),
        recorded_mtime = as.numeric(entry$mtime), matches_recorded_file = match,
        status = if (is.na(info$size)) "missing" else if (match) "matches_size_and_mtime" else "changed")
    }
  }
  qc_bind_rows(rows, data.frame(job_id = character(), run_id = character(),
    attempt = integer(), contract_id = character(), stage = character(), stream = character(),
    is_current_attempt = logical(), path = character(), recorded_size = numeric(),
    recorded_mtime = numeric(), matches_recorded_file = logical(), status = character()))
}

#' Print a compact QC snapshot summary
#' @param x A `bg_qc_inventory` object.
#' @param ... Unused arguments.
#' @return Invisibly returns `x`.
#' @export
print.bg_qc_inventory <- function(x, ...) {
  cat("BrainGnomes QC inventory: ", x$metadata$project, "\n", sep = "")
  cat("Snapshot: ", x$metadata$created_at, "\n", sep = "")
  cat(nrow(x$acquisitions), "raw BOLD files;", nrow(x$workflow), "workflow units;",
    nrow(x$inventory), "derivative records\n")
  cat(sum(x$inventory$output_status == "missing"), "missing derivatives;",
    nrow(x$issues), "collection issues\n")
  invisible(x)
}
