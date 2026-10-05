run_provenance_timestamp <- function(time = Sys.time()) {
  format(time, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
}

run_provenance_directory <- function(scfg, run_id) {
  file.path(scfg$metadata$log_directory, "runs", as.character(run_id))
}

run_provenance_file <- function(scfg, run_id) {
  file.path(run_provenance_directory(scfg, run_id), "provenance.json")
}

#' Reject image payloads before serializing provenance metadata
#'
#' @param value Metadata tree containing settings, identities, headers, or QA summaries.
#' @param path Field path used to identify a rejected payload.
#' @return NULL invisibly; image objects, spatial arrays, scientific matrices,
#'   and binary payloads raise an error. Header transforms and sampled locations
#'   remain valid metadata.
#' @noRd
assert_provenance_metadata <- function(value, path = "metadata") {
  # Map a numeric vector/list to its array rank, or NA for a named record.
  # Named records interrupt the structure so lists of records are not spatial data.
  numeric_rank <- function(x) {
    if (is.numeric(x) || is.logical(x)) return(if (length(x) > 1L) 1L else 0L)
    if (!is.list(x) || !is.null(names(x)) || !length(x)) return(NA_integer_)
    ranks <- vapply(x, numeric_rank, integer(1))
    if (anyNA(ranks)) return(NA_integer_)
    1L + max(ranks)
  }
  rank <- numeric_rank(value)
  image <- inherits(value, c("niftiImage", "internalImage", "nifti", "anlz"))
  field <- tail(strsplit(path, "$", fixed = TRUE)[[1L]], 1L)
  numeric_matrix <- (is.matrix(value) && (is.numeric(value) || is.logical(value))) ||
    (!is.na(rank) && rank == 2L)
  # Numeric matrices could be voxel-by-time data. Allow only the documented
  # coordinate/transform fields; scientific matrices belong in derivative files.
  shape <- if (is.matrix(value)) dim(value) else if (numeric_matrix) {
    c(length(value), length(value[[1L]]))
  } else integer()
  metadata_matrix <- (field == "normalized_coords" && length(shape) == 2L &&
    shape[2L] == 3L) || (field %in% c("Transform", "qform", "sform") &&
    identical(as.integer(shape), c(4L, 4L)))
  spatial <- length(dim(value)) >= 3L || (!is.na(rank) && rank >= 3L) ||
    (numeric_matrix && !metadata_matrix)
  binary <- is.raw(value) || typeof(value) %in% c("externalptr", "environment")
  # These names identify voxel payloads even when callers flatten an image.
  payload <- field %in% c("scale_map", "voxel_data", "voxel_values", "image_data",
                         "baseline_map") && !is.null(value)
  if (image || spatial || binary || payload) {
    stop("Voxel/image or binary data are not permitted in provenance: ", path,
         ". Store image data in a NIfTI file and record its path/header.", call. = FALSE)
  }
  if (is.list(value)) {
    labels <- names(value)
    if (is.null(labels)) labels <- paste0("[", seq_along(value), "]")
    for (i in seq_along(value)) {
      assert_provenance_metadata(value[[i]], paste0(path, "$", labels[[i]]))
    }
  }
  # RDS snapshots also preserve custom attributes. Check those for payloads;
  # ordinary shape/class attributes and data.table's bookkeeping pointer are safe.
  extra <- attributes(value)
  extra[c("names", "class", "dim", "dimnames", "row.names", ".internal.selfref")] <- NULL
  if (length(extra)) assert_provenance_metadata(extra, paste0(path, "$attributes"))
  invisible(NULL)
}

write_json_atomic <- function(value, file) {
  assert_provenance_metadata(value)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  temp <- tempfile("provenance-", tmpdir = dirname(file), fileext = ".json")
  on.exit(if (file.exists(temp)) unlink(temp), add = TRUE)
  jsonlite::write_json(
    value, temp, pretty = TRUE, auto_unbox = TRUE, na = "null",
    null = "null", digits = NA
  )
  if (!file.rename(temp, file)) {
    if (!file.copy(temp, file, overwrite = TRUE)) {
      stop("Failed to write run provenance: ", file, call. = FALSE)
    }
    unlink(temp)
  }
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

write_yaml_atomic <- function(value, file) {
  assert_provenance_metadata(value)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  temp <- tempfile("run-config-", tmpdir = dirname(file), fileext = ".yaml")
  on.exit(if (file.exists(temp)) unlink(temp), add = TRUE)
  yaml::write_yaml(value, temp)
  if (!file.rename(temp, file)) {
    if (!file.copy(temp, file, overwrite = TRUE)) {
      stop("Failed to write run configuration snapshot: ", file, call. = FALSE)
    }
    unlink(temp)
  }
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

write_table_atomic <- function(value, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  temp <- tempfile("run-subjects-", tmpdir = dirname(file), fileext = ".tsv")
  on.exit(if (file.exists(temp)) unlink(temp), add = TRUE)
  utils::write.table(
    value, temp, sep = "\t", quote = FALSE, row.names = FALSE, na = ""
  )
  if (!file.rename(temp, file)) {
    if (!file.copy(temp, file, overwrite = TRUE)) {
      stop("Failed to write resolved run scope: ", file, call. = FALSE)
    }
    unlink(temp)
  }
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

format_artifact_size <- function(size_bytes) {
  units <- c("bytes", "KiB", "MiB", "GiB", "TiB")
  unit <- 1L
  size <- as.numeric(size_bytes)
  while (is.finite(size) && size >= 1024 && unit < length(units)) {
    size <- size / 1024
    unit <- unit + 1L
  }
  digits <- if (unit == 1L || size >= 10) 0L else 1L
  paste0(format(round(size, digits), nsmall = digits, trim = TRUE), " ", units[[unit]])
}

run_artifact_display_name <- function(role, path) {
  labels <- c(
    "bids_conversion.heudiconv_container" = "HeuDiConv container",
    "mriqc.mriqc_container" = "MRIQC container",
    "fmriprep.fmriprep_container" = "fMRIPrep container",
    "aroma.aroma_container" = "AROMA container",
    "postprocess.fsl_container" = "FSL container"
  )
  if (role %in% names(labels)) return(unname(labels[[role]]))
  paste0("file ", basename(path))
}

announce_full_artifact_read <- function(path, label = NULL,
                                        threshold_bytes = 100 * 1024^2) {
  info <- file.info(path)
  size <- as.numeric(info$size[[1L]])
  if (!is.finite(size) || size < threshold_bytes) return(invisible(FALSE))
  if (!checkmate::test_string(label)) label <- paste0("file ", basename(path))
  cli::cli_alert_info(paste0(
    "Reading the full ", label, " (", format_artifact_size(size), ") so ",
    "BrainGnomes can identify the exact copy used. This can take a minute ",
    "when the file is first used in this project or has changed since the ",
    "last run."
  ))
  invisible(TRUE)
}

cached_artifact_checksum <- function(path, cache_file = NULL, label = NULL,
                                     message_threshold_bytes = 100 * 1024^2) {
  if (!checkmate::test_string(cache_file)) {
    announce_full_artifact_read(path, label, message_threshold_bytes)
    return(unname(tools::md5sum(path)))
  }
  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
  lock_file <- paste0(cache_file, ".lock")
  lock <- tryCatch(
    filelock::lock(lock_file, timeout = 10000),
    error = function(e) NULL
  )
  if (is.null(lock)) {
    announce_full_artifact_read(path, label, message_threshold_bytes)
    return(unname(tools::md5sum(path)))
  }
  on.exit(filelock::unlock(lock), add = TRUE)

  resolved <- normalizePath(path, winslash = "/", mustWork = TRUE)
  info <- file.info(path)
  size <- as.numeric(info$size[[1L]])
  modified <- as.numeric(info$mtime[[1L]])
  changed <- as.numeric(info$ctime[[1L]])
  cache <- suppressWarnings(tryCatch(
    readRDS(cache_file), error = function(e) NULL
  ))
  if (!is.data.frame(cache) || !all(c(
    "path", "size_bytes", "modified", "changed", "checksum"
  ) %in% names(cache))) {
    cache <- data.frame(
      path = character(), size_bytes = numeric(), modified = numeric(),
      changed = numeric(), checksum = character(), stringsAsFactors = FALSE
    )
  }
  match <- cache$path == resolved & cache$size_bytes == size &
    cache$modified == modified & cache$changed == changed
  if (any(match)) return(cache$checksum[which(match)[[1L]]])

  announce_full_artifact_read(path, label, message_threshold_bytes)
  checksum <- unname(tools::md5sum(path))
  cache <- cache[cache$path != resolved, , drop = FALSE]
  cache <- rbind(cache, data.frame(
    path = resolved, size_bytes = size, modified = modified, changed = changed,
    checksum = checksum, stringsAsFactors = FALSE
  ))
  temp <- tempfile("artifact-checksums-", tmpdir = dirname(cache_file))
  on.exit(if (file.exists(temp)) unlink(temp), add = TRUE)
  saveRDS(cache, temp)
  if (!file.rename(temp, cache_file)) {
    file.copy(temp, cache_file, overwrite = TRUE)
    unlink(temp)
  }
  checksum
}

fingerprint_run_artifact <- function(role, path, checksum_cache = NULL) {
  configured_path <- if (length(path) == 0L || is.na(path[[1L]]) ||
    !nzchar(path[[1L]])) NA_character_ else as.character(path[[1L]])
  exists <- !is.na(configured_path) && file.exists(configured_path)
  is_file <- exists && !dir.exists(configured_path)
  resolved_path <- if (exists) {
    normalizePath(configured_path, winslash = "/", mustWork = TRUE)
  } else if (!is.na(configured_path)) {
    normalizePath(configured_path, winslash = "/", mustWork = FALSE)
  } else {
    NA_character_
  }
  info <- if (exists) file.info(configured_path) else NULL
  data.frame(
    role = as.character(role),
    configured_path = configured_path,
    path = resolved_path,
    exists = exists,
    size_bytes = if (is_file) as.numeric(info$size[[1L]]) else NA_real_,
    modified_at = if (exists) {
      run_provenance_timestamp(info$mtime[[1L]])
    } else NA_character_,
    checksum_algorithm = if (is_file) "md5" else NA_character_,
    checksum = if (is_file) {
      cached_artifact_checksum(
        configured_path, checksum_cache,
        label = run_artifact_display_name(role, configured_path)
      )
    } else NA_character_,
    stringsAsFactors = FALSE
  )
}

collect_nested_artifact_paths <- function(value, prefix) {
  paths <- list()
  recurse <- function(x, field) {
    if (is.list(x) && !is.data.frame(x)) {
      nms <- names(x)
      if (is.null(nms)) nms <- as.character(seq_along(x))
      for (i in seq_along(x)) recurse(x[[i]], paste(field, nms[[i]], sep = "."))
      return(invisible(NULL))
    }
    leaf <- sub("^.*\\.", "", field)
    file_field <- grepl(
      "(file|files|path|paths|container|atlas|atlases|license|executable)$",
      leaf, ignore.case = TRUE
    )
    if (!file_field || !is.character(x) || length(x) == 0L) {
      return(invisible(NULL))
    }
    for (i in seq_along(x)) {
      candidate <- x[[i]]
      if (!is.na(candidate) && nzchar(candidate) && candidate != "template") {
        paths[[paste0(field, if (length(x) > 1L) paste0(".", i) else "")]] <<-
          candidate
      }
    }
    invisible(NULL)
  }
  recurse(value, prefix)
  paths
}

collect_run_artifacts <- function(scfg, execution, config_snapshot) {
  paths <- list(project_config_snapshot = config_snapshot)
  config_source <- attr(scfg, "yaml_file", exact = TRUE)
  if (checkmate::test_string(config_source)) paths$project_config_source <- config_source
  context <- attr(scfg, "provenance_context", exact = TRUE)
  if (is.list(context) && checkmate::test_string(context$plan_file)) {
    paths$submitted_plan <- context$plan_file
  }

  package_root <- tryCatch(find.package("BrainGnomes"), error = function(e) "")
  if (nzchar(package_root)) {
    package_files <- c(
      file.path(package_root, "DESCRIPTION"),
      file.path(package_root, "R", "BrainGnomes.rdb"),
      file.path(package_root, "R", "BrainGnomes.rdx"),
      file.path(package_root, "shell_functions"),
      file.path(package_root, c(
        "insert_tracked_job.R", "upd_job_status.R", "add_parent.R"
      ))
    )
    library_files <- list.files(
      file.path(package_root, "libs"), recursive = TRUE, full.names = TRUE
    )
    operational_directories <- c(
      file.path(package_root, "R"),
      file.path(package_root, "src"),
      file.path(package_root, "hpc_scripts"),
      file.path(package_root, "inst", "hpc_scripts")
    )
    operational_files <- unlist(lapply(
      operational_directories,
      function(directory) list.files(
        directory, recursive = TRUE, full.names = TRUE
      )
    ), use.names = FALSE)
    package_files <- unique(c(
      package_files, library_files, operational_files
    ))
    package_files <- package_files[file.exists(package_files) &
      !dir.exists(package_files)]
    normalized_root <- normalizePath(
      package_root, winslash = "/", mustWork = TRUE
    )
    for (i in seq_along(package_files)) {
      normalized_file <- normalizePath(
        package_files[[i]], winslash = "/", mustWork = TRUE
      )
      prefix <- paste0(normalized_root, "/")
      relative <- if (startsWith(normalized_file, prefix)) {
        substring(normalized_file, nchar(prefix) + 1L)
      } else basename(normalized_file)
      paths[[paste0("braingnomes_package.", gsub("/", ".", relative))]] <-
        package_files[[i]]
    }
  }

  scheduler_command <- switch(scfg$compute_environment$scheduler,
    slurm = "sbatch", torque = "qsub", sh = "sh", local = "sh",
    as.character(scfg$compute_environment$scheduler)
  )
  scheduler_path <- Sys.which(scheduler_command)
  if (nzchar(scheduler_path)) paths$scheduler_executable <- unname(scheduler_path)

  container_steps <- intersect(
    execution$steps,
    c("bids_conversion", "mriqc", "fmriprep", "aroma", "postprocess")
  )
  if (length(container_steps) > 0L) {
    runtime <- Sys.which("singularity")
    if (!nzchar(runtime)) runtime <- Sys.which("apptainer")
    if (nzchar(runtime)) paths$container_runtime <- unname(runtime)
  }

  explicit <- list(
    flywheel_sync = list(
      flywheel_executable = scfg$compute_environment$flywheel
    ),
    bids_conversion = list(
      heudiconv_container = scfg$compute_environment$heudiconv_container,
      heuristic_file = scfg$bids_conversion$heuristic_file
    ),
    mriqc = list(
      mriqc_container = scfg$compute_environment$mriqc_container
    ),
    fmriprep = list(
      fmriprep_container = scfg$compute_environment$fmriprep_container,
      freesurfer_license = scfg$fmriprep$fs_license_file
    ),
    aroma = list(
      aroma_container = scfg$compute_environment$aroma_container
    ),
    postprocess = list(
      fsl_container = scfg$compute_environment$fsl_container
    )
  )
  for (stage in intersect(execution$steps, names(explicit))) {
    for (label in names(explicit[[stage]])) {
      value <- explicit[[stage]][[label]]
      if (checkmate::test_string(value)) {
        paths[[paste(stage, label, sep = ".")]] <- value
      }
    }
  }

  if ("postprocess" %in% execution$steps) {
    for (stream in execution$postprocess_streams) {
      paths <- c(paths, collect_nested_artifact_paths(
        scfg$postprocess[[stream]], paste("postprocess", stream, sep = ".")
      ))
    }
  }
  if ("extract_rois" %in% execution$steps) {
    for (stream in execution$extract_streams) {
      paths <- c(paths, collect_nested_artifact_paths(
        scfg$extract_rois[[stream]], paste("extract_rois", stream, sep = ".")
      ))
    }
  }

  roles <- names(paths)
  checksum_cache <- file.path(
    scfg$metadata$log_directory, "runs", ".artifact_checksums.rds"
  )
  rows <- lapply(seq_along(paths), function(i) {
    fingerprint_run_artifact(roles[[i]], paths[[i]], checksum_cache)
  })
  artifacts <- if (length(rows) == 0L) {
    data.frame(
      role = character(), configured_path = character(), path = character(),
      exists = logical(), size_bytes = numeric(), modified_at = character(),
      checksum_algorithm = character(), checksum = character(),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, rows)
  }
  rownames(artifacts) <- NULL
  artifacts[!duplicated(artifacts[c("role", "configured_path")]), , drop = FALSE]
}

run_package_identity <- function() {
  description <- utils::packageDescription("BrainGnomes")
  package_path <- tryCatch(find.package("BrainGnomes"), error = function(e) "")
  git_commit <- value_or_default(
    description$RemoteSha,
    value_or_default(description$GithubSHA1, NA_character_)
  )
  git_dirty <- NA
  if (nzchar(package_path) && dir.exists(file.path(package_path, ".git")) &&
      nzchar(Sys.which("git"))) {
    commit <- suppressWarnings(system2(
      "git", c("-C", shQuote(package_path), "rev-parse", "HEAD"),
      stdout = TRUE, stderr = FALSE
    ))
    if (length(commit) > 0L && identical(attr(commit, "status"), NULL)) {
      git_commit <- commit[[1L]]
    }
    dirty <- suppressWarnings(system2(
      "git", c("-C", shQuote(package_path), "status", "--porcelain"),
      stdout = TRUE, stderr = FALSE
    ))
    git_dirty <- length(dirty) > 0L
  }
  list(
    package = "BrainGnomes",
    version = as.character(utils::packageVersion("BrainGnomes")),
    library_path = if (nzchar(package_path)) {
      normalizePath(package_path, winslash = "/", mustWork = TRUE)
    } else NA_character_,
    built = value_or_default(description$Built, NA_character_),
    repository = value_or_default(description$RemoteRepo, NA_character_),
    git_commit = git_commit,
    git_dirty = git_dirty
  )
}

run_software_identity <- function() {
  namespaces <- sort(loadedNamespaces())
  versions <- vapply(namespaces, function(package) {
    tryCatch(
      as.character(utils::packageVersion(package)),
      error = function(e) NA_character_
    )
  }, character(1))
  list(
    braingnomes = run_package_identity(),
    r = list(
      version = R.version.string,
      platform = R.version$platform,
      architecture = R.version$arch,
      r_home = R.home(),
      library_paths = .libPaths()
    ),
    loaded_packages = as.list(versions)
  )
}

run_host_identity <- function() {
  info <- Sys.info()
  environment_names <- c(
    "LOADEDMODULES", "CONDA_PREFIX", "VIRTUAL_ENV", "RETICULATE_PYTHON",
    "SLURM_CLUSTER_NAME", "PBS_SERVER", "SINGULARITY_NAME", "APPTAINER_NAME"
  )
  environment <- Sys.getenv(environment_names, unset = NA_character_)
  names(environment) <- environment_names
  list(
    system = as.list(info),
    working_directory = normalizePath(getwd(), winslash = "/", mustWork = TRUE),
    timezone = Sys.timezone(),
    locale = Sys.getlocale(),
    selected_environment = as.list(environment)
  )
}

run_invocation_context <- function(scfg) {
  context <- attr(scfg, "provenance_context", exact = TRUE)
  if (is.null(context)) context <- list()
  if (is.null(context$interface)) {
    context$interface <- if (interactive()) "r_interactive" else "r"
  }
  context$command <- commandArgs(trailingOnly = FALSE)
  context
}

record_run_provenance <- function(scfg, run_id, execution, debug = FALSE,
                                  log_level = "INFO") {
  checkmate::assert_class(scfg, "bg_project_cfg")
  checkmate::assert_string(run_id)
  checkmate::assert_class(execution, "bg_project_execution")
  checkmate::assert_flag(debug)
  checkmate::assert_string(log_level)

  run_dir <- run_provenance_directory(scfg, run_id)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  config_file <- file.path(run_dir, "project_config.yaml")
  subjects_file <- file.path(run_dir, "subjects.tsv")
  write_yaml_atomic(unclass(scfg), config_file)
  write_table_atomic(execution$subjects, subjects_file)
  artifacts <- collect_run_artifacts(scfg, execution, config_file)
  config_row <- artifacts[artifacts$role == "project_config_snapshot", , drop = FALSE]
  context <- run_invocation_context(scfg)
  recorded_at <- run_provenance_timestamp()

  record <- list(
    schema_version = "brain-gnomes-run-provenance-v1",
    run_id = run_id,
    recorded_at = recorded_at,
    state = "submission_started",
    invocation = context,
    request = list(
      steps = execution$steps,
      step_flags = as.list(execution$step_flags),
      subject_filter = execution$subject_filter,
      postprocess_streams = execution$postprocess_streams,
      extract_streams = execution$extract_streams,
      work_units = execution$work_units,
      force = execution$force,
      debug = debug,
      log_level = log_level
    ),
    execution = list(
      scope_deferred = execution$scope_deferred,
      scope_status = execution$scope_status,
      deferred_reasons = execution$deferred_reasons,
      scope_resolved_at = if (execution$scope_deferred) {
        NA_character_
      } else recorded_at,
      subjects = execution$subjects,
      job_plan = build_project_jobs(scfg, execution),
      scope_events = list(list(
        event = if (execution$scope_deferred) "scope_deferred" else "scope_resolved",
        recorded_at = recorded_at,
        reason = if (execution$scope_deferred) execution$deferred_reasons else NULL,
        n_subjects = length(unique(execution$subjects$sub_id)),
        n_subject_sessions = nrow(execution$subjects)
      ))
    ),
    configuration = list(
      source_file = attr(scfg, "yaml_file", exact = TRUE),
      snapshot_file = normalizePath(
        config_file, winslash = "/", mustWork = TRUE
      ),
      snapshot_checksum_algorithm = if (nrow(config_row)) {
        config_row$checksum_algorithm[[1L]]
      } else NA_character_,
      snapshot_checksum = if (nrow(config_row)) {
        config_row$checksum[[1L]]
      } else NA_character_,
      values = unclass(scfg)
    ),
    software = run_software_identity(),
    host = run_host_identity(),
    scheduler = list(
      configured = scfg$compute_environment$scheduler,
      executable = artifacts$path[artifacts$role == "scheduler_executable"]
    ),
    artifacts = artifacts,
    files = list(
      provenance = normalizePath(
        run_provenance_file(scfg, run_id), winslash = "/", mustWork = FALSE
      ),
      configuration = normalizePath(config_file, winslash = "/", mustWork = TRUE),
      subjects = normalizePath(subjects_file, winslash = "/", mustWork = TRUE)
    ),
    submission = list(
      updated_at = NA_character_,
      submitted_job_ids = character(),
      deferred_subject_submission = execution$scope_deferred,
      tracked_jobs = data.frame()
    )
  )
  write_json_atomic(record, run_provenance_file(scfg, run_id))
}

normalize_scope_table <- function(subjects) {
  result <- as.data.frame(subjects, stringsAsFactors = FALSE)
  result <- result[, sort(names(result)), drop = FALSE]
  result[] <- lapply(result, function(value) {
    value <- as.character(value)
    value[is.na(value)] <- "<NA>"
    value
  })
  if (nrow(result) > 1L && ncol(result) > 0L) {
    result <- result[do.call(order, unname(result)), , drop = FALSE]
  }
  rownames(result) <- NULL
  result
}

record_run_scope_realization <- function(scfg, run_id, subjects,
                                         reason = "flywheel_sync") {
  checkmate::assert_class(scfg, "bg_project_cfg")
  checkmate::assert_string(run_id)
  checkmate::assert_data_frame(subjects)
  run_dir <- run_provenance_directory(scfg, run_id)
  recorded_at <- run_provenance_timestamp()
  realization <- list(
    schema_version = "brain-gnomes-run-scope-v1",
    run_id = run_id,
    recorded_at = recorded_at,
    reason = reason,
    n_subjects = length(unique(subjects$sub_id)),
    n_subject_sessions = nrow(subjects),
    subjects = subjects
  )
  realization_file <- file.path(run_dir, "scope-realization.json")
  if (file.exists(realization_file)) {
    existing <- tryCatch(
      jsonlite::read_json(realization_file, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (!is.list(existing) ||
        !identical(existing$schema_version, "brain-gnomes-run-scope-v1") ||
        !identical(normalize_scope_table(existing$subjects),
                   normalize_scope_table(subjects))) {
      stop(
        "The run's saved subject scope differs from the newly discovered scope: ",
        realization_file, call. = FALSE
      )
    }
    recorded_at <- existing$recorded_at
    subjects <- as.data.frame(existing$subjects, stringsAsFactors = FALSE)
  } else {
    write_job_contract_once(
      realization, realization_file, "Run scope realization"
    )
  }
  subjects_file <- file.path(run_dir, "subjects.tsv")
  write_table_atomic(subjects, subjects_file)

  provenance_file <- run_provenance_file(scfg, run_id)
  if (file.exists(provenance_file)) {
    record <- jsonlite::read_json(provenance_file, simplifyVector = FALSE)
    if (identical(record$execution$scope_status, "resolved") &&
        !is.null(record$files$scope_realization)) {
      return(invisible(normalizePath(
        realization_file, winslash = "/", mustWork = TRUE
      )))
    }
    record$execution$scope_status <- "resolved"
    record$execution$scope_resolved_at <- recorded_at
    record$execution$subjects <- subjects
    events <- record$execution$scope_events
    if (is.null(events)) events <- list()
    events[[length(events) + 1L]] <- list(
      event = "scope_resolved",
      recorded_at = recorded_at,
      reason = reason,
      n_subjects = length(unique(subjects$sub_id)),
      n_subject_sessions = nrow(subjects)
    )
    record$execution$scope_events <- events
    record$files$scope_realization <- normalizePath(
      realization_file, winslash = "/", mustWork = TRUE
    )
    record$files$subjects <- normalizePath(
      subjects_file, winslash = "/", mustWork = TRUE
    )
    write_json_atomic(record, provenance_file)
  }
  invisible(normalizePath(
    realization_file, winslash = "/", mustWork = TRUE
  ))
}

realize_deferred_subjects <- function(snapshot) {
  checkmate::assert_list(snapshot)
  subjects <- discover_project_subjects(
    snapshot$scfg,
    steps = names(snapshot$steps)[snapshot$steps],
    subject_filter = snapshot$subject_filter,
    allow_empty = FALSE
  )
  subjects <- retry_subject_scope(subjects,
    normalize_retry_work_units(attr(snapshot$scfg, "retry_work_units", exact = TRUE)))
  record_run_scope_realization(
    snapshot$scfg, snapshot$sequence_id, subjects,
    reason = "flywheel_sync"
  )
  subjects
}

tracked_jobs_for_provenance <- function(scfg, run_id) {
  db <- scfg$metadata$sqlite_db
  if (!checkmate::test_file_exists(db) ||
      !sqlite_table_exists(db, "job_tracking")) {
    return(data.frame())
  }
  jobs <- tryCatch(
    get_tracked_job_status(sequence_id = run_id, sqlite_db = db),
    error = function(e) data.frame()
  )
  if (nrow(jobs) > 0L && "job_obj" %in% names(jobs)) jobs$job_obj <- NULL
  jobs
}

update_run_provenance_submission <- function(scfg, run_id,
                                             submitted_ids = NULL,
                                             deferred = FALSE,
                                             resolved_subjects = NULL) {
  file <- run_provenance_file(scfg, run_id)
  if (!file.exists(file)) return(invisible(NULL))
  record <- jsonlite::read_json(file, simplifyVector = FALSE)
  previous_ids <- unlist(record$submission$submitted_job_ids, use.names = FALSE)
  tracked_jobs <- tracked_jobs_for_provenance(scfg, run_id)
  tracked_ids <- if (nrow(tracked_jobs) > 0L && "job_id" %in% names(tracked_jobs)) {
    as.character(tracked_jobs$job_id)
  } else character()
  if (!is.null(resolved_subjects)) {
    checkmate::assert_data_frame(resolved_subjects)
    subjects_file <- file.path(dirname(file), "subjects.tsv")
    write_table_atomic(resolved_subjects, subjects_file)
    record$execution$subjects <- resolved_subjects
    record$execution$scope_status <- "resolved"
    record$execution$scope_resolved_at <- run_provenance_timestamp()
    record$files$subjects <- normalizePath(
      subjects_file, winslash = "/", mustWork = TRUE
    )
  }
  record$state <- "submitted"
  record$submission <- list(
    updated_at = run_provenance_timestamp(),
    submitted_job_ids = unique(c(
      as.character(previous_ids), as.character(submitted_ids), tracked_ids
    )),
    deferred_subject_submission = isTRUE(deferred) ||
      isTRUE(record$execution$scope_deferred),
    tracked_jobs = tracked_jobs
  )
  write_json_atomic(record, file)
}

latest_run_provenance_id <- function(scfg) {
  root <- file.path(scfg$metadata$log_directory, "runs")
  if (!dir.exists(root)) return(NULL)
  files <- list.files(
    root, pattern = "^provenance\\.json$", recursive = TRUE,
    full.names = TRUE
  )
  if (length(files) == 0L) return(NULL)
  records <- lapply(files, function(file) {
    tryCatch(
      jsonlite::read_json(file, simplifyVector = TRUE),
      error = function(e) NULL
    )
  })
  valid <- vapply(records, function(x) {
    is.list(x) && checkmate::test_string(x$run_id) &&
      checkmate::test_string(x$recorded_at)
  }, logical(1))
  if (!any(valid)) return(NULL)
  records <- records[valid]
  times <- as.POSIXct(
    vapply(records, `[[`, character(1), "recorded_at"),
    format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"
  )
  records[[which.max(times)]]$run_id
}

#' Read the complete provenance record for a project run
#'
#' Each submitted run records the selected stages, streams, and subjects; an
#' exact copy of the project configuration; requested computing resources and
#' job order; BrainGnomes, R, software, and submission-computer details; the
#' scheduler; and checksums that identify containers and other files that
#' controlled the run. The returned object also includes the currently tracked
#' jobs when available. Use it to confirm exactly what BrainGnomes submitted or
#' to compare an original run with a later retry.
#'
#' @param input A project configuration object, YAML file, or project directory.
#'   Defaults to the current working directory.
#' @param run_id Run ID returned by [run_project()] or listed in
#'   `inspect_project(input)$runs`. Use `"latest"` for the most recently
#'   recorded run.
#' @return A `bg_run_provenance` object.
#' @examples
#' \dontrun{
#' provenance <- get_run_provenance(scfg, run$run_id)
#' provenance$request
#' provenance$execution$subjects
#' provenance$configuration$snapshot_file
#' }
#' @seealso [diagnose_project()] to inspect failures and [retry_project_run()]
#'   to create a new run from failed work.
#' @export
get_run_provenance <- function(input = getwd(), run_id = "latest") {
  scfg <- project_config_from_input(input)
  resolved <- if (identical(run_id, "latest")) {
    value_or_default(latest_run_provenance_id(scfg), resolve_run_id(scfg, run_id))
  } else {
    as.character(run_id)
  }
  file <- run_provenance_file(scfg, resolved)
  if (!file.exists(file)) {
    stop("No provenance record was found for run ", resolved, ".", call. = FALSE)
  }
  record <- jsonlite::read_json(file, simplifyVector = TRUE)
  if (!identical(
    record$schema_version, "brain-gnomes-run-provenance-v1"
  )) {
    stop(
      "Unsupported run provenance schema: ",
      value_or_default(record$schema_version, "<missing>"),
      call. = FALSE
    )
  }
  record$current_jobs <- tracked_jobs_for_provenance(scfg, resolved)
  record$provenance_file <- normalizePath(file, winslash = "/", mustWork = TRUE)
  class(record) <- c("bg_run_provenance", class(record))
  record
}

#' @export
print.bg_run_provenance <- function(x, ...) {
  cli::cli_h2("BrainGnomes run provenance {.val {x$run_id}}")
  cli::cli_text("Recorded: {x$recorded_at}")
  cli::cli_text("Invocation: {x$invocation$interface}")
  cli::cli_text("Steps: {paste(x$request$steps, collapse = ', ')}")
  if (identical(x$execution$scope_status, "deferred")) {
    cli::cli_text("Scope: deferred until Flywheel synchronization")
  } else {
    subjects <- x$execution$subjects
    cli::cli_text(
      "Scope: {length(unique(subjects$sub_id))} subject{?s}, {nrow(subjects)} subject/session row{?s}"
    )
  }
  cli::cli_text(
    "BrainGnomes: {x$software$braingnomes$version}; artifacts: {nrow(x$artifacts)}; tracked jobs: {nrow(x$current_jobs)}"
  )
  cli::cli_text("Record: {.file {x$provenance_file}}")
  invisible(x)
}
