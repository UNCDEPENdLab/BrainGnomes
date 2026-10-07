job_contract_timestamp <- function(time = Sys.time()) {
  format(time, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
}

job_contract_directory <- function(tracking_sqlite_db, tracking_args) {
  run_id <- tracking_args$sequence_id
  configured <- tracking_args$contract_directory
  if (checkmate::test_string(configured)) {
    return(normalizePath(configured, winslash = "/", mustWork = FALSE))
  }
  if (!checkmate::test_string(tracking_sqlite_db)) return(NULL)
  if (!checkmate::test_string(run_id)) {
    return(normalizePath(file.path(dirname(tracking_sqlite_db), "job_contracts"),
                         winslash = "/", mustWork = FALSE))
  }
  normalizePath(
    file.path(dirname(tracking_sqlite_db), "runs", run_id, "jobs"),
    winslash = "/", mustWork = FALSE
  )
}

allocate_job_contract <- function(tracking_sqlite_db, tracking_args) {
  directory <- job_contract_directory(tracking_sqlite_db, tracking_args)
  if (!checkmate::test_string(directory)) return(NULL)
  contract_id <- uuid::UUIDgenerate()
  contract_dir <- file.path(directory, contract_id)
  list(
    contract_id = contract_id,
    directory = contract_dir,
    manifest_path = file.path(contract_dir, "job-manifest.json"),
    receipt_path = file.path(contract_dir, "runtime-receipt.json")
  )
}

#' Choose the logical run attempt, including submissions without an allocation ID
#' @param sqlite_db Tracking database path.
#' @param unit_key Logical work-unit identity.
#' @param sequence_id Run identity; jobs in the same run share an attempt number.
#' @return Integer logical attempt number.
#' @noRd
next_job_contract_attempt <- function(sqlite_db, unit_key, sequence_id) {
  if (!checkmate::test_string(sqlite_db) || !file.exists(sqlite_db) ||
      !checkmate::test_string(unit_key) || !checkmate::test_string(sequence_id)) {
    return(1L)
  }
  value <- suppressWarnings(tryCatch({
      con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_db, flags = RSQLite::SQLITE_RO, synchronous = NULL)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      records <- "SELECT attempt, sequence_id, unit_key FROM job_tracking"
      if (DBI::dbExistsTable(con, "job_submission_attempts")) {
        records <- paste(records, "UNION ALL SELECT attempt, run_id AS sequence_id, unit_key FROM job_submission_attempts")
      }
      DBI::dbGetQuery(con,
        paste(
          "SELECT COALESCE(MAX(attempt), 0) AS max_attempt,",
          "MAX(CASE WHEN sequence_id = ? THEN attempt END) AS run_attempt",
          "FROM (", records, ") WHERE unit_key = ?"
        ),
        params = list(sequence_id, unit_key)
      )
    },
    error = function(e) NULL
  ))
  if (!is.data.frame(value) || nrow(value) == 0L) return(1L)
  if (!is.na(value$run_attempt[[1L]])) {
    return(as.integer(value$run_attempt[[1L]]))
  }
  as.integer(value$max_attempt[[1L]]) + 1L
}

is_sensitive_environment_name <- function(name) {
  grepl(
    "(^|_)(TOKEN|SECRET|PASSWORD|PASSWD|CREDENTIAL|AUTH|KEY)($|_)",
    toupper(name), perl = TRUE
  )
}

contract_environment_table <- function(env_variables) {
  if (is.null(env_variables) || length(env_variables) == 0L) {
    return(data.frame(
      name = character(), value = character(), source = character(),
      redacted = logical(), stringsAsFactors = FALSE
    ))
  }
  values <- as.character(env_variables)
  names_ <- names(env_variables)
  if (is.null(names_)) names_ <- rep("", length(values))
  forwarded <- is.na(values)
  resolved <- values
  resolved[forwarded] <- Sys.getenv(names_[forwarded], unset = NA_character_)
  sensitive <- vapply(names_, is_sensitive_environment_name, logical(1))
  resolved[sensitive & !is.na(resolved)] <- "<redacted>"
  data.frame(
    name = names_, value = resolved,
    source = ifelse(forwarded, "submission_environment", "configured"),
    redacted = sensitive, stringsAsFactors = FALSE
  )
}

redact_job_contract_command <- function(command, env_variables) {
  if (!checkmate::test_string(command) || is.null(env_variables)) return(command)
  names_ <- names(env_variables)
  if (is.null(names_)) return(command)
  sensitive <- vapply(names_, is_sensitive_environment_name, logical(1))
  values <- as.character(env_variables[sensitive])
  values <- values[!is.na(values) & nzchar(values)]
  for (value in unique(values)) {
    command <- gsub(value, "<redacted>", command, fixed = TRUE)
  }
  command
}

default_job_contract_environment_artifacts <- function() {
  c(
    # Run inputs whose contents determine what the scheduled job executes.
    "snapshot_rds", "filelist_path", "heudiconv_heuristic",
    "fs_license_file", "bids_validator", "flywheel_cmd",
    # Container images and executable scripts used by current pipeline stages.
    "heudiconv_container", "fmriprep_container", "mriqc_container",
    "aroma_container", "prefetch_container", "prefetch_script",
    "postprocess_rscript", "postprocess_image_sched_script",
    "extract_rscript", "extract_sched_script",
    # Installed helpers invoked by the scheduler scripts themselves.
    "insert_tracked_job_path", "upd_job_status_path", "add_parent_path"
  )
}

is_mutable_job_contract_environment_name <- function(name) {
  if (!checkmate::test_string(name)) return(FALSE)
  normalized <- tolower(name)
  normalized %in% c("bg_job_manifest", "bg_contract_directory") ||
    grepl(
      paste0(
        "(^|_)(sqlite_db|tracking_db|database_file|database_path|",
        "stdout_log|stdout_file|stdout_path|stderr_log|stderr_file|stderr_path|",
        "log|log_file|log_path|complete_file|complete_marker|fail_file|",
        "fail_marker|status_file|status_dir|status_marker|state_file|state_path|",
        "receipt_file|receipt_path|output_manifest|output_manifest_file|",
        "output_manifest_path|job_manifest|job_manifest_file|job_manifest_path)$"
      ),
      normalized,
      perl = TRUE
    )
}

job_contract_environment_artifact_names <- function(env_variables,
                                                    requested = NULL) {
  if (is.null(env_variables) || length(env_variables) == 0L ||
      is.null(names(env_variables))) {
    return(character())
  }
  requested <- as.character(requested)
  requested <- requested[!is.na(requested) & nzchar(requested)]
  selected <- unique(c(
    default_job_contract_environment_artifacts(), requested
  ))
  selected <- selected[
    !vapply(selected, is_mutable_job_contract_environment_name, logical(1))
  ]
  intersect(selected, names(env_variables))
}

job_contract_artifacts <- function(script, env_variables, tracking_args,
                                   contract_directory) {
  candidates <- list(batch_script = script)
  for (field in c("batch_file", "compute_file", "code_file", "config_snapshot_file", "execution_payload")) {
    value <- tracking_args[[field]]
    if (checkmate::test_string(value)) candidates[[field]] <- value
  }
  # Environment metadata can name logs, status markers, and the live tracking
  # database. Only checksum known immutable inputs or caller-selected inputs;
  # operational paths remain available in execution$environment for provenance.
  artifact_env_names <- job_contract_environment_artifact_names(
    env_variables, tracking_args$contract_artifact_env_names
  )
  if (length(artifact_env_names) > 0L) {
    for (env_name in artifact_env_names) {
      value <- env_variables[[env_name]]
      if (!is.na(value) && nzchar(value) && file.exists(value) && !dir.exists(value)) {
        candidates[[paste0("environment.", env_name)]] <- value
      }
    }
  }
  candidates <- candidates[vapply(candidates, function(path) {
    checkmate::test_string(path) && file.exists(path) && !dir.exists(path)
  }, logical(1))]
  if (length(candidates) == 0L) {
    return(data.frame(
      role = character(), configured_path = character(), path = character(),
      exists = logical(), size_bytes = numeric(), modified_at = character(),
      checksum_algorithm = character(), checksum = character(),
      stringsAsFactors = FALSE
    ))
  }
  cache_file <- file.path(
    dirname(dirname(dirname(contract_directory))), ".artifact_checksums.rds"
  )
  rows <- lapply(seq_along(candidates), function(i) {
    fingerprint_run_artifact(names(candidates)[[i]], candidates[[i]], cache_file)
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result[!duplicated(result$path), , drop = FALSE]
}

write_job_contract_once <- function(value, file, label) {
  assert_provenance_metadata(value, label)
  if (file.exists(file)) {
    stop(label, " already exists and will not be replaced: ", file, call. = FALSE)
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  temp <- tempfile("job-contract-", tmpdir = dirname(file), fileext = ".json")
  on.exit(if (file.exists(temp)) unlink(temp), add = TRUE)
  jsonlite::write_json(
    value, temp, pretty = TRUE, auto_unbox = TRUE, na = "null",
    null = "null", digits = NA
  )
  if (!file.rename(temp, file)) {
    stop("Failed to atomically write ", tolower(label), ": ", file, call. = FALSE)
  }
  normalizePath(file, winslash = "/", mustWork = TRUE)
}

write_job_manifest <- function(contract, script, scheduler, scheduler_executable,
                               scheduler_args, env_variables, export_all,
                               wait_jobs, wait_signal, rendered_command,
                               tracking_args) {
  checkmate::assert_list(contract)
  artifacts <- job_contract_artifacts(
    script, env_variables, tracking_args, contract$directory
  )
  manifest <- list(
    schema_version = "brain-gnomes-job-manifest-v1",
    contract_id = contract$contract_id,
    created_at = job_contract_timestamp(),
    state = "prepared_for_submission",
    run_id = tracking_args$sequence_id,
    logical_work_unit = list(
      unit_key = tracking_args$unit_key,
      stage = tracking_args$stage,
      stream = tracking_args$stream,
      subject_id = tracking_args$sub_id,
      session_id = tracking_args$ses_id,
      role = tracking_args$job_role,
      attempt = tracking_args$attempt
    ),
    scheduler = list(
      configured = scheduler,
      executable = scheduler_executable,
      arguments = scheduler_args,
      dependencies = as.character(wait_jobs),
      dependency_condition = wait_signal,
      export_all = isTRUE(export_all),
      requested_resources = list(
        nodes = tracking_args$n_nodes,
        cpus = tracking_args$n_cpus,
        wall_time = tracking_args$wall_time,
        memory_per_cpu = tracking_args$mem_per_cpu,
        memory_total = tracking_args$mem_total
      )
    ),
    execution = list(
      script = if (file.exists(script)) {
        normalizePath(script, winslash = "/", mustWork = TRUE)
      } else script,
      script_kind = if (file.exists(script)) "file" else "command",
      runtime = tracking_args$runtime,
      rendered_submission_command = redact_job_contract_command(
        rendered_command, env_variables
      ),
      environment = contract_environment_table(env_variables),
      stdout_log = tracking_args$stdout_log,
      stderr_log = tracking_args$stderr_log
    ),
    artifacts = artifacts,
    drift_policy = if (identical(tracking_args$contract_drift_policy, "record")) {
      "record"
    } else {
      "fail"
    }
  )
  path <- write_job_contract_once(manifest, contract$manifest_path, "Job manifest")
  list(
    contract_id = contract$contract_id,
    path = path,
    checksum_algorithm = "md5",
    checksum = unname(tools::md5sum(path))
  )
}

prepare_external_job_manifest <- function(
    tracking_sqlite_db, sequence_id, contract_directory, script, scheduler,
    scheduler_args = NULL, stage, stream = NULL, sub_id = NULL, ses_id = NULL,
    job_role = "job", job_name = NULL, stdout_log = NULL, stderr_log = NULL,
    env_variables = Sys.getenv()) {
  tracking_args <- list(
    job_name = job_name,
    sequence_id = sequence_id,
    contract_directory = contract_directory,
    stage = stage,
    stream = stream,
    sub_id = sub_id,
    ses_id = ses_id,
    job_role = job_role,
    stdout_log = stdout_log,
    stderr_log = stderr_log,
    scheduler_options = scheduler_args
  )
  tracking_args$unit_key <- tracking_unit_key(
    stage, stream, sub_id, ses_id, job_role, job_name
  )
  tracking_args$attempt <- next_job_contract_attempt(
    tracking_sqlite_db, tracking_args$unit_key, sequence_id
  )
  contract <- allocate_job_contract(tracking_sqlite_db, tracking_args)
  if (is.null(contract)) {
    stop("Cannot prepare a job manifest without a run ID and contract directory.", call. = FALSE)
  }
  env_variables["BG_JOB_MANIFEST"] <- contract$manifest_path
  env_variables["BG_RUN_ID"] <- sequence_id
  env_variables["BG_CONTRACT_DIRECTORY"] <- contract_directory
  rendered <- paste(scheduler, scheduler_args, script)
  manifest <- write_job_manifest(
    contract = contract,
    script = script,
    scheduler = scheduler,
    scheduler_executable = unname(Sys.which(scheduler)),
    scheduler_args = scheduler_args,
    env_variables = env_variables,
    export_all = TRUE,
    wait_jobs = NULL,
    wait_signal = "afterok",
    rendered_command = rendered,
    tracking_args = tracking_args
  )
  tracking_args$contract_id <- contract$contract_id
  tracking_args$job_manifest_path <- manifest$path
  tracking_args$job_manifest_checksum <- manifest$checksum
  register_job_submission(tracking_sqlite_db, contract, tracking_args)
  manifest
}

tracking_unit_key <- function(stage, stream = NULL, sub_id = NULL, ses_id = NULL,
                              job_role = NULL, job_name = NULL) {
  scalar <- function(value) {
    if (length(value) == 0L || is.na(value[[1L]])) "" else as.character(value[[1L]])
  }
  stage <- scalar(stage)
  stream <- scalar(stream)
  sub_id <- scalar(sub_id)
  ses_id <- scalar(ses_id)
  if (!nzchar(stage)) {
    return(paste("job", as.character(job_name), sep = "::"))
  }
  if (nzchar(sub_id)) {
    return(paste("subject", sub_id, ses_id, stage, stream, sep = "::"))
  }
  paste("project", stage, stream, scalar(job_name), sep = "::")
}

enrich_job_tracking_args <- function(scfg, tracking_args, stage,
                                     stream = NULL, sub_id = NULL,
                                     ses_id = NULL, job_role = "project",
                                     stdout_log = NULL, stderr_log = NULL) {
  run_id <- tracking_args$sequence_id
  tracking_args$stage <- stage
  tracking_args$stream <- stream
  tracking_args$sub_id <- sub_id
  tracking_args$ses_id <- ses_id
  tracking_args$job_role <- job_role
  tracking_args$unit_key <- tracking_unit_key(
    stage, stream, sub_id, ses_id, job_role, tracking_args$job_name
  )
  tracking_args$stdout_log <- stdout_log
  tracking_args$stderr_log <- stderr_log
  if (checkmate::test_string(run_id) &&
      checkmate::test_string(scfg$metadata$log_directory)) {
    run_dir <- file.path(scfg$metadata$log_directory, "runs", run_id)
    tracking_args$contract_directory <- file.path(run_dir, "jobs")
    config_snapshot <- file.path(run_dir, "project_config.yaml")
    if (file.exists(config_snapshot)) {
      tracking_args$config_snapshot_file <- config_snapshot
    }
  }
  tracking_args
}

job_contract_row <- function(sqlite_db, job_id) {
  result <- suppressWarnings(tryCatch(
    get_tracked_job_status(job_id = job_id, sqlite_db = sqlite_db),
    error = function(e) NULL
  ))
  if (!is.data.frame(result) || nrow(result) == 0L) NULL else result[1L, , drop = FALSE]
}

job_contract_row_value <- function(row, field, default = NA_character_) {
  if (is.null(row) || !field %in% names(row) || length(row[[field]]) == 0L ||
      is.na(row[[field]][[1L]])) {
    return(default)
  }
  as.character(row[[field]][[1L]])
}

job_runtime_details <- function() {
  scheduler_environment <- Sys.getenv(c(
    "SLURM_JOB_ID", "SLURM_ARRAY_JOB_ID", "SLURM_ARRAY_TASK_ID",
    "PBS_JOBID", "PBS_ARRAYID", "PBS_O_HOST", "HOSTNAME"
  ), unset = NA_character_)
  list(
    system = as.list(Sys.info()),
    process_id = Sys.getpid(),
    working_directory = normalizePath(getwd(), winslash = "/", mustWork = TRUE),
    command = commandArgs(trailingOnly = FALSE),
    scheduler_environment = as.list(scheduler_environment)
  )
}

persist_job_runtime_receipt <- function(receipt, receipt_path, sqlite_db,
                                        job_id, contract_status) {
  receipt_path <- write_job_contract_once(
    receipt, receipt_path, "Runtime receipt"
  )
  receipt_checksum <- unname(tools::md5sum(receipt_path))
  try(submit_tracking_query(
    paste(
      "UPDATE job_tracking SET runtime_receipt_path = ?,",
      "runtime_receipt_checksum = ?, runtime_host = ?, contract_status = ?",
      "WHERE job_id = ?"
    ),
    sqlite_db = sqlite_db,
    param = list(
      receipt_path, receipt_checksum, unname(Sys.info()[["nodename"]]),
      contract_status, job_id
    )
  ), silent = TRUE)
  attr(receipt, "path") <- receipt_path
  receipt
}

stop_for_job_contract <- function(receipt_path) {
  stop(
    "Job contract verification failed; see runtime receipt: ",
    receipt_path, call. = FALSE
  )
}

verify_job_contract_artifacts <- function(artifacts, manifest_path) {
  if (!is.data.frame(artifacts) || nrow(artifacts) == 0L) {
    return(data.frame(
      role = character(), path = character(), expected_checksum = character(),
      observed_checksum = character(), status = character(),
      stringsAsFactors = FALSE
    ))
  }
  run_dir <- dirname(dirname(dirname(manifest_path)))
  cache_file <- file.path(dirname(run_dir), ".artifact_checksums.rds")
  rows <- lapply(seq_len(nrow(artifacts)), function(i) {
    path <- as.character(artifacts$path[[i]])
    expected <- as.character(artifacts$checksum[[i]])
    exists <- checkmate::test_string(path) && file.exists(path) && !dir.exists(path)
    observed <- if (exists) {
      cached_artifact_checksum(path, cache_file = cache_file)
    } else {
      NA_character_
    }
    data.frame(
      role = as.character(artifacts$role[[i]]), path = path,
      expected_checksum = expected, observed_checksum = observed,
      status = if (!exists) "missing" else if (identical(observed, expected)) {
        "matched"
      } else {
        "changed"
      },
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

record_job_runtime_receipt <- function(sqlite_db, job_id, manifest_path = NULL) {
  checkmate::assert_string(sqlite_db)
  checkmate::assert_string(job_id)
  row <- job_contract_row(sqlite_db, job_id)
  has_manifest_path <- checkmate::test_string(manifest_path) && nzchar(manifest_path)
  if (!has_manifest_path) {
    manifest_path <- Sys.getenv("BG_JOB_MANIFEST", unset = "")
  }
  has_manifest_path <- checkmate::test_string(manifest_path) && nzchar(manifest_path)
  if (!has_manifest_path && !is.null(row) &&
      "job_manifest_path" %in% names(row)) {
    manifest_path <- as.character(row$job_manifest_path[[1L]])
  }
  if (!checkmate::test_string(manifest_path) || !nzchar(manifest_path)) {
    return(invisible(NULL))
  }
  manifest_path <- normalizePath(manifest_path, winslash = "/", mustWork = FALSE)
  receipt_path <- file.path(dirname(manifest_path), "runtime-receipt.json")
  if (file.exists(receipt_path)) {
    receipt <- tryCatch(
      jsonlite::read_json(receipt_path, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (!is.list(receipt)) {
      stop("Existing runtime receipt cannot be read: ", receipt_path, call. = FALSE)
    }
    if (identical(receipt$enforcement, "failed")) {
      stop_for_job_contract(receipt_path)
    }
    return(invisible(receipt))
  }

  expected_manifest_checksum <- job_contract_row_value(
    row, "job_manifest_checksum"
  )
  manifest_exists <- file.exists(manifest_path) && !dir.exists(manifest_path)
  observed_manifest_checksum <- if (manifest_exists) {
    unname(tools::md5sum(manifest_path))
  } else {
    NA_character_
  }
  manifest_error <- NULL
  manifest <- if (manifest_exists) {
    tryCatch(
      jsonlite::read_json(manifest_path, simplifyVector = TRUE),
      error = function(e) {
        manifest_error <<- conditionMessage(e)
        NULL
      }
    )
  } else {
    manifest_error <- "Manifest file is missing."
    NULL
  }
  if (!is.list(manifest)) {
    manifest_status <- if (manifest_exists) "unreadable" else "missing"
    receipt <- list(
      schema_version = "brain-gnomes-job-runtime-receipt-v1",
      contract_id = job_contract_row_value(row, "contract_id"),
      job_id = job_id,
      run_id = job_contract_row_value(row, "sequence_id"),
      recorded_at = job_contract_timestamp(),
      state = "started",
      verification_status = "drift",
      enforcement = "failed",
      manifest = list(
        path = manifest_path,
        expected_checksum = expected_manifest_checksum,
        observed_checksum = observed_manifest_checksum,
        status = manifest_status,
        error = manifest_error
      ),
      artifacts = verify_job_contract_artifacts(NULL, manifest_path),
      runtime = job_runtime_details()
    )
    receipt <- persist_job_runtime_receipt(
      receipt, receipt_path, sqlite_db, job_id, "DRIFT"
    )
    stop_for_job_contract(attr(receipt, "path"))
  }
  manifest_status <- if (is.na(expected_manifest_checksum) ||
      !nzchar(expected_manifest_checksum)) {
    "unavailable"
  } else if (identical(expected_manifest_checksum, observed_manifest_checksum)) {
    "matched"
  } else {
    "changed"
  }
  artifact_verification <- verify_job_contract_artifacts(
    manifest$artifacts, manifest_path
  )
  drift <- manifest_status == "changed" ||
    any(artifact_verification$status != "matched")
  must_stop <- manifest_status == "changed" ||
    (drift && identical(manifest$drift_policy, "fail"))
  receipt <- list(
    schema_version = "brain-gnomes-job-runtime-receipt-v1",
    contract_id = manifest$contract_id,
    job_id = job_id,
    run_id = manifest$run_id,
    recorded_at = job_contract_timestamp(),
    state = "started",
    verification_status = if (drift) "drift" else "verified",
    enforcement = if (must_stop) "failed" else if (drift) "recorded" else "passed",
    manifest = list(
      path = manifest_path,
      expected_checksum = expected_manifest_checksum,
      observed_checksum = observed_manifest_checksum,
      status = manifest_status
    ),
    artifacts = artifact_verification,
    runtime = job_runtime_details()
  )
  receipt <- persist_job_runtime_receipt(
    receipt, receipt_path, sqlite_db, job_id,
    if (drift) "DRIFT" else "VERIFIED"
  )
  if (must_stop) {
    stop_for_job_contract(attr(receipt, "path"))
  }
  invisible(receipt)
}
