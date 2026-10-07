#' This function submits a single script to a high-performance cluster using a scheduler (Slurm or TORQUE).
#' It accepts a vector of arguments to be passed to the scheduler and
#' a vector of environment variables that should be passed to the compute node at job execution.
#'
#' The function returns the jobid of the scheduled job.
#'
#' @param script A path to a script that should be executed by the scheduler or a single command to be run. When
#'      \code{script} does not point to an existing file, it is treated as a one-line command to execute. In the case of
#'      conflicts, the directives passed with \code{sched_args} will take precedence.
#' @param scheduler Which scheduler to use for job submission. Options are 'qsub', 'torque', 'sbatch', 'slurm', or 'sh'.
#'      The terms 'qsub' and 'torque' are aliases (where 'torque' submits via the qsub command). Likewise for 'sbatch'
#'      and 'slurm'. The scheduler 'sh' does not submit to any scheduler at all, but instead executes the command
#'      immediately via sh.
#' @param sched_args A character vector of arguments to be included in the scheduling command. On TORQUE, these
#'      will typically begin with '-l' such as '-l wall_time=10:00:00'.
#' @param env_variables A named character vector containing environment variables and their values to be passed
#'      to the \code{script} at execution time. This is handled by the '-v' directive on TORQUE clusters and
#'      by '--export' on Slurm clusters. The names of this vector are the environment variable names and
#'      the values of the vector are the environment variable values to be passed in.
#'      If you want to propagate the current value of an environment variable to the compute node at runtime,
#'      use NA as the value of the element in \code{env_variables}. See examples.
#' @param export_all Whether to export all environment variables to the compute node at runtime. Default: FALSE
#' @param echo Whether to echo the job submission command to the terminal at the time it is scheduled. Default: TRUE.
#' @param fail_on_error Whether to stop execution of the script (TRUE), or issue a warning (FALSE) if the job
#'      submission fails. Defaults to FALSE (i.e., issue a warning).
#' @param wait_jobs a character string of jobs or process ids that should complete before this job is executed
#' @param wait_signal on torque or slurm clusters, the signal that should indicate that parent jobs have finished.
#' @param repolling_interval The number of seconds to wait before rechecking job status (used only for local scheduler)
#' @param tracking_sqlite_db Path to a SQLite database used for job tracking. If provided, job submission metadata
#'   will be recorded, including job dependencies and parent-child relationships.
#' @param tracking_args A named list of metadata fields to track about the submitted job. This may include the parent job ID,
#'   batch script path, and scheduler options. The optional
#'   \code{contract_artifact_env_names} element explicitly identifies additional
#'   file-valued environment variables that are immutable execution inputs and
#'   should be checksummed in the job contract. Mutable operational files such
#'   as tracking databases, logs, status markers, and output manifests are
#'   always excluded. This is used in conjunction with
#'   \code{tracking_sqlite_db} to support job tracking.
#'
#' @return A character string containing the jobid of the scheduled job.
#'
#' @examples
#' \dontrun{
#'   #simple PBS submission
#'   cluster_job_submit('myscript.bash', scheduler="torque", 
#'      sched_args=c('-l walltime=10:00:00', '-l nodes=1:ppn=20'),
#'      env_variables=c(RUN_INDEX=2, MODEL_NAME='FSE21'))
#'
#'   #To forward environment variables without explicitly providing values. Note that these must
#'   #  be in R's system environment (cf. Sys.getenv) at execution time to forward as expected.
#'   cluster_job_submit('myscript.sbatch', scheduler="slurm",
#'      sched_args=c('-p general', '-N 1', '-n 12', '--mem=10g', '-t 02-00:00:00'),
#'      env_variables=c(RUN_INDEX=2, R_HOME=NA, JAVA_HOME=NA))
#'
#'   # submit a single command without a script file
#'   cluster_job_submit('echo "hello"', scheduler="slurm")
#' }
#'
#' @author Michael Hallquist
#' @importFrom tools file_path_sans_ext
#' @importFrom checkmate assert_character assert_subset assert_logical
#' @export
cluster_job_submit <- function(script, scheduler="slurm", sched_args=NULL,
                           env_variables=NULL, export_all=FALSE, echo=TRUE, fail_on_error=FALSE, 
                           wait_jobs=NULL, wait_signal="afterok", repolling_interval=60, 
                           tracking_sqlite_db=NULL, tracking_args=list()) {

  checkmate::assert_string(script)
  script_exists <- file.exists(script)
  checkmate::assert_string(scheduler)
  checkmate::assert_choice(scheduler, c("qsub", "torque", "sbatch", "slurm", "sh", "local"))
  checkmate::assert_logical(export_all, max.len = 1L)
  checkmate::assert_logical(echo, max.len = 1L)
  checkmate::assert_logical(fail_on_error, max.len=1L)
  if (is.character(tracking_args) || is.numeric(tracking_args)) tracking_args <- as.list(tracking_args) # coerce tracking args to list
  if (length(tracking_args) > 0 && is.null(tracking_sqlite_db)) {
    warning("Tracking arguments provided to `cluster_job_submit` but `tracking_sqlite_db` is NULL")
  }

  if (scheduler %in% c("torque", "qsub")) {
    scheduler <- "qsub" # simpler internal tracking
    if (isTRUE(export_all)) {
      sched_args <- c("-V", sched_args)
    } # directive to export all environment variables to script
  } else if (scheduler %in% c("slurm", "sbatch")) {
    scheduler <- "sbatch"
    if (isTRUE(export_all)) {
      env_variables <- c(ALL = NA, env_variables)
    } # directive to export all environment variables to script
  } else if (scheduler %in% c("sh", "local")) {
    scheduler <- "sh"
    if (!is.null(sched_args)) {
      message("Omitting scheduler arguments for sh/local execution")
    }
    sched_args <- NULL # not relevant
  }

  # Scheduler arguments are just pasted together with spaces.
  # Thus, arguments like '--mem=5g' and '-n 12' are not handled differently
  if (!is.null(sched_args)) { sched_args <- paste(sched_args, collapse=" ") }

  if (is.null(tracking_args$attempt) &&
      checkmate::test_string(tracking_args$unit_key)) {
    tracking_args$attempt <- next_job_contract_attempt(
      tracking_sqlite_db, tracking_args$unit_key,
      tracking_args$sequence_id
    )
  }

  contract <- allocate_job_contract(tracking_sqlite_db, tracking_args)
  if (!is.null(contract)) {
    env_variables["BG_JOB_MANIFEST"] <- contract$manifest_path
    env_variables["BG_RUN_ID"] <- tracking_args$sequence_id
    env_variables["BG_CONTRACT_DIRECTORY"] <- dirname(contract$directory)
  }

  #subfunction to handle variable=value and variable combinations
  paste_args <- function(str_vec) {
    nms <- names(str_vec)
    stopifnot(!is.null(nms))
    sapply(seq_along(str_vec), function(x) {
      if (is.na(str_vec[x])) {
        return(nms[x]) #just the name of the env variable (forwards from environment)
      } else {
        # force argument to be quoted to avoid problems with spaces
        val <- shQuote(str_vec[x], type = "sh")
        return(paste0(nms[x], "=", val))
      }
    })
  }

  #pass through environment variables
  if (!is.null(tracking_sqlite_db)) {
    env_variables["sqlite_db"] <- tracking_sqlite_db
  } else {
    env_variables["sqlite_db"] <- NA
  }
  assert_absolute_scheduler_paths(env_variables, tracking_sqlite_db)
  contract_env_variables <- env_variables
  if (!is.null(env_variables)) {
    env_variables <- paste_args(env_variables) #convert to appropriate name-value pairs
    if (scheduler == "qsub") {
      env_variables <- paste("-v", paste(env_variables, collapse=","))
    } else if (scheduler == "sbatch") {
      env_variables <- paste0("--export=", paste(env_variables, collapse=","))
    } else if (scheduler == "sh") {
      env_variables <- paste(sapply(env_variables, function(x) {
        ifelse(grepl("=", x, fixed = TRUE), x, paste0(x, "=\"$", x, "\""))
      }), collapse = " ")
    }

    sched_args <- paste(sched_args, env_variables)
  }

  if (!is.null(wait_jobs)) {
    jcomb <- paste(wait_jobs, collapse = ":") # multiple jobs are separated by colons
    if (scheduler == "qsub") {
      sched_args <- paste0(sched_args, " -W depend=", wait_signal, ":", jcomb)
    } else if (scheduler == "sbatch") {
      sched_args <- paste0(sched_args, " --dependency=", wait_signal, ":", jcomb)
    }
  }

  # use unique temp files to avoid parallel collisions in job tracking
  script_label <- if (script_exists) tools::file_path_sans_ext(basename(script)) else "oneliner"
  sub_stdout <- paste0(tempfile(), "_", script_label, "_stdout")
  sub_stderr <- paste0(tempfile(), "_", script_label, "_stderr")
  sub_pid <- paste0(tempfile(), "_", script_label, "_pid")

  if (scheduler == "sh") {
    # if an R script file is provided, execute with Rscript --vanilla
    if (script_exists && grepl(".+\\.R$", script, ignore.case = TRUE)) {
      run_part <- paste("Rscript --vanilla", script)
    } else if (script_exists) {
      run_part <- paste("sh", script)
    } else {
      run_part <- script
    }

    # for local scheduler, we need to hold jobs manually by waiting for relevant parents to complete
    if (!is.null(wait_jobs)) {
      message("Waiting for the following jobs to finish: ", paste(wait_jobs, collapse=","))
      wait_for_job(wait_jobs, repolling_interval = repolling_interval, scheduler = scheduler)
      # ZV: grab return code here for parent job failing?
    }

    # for direct execution, need to pass environment variables by prepending
    cmd <- paste(env_variables, run_part)
  } else if (script_exists) {
    cmd <- paste(scheduler, sched_args, script)
  } else if (scheduler == "sbatch") {
    cmd <- paste(scheduler, sched_args, paste0("--wrap=", shQuote(script)))
  } else if (scheduler == "qsub") {
    cmd <- paste("echo", shQuote(script), "|", scheduler, sched_args)
  } else {
    cmd <- paste(scheduler, sched_args, script)
  }

  if (!is.null(contract)) {
    manifest <- write_job_manifest(
      contract = contract,
      script = script,
      scheduler = scheduler,
      scheduler_executable = unname(Sys.which(scheduler)),
      scheduler_args = sched_args,
      env_variables = contract_env_variables,
      export_all = export_all,
      wait_jobs = wait_jobs,
      wait_signal = wait_signal,
      rendered_command = cmd,
      tracking_args = tracking_args
    )
    tracking_args$contract_id <- manifest$contract_id
    tracking_args$job_manifest_path <- manifest$path
    tracking_args$job_manifest_checksum <- manifest$checksum
  }

  if (scheduler == "sh") {
    if (isTRUE(echo)) cat(cmd, "\n") # echo command to terminal
    # submit the job script and return the jobid by forking to background and returning PID
    jobres <- system(paste(cmd, ">", sub_stdout, "2>", sub_stderr, "& echo $! >", sub_pid), wait = FALSE)
    Sys.sleep(.05) #sometimes the pid file is not in place when file.exists executes -- add a bit of time to ensure that it reads
    jobid <- if (file.exists(sub_pid)) scan(file = sub_pid, what = "char", sep = "\n", quiet = TRUE) else ""
  } else {
    if (script_exists) {
      if (isTRUE(echo)) cat(cmd, "\n")
      jobres <- system2(scheduler, args = paste(sched_args, script), stdout = sub_stdout, stderr = sub_stderr)
    } else if (scheduler == "sbatch") {
      # one-liner command to sbatch using --wrap
      if (isTRUE(echo)) cat(cmd, "\n")
      jobres <- system2(scheduler, args = paste0(sched_args, " --wrap=", shQuote(script)), stdout = sub_stdout, stderr = sub_stderr)
    } else if (scheduler == "qsub") {
      # one-liner command to qsub using echo <cmd> | qsub syntax
      if (isTRUE(echo)) cat(cmd, "\n")
      jobres <- system(paste(cmd, ">", sub_stdout, "2>", sub_stderr))
    } else {
      if (isTRUE(echo)) cat(cmd, "\n")
      jobres <- system2(scheduler, args = paste(sched_args, script), stdout = sub_stdout, stderr = sub_stderr)
    }
    jobid <- if (file.exists(sub_stdout)) scan(file = sub_stdout, what = "char", sep = "\n", quiet = TRUE) else ""
  }

  joberr <- if (file.exists(sub_stderr)) {
    paste(scan(file = sub_stderr, what = "char", sep = "\n", quiet = TRUE), collapse = ". ")
  } else {
    ""
  }

  if (jobres != 0) {
    jobid <- NULL
    if (isTRUE(fail_on_error)) {
      stop("Job submission failed: ", script, ", error: ", joberr, ", errcode: ", jobres)
    } else {
      warning("Job submission failed: ", script, ", error: ", joberr, ", errcode: ", jobres)
    }
  } else {
    jobid <- sub("Submitted batch job ", "", jobid, fixed = TRUE) # replace irrelevant details if needed
    if (!is.null(tracking_args)) tracking_args$status <- "QUEUED" # on successful submission, tracking status defaults to "QUEUED"
  }

  # once a job_id has been generated, we add it to the tracking db
  # if a job has a NULL id or the tracking_sqlite_db arg is NULL, the function will return invisible NULL
  insert_tracked_job(sqlite_db = tracking_sqlite_db, job_id = jobid, tracking_args = tracking_args)

  if (!is.null(wait_jobs)) {
    # add any parent jobs using the wait_jobs argument (defaults to last parent id in list)
    add_tracked_job_parent(sqlite_db = tracking_sqlite_db, job_id = jobid, parent_job_id = wait_jobs[length(wait_jobs)], child_level = 1)
  } else if (!is.null(tracking_args$parent_job_id)) {
    # in the case that a parent job id is passed in through the tracking_args list
    add_tracked_job_parent(sqlite_db = tracking_sqlite_db, job_id = jobid, parent_job_id = tracking_args$parent_job_id, child_level = 1)
  }
  
  if (!is.null(jobid)) attr(jobid, "cmd") <- cmd # add the command executed as an attribute

  return(jobid)
}


# Convert scheduler-specific states to the lifecycle vocabulary used by job
# waiting and project inspection. Keep this scheduler-aware because, for
# example, `S` means sleeping (active) to ps but suspended to some schedulers.
normalize_scheduler_job_status <- function(status, scheduler) {
  scheduler <- tolower(as.character(scheduler)[[1L]])
  scheduler <- switch(scheduler,
    sbatch = "slurm", qsub = "torque", sh = "local", scheduler
  )
  status <- toupper(trimws(as.character(status)))
  status <- sub("[+].*$", "", status)
  status <- sub("[[:space:]].*$", "", status)
  normalized <- rep("UNKNOWN", length(status))
  normalized[is.na(status) | !nzchar(status) | status == "MISSING"] <-
    "MISSING"

  if (scheduler == "local") {
    normalized[status %in% c("D", "I", "R", "S", "U", "W")] <- "RUNNING"
    normalized[status == "T"] <- "SUSPENDED"
    normalized[status == "C"] <- "COMPLETED"
    normalized[status %in% c("X", "Z")] <- "FAILED"
  } else {
    normalized[status %in% c(
      "PENDING", "CONFIGURING", "REQUEUED", "RESIZING", "Q", "W", "QUEUED"
    )] <- "QUEUED"
    normalized[status %in% c("RUNNING", "COMPLETING", "R", "E", "H")] <-
      "RUNNING"
    normalized[status %in% c("SUSPENDED", "STOPPED", "S", "T")] <-
      "SUSPENDED"
    normalized[status %in% c("COMPLETED", "COMPLETE", "C")] <- "COMPLETED"
    normalized[status == "CANCELLED"] <- "CANCELLED"
    normalized[status %in% c(
      "BOOT_FAIL", "DEADLINE", "FAILED", "NODE_FAIL", "OUT_OF_MEMORY",
      "PREEMPTED", "REVOKED", "TIMEOUT", "ZOMBIE", "Z"
    )] <- "FAILED"
  }
  normalized[status == "UNAVAILABLE"] <- "UNAVAILABLE"
  normalized
}

# Scheduler-neutral adapter over the existing scheduler-specific query
# functions. This is deliberately internal: users inspect in-flight work with
# inspect_project(), while wait_for_job() consumes the same canonical result.
scheduler_job_status <- function(job_ids, scheduler = "local", user = NULL) {
  job_ids <- as.character(job_ids)
  scheduler <- tolower(as.character(scheduler)[[1L]])
  scheduler <- switch(scheduler,
    sbatch = "slurm", qsub = "torque", sh = "local", scheduler
  )
  empty_result <- function() {
    data.frame(
      job_id = character(), scheduler = character(),
      scheduler_status = character(), scheduler_raw_status = character(),
      query_detail = character(), stringsAsFactors = FALSE
    )
  }
  unavailable_result <- function(detail) {
    data.frame(
      job_id = job_ids, scheduler = scheduler,
      scheduler_status = rep("UNAVAILABLE", length(job_ids)),
      scheduler_raw_status = rep(NA_character_, length(job_ids)),
      query_detail = rep(detail, length(job_ids)), stringsAsFactors = FALSE
    )
  }
  if (length(job_ids) == 0L) return(empty_result())

  command <- switch(scheduler,
    slurm = "sacct", torque = "qselect", local = "ps", ""
  )
  if (!nzchar(command)) {
    return(unavailable_result(paste0("Unsupported scheduler: ", scheduler)))
  }
  if (!nzchar(Sys.which(command))) {
    return(unavailable_result(paste0(
      "Scheduler command is unavailable: ", command
    )))
  }

  result <- tryCatch(
    switch(scheduler,
      slurm = slurm_job_status(job_ids, user = user),
      torque = torque_job_status(job_ids, user = user),
      local = local_job_status(job_ids, user = user)
    ),
    error = function(error) error
  )
  if (inherits(result, "error")) {
    return(unavailable_result(conditionMessage(result)))
  }

  id_column <- if (scheduler == "local") "PID" else "JobID"
  status_column <- if (scheduler == "local") "STAT" else "State"
  if (!all(c(id_column, status_column) %in% names(result))) {
    return(unavailable_result(
      "Scheduler query did not return job identifiers and states."
    ))
  }
  keep <- !duplicated(as.character(result[[id_column]]))
  match_index <- match(job_ids, as.character(result[[id_column]][keep]))
  raw_status <- as.character(result[[status_column]][keep])[match_index]
  raw_status[is.na(raw_status) | !nzchar(raw_status)] <- "MISSING"
  data.frame(
    job_id = job_ids, scheduler = scheduler,
    scheduler_status = normalize_scheduler_job_status(raw_status, scheduler),
    scheduler_raw_status = raw_status,
    query_detail = if ("QueryDetail" %in% names(result)) {
      as.character(result$QueryDetail[keep])[match_index]
    } else {
      rep(NA_character_, length(job_ids))
    },
    stringsAsFactors = FALSE
  )
}


#' This function pauses execution of an R script while a scheduled qsub job is not yet complete.
#'
#' It is intended to give you control over job dependencies within R when the formal PBS
#' depend approach is insufficient, especially in the case of a script that spawns child jobs that
#' need to be scheduled or complete before the parent script should continue.
#'
#' @param job_ids One or more job ids of existing PBS or slurm jobs, or process ids of a local process for
#'   \code{scheduler="sh"}.
#' @param repolling_interval How often to recheck the job status, in seconds. Default: 30
#' @param max_wait How long to wait on the job before giving up, in seconds. Default: 24 hours (86,400 seconds)
#' @param scheduler What scheduler is used for job execution.
#'   Options: c("torque", "qsub", "slurm", "sbatch", "sh", "local")
#' @param quiet If \code{TRUE}, \code{wait_for_job} will not print out any status updates on jobs. If \code{FALSE},
#'   the function prints out status updates for each tracked job so that the user knows what's holding up progress.
#' @param stop_on_timeout Logical. If `TRUE`, the function throws an error if the `max_wait` is exceeded.
#'   If `FALSE`, it returns `FALSE` instead of stopping. Default is `TRUE`.
#'
#' @return Returns (invisibly) `TRUE` if all jobs completed successfully, `FALSE` if any job failed or timeout occurred
#'   and `stop_on_timeout = FALSE`. Otherwise, stops execution with an error if the timeout is exceeded.
#'
#' @details Note that for the \code{scheduler} argument, "torque" and "qsub" are the same;
#'   "slurm" and "sbatch" are the same, and "sh" and "local" are the same.
#'   This function waits on scheduler observations, not the project's SQLite
#'   tracking state. Missing Slurm or TORQUE records are not evidence of success:
#'   waiting continues until a known terminal state or the timeout. In particular,
#'   expired TORQUE records cannot establish successful completion.
#'   Confirmed terminal states are retained for this wait invocation while other
#'   jobs finish, so subsequent accounting expiry does not erase that evidence.
#' @examples
#' \dontrun{
#' # example on qsub/torque cluster
#' wait_for_job("7968857.torque01.util.production.int.aci.ics.psu.edu", scheduler = "torque")
#'
#' # example of waiting for two jobs on slurm cluster
#' wait_for_job(c("24147864", "24147876"), scheduler = "slurm")
#'
#' # example of waiting for two jobs on local machine
#' wait_for_job(c("9843", "9844"), scheduler = "local")
#' }
#'
#' @author Michael Hallquist
#' @importFrom checkmate assert_number assert_subset
#' @export
wait_for_job <- function(job_ids, repolling_interval = 60, max_wait = 60 * 60 * 24,
                         scheduler = "local", quiet = TRUE, stop_on_timeout = TRUE) {
  checkmate::assert_number(repolling_interval, lower = 0.1, upper = 2e5)
  checkmate::assert_number(max_wait, lower = 1, upper = 1814400) # 21 days
  scheduler <- tolower(scheduler) # ignore case
  checkmate::assert_subset(scheduler, c("torque", "qsub", "slurm", "sbatch", "sh", "local"))

  job_complete <- FALSE
  wait_start <- Sys.time()
  terminal_status <- rep(NA_character_, length(job_ids))

  # Poll unresolved jobs only; terminal observations cannot expire out of this
  # invocation's evidence while slower jobs are still running. This cache is
  # not a tracking-database update, and never infers success from disappearance.
  get_job_status <- function() {
    unresolved <- which(is.na(terminal_status))
    state <- terminal_status
    if (length(unresolved)) {
      status <- scheduler_job_status(job_ids[unresolved], scheduler = scheduler)
      observed <- tolower(status$scheduler_status)
      observed[observed == "completed"] <- "complete"
      observed[observed == "unavailable"] <- "unknown"
      state[unresolved] <- observed
      terminal <- observed %in% c("complete", "failed", "cancelled")
      terminal_status[unresolved[terminal]] <<- observed[terminal]
    }
    state
  }

  ret_code <- NULL # should be set to TRUE if all jobs complete and FALSE if any job fails 

  while (job_complete == FALSE) {
    status <- get_job_status()

    # update wait time
    wait_total <- as.numeric(difftime(Sys.time(), wait_start, units = "secs"))

    # Debugging
    # cat("Wait so far: ", wait_total, "\n")

    if (any(status == "running") && isFALSE(quiet)) {
      cat("Job(s) still running:", paste(job_ids[status == "running"], collapse = ", "), "\n")
    }

    if (any(status == "queued") && isFALSE(quiet)) {
      cat("Job(s) still queued:", paste(job_ids[status == "queued"], collapse = ", "), "\n")
    }

    if (any(status == "suspended") && isFALSE(quiet)) {
      cat("Job(s) suspended:", paste(job_ids[status == "suspended"], collapse = ", "), "\n")
    }

    if (any(status == "missing") && isFALSE(quiet)) {
      cat("Job(s) missing from scheduler response:", paste(job_ids[status == "missing"], collapse = ", "), "\n")
    }

    if (wait_total > max_wait) {
      if (isTRUE(stop_on_timeout)) {
        stop("Maximum wait time: ", max_wait, " exceeded. Stopping execution of parent script because something is wrong.")
      } else {
        return(FALSE)
      }
    } else if (all(status %in% c("failed", "complete", "cancelled"))) {
      job_complete <- TRUE # drop out of this loop
      if (isFALSE(quiet)) {
        cat("All jobs have finished.\n")
      }
      if (any(status %in% c("failed", "cancelled"))) {
        cat("The following job(s) failed or were cancelled:", paste(job_ids[status %in% c("failed", "cancelled")], collapse = ", "), "\n")
        ret_code <- FALSE
      } else {
        ret_code <- TRUE
      }
    } else {
      Sys.sleep(repolling_interval) # wait and repoll jobs
    }
  }

  return(invisible(ret_code))
}

#' Collapse Slurm allocation/task observations to the requested job identifiers
#'
#' @param records Parsed sacct allocation records with JobID and State columns.
#' @param job_ids Requested allocation or individual array-task identifiers.
#' @return One row per requested identifier, with array state counts in QueryDetail.
#' @noRd
aggregate_slurm_job_status <- function(records, job_ids) {
  records$JobID <- as.character(records$JobID)
  # Steps are not independently scheduled tasks; do not let a .batch/.extern
  # record mask its allocation state. Exact task requests remain independent.
  records <- records[!grepl("[.]", records$JobID), , drop = FALSE]
  if (length(job_ids) == 0L) {
    records$QueryDetail <- character(nrow(records))
    return(records[FALSE, , drop = FALSE])
  }
  rows <- lapply(as.character(job_ids), function(job_id) {
    task_rows <- grepl("_[0-9]+$", records$JobID) &
      sub("_[0-9]+$", "", records$JobID) == job_id
    indices <- if (any(task_rows)) which(task_rows) else which(records$JobID == job_id)
    if (!length(indices)) {
      row <- records[NA_integer_, , drop = FALSE]
      row$JobID <- job_id
      row$State <- "MISSING"
      row$QueryDetail <- NA_character_
      return(row)
    }
    observed <- records[indices, , drop = FALSE]
    row <- observed[1L, , drop = FALSE]
    row$JobID <- job_id
    row$QueryDetail <- NA_character_
    if (any(task_rows)) {
      states <- normalize_scheduler_job_status(observed$State, "slurm")
      # Keep waiting while any task is active, even when another has failed.
      # Unknown tasks prevent a successful terminal result. Accounting is an
      # observation only: these aggregates never write tracking SQLite state.
      precedence <- c("RUNNING", "QUEUED", "SUSPENDED", "UNAVAILABLE",
                      "MISSING", "UNKNOWN", "FAILED", "CANCELLED", "COMPLETED")
      state <- precedence[precedence %in% states][[1L]]
      row$State <- state
      counts <- table(states)
      row$QueryDetail <- paste0(
        "Observed ", nrow(observed), " array tasks: ",
        paste(names(counts), as.integer(counts), sep = "=", collapse = ", ")
      )
      # Task-specific values (including exit codes) must not masquerade as
      # parent allocation values. Only aggregate the standard timestamps.
      other_columns <- setdiff(names(row), c("JobID", "State", "QueryDetail"))
      row[other_columns] <- NA_character_
      for (column in intersect(c("Submit", "Start", "End"), names(row))) {
        values <- observed[[column]]
        known <- !is.na(values) & nzchar(values) & !values %in% c("Unknown", "None")
        if (column == "End") {
          terminal <- all(states %in% c("COMPLETED", "FAILED", "CANCELLED"))
          if (terminal && all(known)) row[[column]] <- max(values)
        } else if (any(known)) {
          row[[column]] <- min(values[known])
        }
      }
    }
    row
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

#' Query Slurm accounting, expanding arrays before aggregating task states
#'
#' @param job_ids Requested job identifiers, or NULL to list observed allocations.
#' @param user Optional usernames to filter accounting records.
#' @param sacct_format Comma-separated sacct fields including JobID and State.
#' @return A data frame of scheduler observations, not persisted tracking states.
#' @noRd
slurm_job_status <- function(job_ids = NULL, user = NULL,
                             sacct_format = "jobid%100,submit,timelimit,start,end,state%40") {
  if (!is.null(job_ids) && !length(job_ids)) {
    return(data.frame(JobID = character(), State = character(), QueryDetail = character()))
  }
  args <- c("--array", "-X", "-P", "-o", shQuote(sacct_format))
  if (!is.null(job_ids)) args <- c(args, "-j", shQuote(paste(job_ids, collapse = ",")))
  if (!is.null(user)) args <- c(args, "-u", shQuote(paste(user, collapse = ",")))
  # The timestamp aggregates below require sortable ISO-style timestamps,
  # regardless of a user's interactive SLURM_TIME_FORMAT preference.
  res <- suppressWarnings(system2("sacct", args = args, stdout = TRUE, stderr = TRUE,
                                  env = "SLURM_TIME_FORMAT=standard"))
  if (!is.null(attr(res, "status")) && attr(res, "status") != 0L) {
    stop("sacct query failed: ", paste(res, collapse = "\n"), call. = FALSE)
  }
  out <- if (length(res) && any(nzchar(trimws(res)))) {
    data.table::fread(text = paste(res, collapse = "\n"), data.table = FALSE,
                      colClasses = "character")
  } else {
    data.frame(JobID = character(), State = character(), stringsAsFactors = FALSE)
  }
  if (!all(c("JobID", "State") %in% names(out))) {
    stop("Missing JobID or State columns in sacct output.", call. = FALSE)
  }
  if (is.null(job_ids)) job_ids <- unique(sub("_[0-9]+$", "", out$JobID[!grepl("[.]", out$JobID)]))
  aggregate_slurm_job_status(out, job_ids)
}

#' Query TORQUE without treating expired records as successful jobs
#'
#' @param job_ids Requested TORQUE job identifiers.
#' @param user Optional usernames; defaults to the current user.
#' @return Scheduler observations; retained C jobs require a known exit status.
#' @noRd
torque_job_status <- function(job_ids, user = NULL) {
  job_ids <- as.character(job_ids)
  result <- data.frame(JobID = job_ids, State = rep("MISSING", length(job_ids)),
                       QueryDetail = rep(NA_character_, length(job_ids)))
  if (!length(job_ids)) return(result)
  if (is.null(user)) user <- Sys.info()[["user"]]
  # Query lists separately because qselect permits only a single state selector.
  lists <- lapply(c("QWH", "ERT", "C"), function(states) {
    res <- suppressWarnings(system2("qselect", args = c(
      "-u", shQuote(paste(user, collapse = ",")), "-s", states
    ), stdout = TRUE, stderr = TRUE))
    if (!is.null(attr(res, "status")) && attr(res, "status") != 0L) {
      stop("qselect query failed: ", paste(res, collapse = "\n"), call. = FALSE)
    }
    trimws(res[nzchar(trimws(res))])
  })
  result$State[job_ids %in% lists[[1L]]] <- "QUEUED"
  result$State[job_ids %in% lists[[2L]]] <- "RUNNING"
  for (index in which(job_ids %in% lists[[3L]])) {
    # C means finished, not necessarily successful. Records can also expire
    # between qselect and qstat; preserve uncertainty rather than guess success.
    res <- suppressWarnings(system2("qstat", args = c("-f", shQuote(job_ids[[index]])),
                                    stdout = TRUE, stderr = TRUE))
    exit_line <- grep("^[[:space:]]*exit_status[[:space:]]*=[[:space:]]*-?[0-9]+[[:space:]]*$",
                      res, value = TRUE, ignore.case = TRUE)
    if ((!is.null(attr(res, "status")) && attr(res, "status") != 0L) ||
        length(exit_line) != 1L) {
      result$State[[index]] <- "UNKNOWN"
      result$QueryDetail[[index]] <- "Retained TORQUE job has no readable exit status."
    } else {
      exit_code <- trimws(sub(".*=", "", exit_line))
      result$State[[index]] <- if (exit_code == "0") "COMPLETED" else "FAILED"
      result$QueryDetail[[index]] <- paste0("TORQUE exit_status=", exit_code)
    }
  }
  result
}

#' Query local process status by PID
#'
#' This function queries the local system for process information using the `ps` command.
#' It returns a data frame of status information for a set of PIDs (process IDs), including
#' whether each process is running, sleeping, or has completed.
#'
#' The function is useful for checking the status of local jobs launched via background
#' processes or system calls, particularly in scripting or pipeline execution.
#'
#' @param job_ids A numeric or character vector of process IDs (PIDs) to query.
#' @param user Optional character vector of usernames used to filter processes by owner.
#' @param ps_format A character string specifying the `ps` output format. Default:
#'   `"user,pid,state,time,etime,\%cpu,\%mem,comm,xstat"`.
#'
#' @return A data frame with one row per PID, including the `STAT` column which reports
#'   the process status (e.g., R = running, S = sleeping, C = complete, Z = zombie).
#'
#' @details
#' This function uses the `ps` system command to query process state. If a PID
#' does not appear in the `ps` output, it is assumed to have completed and its
#' `STAT` value will be set to `"C"`. For running or sleeping processes, `STAT`
#' reflects the current process state reported by the OS.
#'
#' Column names are harmonized across systems (e.g., renaming `S` to `STAT`
#' and `COMMAND` to `COMM` if present). It uses `data.table::fread()` to parse output.
#'
#' @note This function is platform-dependent and intended for UNIX-like systems.
#' It may not work properly on Windows.
#'
#' @examples
#' \dontrun{
#' # Check status of current R process
#' local_job_status(Sys.getpid())
#' }
#'
#' @importFrom checkmate assert_integerish
#' @importFrom data.table fread setnames
#' @keywords internal
#' @noRd
local_job_status <- function(job_ids = NULL, user = NULL,
                             ps_format = "user,pid,state,time,etime,%cpu,%mem,comm,xstat") {
  job_ids <- type.convert(job_ids, as.is = T) # convert to integers
  checkmate::assert_integerish(job_ids)

  jstring <- if (!is.null(job_ids)) paste("-p", paste(job_ids, collapse = ",")) else ""
  ustring <- if (!is.null(user)) paste("-u", paste(user, collapse = ",")) else ""

  # cat(paste("ps", jstring, ustring, "-o", ps_format), sep = "\n")
  res <- suppressWarnings(system2("ps", args = paste(jstring, ustring, "-o", ps_format), stdout = TRUE))

  # need to trap res of length 1 (just header row) to avoid data.table bug.
  if (!is.null(attr(res, "status")) && attr(res, "status") != 0) {
    hrow <- strsplit(res, "\\s+")[[1]]
    dt <- data.frame(matrix(NA, nrow = length(job_ids), ncol = length(hrow)))
    names(dt) <- hrow
    dt$PID <- as.integer(job_ids)
  } else {
    stopifnot(length(res) > 1)
    # fread and any other parsing can break down with consecutive spaces in body of output.
    # This happens with lstart and start, avoid these for now.
    # header <- gregexpr("\\b", res[1], perl = T)
    # l2 <- gregexpr("\\b", res[2], perl=T)
    dt <- data.table::fread(text = res)
  }

  # fix difference in column naming between FreeBSD and *nux (make all like FreeBSD)
  data.table::setnames(dt, c("S", "COMMAND"), c("STAT", "COMM"), skip_absent = TRUE)

  # Build full job ID frame, filling in missing jobs (completed/killed)
  base_df <- data.frame(PID = as.integer(job_ids), stringsAsFactors = FALSE)

  # Full join in base R
  all_dt <- merge(base_df, dt, by = "PID", all = TRUE, sort = FALSE)

  # Replace missing STAT values with "C"
  if ("STAT" %in% names(all_dt)) {
    all_dt$STAT <- substr(all_dt$STAT, 1, 1)
    all_dt$STAT[is.na(all_dt$STAT)] <- "C"
  } else {
    all_dt$STAT <- rep("C", nrow(all_dt))
  }

  return(all_dt)
}
