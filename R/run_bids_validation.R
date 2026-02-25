#' Run BIDS validation on the project BIDS directory
#'
#' This helper submits the BIDS validator as a standalone job. It does not run as part
#' of \code{run_project()} and can be invoked whenever validation of the project
#' BIDS directory is desired.
#'
#' @param scfg A \code{bg_project_cfg} object returned by \code{setup_project()} or
#'   \code{load_project()}.
#' @param outfile The output HTML report path for bids-validator. Relative paths are
#'   written under \code{scfg$metadata$log_directory} (to avoid contaminating the BIDS
#'   dataset); absolute paths are used as provided. If \code{NULL}, the value stored in
#'   \code{scfg$bids_validation$outfile} is used.
#' @param wait_jobs Optional character vector of upstream scheduler job IDs that
#'   must complete before this validation job starts.
#' @param sequence_id Optional sequence ID used for job tracking.
#' @return The job id returned by the scheduler.
#' @export
#' @examples
#' \dontrun{
#'   run_bids_validation(study_config, outfile = "bids_validator_output.html")
#' }
run_bids_validation <- function(scfg, outfile = NULL, wait_jobs = NULL, sequence_id = NULL) {
  checkmate::assert_class(scfg, "bg_project_cfg")

  if (!validate_exists(scfg$compute_environment$bids_validator)) {
    stop("Cannot run BIDS validation without a bids_validator location.")
  }

  if (is.null(outfile)) outfile <- scfg$bids_validation$outfile
  if (is.null(outfile)) outfile <- "bids_validator_output.html"
  checkmate::assert_string(outfile)

  log_file <- file.path(scfg$metadata$log_directory, "bids_validation_log.txt")
  lg <- lgr::get_logger_glue("bids_validation")
  if (!"bids_validation_log" %in% names(lg$appenders)) {
    lg$add_appender(lgr::AppenderFile$new(log_file), name = "bids_validation_log")
  }
  log_level_value <- resolve_log_level(scfg$log_level)
  if (is.null(log_level_value)) log_level_value <- "INFO"
  set_logger_threshold(lg, log_level_value)

  env_variables <- c(
    debug_pipeline = scfg$debug,
    pkg_dir = find.package(package = "BrainGnomes"),
    R_HOME = R.home(),
    log_file = log_file,
    stdout_log = glue::glue("{scfg$metadata$log_directory}/bids_validation_jobid-%j_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.out"),
    stderr_log = glue::glue("{scfg$metadata$log_directory}/bids_validation_jobid-%j_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.err"),
    upd_job_status_path = system.file("upd_job_status.R", package = "BrainGnomes"),
    log_level = log_level_value
  )

  sched_script <- get_job_script(scfg, "bids_validation", subject_suffix = FALSE)
  sched_args <- get_job_sched_args(scfg, "bids_validation", stdout_log = env_variables["stdout_log"], stderr_log = env_variables["stderr_log"])

  tracking_args <- list(
    job_name = "bids_validation_project",
    sequence_id = sequence_id,
    n_nodes = 1
  )
  if (!is.null(scfg$bids_validation$ncores)) tracking_args$n_cpus <- scfg$bids_validation$ncores
  if (!is.null(scfg$bids_validation$nhours)) tracking_args$wall_time <- hours_to_dhms(scfg$bids_validation$nhours)
  if (!is.null(scfg$bids_validation$memgb)) tracking_args$mem_total <- scfg$bids_validation$memgb
  if (!is.null(scfg$compute_environment$scheduler)) tracking_args$scheduler <- scfg$compute_environment$scheduler
  tracking_args$scheduler_options <- sched_args
  
  job_id <- submit_bids_validation(
    scfg,
    sub_dir = scfg$metadata$bids_directory,
    outfile = outfile,
    env_variables = env_variables,
    sched_script = sched_script,
    sched_args = sched_args,
    parent_ids = wait_jobs,
    lg = lg,
    tracking_sqlite_db = scfg$metadata$sqlite_db,
    tracking_args = tracking_args
  )

  invisible(job_id)
}
