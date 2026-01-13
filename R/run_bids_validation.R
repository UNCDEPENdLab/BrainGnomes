#' Run BIDS validation on the project BIDS directory
#'
#' This helper submits the BIDS validator as a standalone job. It does not run as part
#' of \code{run_project()} and can be invoked whenever validation of the project
#' BIDS directory is desired.
#'
#' @param scfg A \code{bg_project_cfg} object returned by \code{setup_project()} or
#'   \code{load_project()}.
#' @param outfile The name of the HTML report to create in the BIDS directory. If
#'   \code{NULL}, the value stored in \code{scfg$bids_validation$outfile} is used.
#' @return The job id returned by the scheduler.
#' @export
#' @examples
#' \dontrun{
#'   run_bids_validation(study_config, outfile = "bids_validator_output.html")
#' }
run_bids_validation <- function(scfg, outfile = NULL) {
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
    log_level = log_level_value
  )

  sched_script <- get_job_script(scfg, "bids_validation")
  sched_args <- get_job_sched_args(scfg, "bids_validation", stdout_log = env_variables["stdout_log"], stderr_log = env_variables["stderr_log"])
  
  job_id <- submit_bids_validation(
    scfg,
    sub_dir = scfg$metadata$bids_directory,
    sub_id = "project",
    outfile = outfile,
    env_variables = env_variables,
    sched_script = sched_script,
    sched_args = sched_args,
    lg = lg
  )

  invisible(job_id)
}
