#!/usr/bin/env Rscript

pkg_dir <- Sys.getenv("pkg_dir", unset = "")
if (nzchar(pkg_dir)) {
  lib_dir <- dirname(pkg_dir)
  if (!(lib_dir %in% .libPaths())) .libPaths(c(lib_dir, .libPaths()))
}

print_help <- function() {
  cat(paste(
    "Create the sealed job description used by a scheduler-submitted worker.",
    "This is an internal BrainGnomes scheduler helper.",
    "Options:",
    "  --sqlite_db <file>: Job-tracking database.",
    "  --sequence_id <id>: Run identifier.",
    "  --contract_directory <dir>: Parent directory for job records.",
    "  --script <file>: Scheduler script that will run.",
    "  --scheduler <name>: Scheduler command, such as slurm or torque.",
    "  --scheduler_options <options>: Scheduler arguments.",
    "  --stage <name>: Pipeline stage.",
    "  --stream, --sub_id, --ses_id, --job_role, --job_name: Job identity.",
    "  --stdout_log, --stderr_log: Expected scheduler log paths.",
    "  --help: Print this help menu.",
    sep = "\n"
  ), "\n")
}

command_args <- commandArgs(trailingOnly = TRUE)
if ("--help" %in% command_args) {
  print_help()
  quit(save = "no", status = 0)
}

args <- BrainGnomes:::parse_cli_path_args(command_args)
null_if_missing <- function(value) {
  if (is.null(value) || identical(value, "NULL") || identical(value, "")) NULL else value
}

manifest <- suppressMessages(
  BrainGnomes:::prepare_external_job_manifest(
    tracking_sqlite_db = args$sqlite_db,
    sequence_id = args$sequence_id,
    contract_directory = args$contract_directory,
    script = args$script,
    scheduler = args$scheduler,
    scheduler_args = null_if_missing(args$scheduler_options),
    stage = args$stage,
    stream = null_if_missing(args$stream),
    sub_id = null_if_missing(args$sub_id),
    ses_id = null_if_missing(args$ses_id),
    job_role = null_if_missing(args$job_role),
    job_name = null_if_missing(args$job_name),
    stdout_log = null_if_missing(args$stdout_log),
    stderr_log = null_if_missing(args$stderr_log)
  )
)
cat(manifest$path)
