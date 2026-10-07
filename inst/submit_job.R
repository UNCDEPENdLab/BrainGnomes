#!/usr/bin/env Rscript
# Scheduler child submission entrypoint. Preserve argv values (including
# scheduler options starting with --) rather than flattening shell quoting.
if (nzchar(Sys.getenv("BG_RUNTIME_LOADER"))) source(Sys.getenv("BG_RUNTIME_LOADER"), local = TRUE)
argv <- commandArgs(trailingOnly = TRUE)
if ("--help" %in% argv) {
  cat("Usage: submit_job.R --script --scheduler --sqlite_db --sequence_id",
      "--contract_directory --scheduler_options --array --job_name --stage --stream",
      "--sub_id --ses_id --job_role --stdout_log --stderr_log --parent_job_id --child_level\n")
  quit(save = "no", status = 0)
}
if (!length(argv) || length(argv) %% 2L) stop("Submission arguments must be option/value pairs.")
args <- list()
for (index in seq.int(1L, length(argv), by = 2L)) {
  name <- sub("^--", "", argv[[index]])
  value <- argv[[index + 1L]]
  args[name] <- list(if (value %in% c("NULL", "")) NULL else value)
}
required <- c("script", "scheduler")
if (any(!required %in% names(args))) stop("Missing child submission arguments.")
tracking <- args[intersect(names(args), c(
  "job_name", "sequence_id", "contract_directory", "stage", "stream", "sub_id", "ses_id",
  "job_role", "stdout_log", "stderr_log", "parent_job_id", "child_level"
))]
tracking$scheduler_options <- args$scheduler_options
if (!is.null(tracking$child_level)) tracking$child_level <- as.integer(tracking$child_level)
tracking$unit_key <- BrainGnomes:::tracking_unit_key(args$stage, args$stream, args$sub_id,
                                                   args$ses_id, args$job_role, args$job_name)
options <- args$scheduler_options
slurm <- args$scheduler %in% c("slurm", "sbatch")
flags <- if (slurm) c("--job-name", "--output", "--error", "--array") else c("-N", "-o", "-e", "-t")
values <- list(args$job_name, args$stdout_log, args$stderr_log, args$array)
for (index in seq_along(flags)) {
  if (!is.null(values[[index]])) options <- c(options, flags[[index]], shQuote(values[[index]]))
}
names <- c("pkg_dir", "R_HOME", "BG_RUNTIME_BUNDLE", "BG_RUNTIME_LIBRARY", "BG_RUNTIME_SOURCE", "BG_RUNTIME_LOADER",
           "postprocess_cli", "postprocess_rscript", "postprocess_image_sched_script", "insert_tracked_job_path",
           "upd_job_status_path", "add_parent_path", "filelist_path", "task_status_dir", "out_dir", "sub_id",
           "ses_id", "log_file", "debug_pipeline", "stream_name", "max_concurrent_images")
environment <- Sys.getenv(names)
environment <- environment[nzchar(environment)]
if (!is.null(args$stdout_log)) environment["stdout_log"] <- args$stdout_log
if (!is.null(args$stderr_log)) environment["stderr_log"] <- args$stderr_log
job_id <- suppressMessages(BrainGnomes::cluster_job_submit(
  args$script, scheduler = args$scheduler, sched_args = options,
  env_variables = environment, export_all = TRUE, echo = FALSE, fail_on_error = TRUE,
  tracking_sqlite_db = args$sqlite_db, tracking_args = tracking
))
cat(as.character(job_id), "\n", sep = "")
