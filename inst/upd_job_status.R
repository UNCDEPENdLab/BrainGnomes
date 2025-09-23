#!/usr/bin/env Rscript
# 
# Command-line version of 'update_tracked_job_status'
# Used for jobs submitted to 'cluster_job_submit' that are command-line only 
# retrieve in code using:   upd_job_status_path <- system.file("upd_job_status.R", package = "BrainGnomes")
# example status update: paste("Rscript", upd_job_status_path, "--job_id" , JOBID, "--sqlite_db", SQLITE_DB, "--status", STATUS)

print_help <- function() {
  cat(paste("This script makes a call to the `update_tracked_job_status` command and is to be",
            "used internally in command-line only scripts submitted to `cluster_job_submit`.",
            "Options:",
            "  --job_id <job_id>: The job id of the job whose status is to be updated.",
            "  --sqlite_db <sqlite_db>: The path to the tracking SQLite database",
            "  --status <status>: The new status of the job specified by `--job_id`.",
            "                     One of QUEUED, STARTED, COMPLETED, FAILED, or FAILED_BY_EXT",
            "  --cascade: If specified, then new status will cascade to child jobs.",
            "             Only works for status 'FAILED'/'FAILED_BY_EXT'",
            "  --help: Print the help menu",
            "\n\n",
            sep = "\n"
  ))
}

#read in command line arguments
tmp <- commandArgs(trailingOnly = TRUE)

args <- BrainGnomes::parse_cli_args(tmp)

# convert string versions of NULL to regular NULL
if (isTRUE(args$job_id == "NULL")) args$job_id <- NULL
if (isTRUE(args$sqlite_db == "NULL")) args$sqlite_db <- NULL
if (isTRUE(args$status == "NULL")) args$status <- NULL
if (isTRUE(args$cascade == "NULL") | isTRUE(is.null(args$cascade))) args$cascade <- FALSE

BrainGnomes::update_tracked_job_status(job_id = args$job_id, 
                                       sqlite_db = args$sqlite_db, 
                                       status = args$status, 
                                       cascade = args$cascade)