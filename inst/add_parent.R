#!/usr/bin/env Rscript
# 
# Command-line version of 'add_tracked_job_parent'
# Used for jobs submitted to 'cluster_job_submit' that are command-line only (e.g., not nested in R_batch_job)
# retreive in code using:   add_parent_path <- system.file("add_parent.R", package = "BrainGnomes")
# example status update: paste("Rscript", add_parent_path, "--sqlite_db", SQLITE_DB, "--job_id" , JOBID,, "--parent_job_id" , PARENTJOBID, "--child_level", CHILDLEVEL)

print_help <- function() {
  cat(paste("This script makes a call to the `add_tracked_job_parent` command and is to be",
            "used internally in command-line only scripts submitted to `cluster_job_submit`.",
            "Options:",
            "  --sqlite_db <sqlite_db>: The path to the tracking SQLite database",
            "  --job_id <job_id>: The job id of the job whose parent is to be added",
            "  --parent_job_id <parent_job_id>: The job id of the parent job",
            "  --child_level <child_level>: The level of the child whose parent is being added",
            "  --help: Print the help menu",
            "\n\n",
            sep = "\n"
  ))
}

#read in command line arguments
tmp <- commandArgs(trailingOnly = FALSE)

args <- BrainGnomes::parse_cli_args(tmp)

# convert string versions of NULL to regular NULL
if (isTRUE(args$job_id == "NULL")) job_id <- NULL
if (isTRUE(args$sqlite_db == "NULL")) sqlite_db <- NULL
if (isTRUE(args$status == "NULL")) status <- NULL

BrainGnomes::add_tracked_job_parent(sqlite_db = args$sqlite_db,
                                    job_id = args$job_id,
                                    parent_job_id = args$parent_job_id,
                                    child_level = args$child_level)