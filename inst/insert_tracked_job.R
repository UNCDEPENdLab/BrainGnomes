#!/usr/bin/env Rscript
# 
# Command-line version of 'insert_tracked_jobs'
# Used for jobs submitted via bash
# Retrieve in code using:   insert_tracked_job_path <- system.file("insert_tracked_job.R", package = "BrainGnomes")

print_help <- function() {
  cat(paste("This script makes a call to the `insert_tracked_job` command and is to be",
            "used internally in command-line only scripts.",
            "Options:",
            "  --sqlite_db <sqlite_db>: The path to the tracking SQLite databas.",
            "  --job_id <job_id>: The job id of the job to be added.",
            "  --help: Print the help menu",
            "  Tracking Arguments:",
            "  --job_name",
            "  --sequence_id",
            "  --n_nodes",
            "  --n_cpus",
            "  --wall_time",
            "  --mem_per_cpu",
            "  --mem_total",
            "  --scheduler",
            "  --scheduler_options",
            "\n\n",
            sep = "\n"
  ))
}

#read in command line arguments
tmp <- commandArgs(trailingOnly = TRUE)

args <- BrainGnomes::parse_cli_args(tmp)

# build tracking arguments link
tracking_args <- list(
  job_name = args$job_name,
  sequence_id = args$sequence_id, 
  n_nodes = args$n_nodes, 
  n_cpus = args$n_cpus, 
  wall_time = args$wall_time, 
  mem_per_cpu = args$mem_per_cpu, 
  mem_total = args$mem_total, 
  scheduler = args$scheduler,
  scheduler_options = args$scheduler_options
)

# convert string versions of NULL to regular NULL
if (isTRUE(args$job_id == "NULL")) args$job_id <- NULL
if (isTRUE(args$sqlite_db == "NULL")) args$sqlite_db <- NULL

BrainGnomes::insert_tracked_job(sqlite_db = args$sqlite_db,
                                job_id = args$job_id,
                                tracking_args = tracking_args)
