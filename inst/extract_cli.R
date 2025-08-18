#!/usr/bin/env Rscript
## simple script to handle ROI extraction

#read in command line arguments.
args <- commandArgs(trailingOnly = FALSE)

scriptpath <- dirname(sub("--file=", "", grep("--file=", args, fixed=TRUE, value=TRUE), fixed=TRUE))
argpos <- grep("--args", args, fixed=TRUE)
if (length(argpos) > 0L) {
  args <- args[(argpos + 1):length(args)]
} else {
  args <- c()
}

if (is.null(args) || length(args) < 2L) {
  message("Minimal usage: extract_cli.R --input=<input_file> --config_yaml=<config_yaml.yaml> --extract_streams=<stream names>")
  quit(save="no", 1, FALSE)
}

# ensure that package directory is in R's search path
pkg_dir <- Sys.getenv("pkg_dir") # location of BrainGnomes installation at time of job queueing
if (pkg_dir != "") {
  lib_dir <- dirname(pkg_dir)

  if (!(lib_dir %in% .libPaths())) .libPaths(c(lib_dir, .libPaths()))
}

if (!suppressMessages(require("BrainGnomes", character.only=TRUE))) {
  stop("This script must be run in an R environment with BrainGnomes installed.")
}

# handle package dependencies
for (pkg in c("glue", "checkmate", "data.table", "yaml")) {
  if (!suppressMessages(require(pkg, character.only = TRUE))) {
    message("Installing missing package dependency: ", pkg)
    install.packages(pkg)
    suppressMessages(require(pkg, character.only = TRUE))
  }
}

# parse CLI inputs into a nested list, if relevant
cli_args <- BrainGnomes::parse_cli_args(args)

if (!is.null(cli_args$config_yaml)) {
  checkmate::assert_file_exists(cli_args$config_yaml)
  cfg <- yaml::read_yaml(cli_args$config_yaml)
  cli_args$config_yaml <- NULL # remove prior to additional updates
} else {
  cfg <- list()
}

# Now add additional command line arguments to cfg -- this leads any settings in YAML to be overridden by the same CLI arguments
cfg <- modifyList(cfg, cli_args)

if (!checkmate::test_string(cfg$input)) stop("A valid --input must be provided pointing either to a folder with data to postprocess or to a single 4D NIfTI file")

# accept an input directory and input regex, or a single input file
input_regex <- cfg$input_regex
if (checkmate::test_directory(cfg$input)) {
  # input is a directory -- find all relevant nifti files to postprocess
  if (is.null(input_regex)) input_regex <- "desc:preproc suffix:bold"
  input_regex <- BrainGnomes:::construct_bids_regex(input_regex)
  input_files <- list.files(path = cfg$input, pattern = input_regex, recursive = TRUE, full.names = TRUE)
} else if (!checkmate::test_file_exists(cfg$input)) {
  stop("A valid 4D NIfTI file to process must be passed in as --input=<4d file>")
} else {
  input_files <- cfg$input # single file input
}

if (length(input_files) == 0L) {
  stop("Cannot find files to postprocess with --input: ", cfg$input)
}

# cat("About to postprocess the following files: ")
# print(input_files)

log_file <- Sys.getenv("log_file")
if (log_file == "") {
  warning("log_file variable not set. ")
}

atlases <- cfg$atlases
a_exists <- checkmate::test_file_exists(atlases)
if (any(!a_exists)) {
  warning()
}
output_files <- c()
for (ii in input_files) {
  
}
out_files <- sapply(input_files, function(ii) BrainGnomes::extract_rois(ii, cfg), USE.NAMES = FALSE)
# cat("Processing completed. Output files: \n")
# print(out_files)
