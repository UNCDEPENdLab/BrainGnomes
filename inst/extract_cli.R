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

# Require
# --input_regex: the regular expression used for files that entered the relevant postprocess stream
# --postproc_bids_desc: the BIDS desc field for output files from the stream
# --input: the directory in which to look for files

if (!checkmate::test_directory(cfg$input)) {
  stop("No valid directory provided as --input")
}

input_files <- get_postproc_stream_outputs(cfg$input, cfg$input_regex, cfg$bids_desc)

if (length(input_files) == 0L) {
  stop("Cannot find files to postprocess with --input: ", cfg$input)
}

# cat("About to postprocess the following files: ")
# print(input_files)

log_file <- Sys.getenv("log_file")
if (log_file == "") {
  log_file <- NULL
  warning("log_file variable not set.")
}

atlases <- cfg$atlases
a_exists <- checkmate::test_file_exists(atlases)
if (any(!a_exists)) {
  warning("Cannot find atlas: ", paste(atlases[!a_exists], collapse = ","))
  atlases <- atlases[a_exists]
}

arg_list <- list(
  atlas_files = atlases, # extract_rois loops over these
  out_dir = cfg$out_dir,
  cor_method = cfg$cor_method,
  roi_reduce = cfg$roi_reduce,
  brain_mask = cfg$brain_mask,
  min_vox_per_roi = cfg$min_vox_per_roi,
  rtoz = cfg$rtoz,
  log_file = log_file
)

for (i in input_files) {
  arg_list$bold_file <- i
  do.call(BrainGnomes::extract_rois, arg_list)
}

# cat("Processing completed. Output files: \n")
# print(out_files)
