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

log_level_env <- toupper(Sys.getenv("log_level", unset = ""))
if (nzchar(log_level_env)) {
  options(BrainGnomes.log_level = log_level_env)
  try(lgr::get_logger_glue("BrainGnomes")$set_threshold(log_level_env), silent = TRUE)
}

# parse CLI inputs into a nested list, if relevant
cli_args <- BrainGnomes:::parse_cli_path_args(args)

if (!is.null(cli_args$config_yaml)) {
  checkmate::assert_file_exists(cli_args$config_yaml)
  cfg <- yaml::read_yaml(cli_args$config_yaml)
  cli_args$config_yaml <- NULL # remove prior to additional updates
} else {
  cfg <- list()
}

# Now add additional command line arguments to cfg -- this leads any settings in YAML to be overridden by the same CLI arguments
cfg <- modifyList(cfg, cli_args)

nested_cor_method <- cfg$correlation$method
legacy_cor_method <- cfg$cor_method
if (!is.null(nested_cor_method) && !is.null(legacy_cor_method) &&
    !identical(nested_cor_method, legacy_cor_method)) {
  stop("Conflicting correlation methods configured in correlation/method and cor_method.")
}

cor_method <- if (!is.null(nested_cor_method)) nested_cor_method else legacy_cor_method
if (is.null(cor_method)) {
  stop("A correlation method must be configured in correlation/method or cor_method.")
}

if (!checkmate::test_string(cfg$input)) stop("A valid --input must be provided pointing either to a folder with data to postprocess or to a single 4D NIfTI file")

# Require
# --input_regex: the regular expression used for files that entered the relevant postprocess stream
# --postproc_bids_desc: the BIDS desc field for output files from the stream
# --input: the directory in which to look for files

if (!checkmate::test_directory(cfg$input)) {
  stop("No valid directory provided as --input")
}

input_files <- get_postproc_output_files(cfg$input, cfg$input_regex, cfg$bids_desc)

if (length(input_files) == 0L) {
  stop("Cannot find files for ROI extraction with --input: ", cfg$input)
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
  cor_method = cor_method,
  roi_reduce = cfg$roi_reduce,
  mask_file = cfg$mask_file,
  min_vox_per_roi = cfg$min_vox_per_roi,
  save_ts = if (is.null(cfg$save_ts)) TRUE else cfg$save_ts,
  save_diagnostics = if (is.null(cfg$save_diagnostics)) FALSE else cfg$save_diagnostics,
  allow_atlas_resampling = if (is.null(cfg$allow_atlas_resampling)) FALSE else cfg$allow_atlas_resampling,
  atlas_space = cfg$atlas_space,
  rtoz = cfg$rtoz,
  log_file = log_file,
  overwrite = isTRUE(cfg$overwrite)
)

output_files <- character()
for (i in input_files) {
  arg_list$bold_file <- i
  result <- do.call(BrainGnomes::extract_rois, arg_list)
  result_files <- unname(unlist(result, recursive = TRUE, use.names = FALSE))
  if (length(result_files) > 0L) {
    result_files <- as.character(result_files)
    output_files <- c(
      output_files,
      result_files[!is.na(result_files) & nzchar(result_files)]
    )
  }
}

if (!is.null(cfg$output_manifest_file)) {
  checkmate::assert_string(cfg$output_manifest_file)
  output_files <- sort(unique(output_files))
  if (length(output_files) == 0L) {
    stop("ROI extraction completed without producing any files to record.")
  }
  if (any(!file.exists(output_files))) {
    stop(
      "ROI extraction did not produce every reported output: ",
      paste(output_files[!file.exists(output_files)], collapse = ", ")
    )
  }

  manifest_json <- BrainGnomes:::capture_output_manifest(
    cfg$out_dir,
    files = output_files
  )
  manifest_dir <- dirname(cfg$output_manifest_file)
  if (!dir.exists(manifest_dir)) {
    stop("Output manifest directory does not exist: ", manifest_dir)
  }
  write_manifest <- function(manifest_json, manifest_file) {
    manifest_tmp <- tempfile(
      pattern = paste0(".", basename(manifest_file), "-"),
      tmpdir = dirname(manifest_file)
    )
    on.exit(unlink(manifest_tmp, force = TRUE), add = TRUE)
    writeLines(manifest_json, manifest_tmp, useBytes = TRUE)
    if (file.exists(manifest_file)) unlink(manifest_file, force = TRUE)
    if (!file.rename(manifest_tmp, manifest_file)) {
      stop("Could not atomically install output manifest: ", manifest_file)
    }
  }
  write_manifest(manifest_json, cfg$output_manifest_file)
}

# cat("Processing completed. Output files: \n")
# print(output_files)
