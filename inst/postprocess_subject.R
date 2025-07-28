#!/usr/bin/env Rscript
## simple script to handle post-fmriprep processing

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
  message("Minimal usage: postprocess_subject.R --input=<input_file> --config_yaml=<config_yaml.yaml> [--fsl_img=<fsl_singularity_image>]")
  quit(save="no", 1, FALSE)
}

# ensure that package directory is in R's search path
pkg_dir <- Sys.getenv("pkg_dir") # location of BrainGnomes installation at time of job queueing
if (pkg_dir != "") {
  lib_dir <- dirname(pkg_dir)

  if (!(lib_dir %in% .libPaths())) {
    .libPaths(c(lib_dir, .libPaths()))
  }
}

if (!suppressMessages(require("BrainGnomes", character.only=TRUE))) {
  stop("This script must be run in an R environment with BrainGnomes installed.")
}

# handle package dependencies
for (pkg in c("glue", "oro.nifti", "checkmate", "data.table", "yaml")) {
  if (!suppressMessages(require(pkg, character.only = TRUE))) {
    message("Installing missing package dependency: ", pkg)
    install.packages(pkg)
    suppressMessages(require(pkg, character.only = TRUE))
  }
}

# for debugging and testing
# args <- paste(c(
#   "--keep_intermediates='FALSE' --overwrite='TRUE' --tr='0.6' --apply_mask='TRUE' --brain_mask='NA'",
#   "--processing_steps='spatial_smooth' 'apply_aroma' 'temporal_filter' 'intensity_normalize' --spatial_smooth/fwhm_mm='5'",
#   "--spatial_smooth/prefix='s' --apply_aroma/nonaggressive='TRUE' --apply_aroma/prefix='a' --temporal_filter/low_pass_hz='0'",
#   "--temporal_filter/high_pass_hz='0.00833' --temporal_filter/prefix='f' --intensity_normalize/global_median='10000' --intensity_normalize/prefix='n'",
#   "--confound_calculate/columns='filt*' --confound_calculate/noproc_columns='nofilt*' --confound_calculate/demean='TRUE'",
#   "--force_processing_order='FALSE'",
#   "--input='/proj/mnhallqlab/projects/preproc_pipeline_test_data/data_fmriprep/sub-540294'",
#   "--fsl_img='/proj/mnhallqlab/users/michael/fmriprep_pipeline_setup/fmriprep-25.0.0.simg'"
# ), collapse = " ")

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
  if (is.null(input_regex)) input_regex <- ".*_desc-preproc_bold.nii.gz$"
  input_files <- list.files(path=cfg$input, pattern=input_regex, recursive=TRUE, full.names = TRUE)
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

out_files <- sapply(input_files, function(ii) BrainGnomes:::postprocess_subject(ii, cfg))
cat("Processing completed. Output files: \n")
print(out_files)
