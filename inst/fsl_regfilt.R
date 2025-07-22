#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# Helper: Parse --key=value into a named list
parse_args <- function(args) {
  arg_list <- list()
  for (arg in args) {
    if (grepl("^--[^=]+=", arg)) {
      split_arg <- strsplit(sub("^--", "", arg), "=")[[1]]
      key <- split_arg[1]
      value <- paste(split_arg[-1], collapse = "=")  # Recombine in case value had "="
      arg_list[[key]] <- value
    } else {
      stop("Arguments must be specified in the form --key=value. Got: ", arg)
    }
  }
  return(arg_list)
}

# for debugging
# args <- c(
#   "--input=/proj/mnhallqlab/projects/preproc_pipeline_test_data/logs/sub-540294/sub-540294_task-ridl_run-04_desc-confounds_timeseries.nii.gz",
#   "--melodic_mix=/proj/mnhallqlab/projects/preproc_pipeline_test_data/data_fmriprep/sub-540294/func/sub-540294_task-ridl_run-04_res-2_desc-melodic_mixing.tsv",
#   "--filter=14,15,26",
#   "--njobs=1",
#   "--output=/proj/mnhallqlab/projects/preproc_pipeline_test_data/logs/sub-540294/sub-540294_task-ridl_run-04_desc-confoundsa_timeseries.nii.gz"
# )

# Parse input args
params <- parse_args(args)

# Validate required parameters
required <- c("input", "melodic_mix", "filter")
missing <- setdiff(required, names(params))
if (length(missing) > 0) {
  stop("Missing required argument(s): ", paste(missing, collapse = ", "))
}

# Optional params with defaults
params$njobs <- if (!is.null(params$njobs)) as.integer(params$njobs) else 4
params$output <- if (!is.null(params$output)) params$output else "denoised_func_data_nonaggr"

# Strip .nii.gz extension if present to avoid double extension in writeNIfTI
params$output <- sub("\\.nii(\\.gz)*$", "", params$output, perl = TRUE)

# Install and load required packages
for (pkg in c("speedglm", "oro.nifti", "doParallel", "pracma")) {
  if (!suppressMessages(require(pkg, character.only = TRUE))) {
    message("Installing missing package dependency: ", pkg)
    install.packages(pkg)
    suppressMessages(require(pkg, character.only = TRUE))
  }
}

# Validate files
stopifnot(file.exists(params$input))
stopifnot(file.exists(params$melodic_mix))

# Determine components to remove from --filter=
if (file.exists(params$filter)) {
  # Read from file
  filter_text <- scan(params$filter, what = "character", quiet = TRUE)
  badics <- as.numeric(strsplit(filter_text, ",")[[1]])
} else {
  # Assume comma-separated list of integers
  badics <- as.numeric(strsplit(params$filter, ",")[[1]])
}

if (any(is.na(badics))) stop("Invalid values in --filter argument. Must be a file path or comma-separated integers.")

cat(length(badics), "components will be removed from the data using partial regression.\n")

# Load inputs
message("Reading input dataset: ", params$input)
fmri_ts_data <- readNIfTI(params$input, reorient = FALSE)
melmix <- data.matrix(read.table(params$melodic_mix, header = FALSE, colClasses = "numeric"))

# Identify non-constant voxels
nonconst <- apply(fmri_ts_data, c(1,2,3), function(ts) !all(ts == ts[1]))
mi <- which(nonconst, arr.ind = TRUE)
if (nrow(mi) == 0L) stop("Unable to find non-constant voxels in fMRI input: ", params$input, ". fsl_regfilt.R cannot proceed")
toprocess <- apply(fmri_ts_data, 4, function(x) x[nonconst])

# in case of singleton result, apply will drop the 2nd dimension
if (is.vector(toprocess)) toprocess <- matrix(toprocess, nrow=1)
rownames(toprocess) <- 1:nrow(toprocess)
message("fMRI data has ", nrow(toprocess), " voxels and ", ncol(toprocess), " timepoints")

# Partial regression function
partialLm <- function(y, X, ivs = NULL) {
  m <- speedlm.fit(y = y, X = X, intercept = FALSE)
  pred <- X[, ivs] %*% coef(m)[ivs]
  return(as.vector(y - pred))
}

# Parallelization
if (params$njobs > 1) {
  cl <- parallel::makePSOCKcluster(params$njobs, outfile = "")
  doParallel::registerDoParallel(cl)
} else {
  foreach::registerDoSEQ()
}

message("Starting voxelwise partial regression")
start_time <- Sys.time()
res <- foreach::foreach(v = iter(toprocess, by = "row"), .noexport = "fmri_ts_data", .packages = "speedglm", .inorder = TRUE, .multicombine = TRUE, .combine = rbind) %dopar% {
  partialLm(matrix(v, ncol = 1), melmix, badics)
}
# handle singleton returned as vector, not matrix
if (is.vector(res)) res <- matrix(res, nrow = 1)

end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "mins")
message("partialLm fitting completed in ", round(duration, 3), " minutes.")

if (params$njobs > 1) stopCluster(cl)

# Reconstruct image
miassign <- cbind(pracma::repmat(mi, ncol(res), 1), rep(1:ncol(res), each = nrow(res)))
fmri_ts_data@.Data[miassign] <- res

#add min/max to header to have it play well across packages
fmri_ts_data@cal_min <- min(fmri_ts_data)
fmri_ts_data@cal_max <- max(fmri_ts_data)

# Save output to file
res <- writeNIfTI(fmri_ts_data, filename = params$output)

message("Finished. Output saved to: ", params$output, ".nii.gz")
