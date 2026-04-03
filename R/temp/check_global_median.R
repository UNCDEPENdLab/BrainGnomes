#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if (!requireNamespace("RNifti", quietly = TRUE)) {
    stop("Package 'RNifti' is required. Install with install.packages('RNifti').")
  }
})

args <- commandArgs(trailingOnly = TRUE)

# --- minimal CLI parsing ---
get_arg <- function(flag, default = NULL) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (length(hit) == 0L) return(default)
  sub(paste0("^", flag, "="), "", hit[1L])
}

img_path <- get_arg("--image")
msk_path <- get_arg("--mask")
tol_arg  <- get_arg("--tolerance", "0")  # absolute tolerance
target_arg <- get_arg("--target", "10000")

usage <- function() {
  cat(
"Usage:
  Rscript check_global_median.R --image=func_4d.nii.gz --mask=brain_mask.nii.gz [--tolerance=1e-3] [--target=10000]

Description:
  Validates that the global median of a 4D image, computed over all voxels within the brain mask and across all timepoints, equals the target (default 10000) within the given absolute tolerance.

Exit codes:
  0 = PASS, 1 = FAIL

Options:
  --image       Path to 4D NIfTI image
  --mask        Path to 3D brain mask (nonzero voxels are treated as in-mask)
  --tolerance   Absolute tolerance for median comparison (default 0)
  --target      Target median value (default 10000)
")
}

if (is.null(img_path) || is.null(msk_path)) {
  usage()
  quit(status = 1L)
}

tol <- suppressWarnings(as.numeric(tol_arg))
if (!is.finite(tol) || tol < 0) stop("Invalid --tolerance value: ", tol_arg)

target <- suppressWarnings(as.numeric(target_arg))
if (!is.finite(target)) stop("Invalid --target value: ", target_arg)

# --- load images ---
img <- RNifti::readNifti(img_path)
msk <- RNifti::readNifti(msk_path)

# --- basic checks ---
if (length(dim(img)) < 3L)
  stop("Input image must be at least 3D; got dims: ", paste(dim(img), collapse = "x"))

if (length(dim(img)) == 3L) {
  # allow 3D input by treating it as a single timepoint
  dim(img) <- c(dim(img), 1L)
}

if (length(dim(msk)) != 3L)
  stop("Mask must be 3D; got dims: ", paste(dim(msk), collapse = "x"))

if (!all(dim(img)[1:3] == dim(msk))) {
  stop(sprintf("Spatial dims mismatch: image [%s] vs mask [%s]",
               paste(dim(img)[1:3], collapse = "x"),
               paste(dim(msk), collapse = "x")))
}

# --- prepare mask and values ---
mask_logical <- (msk != 0) & is.finite(msk)
n_mask_vox <- sum(mask_logical)
if (n_mask_vox == 0L) stop("Mask contains zero in-brain voxels (all zeros/NA).")

img_dims <- dim(img)  # x,y,z,t
n_vox <- prod(img_dims[1:3])
n_t   <- img_dims[4]

# reshape 4D -> (voxels x time)
img_matrix <- array(img, dim = c(n_vox, n_t))

# extract all masked voxel values across all time points
vals <- as.vector(img_matrix[mask_logical, , drop = FALSE])
vals <- vals[is.finite(vals)]

if (length(vals) == 0L) stop("No finite values found within mask.")

# --- compute median and validate ---
gmed <- stats::median(vals)

cat(sprintf("Global median within mask over %d voxels x %d timepoints = %.6f\n",
            n_mask_vox, n_t, gmed))
cat(sprintf("Target = %.6f, Tolerance (abs) = %.6g\n", target, tol))

diff <- abs(gmed - target)
if (diff <= tol) {
  cat("PASS: |median - target| <=", tol, "\n")
  quit(status = 0L)
} else {
  cat(sprintf("FAIL: |%.6f - %.6f| = %.6f exceeds tolerance %.6g\n",
              gmed, target, diff, tol))
  quit(status = 1L)
}
