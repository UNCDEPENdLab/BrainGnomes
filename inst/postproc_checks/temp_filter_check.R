#!/usr/bin/env Rscript

# lab_lib <- "/proj/mnhallqlab/lab_resources/lab_rpackages_v451"
# 
# install_if_missing <- function(pkg, lib=lab_lib) {
#   if (!requireNamespace(pkg, quietly=TRUE, lib.loc=lib)) {
#     cat("Package", pkg, "not found in", lib, ". Installing...\n")
#     install.packages(pkg, lib=lib, repos="https://cloud.r-project.org")
#   }
#   suppressPackageStartupMessages(library(pkg, character.only=TRUE, lib.loc=lib))
# }
# 
# # --- required packages ---
# install_if_missing("RNifti")
# install_if_missing("pracma")
# install_if_missing("multitaper")
# install_if_missing("psd")
# install_if_missing("dplyr")
# install_if_missing("signal")
# install_if_missing("checkmate")
# 

suppressPackageStartupMessages({
  library(pracma)
  library(multitaper)
  library(RNifti)
  library(dplyr)
  library(ggplot2)
})

# Resolve the directory of the current script so we can source helpers reliably
get_script_dir <- function() {
  args_full <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args_full, value = TRUE)
  if (length(file_arg) == 0) return(getwd())
  dirname(normalizePath(sub("^--file=", "", file_arg)))
}

script_dir <- tryCatch(get_script_dir(), error = function(e) getwd())
# Pull in shared multitaper utilities that live alongside this script
source(file.path(script_dir, "power_multitaper.R"))
source(file.path(script_dir, "mtm_bandpower.R"))

# Guard against constant or non-finite time series before running spectral routines
.is_valid_series <- function(x, tol = 2 * .Machine$double.eps) {
  all(is.finite(x)) && stats::var(x) > tol
}

# Sample a set of mask voxels whose pre/post series both have usable variance
.select_nonconstant_voxels <- function(
    mask_idx,
    get_pre_ts,
    get_post_ts,
    n_voxels,
    var_tol = 2 * .Machine$double.eps) {

  candidate_positions <- seq_along(mask_idx)

  if (is.null(n_voxels) || is.na(n_voxels)) {
    valid_mask <- vapply(
      candidate_positions,
      function(pos) {
        .is_valid_series(get_pre_ts(pos), tol = var_tol) &&
          .is_valid_series(get_post_ts(pos), tol = var_tol)
      },
      logical(1)
    )
    valid_positions <- candidate_positions[valid_mask]
    if (!length(valid_positions)) stop("No non-constant voxels available in both pre and post series.")
    return(list(indices = mask_idx[valid_positions], positions = valid_positions))
  }

  candidate_order <- sample(candidate_positions, length(candidate_positions))
  valid_positions <- integer(n_voxels)
  found <- 0L
  for (pos in candidate_order) {
    if (.is_valid_series(get_pre_ts(pos), tol = var_tol) &&
        .is_valid_series(get_post_ts(pos), tol = var_tol)) {
      found <- found + 1L
      valid_positions[found] <- pos
      if (found == n_voxels) break
    }
  }
  if (found < n_voxels) {
    stop("Fewer than ", n_voxels, " non-constant voxels available in both pre and post series.")
  }
  list(indices = mask_idx[valid_positions], positions = valid_positions)
}

# Build a closure that extracts a voxel time series from the 4D image
.make_ts_extractor <- function(img, coords_matrix) {
  function(position) {
    coord <- coords_matrix[position, ]
    img[coord[1], coord[2], coord[3], , drop = TRUE]
  }
}

# Average multitaper spectra across voxels, keeping frequency grid intact
.average_multitaper_spectra <- function(spec_list) {
  if (!length(spec_list)) stop("No spectra supplied for averaging.")
  dplyr::bind_rows(spec_list, .id = "voxel") %>%
    dplyr::rename(freq = f, power_db = power) %>%
    dplyr::group_by(freq) %>%
    dplyr::summarise(power_db = mean(power_db, na.rm = TRUE), .groups = "drop")
}

# Average bandpower summaries across voxels and convert back to dB space
.average_bandpower <- function(bp_list) {
  if (!length(bp_list)) stop("No bandpower estimates supplied for averaging.")
  dplyr::bind_rows(bp_list, .id = "voxel") %>%
    dplyr::group_by(label, low, high) %>%
    dplyr::summarise(
      power_linear_mean = mean(power_linear, na.rm = TRUE),
      relative_power = mean(relative_power, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      power_db = ifelse(
        is.finite(power_linear_mean) & power_linear_mean > 0,
        10 * log10(power_linear_mean),
        NA_real_
      )
    ) %>%
    dplyr::rename(power_linear = power_linear_mean)
}

.extract_psd_linear <- function(spec_df) {
  psd <- attr(spec_df, "psd_linear")
  if (is.null(psd) || is.null(psd$freq) || is.null(psd$spec)) {
    stop("Multitaper spectrum is missing its `psd_linear` attribute; update power_multitaper().")
  }
  psd
}

# ----------------------------
# Parse command-line arguments
# ----------------------------
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  val <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (length(val) == 0) return(default)
  sub(paste0("^", flag, "="), "", val)
}

get_bool_flag <- function(flag, default = FALSE) {
  matches <- grep(paste0("^", flag, "(=|$)"), args, value = TRUE)
  if (!length(matches)) return(default)
  last <- matches[length(matches)]
  if (identical(last, flag)) return(TRUE)
  value <- tolower(sub(paste0("^", flag, "="), "", last))
  if (value %in% c("1", "true", "t", "yes", "y")) return(TRUE)
  if (value %in% c("0", "false", "f", "no", "n")) return(FALSE)
  stop("Argument '", flag, "' must be a boolean (one of 1/0/true/false/yes/no).")
}

pre_path   <- get_arg("--pre")
post_path  <- get_arg("--post")
mask_path  <- get_arg("--mask")
dt         <- as.numeric(get_arg("--dt"))
subnum     <- get_arg("--subnum")
save_plots <- get_bool_flag("--save_plots")
save_spectrum <- get_bool_flag("--save_spectrum")
seed_arg   <- get_arg("--seed")
band_low_arg  <- get_arg("--band_low")
band_high_arg <- get_arg("--band_high")
log_output <- get_bool_flag("--log")
band_low  <- if (is.null(band_low_arg)) NA_real_ else as.numeric(band_low_arg)
band_high <- if (is.null(band_high_arg)) NA_real_ else as.numeric(band_high_arg)
seed <- if (is.null(seed_arg)) NA_integer_ else suppressWarnings(as.integer(seed_arg))
if (!is.null(seed_arg) && is.na(seed)) {
  stop("Argument '--seed' must be an integer.")
}

if (!is.na(seed)) {
  set.seed(seed)
  cat("Sampling seed set to", seed, "\n")
}

# #For testing
# pre_path   <- "/proj/mnhallqlab/studies/momentum/data_curation/fMRI_MRI/Brain_Gnomes_Final/data_postproc/sub-540104/sub-540104_task-ridl_run-01_space-MNI152NLin2009cAsym_desc-asmPostproc_bold.nii.gz"
# post_path  <- "/proj/mnhallqlab/studies/momentum/data_curation/fMRI_MRI/Brain_Gnomes_Final/data_postproc/sub-540104/sub-540104_task-ridl_run-01_space-MNI152NLin2009cAsym_desc-fasmPostproc_bold.nii.gz"
# mask_path  <- "/proj/mnhallqlab/studies/momentum/data/fMRI_MRI/pre_proc/sub-540104/func/sub-540104_task-ridl_run-01_space-MNI152NLin2009cAsym_desc-preproc_templatemask.nii.gz"
# dt         <- .635
# subnum     <- 540104
# save_plots <- TRUE
# save_spectrum <- TRUE


if (is.null(pre_path) || is.null(post_path) || is.na(dt) || is.null(subnum)) {
  cat("Usage:\n")
  cat("  Rscript temp_filter_check.R --pre=pre.nii.gz --post=post.nii.gz --mask=mask.nii.gz \\\n")
  cat("    --dt=TR --subnum=ID [--save_plots] [--save_spectrum] [--band_low=Hz] [--band_high=Hz] [--seed=INT] [--log]\n")
  quit(status = 1)
}

log_file <- NULL
if (log_output) {
  log_file <- paste0("filter_check_", subnum, ".txt")
  prev_sink_depth <- sink.number()
  # Mirror console output to a persistent log for QC records
  sink(log_file, split = TRUE)
  on.exit({
    while (sink.number() > prev_sink_depth) sink()
  }, add = TRUE)
}

cat("----- MRI Temporal Filtering Check Log -----\n")
cat("Started:", format(Sys.time()), "\n\n")

if (log_output) {
  cat("Logging all output to", log_file, "\n\n")
}

cat("Subject:", subnum, "\n")
cat("Pre path:", pre_path, "\n")
cat("Post path:", post_path, "\n")
cat("Mask path:", mask_path, "\n")
cat("TR (dt):", dt, "\n")
cat("Save plots:", save_plots, "\n")
cat("Save spectrum:", save_spectrum, "\n\n")
cat("Band low (Hz):", ifelse(is.na(band_low), "NA", band_low), "\n")
cat("Band high (Hz):", ifelse(is.na(band_high), "NA", band_high), "\n\n")


# ---- Load pre NIfTI ----
cat("Loading NIfTI images...\n")
pre_img  <- RNifti::readNifti(pre_path)
pre_img_dims <- dim(pre_img)
if (length(pre_img_dims) == 3L) {
  # Promote 3D images to 4D so downstream indexing code can assume a time axis
  pre_img_dims <- c(pre_img_dims, 1L) # treat 3D as 1 timepoint
  dim(pre_img) <- pre_img_dims
}
pre_n_t <- pre_img_dims[4]
pre_n_vox <- prod(pre_img_dims[1:3])

mask_idx <- NULL
mask_dims <- NULL

# ---- Extract pre voxel time series ----
if (!is.null(mask_path)) {
  mask <- RNifti::readNifti(mask_path)
  mask_dims <- dim(mask)
  if (!all(mask_dims == pre_img_dims[1:3])) stop("Mask dimensions do not match image.")
  mask_logical <- (mask != 0) & is.finite(mask)
  n_mask_vox <- sum(mask_logical)
  if (n_mask_vox == 0) stop("Mask contains no voxels.")
  mask_idx <- which(mask_logical)
  rm(mask)
} else {
  mask_idx <- seq_len(pre_n_vox)
  mask_dims <- pre_img_dims[1:3]
}
# Convert linear indices into i/j/k coordinates used by the extractor closures
mask_coords <- arrayInd(mask_idx, pre_img_dims[1:3], useNames = FALSE)

# ---- Load post NIfTI ----
post_img  <- RNifti::readNifti(post_path)
post_img_dims <- dim(post_img)
if (length(post_img_dims) == 3L) {
  post_img_dims <- c(post_img_dims, 1L)
  dim(post_img) <- post_img_dims
}
post_n_t <- post_img_dims[4]
post_n_vox <- prod(post_img_dims[1:3])

# ---- Extract post voxel time series ----
if (!is.null(mask_path) && !all(mask_dims == post_img_dims[1:3])) {
  stop("Mask dimensions do not match post image.")
}
if (pre_n_t != post_n_t) stop("Pre and post series must have the same number of timepoints.")
if (pre_n_vox != post_n_vox) stop("Pre and post images must have the same spatial dimensions.")

var_tol <- if (is.null(mask_path)) 1e-3 else 2 * .Machine$double.eps
default_sample_size <- 30L
voxels_to_sample <- if (length(mask_idx) <= default_sample_size) length(mask_idx) else default_sample_size
get_pre_ts <- .make_ts_extractor(pre_img, mask_coords)
get_post_ts <- .make_ts_extractor(post_img, mask_coords)
# Identify a reproducible set of voxels to summarise spectral behaviour
selection <- .select_nonconstant_voxels(
  mask_idx = mask_idx,
  get_pre_ts = get_pre_ts,
  get_post_ts = get_post_ts,
  n_voxels = voxels_to_sample,
  var_tol = var_tol
)
selected_idx <- selection$indices
selected_positions <- selection$positions
if (is.null(mask_path)) {
  cat(
    "Mask not provided; randomly selected",
    length(selected_idx),
    "non-constant voxels with variance >",
    signif(var_tol, 3),
    "for multitaper averaging.\n"
  )
} else {
  cat("Selected", length(selected_idx), "non-constant voxels for multitaper averaging.\n")
}

cat("Estimating smoothed multitaper spectra...\n")
# Compute per-voxel multitaper spectra and aggregate them for comparison
pre_spectra <- lapply(
  selected_positions,
  function(pos) power_multitaper(
    get_pre_ts(pos),
    dt = dt,
    nw = 4,
    pad_factor = 2
  )
)
post_spectra <- lapply(
  selected_positions,
  function(pos) power_multitaper(
    get_post_ts(pos),
    dt = dt,
    nw = 4,
    pad_factor = 2
  )
)
pre_psd <- lapply(pre_spectra, .extract_psd_linear)
post_psd <- lapply(post_spectra, .extract_psd_linear)

rm(pre_img, post_img) # cleanup big objects

mt_pre <- .average_multitaper_spectra(pre_spectra)
mt_post <- .average_multitaper_spectra(post_spectra)

file_prefix <- paste0("multitaper_spectrum_", subnum)
pre_out_file_mtap <- paste0(file_prefix, "_pre.csv")
post_out_file_mtap <- paste0(file_prefix, "_post.csv")

mt_diff <- mt_pre %>%
  inner_join(mt_post, by = "freq", suffix = c("_pre", "_post")) %>%
  mutate(power_diff_db = power_db_pre - power_db_post)

diff_out_file <- paste0(file_prefix, "_diff.csv")

if (save_spectrum) {
  # Persist the averaged spectra and their difference for downstream inspection
  write.csv(mt_pre, pre_out_file_mtap, row.names = FALSE)
  write.csv(mt_post, post_out_file_mtap, row.names = FALSE)
  write.csv(mt_diff, diff_out_file, row.names = FALSE)
  cat("Saved multitaper spectra to", pre_out_file_mtap, "and", post_out_file_mtap, "\n")
  cat("Saved multitaper difference to", diff_out_file, "\n")
} else {
  cat("Skipping multitaper spectrum export (use --save_spectrum to enable).\n")
}

if (save_plots) {
  spectrum_out_file <- paste0(file_prefix, "_pre_post.png")
  spectrum_df <- dplyr::bind_rows(
    dplyr::mutate(mt_pre, series = "Pre-filter"),
    dplyr::mutate(mt_post, series = "Post-filter")
  )
  spectrum_plot <- ggplot(spectrum_df, aes(x = freq, y = power_db, color = series)) +
    geom_line(linewidth = 1) +
    labs(
      title = paste("Smoothed Multitaper Spectrum | Subject", subnum),
      x = "Frequency (Hz)",
      y = "Power (dB)",
      color = NULL
    ) +
    scale_color_manual(values = c("Pre-filter" = "steelblue", "Post-filter" = "firebrick")) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top")
  cutoff_freqs <- c(
    if (!is.na(band_low)) band_low else NULL,
    if (!is.na(band_high)) band_high else NULL
  )
  if (length(cutoff_freqs) > 0) {
    # Overlay vertical markers where the temporal filter is expected to act
    spectrum_plot <- spectrum_plot +
      geom_vline(xintercept = cutoff_freqs, linetype = "dashed", color = "gray40", linewidth = 0.7)
  }
  ggsave(filename = spectrum_out_file, plot = spectrum_plot, width = 8, height = 6, units = "in", dpi = 300)
  cat("Saved multitaper spectrum plot to", spectrum_out_file, "\n")

  diff_plot_out_file <- paste0(file_prefix, "_diff.png")
  diff_plot <- ggplot(mt_diff, aes(x = freq, y = power_diff_db)) +
    geom_line(color = "navy", linewidth = 1) +
    geom_hline(yintercept = 0, color = "gray70", linetype = "dotted") +
    labs(
      title = paste("Multitaper Power Difference (Pre - Post) | Subject", subnum),
      x = "Frequency (Hz)",
      y = "Power Difference (dB)"
    ) +
    theme_minimal(base_size = 14)
  if (length(cutoff_freqs) > 0) {
    diff_plot <- diff_plot +
      geom_vline(xintercept = cutoff_freqs, linetype = "dashed", color = "gray40", linewidth = 0.7)
  }
  ggsave(filename = diff_plot_out_file, plot = diff_plot, width = 8, height = 6, units = "in", dpi = 300)
  cat("Saved multitaper power difference plot to", diff_plot_out_file, "\n")
} else {
  cat("Skipping plot export (use --save_plots to enable).\n")
}

nyquist <- 1 / (2 * dt)
outside_bands <- list()
bandpower_results <- list()
if (!is.na(band_low) && band_low > 0) {
  outside_bands$below <- c(0, max(0, band_low))
}
if (!is.na(band_high) && band_high < nyquist) {
  outside_bands$above <- c(min(band_high, nyquist), nyquist)
}

if (length(outside_bands) > 0) {
  cat("Computing band power outside filter bounds...\n")
  pre_bp_list <- lapply(pre_psd, function(psd) {
    # Quantify how much energy remains outside the intended stop bands
    mtm_bandpower(
      y = NULL,
      dt = dt,
      bands = outside_bands,
      detrend = "linear",
      exclude_dc = TRUE,
      total_band = c(0, nyquist),
      psd = psd
    )
  })
  post_bp_list <- lapply(post_psd, function(psd) {
    mtm_bandpower(
      y = NULL,
      dt = dt,
      bands = outside_bands,
      detrend = "linear",
      exclude_dc = TRUE,
      total_band = c(0, nyquist),
      psd = psd
    )
  })

  pre_bp_avg <- .average_bandpower(pre_bp_list)
  post_bp_avg <- .average_bandpower(post_bp_list)

  bandpower_diff <- pre_bp_avg %>%
    select(label, low, high, power_db_pre = power_db, relative_power_pre = relative_power) %>%
    inner_join(post_bp_avg %>%
                 select(label, low, high, power_db_post = power_db, relative_power_post = relative_power),
               by = c("label", "low", "high")) %>%
    mutate(
      power_db_change = power_db_post - power_db_pre,
      relative_power_change = relative_power_post - relative_power_pre,
      band_type = "outside"
    )

  avg_reduction <- mean(bandpower_diff$power_db_pre - bandpower_diff$power_db_post, na.rm = TRUE)
  bandpower_results$outside <- bandpower_diff
  cat("Average power reduction outside bands (dB):", signif(avg_reduction, 4), "\n")
} else {
  cat("Filter bounds not provided or cover entire spectrum; skipping bandpower computation.\n")
}

passband_low <- if (!is.na(band_low)) max(0, band_low) else 0
passband_high <- if (!is.na(band_high)) min(nyquist, band_high) else nyquist
has_passband_bounds <- (!is.na(band_low) || !is.na(band_high)) && passband_high > passband_low

if (has_passband_bounds) {
  cat("Computing band power within passband...\n")
  passband <- list(passband = c(passband_low, passband_high))
  pre_pass_list <- lapply(pre_psd, function(psd) {
    # Within-band comparisons help confirm signal preservation where it matters
    mtm_bandpower(
      y = NULL,
      dt = dt,
      bands = passband,
      detrend = "linear",
      exclude_dc = TRUE,
      total_band = c(0, nyquist),
      psd = psd
    )
  })
  post_pass_list <- lapply(post_psd, function(psd) {
    mtm_bandpower(
      y = NULL,
      dt = dt,
      bands = passband,
      detrend = "linear",
      exclude_dc = TRUE,
      total_band = c(0, nyquist),
      psd = psd
    )
  })

  pre_pass_avg <- .average_bandpower(pre_pass_list)
  post_pass_avg <- .average_bandpower(post_pass_list)

  passband_diff <- pre_pass_avg %>%
    select(label, low, high, power_db_pre = power_db, relative_power_pre = relative_power) %>%
    inner_join(post_pass_avg %>%
                 select(label, low, high, power_db_post = power_db, relative_power_post = relative_power),
               by = c("label", "low", "high")) %>%
    mutate(
      power_db_change = power_db_post - power_db_pre,
      relative_power_change = relative_power_post - relative_power_pre,
      band_type = "passband"
    )

  power_changes <- passband_diff$power_db_change
  avg_change_db <- if (all(is.na(power_changes))) NA_real_ else mean(power_changes, na.rm = TRUE)
  bandpower_results$passband <- passband_diff
  cat(
    "Average power change within passband (dB):",
    if (is.na(avg_change_db)) "NA" else signif(avg_change_db, 4),
    "\n"
  )
  
  if (!is.na(avg_change_db) && is.finite(avg_change_db) && avg_change_db < -3) {
    cat("WARNING: Detected a large power change within the passband. Review filtering parameters.\n")
  }
} else {
  cat("Passband bounds not provided; skipping passband power comparison.\n")
}

if (length(bandpower_results) > 0) {
  combined_bandpower <- dplyr::bind_rows(bandpower_results) %>%
    select(
      band_type, label, low, high,
      power_db_pre, relative_power_pre,
      power_db_post, relative_power_post,
      power_db_change, relative_power_change
    )
  bandpower_file <- paste0("bandpower_", subnum, ".csv")
  write.csv(combined_bandpower, bandpower_file, row.names = FALSE)
  cat("Bandpower details saved to", bandpower_file, "\n")
}

cat("\nCompleted:", format(Sys.time()), "\n")
