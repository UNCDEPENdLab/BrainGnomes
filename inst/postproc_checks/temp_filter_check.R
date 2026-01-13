#!/usr/bin/env Rscript

## ----------------------------------------------------------
##  MRI temporal filtering check via multitaper spectra
##  - Compares pre/post-filter spectra
##  - Exports multitaper spectra as CSVs
##  - Produces two plots (spectrum + difference)
##  - Computes bandpower inside/outside band
##  - Issues PASS/FAIL QC based on simple thresholds
## ----------------------------------------------------------

suppressPackageStartupMessages({
  library(RNifti)
  library(multitaper)
  library(ggplot2)
})

## ----------------------------------------------------------
## Helpers: script location + sourced utilities
## ----------------------------------------------------------

get_script_dir <- function() {
  args_full <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args_full, value = TRUE)
  if (length(file_arg) == 0) return(getwd())
  dirname(normalizePath(sub("^--file=", "", file_arg)))
}

script_dir <- tryCatch(get_script_dir(), error = function(e) getwd())

## These should provide:
##   power_multitaper(y, dt, ...)
##   mtm_bandpower(y, dt, bands, ..., psd = NULL)
source(file.path(script_dir, "power_multitaper.R"))
source(file.path(script_dir, "mtm_bandpower.R"))

## ----------------------------------------------------------
## Core utility functions (validity, extraction, averaging)
## ----------------------------------------------------------

.bind_rows <- function(data_list, id_name = NULL) {
  if (!length(data_list)) return(data.frame())
  df_list <- lapply(seq_along(data_list), function(i) {
    df <- data_list[[i]]
    if (is.null(df)) return(NULL)
    df <- as.data.frame(df)
    if (!is.null(id_name)) {
      id_val <- names(data_list)[i]
      if (is.null(id_val) || !nzchar(id_val)) id_val <- as.character(i)
      df[[id_name]] <- id_val
    }
    df
  })
  df_list <- Filter(Negate(is.null), df_list)
  if (!length(df_list)) return(data.frame())
  res <- do.call(rbind, df_list)
  rownames(res) <- NULL
  res
}

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

  if (is.null(n_voxels) || is.na(n_voxels) || n_voxels <= 0) {
    n_voxels <- length(mask_idx)
  }
  if (n_voxels > length(mask_idx)) {
    n_voxels <- length(mask_idx)
  }

  candidate_positions <- seq_along(mask_idx)
  valid_positions <- integer(n_voxels)
  found <- 0L

  for (pos in sample(candidate_positions)) {
    if (.is_valid_series(get_pre_ts(pos), tol = var_tol) &&
        .is_valid_series(get_post_ts(pos), tol = var_tol)) {
      found <- found + 1L
      valid_positions[found] <- pos
      if (found == n_voxels) break
    }
  }

  if (found == 0L) {
    stop("No non-constant voxels available in both pre and post series.")
  }

  if (found < n_voxels) {
    cat("Requested", n_voxels,
        "voxels; found only", found,
        "with sufficient variance in pre and post series.\n")
  }

  valid_positions <- valid_positions[seq_len(found)]

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
  df <- .bind_rows(spec_list, id_name = "voxel")
  names(df)[names(df) == "f"] <- "freq"
  names(df)[names(df) == "power"] <- "power_db"
  aggregate(power_db ~ freq, data = df, FUN = function(x) mean(x, na.rm = TRUE))
}

# Average bandpower summaries across voxels and convert back to dB space
.average_bandpower <- function(bp_list) {
  if (!length(bp_list)) stop("No bandpower estimates supplied for averaging.")
  df <- .bind_rows(bp_list, id_name = "voxel")
  agg <- aggregate(
    cbind(power_linear, relative_power) ~ label + low + high,
    data = df,
    FUN = function(x) mean(x, na.rm = TRUE)
  )
  agg$power_db <- ifelse(
    is.finite(agg$power_linear) & agg$power_linear > 0,
    10 * log10(agg$power_linear),
    NA_real_
  )
  agg
}

.extract_psd_linear <- function(spec_df) {
  psd <- attr(spec_df, "psd_linear")
  if (is.null(psd) || is.null(psd$freq) || is.null(psd$spec)) {
    stop("Multitaper spectrum is missing its `psd_linear` attribute; ",
         "update power_multitaper().")
  }
  psd
}

# Human-friendly formatting for QC outputs
.format_stopband_reduction <- function(val) {
  if (is.null(val) || is.na(val)) return("NA")
  formatted <- signif(val, 4)
  if (val < 0) {
    return(paste0(
      formatted,
      " (WARNING: stopband went *up* in power by this amount!)"
    ))
  }
  as.character(formatted)
}

.format_passband_change <- function(val) {
  if (is.null(val) || is.na(val)) return("NA")
  sprintf("%+.4g", signif(val, 4))
}

## ----------------------------------------------------------
## Argument parsing + logging
## ----------------------------------------------------------

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  print_help <- function() {
    cat("MRI Temporal Filtering QC\n")
    cat("Usage:\n")
    cat("  Rscript temp_filter_new.R --pre=pre.nii.gz --post=post.nii.gz ",
        "--mask=mask.nii.gz --dt=TR --subnum=ID ",
        "[--save_plots] [--save_spectrum] ",
        "[--passband_low=Hz] [--passband_high=Hz] [--seed=INT] [--log] ",
        "[--min_stopband_reduction_db=NUM] [--max_passband_reduction_db=NUM]\n",
        sep = "")
    cat(
      "\nArguments:",
      "  --pre=PATH                      Pre-filter 4D NIfTI",
      "  --post=PATH                     Post-filter 4D NIfTI",
      "  --mask=PATH                     Brain mask (matches pre/post dims)",
      "  --dt=NUM                        TR (seconds)",
      "  --subnum=ID                     Subject identifier used in outputs",
      "  --passband_low=NUM              Low edge of retained passband (Hz)",
      "  --passband_high=NUM             High edge of retained passband (Hz)",
      "  --min_stopband_reduction_db=NUM Minimum acceptable stopband reduction (dB, default 10)",
      "  --max_passband_reduction_db=NUM Maximum acceptable passband reduction (dB, default 3)",
      "  --save_plots                    Save PNG plots",
      "  --save_spectrum                 Save multitaper spectra CSVs",
      "  --seed=INT                      Seed for voxel sampling",
      "  --log                           Write log to file",
      "  --help                          Show this message",
      sep = "\n"
    )
  }

  if ("--help" %in% args || "-h" %in% args || length(args) == 0) {
    print_help()
    quit(status = 0)
  }

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
    stop("Argument '", flag,
         "' must be a boolean (one of 1/0/true/false/yes/no).")
  }

  pre_path      <- get_arg("--pre")
  post_path     <- get_arg("--post")
  mask_path     <- get_arg("--mask")
  dt_arg        <- get_arg("--dt")
  subnum        <- get_arg("--subnum")
  save_plots    <- get_bool_flag("--save_plots")
  save_spectrum <- get_bool_flag("--save_spectrum")
  seed_arg      <- get_arg("--seed")
  passband_low_arg  <- get_arg("--passband_low")
  passband_high_arg <- get_arg("--passband_high")
  log_output    <- get_bool_flag("--log")
  stopband_thresh_arg <- get_arg("--min_stopband_reduction_db")
  passband_thresh_arg <- get_arg("--max_passband_reduction_db")

  if (any(grepl("^--band_low(=|$)", args))) {
    stop("Flag '--band_low' has been removed; use '--passband_low'.")
  }
  if (any(grepl("^--band_high(=|$)", args))) {
    stop("Flag '--band_high' has been removed; use '--passband_high'.")
  }

  dt <- suppressWarnings(as.numeric(dt_arg))
  if (is.null(dt_arg) || is.na(dt) || dt <= 0) dt <- NA_real_

  passband_low  <- if (is.null(passband_low_arg)) NA_real_ else as.numeric(passband_low_arg)
  passband_high <- if (is.null(passband_high_arg)) NA_real_ else as.numeric(passband_high_arg)
  if (!is.na(passband_low) && passband_low < 0) {
    stop("Argument '--passband_low' must be non-negative.")
  }
  if (!is.na(passband_high) && passband_high < 0) {
    stop("Argument '--passband_high' must be non-negative.")
  }

  min_stopband_reduction_db <- if (is.null(stopband_thresh_arg)) {
    10
  } else {
    val <- suppressWarnings(as.numeric(stopband_thresh_arg))
    if (is.na(val)) {
      stop("Argument '--min_stopband_reduction_db' must be numeric.")
    }
    val
  }

  max_passband_reduction_db <- if (is.null(passband_thresh_arg)) {
    3
  } else {
    val <- suppressWarnings(as.numeric(passband_thresh_arg))
    if (is.na(val)) {
      stop("Argument '--max_passband_reduction_db' must be numeric.")
    }
    val
  }

  seed <- if (is.null(seed_arg)) {
    NA_integer_
  } else {
    s <- suppressWarnings(as.integer(seed_arg))
    if (is.na(s)) stop("Argument '--seed' must be an integer.")
    s
  }

  if (is.null(pre_path) || is.null(post_path) || is.na(dt) || is.null(subnum)) {
    print_help()
    quit(status = 1)
  }

  list(
    pre_path = pre_path,
    post_path = post_path,
    mask_path = mask_path,
    dt = dt,
    subnum = subnum,
    save_plots = save_plots,
    save_spectrum = save_spectrum,
    passband_low = passband_low,
    passband_high = passband_high,
    seed = seed,
    log_output = log_output,
    min_stopband_reduction_db = min_stopband_reduction_db,
    max_passband_reduction_db = max_passband_reduction_db
  )
}

setup_logging <- function(subnum, log_output) {
  if (!log_output) return(NULL)

  log_file <- paste0("filter_check_", subnum, ".txt")
  prev_sink_depth <- sink.number()

  sink(log_file, split = TRUE)

  list(file = log_file, depth = prev_sink_depth)
}

## ----------------------------------------------------------
## Image / mask loading and voxel sampling
## ----------------------------------------------------------

load_images_and_mask <- function(pre_path, post_path, mask_path) {
  cat("Loading NIfTI images...\n")

  pre_img <- RNifti::readNifti(pre_path)
  pre_dims <- dim(pre_img)
  if (length(pre_dims) == 3L) {
    pre_dims <- c(pre_dims, 1L)  # treat 3D as 1 timepoint
    dim(pre_img) <- pre_dims
  }
  pre_n_t   <- pre_dims[4]
  pre_n_vox <- prod(pre_dims[1:3])

  mask_idx  <- NULL
  mask_dims <- NULL

  if (!is.null(mask_path)) {
    mask <- RNifti::readNifti(mask_path)
    mask_dims <- dim(mask)
    if (!all(mask_dims == pre_dims[1:3])) {
      stop("Mask dimensions do not match pre image.")
    }
    mask_logical <- (mask != 0) & is.finite(mask)
    n_mask_vox <- sum(mask_logical)
    if (n_mask_vox == 0) stop("Mask contains no voxels.")
    mask_idx <- which(mask_logical)
    rm(mask)
  } else {
    mask_idx <- seq_len(pre_n_vox)
    mask_dims <- pre_dims[1:3]
  }

  # Convert linear indices into i/j/k coordinates used by the extractor closures
  mask_coords <- arrayInd(mask_idx, pre_dims[1:3], useNames = FALSE)

  post_img <- RNifti::readNifti(post_path)
  post_dims <- dim(post_img)
  if (length(post_dims) == 3L) {
    post_dims <- c(post_dims, 1L)
    dim(post_img) <- post_dims
  }
  post_n_t   <- post_dims[4]
  post_n_vox <- prod(post_dims[1:3])

  if (!is.null(mask_path) && !all(mask_dims == post_dims[1:3])) {
    stop("Mask dimensions do not match post image.")
  }
  if (pre_n_t != post_n_t) {
    stop("Pre and post series must have the same number of timepoints.")
  }
  if (pre_n_vox != post_n_vox) {
    stop("Pre and post images must have the same spatial dimensions.")
  }

  list(
    pre_img = pre_img,
    post_img = post_img,
    pre_dims = pre_dims,
    post_dims = post_dims,
    mask_idx = mask_idx,
    mask_coords = mask_coords,
    mask_dims = mask_dims,
    n_t = pre_n_t
  )
}

sample_voxels_for_spectra <- function(pre_img, post_img, mask_idx,
                                      mask_coords, mask_path) {
  var_tol <- if (is.null(mask_path)) 1e-3 else 2 * .Machine$double.eps
  default_sample_size <- 100L

  voxels_to_sample <- if (length(mask_idx) <= default_sample_size) {
    length(mask_idx)
  } else {
    default_sample_size
  }

  get_pre_ts  <- .make_ts_extractor(pre_img, mask_coords)
  get_post_ts <- .make_ts_extractor(post_img, mask_coords)

  selection <- .select_nonconstant_voxels(
    mask_idx = mask_idx,
    get_pre_ts = get_pre_ts,
    get_post_ts = get_post_ts,
    n_voxels = voxels_to_sample,
    var_tol = var_tol
  )

  selected_idx       <- selection$indices
  selected_positions <- selection$positions

  if (is.null(mask_path)) {
    cat("Mask not provided; randomly selected",
        length(selected_idx),
        "non-constant voxels with variance >",
        signif(var_tol, 3),
        "for multitaper averaging.\n")
  } else {
    cat("Selected", length(selected_idx),
        "mask voxels with variance >", signif(var_tol, 3),
        "for multitaper averaging.\n")
  }

  list(
    selected_idx = selected_idx,
    selected_positions = selected_positions,
    get_pre_ts = get_pre_ts,
    get_post_ts = get_post_ts
  )
}

## ----------------------------------------------------------
## Multitaper spectra, plots, CSV export
## ----------------------------------------------------------

compute_multitaper_spectra <- function(selected_positions,
                                       get_pre_ts, get_post_ts,
                                       dt, subnum,
                                       save_spectrum, save_plots,
                                       passband_low, passband_high) {

  cat("Computing multitaper spectra for", length(selected_positions),
      "voxels...\n")

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

  pre_psd  <- lapply(pre_spectra, .extract_psd_linear)
  post_psd <- lapply(post_spectra, .extract_psd_linear)

  mt_pre  <- .average_multitaper_spectra(pre_spectra)
  mt_post <- .average_multitaper_spectra(post_spectra)

  file_prefix       <- paste0("multitaper_spectrum_", subnum)
  pre_out_file_mtap <- paste0(file_prefix, "_pre.csv")
  post_out_file_mtap <- paste0(file_prefix, "_post.csv")

  mt_diff <- merge(mt_pre, mt_post, by = "freq", suffixes = c("_pre", "_post"))
  mt_diff$power_diff_db <- mt_diff$power_db_pre - mt_diff$power_db_post

  diff_out_file <- paste0(file_prefix, "_diff.csv")

  if (save_spectrum) {
    write.csv(mt_pre,  pre_out_file_mtap,  row.names = FALSE)
    write.csv(mt_post, post_out_file_mtap, row.names = FALSE)
    write.csv(mt_diff, diff_out_file,      row.names = FALSE)
    cat("Saved multitaper spectra to", pre_out_file_mtap, "and",
        post_out_file_mtap, "\n")
    cat("Saved multitaper difference to", diff_out_file, "\n")
  } else {
    cat("Skipping multitaper spectrum export (use --save_spectrum to enable).\n")
  }

  if (save_plots) {
    spectrum_out_file <- paste0(file_prefix, "_pre_post.png")
    spectrum_df <- rbind(
      transform(mt_pre, series = "Pre-filter"),
      transform(mt_post, series = "Post-filter")
    )

    max_freq <- max(c(mt_pre$freq, mt_post$freq), na.rm = TRUE)
    stopband_ranges <- list()
    if (!is.na(passband_low) && passband_low > 0) {
      stopband_ranges[[length(stopband_ranges) + 1L]] <- c(0, passband_low)
    }
    if (!is.na(passband_high) && passband_high < max_freq) {
      stopband_ranges[[length(stopband_ranges) + 1L]] <- c(passband_high, max_freq)
    }
    stopband_df <- if (length(stopband_ranges)) {
      data.frame(
        xmin = vapply(stopband_ranges, function(x) x[1], numeric(1)),
        xmax = vapply(stopband_ranges, function(x) x[2], numeric(1))
      )
    } else {
      NULL
    }
    caption_text <- if (is.null(stopband_df)) {
      NULL
    } else {
      "Gray shading indicates specified stopband frequencies (filtered out)."
    }

    spectrum_plot <- ggplot(spectrum_df,
                            aes(x = freq, y = power_db, color = series)) +
      {
        if (is.null(stopband_df)) NULL else
          geom_rect(data = stopband_df,
                    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                    inherit.aes = FALSE,
                    fill = "gray90",
                    alpha = 0.6)
      } +
      geom_line(linewidth = 1) +
      labs(
        title = paste("Smoothed Multitaper Spectrum | Subject", subnum),
        x = "Frequency (Hz)",
        y = "Power (dB)",
        color = NULL
      ) +
      scale_color_manual(values = c(
        "Pre-filter"  = "steelblue",
        "Post-filter" = "firebrick"
      )) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")

    cutoff_freqs <- c(
      if (!is.na(passband_low)) passband_low else NULL,
      if (!is.na(passband_high)) passband_high else NULL
    )

    if (length(cutoff_freqs) > 0) {
      spectrum_plot <- spectrum_plot +
        geom_vline(xintercept = cutoff_freqs,
                   linetype = "dashed",
                   color = "gray40",
                   linewidth = 0.7)
    }
    if (!is.null(caption_text)) {
      spectrum_plot <- spectrum_plot + labs(caption = caption_text)
    }

    ggsave(filename = spectrum_out_file, plot = spectrum_plot,
           width = 8, height = 6, units = "in", dpi = 300)
    cat("Saved multitaper spectrum plot to", spectrum_out_file, "\n")

    diff_plot_out_file <- paste0(file_prefix, "_diff.png")
    diff_plot <- ggplot(mt_diff, aes(x = freq, y = power_diff_db)) +
      {
        if (is.null(stopband_df)) NULL else
          geom_rect(data = stopband_df,
                    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                    inherit.aes = FALSE,
                    fill = "gray90",
                    alpha = 0.5)
      } +
      geom_line(color = "navy", linewidth = 1) +
      geom_hline(yintercept = 0,
                 color = "gray70",
                 linetype = "dotted") +
      labs(
        title = paste("Multitaper Power Difference (Pre - Post) | Subject",
                      subnum),
        x = "Frequency (Hz)",
        y = "Power Difference (dB)"
      ) +
      theme_minimal(base_size = 14)

    if (length(cutoff_freqs) > 0) {
      diff_plot <- diff_plot +
        geom_vline(xintercept = cutoff_freqs,
                   linetype = "dashed",
                   color = "gray40",
                   linewidth = 0.7)
    }
    if (!is.null(caption_text)) {
      diff_plot <- diff_plot + labs(caption = caption_text)
    }

    ggsave(filename = diff_plot_out_file, plot = diff_plot,
           width = 8, height = 6, units = "in", dpi = 300)
    cat("Saved multitaper power difference plot to", diff_plot_out_file, "\n")
  } else {
    cat("Skipping plot export (use --save_plots to enable).\n")
  }

  list(pre_psd = pre_psd, post_psd = post_psd)
}

## ----------------------------------------------------------
## Bandpower summaries + QC metrics
## ----------------------------------------------------------

compute_bandpower_summaries <- function(pre_psd, post_psd, dt,
                                        passband_low, passband_high) {
  nyquist <- 1 / (2 * dt)
  outside_bands <- list()
  bandpower_results <- list()
  metrics <- list(
    outside_avg_reduction_db = NA_real_,
    passband_avg_change_db = NA_real_
  )
  format_band <- function(low, high) {
    paste0("[", signif(low, 4), ", ", signif(high, 4), "] Hz")
  }

  if (!is.na(passband_low) && passband_low > 0) {
    outside_bands$below <- c(0, max(0, passband_low))
  }
  if (!is.na(passband_high) && passband_high < nyquist) {
    outside_bands$above <- c(min(passband_high, nyquist), nyquist)
  }

  if (length(outside_bands) > 0) {
    stopband_desc <- vapply(
      outside_bands,
      function(b) format_band(b[1], b[2]),
      character(1)
    )
    cat("Stopband segments: ", paste(stopband_desc, collapse = "; "), "\n", sep = "")
    cat("Computing band power outside filter bounds...\n")

    pre_bp_list <- lapply(pre_psd, function(psd) {
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

    pre_bp_avg  <- .average_bandpower(pre_bp_list)
    post_bp_avg <- .average_bandpower(post_bp_list)

    pre_sel <- pre_bp_avg[, c("label", "low", "high", "power_db", "relative_power")]
    names(pre_sel)[names(pre_sel) == "power_db"] <- "power_db_pre"
    names(pre_sel)[names(pre_sel) == "relative_power"] <- "relative_power_pre"
    post_sel <- post_bp_avg[, c("label", "low", "high", "power_db", "relative_power")]
    names(post_sel)[names(post_sel) == "power_db"] <- "power_db_post"
    names(post_sel)[names(post_sel) == "relative_power"] <- "relative_power_post"
    bandpower_diff <- merge(pre_sel, post_sel, by = c("label", "low", "high"))
    bandpower_diff$power_db_change <- bandpower_diff$power_db_post - bandpower_diff$power_db_pre
    bandpower_diff$relative_power_change <- bandpower_diff$relative_power_post - bandpower_diff$relative_power_pre
    bandpower_diff$band_type <- "outside"

    avg_reduction <- mean(
      bandpower_diff$power_db_pre - bandpower_diff$power_db_post,
      na.rm = TRUE
    )

    metrics$outside_avg_reduction_db <- if (is.nan(avg_reduction)) NA_real_ else avg_reduction
    bandpower_results$outside <- bandpower_diff

    cat("Average power reduction outside bands (dB):",
        .format_stopband_reduction(metrics$outside_avg_reduction_db), "\n")
  } else {
    cat("No stopband segments derived from input cutoffs; ",
        "skipping outside-band bandpower computation.\n", sep = "")
  }

  passband_low_adj  <- if (!is.na(passband_low)) max(0, passband_low) else 0
  passband_high_adj <- if (!is.na(passband_high)) min(nyquist, passband_high) else nyquist
  has_passband_bounds <- (!is.na(passband_low) || !is.na(passband_high)) &&
    passband_high_adj > passband_low_adj
  passband_label <- format_band(passband_low_adj, passband_high_adj)
  if (is.na(passband_low) && is.na(passband_high)) {
    cat("Passband segment: ", passband_label,
        " (full spectrum; no cutoffs provided)\n", sep = "")
  } else {
    cat("Passband segment: ", passband_label, "\n", sep = "")
  }

  if (has_passband_bounds) {
    cat("Computing band power within passband...\n")
    passband <- list(passband = c(passband_low_adj, passband_high_adj))

    pre_pass_list <- lapply(pre_psd, function(psd) {
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

    pre_pass_avg  <- .average_bandpower(pre_pass_list)
    post_pass_avg <- .average_bandpower(post_pass_list)

    pre_sel <- pre_pass_avg[, c("label", "low", "high", "power_db", "relative_power")]
    names(pre_sel)[names(pre_sel) == "power_db"] <- "power_db_pre"
    names(pre_sel)[names(pre_sel) == "relative_power"] <- "relative_power_pre"
    post_sel <- post_pass_avg[, c("label", "low", "high", "power_db", "relative_power")]
    names(post_sel)[names(post_sel) == "power_db"] <- "power_db_post"
    names(post_sel)[names(post_sel) == "relative_power"] <- "relative_power_post"
    passband_diff <- merge(pre_sel, post_sel, by = c("label", "low", "high"))
    passband_diff$power_db_change <- passband_diff$power_db_post - passband_diff$power_db_pre
    passband_diff$relative_power_change <- passband_diff$relative_power_post - passband_diff$relative_power_pre
    passband_diff$band_type <- "passband"

    power_changes <- passband_diff$power_db_change
    avg_change_db <- if (all(is.na(power_changes))) {
      NA_real_
    } else {
      mean(power_changes, na.rm = TRUE)
    }

    metrics$passband_avg_change_db <- avg_change_db
    bandpower_results$passband <- passband_diff

    cat("Average power change within passband (dB):",
        .format_passband_change(avg_change_db), "\n")
  } else {
    cat("Passband bounds not provided; skipping passband power comparison.\n")
  }

  list(results = bandpower_results, metrics = metrics)
}

## ----------------------------------------------------------
## QC: automated PASS/FAIL based on bandpower metrics
## ----------------------------------------------------------

evaluate_filter_qc <- function(metrics,
                               min_stopband_reduction_db = 10,
                               max_passband_reduction_db = 3) {
  status <- "PASS"
  messages <- character()

  outside_red <- metrics$outside_avg_reduction_db
  passband_chg <- metrics$passband_avg_change_db

  if (!is.null(outside_red) && is.finite(outside_red)) {
    if (outside_red < min_stopband_reduction_db) {
      status <- "FAIL"
      messages <- c(
        messages,
        sprintf(
          "Insufficient attenuation outside band: average reduction %s dB (threshold %.1f dB).",
          .format_stopband_reduction(outside_red), min_stopband_reduction_db
        )
      )
    }
  } else {
    messages <- c(
      messages,
      "Stopband attenuation could not be estimated (no frequencies outside band or NA values)."
    )
  }

  if (!is.null(passband_chg) && is.finite(passband_chg)) {
    if (passband_chg < -max_passband_reduction_db) {
      status <- "FAIL"
      messages <- c(
        messages,
        sprintf(
          "Excessive attenuation in passband: average change %s dB (max allowed reduction %.1f dB).",
          .format_passband_change(passband_chg), max_passband_reduction_db
        )
      )
    }
  } else {
    messages <- c(
      messages,
      "Passband change could not be estimated (no passband bounds or NA values)."
    )
  }

  list(status = status, messages = messages)
}

## ----------------------------------------------------------
## Main driver
## ----------------------------------------------------------

main <- function() {
  args <- parse_args()

  if (!is.na(args$seed)) {
    set.seed(args$seed)
    cat("Sampling seed set to", args$seed, "\n")
  }

  log_info <- setup_logging(args$subnum, args$log_output)
  log_file <- NULL

  if (!is.null(log_info)) {
    log_file <- log_info$file
    on.exit({
      # Ensure sinks are restored to the prior depth even if the script errors
      while (sink.number() > log_info$depth) sink()
    }, add = TRUE)
  }

  cat("----- MRI Temporal Filtering Check Log -----\n")
  cat("Started:", format(Sys.time()), "\n\n")

  if (!is.null(log_file)) {
    cat("Logging all output to", log_file, "\n\n")
  }

  cat("Subject:", args$subnum, "\n")
  cat("Pre path:", args$pre_path, "\n")
  cat("Post path:", args$post_path, "\n")
  cat("Mask path:", ifelse(is.null(args$mask_path), "None", args$mask_path), "\n")
  cat("TR (dt):", args$dt, "\n")
  cat("Save plots:", args$save_plots, "\n")
  cat("Save spectrum:", args$save_spectrum, "\n")
  cat("Passband low (Hz):", ifelse(is.na(args$passband_low), "NA", args$passband_low), "\n")
  cat("Passband high (Hz):", ifelse(is.na(args$passband_high), "NA", args$passband_high), "\n")
  cat("Min stopband reduction (dB):", args$min_stopband_reduction_db, "\n")
  cat("Max passband reduction (dB):", args$max_passband_reduction_db, "\n\n")

  img_info <- load_images_and_mask(
    pre_path = args$pre_path,
    post_path = args$post_path,
    mask_path = args$mask_path
  )

  voxel_info <- sample_voxels_for_spectra(
    pre_img = img_info$pre_img,
    post_img = img_info$post_img,
    mask_idx = img_info$mask_idx,
    mask_coords = img_info$mask_coords,
    mask_path = args$mask_path
  )

  spectra <- compute_multitaper_spectra(
    selected_positions = voxel_info$selected_positions,
    get_pre_ts = voxel_info$get_pre_ts,
    get_post_ts = voxel_info$get_post_ts,
    dt = args$dt,
    subnum = args$subnum,
    save_spectrum = args$save_spectrum,
    save_plots = args$save_plots,
    passband_low = args$passband_low,
    passband_high = args$passband_high
  )

  bp <- compute_bandpower_summaries(
    pre_psd = spectra$pre_psd,
    post_psd = spectra$post_psd,
    dt = args$dt,
    passband_low = args$passband_low,
    passband_high = args$passband_high
  )

  bandpower_results <- bp$results
  metrics <- bp$metrics

  if (length(bandpower_results) > 0) {
    combined_bandpower <- .bind_rows(bandpower_results)
    combined_bandpower <- combined_bandpower[, c(
      "band_type", "label", "low", "high",
      "power_db_pre", "relative_power_pre",
      "power_db_post", "relative_power_post",
      "power_db_change", "relative_power_change"
    ), drop = FALSE]

    bandpower_file <- paste0("bandpower_", args$subnum, ".csv")
    write.csv(combined_bandpower, bandpower_file, row.names = FALSE)
    cat("Bandpower details saved to", bandpower_file, "\n")
  }

  qc <- evaluate_filter_qc(
    metrics = metrics,
    min_stopband_reduction_db = args$min_stopband_reduction_db,
    max_passband_reduction_db = args$max_passband_reduction_db
  )
  stopband_change_db <- if (is.null(metrics$outside_avg_reduction_db)) {
    NA_real_
  } else {
    -metrics$outside_avg_reduction_db  # post - pre (expect negative if attenuated)
  }
  stopband_pass <- !is.null(metrics$outside_avg_reduction_db) &&
    is.finite(metrics$outside_avg_reduction_db) &&
    metrics$outside_avg_reduction_db >= args$min_stopband_reduction_db

  passband_change_db <- metrics$passband_avg_change_db
  passband_pass <- !is.null(passband_change_db) &&
    is.finite(passband_change_db) &&
    passband_change_db >= -args$max_passband_reduction_db

  cat("\n----- FILTER QC SUMMARY -----\n")
  cat("Overall QC status:", qc$status, "\n")
  cat("Average stopband change (dB):",
      .format_passband_change(stopband_change_db),
      if (stopband_pass) "(PASS)" else "(FAIL)", "\n")
  cat("Average passband change (dB):",
      .format_passband_change(passband_change_db),
      if (passband_pass) "(PASS)" else "(FAIL)", "\n")

  if (length(qc$messages)) {
    cat("\nQC Notes:\n")
    for (m in qc$messages) cat("  -", m, "\n")
  }

  cat("\nCompleted:", format(Sys.time()), "\n")

  exit_code <- if (identical(qc$status, "PASS")) 0L else 1L
  invisible(exit_code)
}

if (sys.nframe() == 0L) {
  status <- main()
  quit(status = status)
}
