#' Postprocess confounds to match fMRI operations
#'
#' Handles optional spike regression file creation, filtering of confound
#' time series and writing of postprocessed confounds/regressor files.
#' This is used internally by `postprocess_subject()`.
#'
#' @param proc_files List of files returned by `get_fmriprep_outputs()`.
#' @param cfg Configuration list passed to `postprocess_subject`.
#' @param processing_sequence Character vector of enabled processing steps.
#' @param output_bids_info Named list of BIDS entities for the postprocessed output file
#' @param fsl_img Optional path to a Singularity image with FSL installed.
#' @param lg Logger object for messages.
#' @return Path to the nuisance regressor file or `NULL` if not created.
#' @importFrom stats setNames
#' @keywords internal
postprocess_confounds <- function(proc_files, cfg, processing_sequence,
                                  output_bids_info, fsl_img = NULL, lg = NULL) {
  if (!checkmate::test_class(lg, "Logger")) lg <- lgr::get_logger_glue("BrainGnomes")

  # if no confound steps are enabled, no need to continue
  if (!isTRUE(cfg$confound_regression$enable) &&
      !isTRUE(cfg$confound_calculate$enable) &&
      !isTRUE(cfg$scrubbing$enable)) {
    return(NULL)
  }

  if (!checkmate::test_file_exists(proc_files$confounds)) {
    to_log(lg, "fatal", "Cannot find required confounds file: {proc_files$confounds}")
  }

  # read confounds file
  confounds <- data.table::fread(proc_files$confounds,
                                 na.strings = c("n/a", "NA", "."),
                                 data.table = FALSE)

  requested_confound_cols <- unlist(list(
    cfg$confound_regression$columns,
    cfg$confound_regression$noproc_columns,
    cfg$confound_calculate$columns,
    cfg$confound_calculate$noproc_columns
  ), use.names = FALSE)
  if (!is.null(requested_confound_cols)) {
    requested_confound_cols <- as.character(requested_confound_cols)
    requested_confound_cols <- requested_confound_cols[!is.na(requested_confound_cols)]
    requested_confound_cols <- trimws(requested_confound_cols)
    requested_confound_cols <- requested_confound_cols[nzchar(requested_confound_cols)]
  }
  needs_unfiltered_fd <- length(requested_confound_cols) > 0L &&
    any(tolower(requested_confound_cols) == "framewise_displacement_unfiltered")
  if (needs_unfiltered_fd) {
    if ("framewise_displacement" %in% names(confounds) &&
        !"framewise_displacement_unfiltered" %in% names(confounds)) {
      confounds$framewise_displacement_unfiltered <- confounds$framewise_displacement
    } else if (!"framewise_displacement_unfiltered" %in% names(confounds)) {
      to_log(lg, "warn", "Requested framewise_displacement_unfiltered, but source confounds lack framewise_displacement; this column will be omitted.")
    }
  }

  # handle filtering of motion parameters for respiratory artifacts, recalculation of framewise_displacement
  motion_filter_cfg <- cfg$motion_filter
  if (!is.null(motion_filter_cfg) && isTRUE(motion_filter_cfg$enable)) {
    deprecated_present <- !is.null(motion_filter_cfg$band_stop_min) ||
      !is.null(motion_filter_cfg$band_stop_max)
    if (deprecated_present) {
      warned <- isTRUE(getOption("bg_warned_deprecated_motion_filter", FALSE))
      if (!warned) {
        warning(
          "motion_filter$band_stop_min/band_stop_max are deprecated; use bandstop_min_bpm/bandstop_max_bpm instead.",
          call. = FALSE
        )
        options(bg_warned_deprecated_motion_filter = TRUE)
      }
      if (is.null(motion_filter_cfg$bandstop_min_bpm) && !is.null(motion_filter_cfg$band_stop_min)) {
        motion_filter_cfg$bandstop_min_bpm <- motion_filter_cfg$band_stop_min
      }
      if (is.null(motion_filter_cfg$bandstop_max_bpm) && !is.null(motion_filter_cfg$band_stop_max)) {
        motion_filter_cfg$bandstop_max_bpm <- motion_filter_cfg$band_stop_max
      }
    }

    motion_cols <- c("rot_x", "rot_y", "rot_z", "trans_x", "trans_y", "trans_z")
    filter_type <- motion_filter_cfg$filter_type
    if (is.null(filter_type)) {
      if (!is.null(motion_filter_cfg$lowpass_bpm)) {
        filter_type <- "lowpass"
      } else {
        filter_type <- "notch"
      }
      motion_filter_cfg$filter_type <- filter_type
    }
    lowpass_hz <- NULL
    if (!is.null(motion_filter_cfg$lowpass_bpm)) {
      lowpass_hz <- motion_filter_cfg$lowpass_bpm / 60
    }
    confounds <- run_logged(
      filter_confounds,
      confounds_df = confounds,
      tr = cfg$tr,
      filter_type = filter_type,
      bandstop_min_bpm = motion_filter_cfg$bandstop_min_bpm,
      bandstop_max_bpm = motion_filter_cfg$bandstop_max_bpm,
      low_pass_hz = lowpass_hz,
      filter_order = motion_filter_cfg$filter_order,
      columns = motion_cols,
      lg = lg,
      fun_label = "motion_filter"
    )
    if (all(motion_cols %in% names(confounds))) {
      head_radius <- cfg$scrubbing$head_radius
      if (is.null(head_radius)) head_radius <- 50
      fd <- framewise_displacement(
        motion = confounds[, motion_cols, drop = FALSE],
        head_radius = head_radius,
        columns = motion_cols
      )
      confounds$framewise_displacement <- fd
      if (filter_type == "notch") {
        to_log(lg, "info", "Updated framewise_displacement after notch filtering ({motion_filter_cfg$bandstop_min_bpm}-{motion_filter_cfg$bandstop_max_bpm} BPM).")
      } else {
        order_label <- motion_filter_cfg$filter_order
        if (is.null(order_label)) order_label <- 2L
        to_log(lg, "info", "Updated framewise_displacement after low-pass filtering (cutoff {motion_filter_cfg$lowpass_bpm} BPM, order {order_label}).")
      }
    } else {
      to_log(lg, "warn", "Filtered motion parameters available but required columns are missing; cannot recompute framewise displacement.")
    }
  }

  # handle regular expression and range expansion in column names
  cfg$confound_regression$columns <- expand_confound_columns(
    cfg$confound_regression$columns, names(confounds)
  )
  cfg$confound_calculate$columns <- expand_confound_columns(
    cfg$confound_calculate$columns, names(confounds)
  )
  cfg$confound_regression$noproc_columns <- expand_confound_columns(
    cfg$confound_regression$noproc_columns, names(confounds)
  )
  cfg$confound_calculate$noproc_columns <- expand_confound_columns(
    cfg$confound_calculate$noproc_columns, names(confounds)
  )

  confound_cols <- as.character(union(cfg$confound_regression$columns,
                                        cfg$confound_calculate$columns))
  noproc_cols <- as.character(union(cfg$confound_regression$noproc_columns,
                                      cfg$confound_calculate$noproc_columns))
  confound_cols_txt <- if (length(confound_cols) > 0L) paste(confound_cols, collapse = ", ") else "<none>"
  noproc_cols_txt <- if (length(noproc_cols) > 0L) paste(noproc_cols, collapse = ", ") else "<none>"
  to_log(lg, "debug", "Expanded confound columns: {confound_cols_txt}; noproc columns: {noproc_cols_txt}")
  if (any(noproc_cols %in% confound_cols)) {
    stop("Cannot handle overlaps in noproc_columns and columns for confounds")
  }

  confounds_to_filt <- NULL
  if (isTRUE(cfg$scrubbing$enable)) {
    to_log(lg, "info", "Computing spike regressors using expression: {paste(cfg$scrubbing$expression, collapse=', ')}")
    spike_mat <- compute_spike_regressors(confounds, cfg$scrubbing$expression, lg = lg)
    scrub_file <- construct_bids_filename(
      modifyList(output_bids_info, list(suffix = "scrub", ext = ".tsv")), full.names = TRUE
    )
    
    censor_file <- get_censor_file(output_bids_info)
    if (!is.null(spike_mat)) {
      data.table::fwrite(as.data.frame(spike_mat), file = scrub_file, sep = "\t", col.names = FALSE)
      censor_vec <- ifelse(rowSums(spike_mat) > 0, 0, 1)
      writeLines(as.character(censor_vec), con = censor_file)
    } else {
      to_log(lg, "info", "No spikes detected; censor file will contain all 1s")
      writeLines(rep("1", nrow(confounds)), con = censor_file)
    }
  }

  has_confounds <- !is.null(confound_cols) && length(confound_cols) > 0L
  has_noproc <- !is.null(noproc_cols) && length(noproc_cols) > 0L

  # verify that requested confound columns exist
  if (has_confounds) {
    confounds_to_filt <- subset(confounds, select = confound_cols)

    if (ncol(confounds_to_filt) == 0L || nrow(confounds_to_filt) == 0L) {
      to_log(lg, "warn", "Confound columns were requested but no usable data were found in {proc_files$confounds}; skipping confound filtering.")
      has_confounds <- FALSE
      confound_cols <- character()
    }
  }

  if (!has_confounds && !has_noproc) {
    if (isTRUE(cfg$confound_calculate$enable) || isTRUE(cfg$confound_regression$enable)) {
      to_log(lg, "info", "Confound postprocessing skipped; no confound columns matched the request and no noproc columns were supplied.")
    } else {
      to_log(lg, "debug", "No confound or noproc columns were requested; skipping confound postprocessing.")
    }
    return(NULL)
  }

  if (has_confounds) {
    # generate NIfTI with confound timeseries
    confounds_bids <- extract_bids_info(proc_files$confounds)
    tmp_out <- construct_bids_filename(modifyList(confounds_bids, list(description = cfg$bids_desc, directory=tempdir(), ext=NA)), full.names=TRUE)
    confound_nii <- mat_to_nii(confounds_to_filt, ni_out = tmp_out)

    # TODO: consider whether to worry about motion filtering AROMA components if motion parameters or FD are in confound regressors
    # Regress out AROMA components, if requested (overwrites file in place)
    if ("apply_aroma" %in% processing_sequence) {
      if (is.null(proc_files$melodic_mix) || !checkmate::test_file_exists(proc_files$melodic_mix)) {
        to_log(lg, "warn", "Cannot locate melodic mixing file; skipping AROMA regression for confounds.")
      } else if (is.null(proc_files$noise_ics) || length(proc_files$noise_ics) == 0) {
        to_log(lg, "info", "No AROMA noise components provided; skipping regression for confounds.")
      } else if (!checkmate::test_integerish(proc_files$noise_ics, lower = 1, any.missing = FALSE)) {
        to_log(lg, "warn", "noise_ics must be a vector of positive integers; skipping AROMA regression for confounds.")
      } else {
        nonaggressive_val <- cfg$apply_aroma$nonaggressive
        nonaggressive_flag <- if (is.null(nonaggressive_val) || is.na(nonaggressive_val)) TRUE else isTRUE(nonaggressive_val)
        exclusive_flag <- !nonaggressive_flag
        mode_label <- if (exclusive_flag) "aggressive" else "non-aggressive"

        mixing_mat <- as.matrix(data.table::fread(proc_files$melodic_mix, header = FALSE, data.table = FALSE))
        storage.mode(mixing_mat) <- "double"
        if (nrow(mixing_mat) != nrow(confounds_to_filt)) {
          to_log(lg, "warn", "Mixing matrix has {nrow(mixing_mat)} rows but confounds have {nrow(confounds_to_filt)} timepoints; skipping AROMA regression for confounds.")
        } else if (ncol(mixing_mat) == 0) {
          to_log(lg, "warn", "Mixing matrix {proc_files$melodic_mix} has no components; skipping AROMA regression for confounds.")
        } else {
          comp_idx <- sort(unique(as.integer(proc_files$noise_ics)))
          invalid_idx <- comp_idx[comp_idx < 1 | comp_idx > ncol(mixing_mat)]
          if (length(invalid_idx) > 0) {
            to_log(lg, "warn", "Dropping invalid AROMA component indices for confounds: {paste(invalid_idx, collapse = ', ')}")
            comp_idx <- setdiff(comp_idx, invalid_idx)
          }
          if (length(comp_idx) == 0) {
            to_log(lg, "info", "No valid AROMA noise components remain after filtering; skipping regression for confounds.")
          } else {
            to_log(lg, "info", "Regressing {length(comp_idx)} AROMA noise components from confounds using {mode_label} mode.")
            confound_names <- colnames(confounds_to_filt)
            Y <- as.matrix(confounds_to_filt)
            Y[is.na(Y)] <- 0 # need no NAs for regression
            resid_mat <- lmfit_residuals_mat(
              Y = Y,
              X = mixing_mat,
              include_rows = rep(TRUE, nrow(mixing_mat)),
              add_intercept = FALSE,
              regress_cols = comp_idx,
              exclusive = exclusive_flag
            )
            colnames(resid_mat) <- confound_names
            confounds_to_filt <- resid_mat
            confound_nii <- mat_to_nii(confounds_to_filt, ni_out = confound_nii)
          }
        }
      }
    }

    # Temporally filter confounds, if requested (overwrites file in place)
    if ("temporal_filter" %in% processing_sequence) {
      to_log(lg, "info", "Temporally filtering confounds with low-pass cutoff {cfg$temporal_filter$low_pass_hz}, high-pass cutoff {cfg$temporal_filter$high_pass_hz}, method {cfg$temporal_filter$method}")
      confound_nii <- temporal_filter(confound_nii,
        out_file = confound_nii,
        tr = cfg$tr,
        low_pass_hz = cfg$temporal_filter$low_pass_hz,
        high_pass_hz = cfg$temporal_filter$high_pass_hz,
        overwrite = TRUE, lg = lg, fsl_img = fsl_img,
        method = cfg$temporal_filter$method
      )
    }

    filtered_confounds <- data.frame(nii_to_mat(confound_nii))
    filtered_confounds <- setNames(filtered_confounds, confound_cols)
  } else {
    to_log(lg, "info", "No confound columns were selected; downstream outputs will include only noproc columns (if any).")
    filtered_confounds <- confounds[, integer(0), drop = FALSE]
  }

  if (isTRUE(cfg$confound_calculate$enable)) {
    confile <- construct_bids_filename(
      modifyList(output_bids_info, list(suffix = "confounds", ext = ".tsv")), full.names = TRUE
    )

    calc_cols <- cfg$confound_calculate$columns
    df <- filtered_confounds[, integer(0), drop = FALSE]

    if (!is.null(calc_cols) && length(calc_cols) > 0L) {
      present_cols <- intersect(calc_cols, names(filtered_confounds))
      missing_cols <- setdiff(calc_cols, names(filtered_confounds))

      if (length(missing_cols) > 0L) {
        to_log(lg, "warn", "Requested confound_calculate columns were not available after filtering and will be skipped: {paste(missing_cols, collapse = ', ')}")
      }

      if (length(present_cols) > 0L) {
        df <- cbind(df, filtered_confounds[, present_cols, drop = FALSE])
      }
    }

    calc_noproc <- cfg$confound_calculate$noproc_columns
    has_calc_noproc <- !is.null(calc_noproc) && length(calc_noproc) > 0L && any(!is.na(calc_noproc))
    if (has_calc_noproc) {
      present_cols <- intersect(calc_noproc, names(confounds))
      missing_cols <- setdiff(calc_noproc, names(confounds))

      if (length(missing_cols) > 0L) {
        to_log(lg, "warn", "The following confound_calculate$noproc_columns were not found in the confounds file and will be ignored: {paste(missing_cols, collapse = ', ')}")
      }

      if (length(present_cols) > 0L) {
        noproc_df <- confounds[, present_cols, drop = FALSE]
        noproc_df[is.na(noproc_df)] <- 0
        df <- cbind(df, noproc_df)
      }
    }

    # only add the spike regressors from scrubbing to the postprocessed confounds if we are not scrubbing out those
    # timepoints anyhow (since then the spike regressors would be all zero)
    if (isTRUE(cfg$scrubbing$enable) && !isTRUE(cfg$scrubbing$apply) && isTRUE(cfg$scrubbing$add_to_confounds) 
      && exists("spike_mat") && !is.null(spike_mat)) {
      to_log(lg, "debug", "Adding spike_mat ({ncol(spike_mat)} columns) from scrubbing calculation to confounds file")
      df <- cbind(df, spike_mat)
    }

    if (isTRUE(cfg$confound_calculate$demean) && !is.null(calc_cols) && length(calc_cols) > 0L) {
      demean_cols <- intersect(calc_cols, names(df))
      if (length(demean_cols) > 0L) {
        df[, demean_cols] <- lapply(
          df[, demean_cols, drop = FALSE],
          function(x) x - mean(x, na.rm = TRUE)
        )
      }
    }

    if ("framewise_displacement_unfiltered" %in% names(df) &&
        !"framewise_displacement" %in% names(df)) {
      names(df)[names(df) == "framewise_displacement_unfiltered"] <- "framewise_displacement"
    }

    if (ncol(df) == 0L) {
      to_log(lg, "warn", "confound_calculate is enabled but produced zero columns; no confound file will be written.")
    } else {
      include_header <- isTRUE(cfg$confound_calculate$include_header)
      to_log(lg, "info", "Writing postprocessed confounds to: {confile}")
      to_log(lg, "info", "Columns are: {paste(names(df), collapse=', ')}")
      data.table::fwrite(df, file = confile, sep = "\t", col.names = include_header)
    }
  }

  if (isTRUE(cfg$confound_regression$enable)) {
    reg_cols <- cfg$confound_regression$columns
    df <- filtered_confounds[, integer(0), drop = FALSE]
    if (!is.null(reg_cols) && length(reg_cols) > 0L) {
      present_cols <- intersect(reg_cols, names(filtered_confounds))
      missing_cols <- setdiff(reg_cols, names(filtered_confounds))

      if (length(missing_cols) > 0L) {
        to_log(lg, "warn", "Requested confound_regression columns were not available after filtering and will be skipped: {paste(missing_cols, collapse = ', ')}")
      }

      if (length(present_cols) > 0L) {
        df <- filtered_confounds[, present_cols, drop = FALSE]
        df <- as.data.frame(lapply(df, function(cc) cc - mean(cc, na.rm = TRUE)))
      }
    }

    reg_noproc <- cfg$confound_regression$noproc_columns
    has_reg_noproc <- !is.null(reg_noproc) && length(reg_noproc) > 0L && any(!is.na(reg_noproc))
    if (has_reg_noproc) {
      present_cols <- intersect(reg_noproc, names(confounds))
      missing_cols <- setdiff(reg_noproc, names(confounds))

      if (length(missing_cols) > 0L) {
        to_log(lg, "warn", "The following confound_regression$noproc_columns were not found in the confounds file and will be ignored: {paste(missing_cols, collapse = ', ')}")
      }

      if (length(present_cols) > 0L) {
        noproc_df <- confounds[, present_cols, drop = FALSE]
        noproc_df[is.na(noproc_df)] <- 0
        df <- cbind(df, noproc_df)
      }
    }

    if (ncol(df) == 0L) {
      to_log(lg, "warn", "confound_regression is enabled but produced zero columns; no regressor file will be written.")
      to_regress <- NULL
    } else {
      to_regress <- construct_bids_filename(
        modifyList(output_bids_info, list(suffix = "regressors", ext = ".tsv")), full.names = TRUE
      )

      const_cols <- sapply(df, function(x) all(x == x[1L]))
      if (any(const_cols)) df <- df[, !const_cols, drop = FALSE]
      df <- cbind(1, df) # add intercept

      data.table::fwrite(df, file = to_regress, sep = "\t", col.names = FALSE)
    }
  } else {
    to_regress <- NULL
  }

  return(to_regress)
}

#' Expand confound column patterns
#'
#' Supports perl-compatible regular expressions and simple integer
#' range syntax using angle brackets. For example, the pattern
#' `motion_param_<1-3>` expands to `motion_param_1`,
#' `motion_param_2`, `motion_param_3`, while
#' `motion_param_<1,25>` expands to `motion_param_1` and
#' `motion_param_25`. Patterns without angle brackets are treated as
#' regular expressions evaluated against the available column names.
#' In addition, common motion-confound sets can be requested using the
#' shortcuts `"6p"`, `"12p"`, `"24p"`, and `"36p"` which expand to the
#' corresponding standard collections of parameters.
#'
#' @param patterns Character vector of column patterns.
#' @param available Character vector of available column names.
#' @details
#'   If a given pattern does not match any of the available columns, it
#'   will not appear in the expanded columns (i.e., invalid columns are dropped).
#'   If all patterns fail to match, the funciton will return `NULL`.
#' 
#' @return Character vector of expanded column names.
#' @importFrom stats na.omit
#' @keywords internal
#' @noRd
expand_confound_columns <- function(patterns = NULL, available) {
  if (is.null(patterns) || length(patterns) == 0 || all(is.na(patterns))) return(NULL) # nothing to expand
  checkmate::assert_character(patterns, all.missing = FALSE)
  patterns <- na.omit(patterns) # just in case
  checkmate::assert_character(available)

  # expand standard motion/confound sets if present
  shortcuts <- list(
    "6p" = c("rot_x", "rot_y", "rot_z", "trans_x", "trans_y", "trans_z"),
    "12p" = c(
      "rot_x", "rot_x_derivative1", "rot_y", "rot_y_derivative1",
      "rot_z", "rot_z_derivative1", "trans_x", "trans_x_derivative1",
      "trans_y", "trans_y_derivative1", "trans_z", "trans_z_derivative1"
    ),
    "24p" = c(
      "rot_x", "rot_x_derivative1", "rot_x_derivative1_power2", "rot_x_power2",
      "rot_y", "rot_y_derivative1", "rot_y_derivative1_power2", "rot_y_power2",
      "rot_z", "rot_z_derivative1", "rot_z_derivative1_power2", "rot_z_power2",
      "trans_x", "trans_x_derivative1", "trans_x_derivative1_power2", "trans_x_power2",
      "trans_y", "trans_y_derivative1", "trans_y_derivative1_power2", "trans_y_power2",
      "trans_z", "trans_z_derivative1", "trans_z_derivative1_power2", "trans_z_power2"
    ),
    "27p" = c(
      "rot_x", "rot_x_derivative1", "rot_x_derivative1_power2", "rot_x_power2",
      "rot_y", "rot_y_derivative1", "rot_y_derivative1_power2", "rot_y_power2",
      "rot_z", "rot_z_derivative1", "rot_z_derivative1_power2", "rot_z_power2",
      "trans_x", "trans_x_derivative1", "trans_x_derivative1_power2", "trans_x_power2",
      "trans_y", "trans_y_derivative1", "trans_y_derivative1_power2", "trans_y_power2",
      "trans_z", "trans_z_derivative1", "trans_z_derivative1_power2", "trans_z_power2",
      "white_matter", "csf", "global_signal"
    ),
    "36p" = c(
      "csf", "csf_derivative1", "csf_derivative1_power2", "csf_power2",
      "global_signal", "global_signal_derivative1", "global_signal_derivative1_power2",
      "global_signal_power2",
      "rot_x", "rot_x_derivative1", "rot_x_derivative1_power2", "rot_x_power2",
      "rot_y", "rot_y_derivative1", "rot_y_derivative1_power2", "rot_y_power2",
      "rot_z", "rot_z_derivative1", "rot_z_derivative1_power2", "rot_z_power2",
      "trans_x", "trans_x_derivative1", "trans_x_derivative1_power2", "trans_x_power2",
      "trans_y", "trans_y_derivative1", "trans_y_derivative1_power2", "trans_y_power2",
      "trans_z", "trans_z_derivative1", "trans_z_derivative1_power2", "trans_z_power2",
      "white_matter", "white_matter_derivative1", "white_matter_derivative1_power2",
      "white_matter_power2"
    )
  )

  lp <- tolower(patterns) # ignore case
  if (any(lp %in% names(shortcuts))) {
    expanded <- unlist(shortcuts[lp[lp %in% names(shortcuts)]], use.names = FALSE)
    # drop terms to expand, leaving remainder and forcing unique column names to avoid redundancy
    patterns <- unique(c(setdiff(lp, names(shortcuts)), expanded))
  }

  res <- unlist(lapply(patterns, function(pat) {
    if (is.na(pat) || pat == "") return(character())

    has_index <- grepl("<[^>]+>", pat)
    if (has_index) {
      pre <- sub("^([^<]*)<[^>]+>.*$", "\\1", pat)
      post <- sub("^[^<]*<[^>]+>(.*)$", "\\1", pat)
      idx_str <- sub("^[^<]*<([^>]+)>.*$", "\\1", pat)
      idx_parts <- strsplit(idx_str, ",")[[1]]
      idx <- unlist(lapply(idx_parts, function(x) {
        if (grepl("-", x)) {
          rng <- as.integer(strsplit(x, "-")[[1]])
          if (length(rng) == 2 && !any(is.na(rng))) seq(rng[1], rng[2]) else as.integer(x)
        } else {
          as.integer(x)
        }
      }))
      idx <- sprintf("%02d", idx) # fmriprep uses two-digit zero padding
      return(paste0(pre, idx, post))
    } else {
      # enforce start and end characters to avoid expanding string literals
      if (substr(pat, start = 1, 1) != "^") pat <- paste0("^", pat)
      if (substr(pat, nchar(pat), nchar(pat)) != "$") pat <- paste0(pat, "$")
      matches <- grep(pat, available, value = TRUE, perl = TRUE)
      if (length(matches) == 0L) return(NULL) # return NULL on no match to drop from output
      else return(matches)
    }
  }))

  unique(res)
}


#' Compute spike regressors for volume censoring
#'
#' Evaluates user-supplied expressions against a confounds data.frame to
#' generate spike (one-hot) regressors. Expressions may optionally include
#' a semicolon-separated range of volumes to also flag (e.g. "-1:1; framewise_displacement > 0.5").
#'
#' @param confounds_df Data frame of confounds with one row per volume.
#' @param spike_volume Character vector of expressions to evaluate.
#' @param lg Logger object for messages.
#' @return Matrix of spike regressors or NULL if none detected.
#' @keywords internal
compute_spike_regressors <- function(confounds_df = NULL, spike_volume = NULL, lg = NULL) {
  if (is.null(confounds_df) || is.null(spike_volume)) return(NULL)
  if (!checkmate::test_class(lg, "Logger")) lg <- lgr::get_logger_glue("BrainGnomes")
  checkmate::assert_character(spike_volume, null.ok = TRUE)

  spikes <- do.call(cbind, lapply(seq_along(spike_volume), function(ii) {
    has_bounds <- grepl(";", spike_volume[ii], fixed = TRUE)
    if (isTRUE(has_bounds)) {
      esplit <- strsplit(spike_volume[ii], "\\s*;\\s*", perl = TRUE)[[1L]]
      stopifnot(length(esplit) == 2L)
      spike_bounds <- as.integer(eval(parse(text = paste0("c(", esplit[1], ")"))))
      expr <- esplit[2L]
    } else {
      spike_bounds <- 0L
      expr <- spike_volume[ii]
    }

    expr_label <- if (!is.null(names(spike_volume)[ii]) && names(spike_volume)[ii] != "") {
      names(spike_volume)[ii]
    } else {
      glue::glue("expr{ii}")
    }

    spike_vec <- tryCatch(with(confounds_df, eval(parse(text = expr))), error = function(e) {
      to_log(lg, "error", "Problem evaluating spike expression: {expr}")
      return(NULL)
    })

    if (!checkmate::test_logical(spike_vec)) {
      to_log(lg, "error", "Spike expression {expr} did not return a vector of TRUE/FALSE values.")
      return(NULL)
    }

    which_spike <- which(spike_vec == TRUE)
    if (length(which_spike) == 0L) {
      to_log(lg, "debug", "Spike expression {expr_label} matched zero volumes.")
      return(NULL)
    }
    vol_txt <- paste(which_spike, collapse = ", ")
    to_log(lg, "debug", "Spike expression {expr_label} flagged volumes: {vol_txt}")

    spike_df <- do.call(cbind, lapply(which_spike, function(xx) {
      vec <- rep(0, nrow(confounds_df))
      vec[xx] <- 1
      vec
    }))
    colnames(spike_df) <- paste0("spike_", seq_len(ncol(spike_df)))

    if (!identical(spike_bounds, 0L)) {
      shifts <- spike_bounds[spike_bounds != 0L]
      res <- do.call(cbind, lapply(shifts, function(ss) {
        shift_mat <- apply(spike_df, 2, function(col) {
          if (ss < 0) {
            lead(col, abs(ss), default = 0)
          } else {
            lag(col, ss, default = 0)
          }
        })
        colnames(shift_mat) <- paste0("spike_", ifelse(ss < 0, "m", "p"), abs(ss), "_", 1:ncol(shift_mat))
        shift_mat
      }))
      spike_df <- cbind(spike_df, res)
    }

    colnames(spike_df) <- paste0(expr_label, "_", colnames(spike_df))
    spike_df
  }))

  if (is.null(spikes)) return(NULL)
  spikes <- spikes[, !duplicated(spikes, MARGIN = 2), drop = FALSE]
  spikes
}
