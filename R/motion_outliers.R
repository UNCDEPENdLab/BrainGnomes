#' Summarize framewise displacement outliers across runs
#'
#' Calculates the percentage of framewise displacement (FD) values that exceed
#' one or more thresholds for each run (confounds file) in a project. When
#' requested, FD is recomputed after filtering the motion parameters (notch or
#' low-pass) to provide filtered outlier percentages alongside the unfiltered
#' values. This helper is intended for interactive use and is not part of the
#' postprocessing stream.
#'
#' @param scfg Optional project configuration object produced by
#'   `load_project()` or `setup_project()`. If provided, the fMRIPrep directory
#'   is taken from `scfg$metadata$fmriprep_directory`.
#' @param input_dir Optional directory to search for fMRIPrep confounds files.
#'   Ignored when `confounds_files` is provided.
#' @param confounds_files Optional character vector of confounds TSV files to
#'   summarize directly. If supplied, no directory search is performed.
#' @param thresholds Numeric vector of FD thresholds (in mm). Percentages are
#'   returned for each threshold with column names like `fd_gt_0p5`.
#' @param include_filtered Logical; if `TRUE`, recompute FD after filtering the
#'   motion parameters and include filtered outlier percentages (columns prefixed
#'   with `fd_filt_`).
#' @param filter_method Filtering strategy when `include_filtered = TRUE`.
#'   Either `"notch"` (band-stop, in breaths per minute) or `"lowpass"` (Hz).
#' @param tr Repetition time in seconds, required when `include_filtered = TRUE`.
#' @param bandstop_min_bpm Lower notch stop-band bound in breaths per minute
#'   (default 12; used when `filter_method = "notch"`).
#' @param bandstop_max_bpm Upper notch stop-band bound in breaths per minute
#'   (default 18; used when `filter_method = "notch"`).
#' @param low_pass_hz Low-pass cutoff in Hz (required for
#'   `filter_method = "lowpass"`).
#' @param filter_order Integer filter order for low-pass filtering (default 2).
#' @param motion_cols Motion parameter columns used to recompute FD.
#' @param rot_units Rotation unit for motion parameters (`"rad"` or `"deg"`).
#' @param output_file Optional path to write results as a tab-separated file.
#'   If provided, results are written using `data.table::fwrite()`.
#'
#' @return A data.frame with subject, session, task, run, confounds file location,
#'   max FD, mean FD, and outlier percentages for each threshold (filtered columns
#'   are included when requested).
#'
#' @examples
#' \dontrun{
#' scfg <- load_project("/path/to/project_config.yaml")
#' out <- calculate_motion_outliers(scfg = scfg, thresholds = c(0.3, 0.5))
#'
#' out_filt <- calculate_motion_outliers(
#'   scfg = scfg,
#'   thresholds = c(0.3, 0.5),
#'   include_filtered = TRUE,
#'   filter_method = "notch",
#'   tr = 2,
#'   bandstop_min_bpm = 18,
#'   bandstop_max_bpm = 24
#' )
#' }
#'
#' @importFrom checkmate assert_class assert_directory_exists assert_character
#' @importFrom checkmate assert_numeric assert_flag assert_number
#' @importFrom checkmate assert_integerish
#' @importFrom data.table fread fwrite
#' @export
calculate_motion_outliers <- function(scfg = NULL,
                                      input_dir = NULL,
                                      confounds_files = NULL,
                                      thresholds = 0.3,
                                      include_filtered = FALSE,
                                      filter_method = c("notch", "lowpass"),
                                      tr = NULL,
                                      bandstop_min_bpm = 12,
                                      bandstop_max_bpm = 18,
                                      low_pass_hz = NULL,
                                      filter_order = 2L,
                                      motion_cols = c("rot_x", "rot_y", "rot_z", "trans_x", "trans_y", "trans_z"),
                                      rot_units = c("rad", "deg"),
                                      output_file = NULL) {
  if (!is.null(confounds_files)) {
    checkmate::assert_character(confounds_files, min.len = 1L, any.missing = FALSE)
    missing <- confounds_files[!file.exists(confounds_files)]
    if (length(missing) > 0L) {
      stop("Confounds files not found: ", paste(missing, collapse = ", "))
    }
  } else {
    if (is.null(input_dir)) {
      if (is.null(scfg)) {
        stop("Provide one of confounds_files, input_dir, or scfg.")
      }
      checkmate::assert_class(scfg, "bg_project_cfg")
      input_dir <- scfg$metadata$fmriprep_directory
    }
    checkmate::assert_directory_exists(input_dir)
    confounds_files <- list.files(
      input_dir,
      pattern = "_desc-confounds_(timeseries|regressors)\\.tsv(\\.gz)?$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
  }

  thresholds <- unique(as.numeric(thresholds))
  checkmate::assert_numeric(thresholds, any.missing = FALSE, lower = 0, min.len = 1L)

  include_filtered <- isTRUE(include_filtered)
  checkmate::assert_flag(include_filtered)
  rot_units <- match.arg(rot_units)

  if (include_filtered) {
    filter_method <- match.arg(filter_method)
    checkmate::assert_number(tr, lower = 0.01)
    if (filter_method == "notch") {
      checkmate::assert_number(bandstop_min_bpm, lower = 0)
      checkmate::assert_number(bandstop_max_bpm, lower = 0)
      if (bandstop_max_bpm <= bandstop_min_bpm) {
        stop("bandstop_max_bpm (", bandstop_max_bpm, ") must be greater than bandstop_min_bpm (", bandstop_min_bpm, ").")
      }
    } else {
      checkmate::assert_number(low_pass_hz, lower = 0.001)
      checkmate::assert_integerish(filter_order, len = 1L, lower = 2L)
    }
  }

  format_threshold <- function(x) {
    label <- format(x, trim = TRUE, scientific = FALSE)
    label <- gsub("\\.", "p", label)
    label
  }

  calc_pct <- function(x, thr) {
    n_ok <- sum(!is.na(x))
    if (n_ok == 0L) return(NA_real_)
    mean(x > thr, na.rm = TRUE) * 100
  }

  calc_max <- function(x) {
    if (length(x) == 0L || all(is.na(x))) return(NA_real_)
    max(x, na.rm = TRUE)
  }

  calc_mean <- function(x) {
    if (length(x) == 0L || all(is.na(x))) return(NA_real_)
    mean(x, na.rm = TRUE)
  }

  empty_out <- function() {
    base <- data.frame(
      subject = character(),
      session = character(),
      task = character(),
      run = character(),
      confounds_file = character(),
      fd_max = numeric(),
      fd_mean = numeric(),
      stringsAsFactors = FALSE
    )
    labels <- vapply(thresholds, format_threshold, character(1))
    for (lbl in labels) {
      base[[paste0("fd_gt_", lbl)]] <- numeric()
    }
    if (include_filtered) {
      base[["fd_filt_max"]] <- numeric()
      base[["fd_filt_mean"]] <- numeric()
      for (lbl in labels) {
        base[[paste0("fd_filt_gt_", lbl)]] <- numeric()
      }
    }
    base
  }

  confounds_files <- sort(unique(confounds_files))
  if (length(confounds_files) == 0L) {
    warning("No confounds files found to summarize.", call. = FALSE)
    return(empty_out())
  }

  labels <- vapply(thresholds, format_threshold, character(1))
  res <- lapply(confounds_files, function(cf) {
    confounds <- data.table::fread(cf, showProgress = FALSE, na.strings = c("n/a", "NA", "NaN"), data.table = FALSE)

    bids_info <- as.list(extract_bids_info(cf))
    motion_ok <- all(motion_cols %in% names(confounds))
    fd <- NULL
    if ("framewise_displacement" %in% names(confounds)) {
      fd <- suppressWarnings(as.numeric(confounds$framewise_displacement))
    }

    if (is.null(fd) || length(fd) == 0L || (motion_ok && all(is.na(fd)))) {
      if (motion_ok) {
        fd <- framewise_displacement(
          motion = confounds[, motion_cols, drop = FALSE],
          columns = motion_cols,
          rot_units = rot_units
        )
      } else {
        fd <- rep(NA_real_, nrow(confounds))
      }
    }

    row <- list(
      subject = bids_info$subject,
      session = bids_info$session,
      task = bids_info$task,
      run = bids_info$run,
      confounds_file = cf,
      fd_max = calc_max(fd),
      fd_mean = calc_mean(fd)
    )
    for (ii in seq_along(thresholds)) {
      row[[paste0("fd_gt_", labels[[ii]])]] <- calc_pct(fd, thresholds[[ii]])
    }

    if (include_filtered) {
      if (!motion_ok) {
        filtered_fd <- rep(NA_real_, length(fd))
      } else {
        filtered <- filter_confounds(
          confounds_df = confounds,
          tr = tr,
          filter_type = filter_method,
          bandstop_min_bpm = bandstop_min_bpm,
          bandstop_max_bpm = bandstop_max_bpm,
          low_pass_hz = low_pass_hz,
          filter_order = filter_order,
          columns = motion_cols,
          add_poly = FALSE,
          out_file = NULL,
          padtype = "constant",
          padlen = NULL,
          use_zi = TRUE,
          lg = NULL
        )
        filtered_fd <- framewise_displacement(
          motion = filtered[, motion_cols, drop = FALSE],
          columns = motion_cols,
          rot_units = rot_units
        )
      }

      row[["fd_filt_max"]] <- calc_max(filtered_fd)
      row[["fd_filt_mean"]] <- calc_mean(filtered_fd)
      for (ii in seq_along(thresholds)) {
        row[[paste0("fd_filt_gt_", labels[[ii]])]] <- calc_pct(filtered_fd, thresholds[[ii]])
      }
    }

    as.data.frame(row, stringsAsFactors = FALSE)
  })

  result <- do.call(rbind, res)

  if (!is.null(output_file)) {
    checkmate::assert_string(output_file)
    out_dir <- dirname(output_file)
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }
    # Handle .csv, .tsv, .csv.gz, .tsv.gz extensions
    base_lower <- tolower(output_file)
    sep <- if (grepl("\\.csv(\\.gz)?$", base_lower)) "," else "\t"
    data.table::fwrite(result, file = output_file, sep = sep, compress = "auto")
  }

  result
}
