#' Extract ROI timeseries and connectivity matrices
#'
#' Given a postprocessed BOLD NIfTI file and one or more atlas images,
#' this function computes the mean timeseries within each ROI and
#' optionally computes ROI-to-ROI correlation matrices.
#'
#' Voxels labelled in the atlas but lying outside the brain are
#' automatically excluded by intersecting with a brain mask derived from
#' the input timeseries.
#'
#' @param bold_file Path to a 4D NIfTI file containing postprocessed BOLD data.
#' @param atlas_files Character vector of atlas NIfTI files with integer ROI labels.
#' @param out_dir Directory where output files should be written.
#' @param log_file If not `NULL`, the log file to which details should be written.
#' @param cor_method Correlation method(s) to use when computing functional
#'   connectivity. Supported options include "pearson", "spearman",
#'   "kendall", and "cor.shrink". Use "none" to skip correlation
#'   computation. Multiple methods may be supplied.
#' @param roi_reduce Method used to summarize voxel time series within each
#'   ROI. Options are "mean" (default), "median", "pca", or "huber".
#' @param mask_file Optional path to a mask NIfTI file. Voxels outside of this mask
#'   are excluded from ROI extraction and connectivity calculation. Note that
#'   constant and zero voxels are always automatically removed by extract_rois.
#' @param min_vox_per_roi Minimum ROI size requirement. Supply a positive integer
#'   to require at least that many ROI voxels survive masking and are non-zero, or provide 
#'   a proportion (e.g., `0.8`) or percentage string (e.g., `80%`) to require that fraction of
#'   the ROI voxels to remain. ROIs failing this check are set to `NA`, preserving
#'   consistent ROI matrix size. Default: `5`.
#' @param save_ts If `TRUE`, save the ROI time series (aggregated using `roi_reduce` method)
#'   to `_timeseries.tsv`. files. Useful for running external analyses on the ROIs. Default: `TRUE`.
#' @param rtoz If `TRUE`, using Fisher's z (aka atanh) transformation on correlations to make them
#'   continuous and unbounded, rather than `[0,1]`. The diagonal of the correlation matrices beccomes
#'   15 to approximate the 1.0 correlation, rather than making it `Inf`.
#' @param overwrite If `TRUE`, overwrite existing timeseries.tsv or connectivity.tsv files.
#' 
#' @return A named list. Each element corresponds to an atlas and contains
#'   paths to the written timeseries (\code{timeseries}) and correlation
#'   matrix (\code{correlation}, or \code{NULL} if not computed).
#' @importFrom checkmate assert_file_exists assert_character assert_directory_exists assert_flag
#' @export
extract_rois <- function(bold_file, atlas_files, out_dir, log_file = NULL,
                         cor_method = c("pearson", "spearman", "kendall", "cor.shrink"),
                         roi_reduce = c("mean", "median", "pca", "huber"),
                         mask_file = NULL, min_vox_per_roi = 5, save_ts = TRUE, rtoz = FALSE,
                         overwrite = FALSE) {
  checkmate::assert_file_exists(bold_file)
  checkmate::assert_character(atlas_files, any.missing = FALSE, min.len = 1)
  checkmate::assert_directory_exists(out_dir, access = "w")
  cor_method <- match.arg(cor_method, several.ok = TRUE)
  roi_reduce <- match.arg(roi_reduce)
  checkmate::assert_string(mask_file, null.ok = TRUE, na.ok = TRUE)
  if (isTRUE(is.na(mask_file[1L]))) mask_file <- NULL

  min_vox_spec <- parse_min_vox_per_roi(min_vox_per_roi)
  checkmate::assert_flag(save_ts)
  checkmate::assert_flag(rtoz)
  checkmate::assert_flag(overwrite)

  lg <- lgr::get_logger_glue("extract_rois")
  lg$config(NULL) # reset logger object to clear any appender files
  if (!is.null(log_file)) lg$add_appender(lgr::AppenderFile$new(log_file), name = "extract_logger")

  # Read 4D NIfTI
  bold_img <- RNifti::readNifti(bold_file)
  dim_img <- dim(bold_img)
  if (length(dim_img) != 4L) stop("bold_file must be a 4D NIfTI image")

  # Flatten to a voxels x time matrix -- faster and easier
  n_time <- dim_img[4]
  mat <- matrix(bold_img, prod(dim_img[1:3]), n_time)

  # Start with BOLD-derived mask that drops constant voxels or those with NAs.
  # Use the !all(zero) to screen out 0 voxels because is it faster than computing the variance
  mask_vec <- apply(mat, 1L, function(v) {
    var_ts <- stats::var(v)
    !anyNA(v) && # no NAs
      !all(abs(v) < 2 * .Machine$double.eps) && # not all zero
      !is.na(var_ts) && # variance is defined
      var_ts > 2 * .Machine$double.eps # variance is positive
  })

  # Handle user-specified mask, if provided
  if (!is.null(mask_file)) {
    if (!checkmate::test_file_exists(mask_file)) {
      to_log(lg, "fatal", "mask_file must be a valid NIfTI file or NULL")
    }

    mask_img <- RNifti::readNifti(mask_file)
    mask_dims <- dim(mask_img)
    if (length(mask_dims) > 3L) mask_dims <- mask_dims[1:3]
    if (!identical(mask_dims, dim_img[1:3])) {
      to_log(lg, "fatal", "Mask dimensions {paste(mask_dims, collapse = 'x')} do not match BOLD grid {paste(dim_img[1:3], collapse = 'x')}")
    }

    provided_mask_vec <- as.vector(mask_img > 0)
    if (length(provided_mask_vec) != length(mask_vec)) {
      to_log(lg, "fatal", "Mask voxel count ({length(provided_mask_vec)}) does not match BOLD grid ({length(mask_vec)})")
    }

    # intersect mask file with internal automask (for 0/constant voxels)
    mask_vec <- mask_vec & provided_mask_vec
  }

  mask_vec[is.na(mask_vec)] <- FALSE
  mask_vec <- as.logical(mask_vec)

  compute_correlation <- !is.null(cor_method) && length(cor_method) > 0L
  bids_info <- as.list(extract_bids_info(bold_file))
  sub_id <- bids_info$subject
  outputs <- list()

  # loop over atlases
  for (atlas in atlas_files) {
    checkmate::assert_file_exists(atlas)
    atlas_name <- sub("\\.nii(\\.gz)?$", "", basename(atlas))

    atlas_result <- run_logged(
      function(atlas_path, atlas_label) {
        atlas_img <- RNifti::readNifti(atlas_path)
        out_dir_atlas <- file.path(out_dir, atlas_label)
        if (!dir.exists(out_dir_atlas)) dir.create(out_dir_atlas, recursive = TRUE)

        ts_bids <- modifyList(bids_info, list(rois = bids_camelcase(atlas_label), suffix = "timeseries", ext = ".tsv"))
        ts_file <- file.path(out_dir_atlas, construct_bids_filename(ts_bids, full.names = FALSE))

        if (!identical(dim(atlas_img)[1:3], dim_img[1:3])) {
          to_log(lg, "fatal", "Atlas '{atlas_path}' spatial dimensions {paste(dim(atlas_img)[1:3], collapse = 'x')}\n           do not match BOLD grid {paste(dim_img[1:3], collapse = 'x')}.\n           Resample atlas or BOLD to a common grid.")
        }

        atlas_vec <- as.vector(atlas_img)

        if (!checkmate::test_integerish(atlas_vec, tol = 1e-6)) stop("Atlas ", atlas_path, " contains non-integer labels (outside tolerance).")
        roi_vals <- sort(unique(atlas_vec[atlas_vec > 0 & mask_vec]))

        ts_mat <- sapply(roi_vals, function(lbl) {
          roi_voxels <- sum(atlas_vec == lbl)
          required_vox <- compute_min_vox_required(min_vox_spec, roi_voxels)
          roi_idx <- which((atlas_vec == lbl) & mask_vec)
          if (length(roi_idx) < required_vox) {
            req_txt <- as.character(format_min_vox_requirement(min_vox_spec, roi_voxels))
            to_log(lg, "info", "ROI {lbl} has {length(roi_idx)} usable voxels but requires {req_txt}. Dropping")
            rep(NA_real_, n_time)
          } else {
            roi_vox <- t(mat[roi_idx, , drop = FALSE])
            if (roi_reduce == "pca") {
              pc <- stats::prcomp(roi_vox, scale. = TRUE)$x[, 1]
              mn <- rowMeans(roi_vox)
              if (stats::cor(pc, mn) < 0) pc <- -pc
              pc
            } else if (roi_reduce == "median") {
              apply(roi_vox, 1, median)
            } else if (roi_reduce == "huber") {
              apply(roi_vox, 1, function(x) huber(x)$mu)
            } else {
              rowMeans(roi_vox)
            }
          }
        })
        if (is.null(dim(ts_mat))) ts_mat <- matrix(ts_mat, ncol = 1L)

        ts_df <- as.data.frame(ts_mat)
        colnames(ts_df) <- paste0("roi", roi_vals)
        ts_df$volume <- seq_len(n_time)
        ts_df <- ts_df[, c("volume", paste0("roi", roi_vals))]
        surviving_idx <- which(colSums(!is.na(ts_mat)) > 0L)
        surviving_labels <- if (length(surviving_idx) > 0L) paste(head(paste0("roi", roi_vals[surviving_idx]), 5L), collapse = ", ") else "<none>"
        to_log(lg, "debug", "Atlas {atlas_label}: retained {length(surviving_idx)} of {length(roi_vals)} ROIs after masking/min_vox (examples: {surviving_labels})")

        censor_file <- get_censor_file(bids_info)
        if (file.exists(censor_file)) {
          censor <- as.integer(readLines(censor_file))
          to_drop <- which(1L - censor == 1L)
          if (any(to_drop)) {
            to_log(lg, "info", "Dropping volumes {paste(to_drop, collapse=', ')}")
            ts_df <- ts_df[-to_drop, , drop = FALSE]
            ts_mat <- ts_mat[-to_drop, , drop = FALSE]
          }
        }

        if (isTRUE(save_ts)) {
          if (file.exists(ts_file) && isFALSE(overwrite)) {
            to_log(lg, "info", "Not overwriting existing time series file {ts_file}")
          } else {
            if (file.exists(ts_file)) {
              to_log(lg, "info", "Overwriting subject {sub_id} extracted time series: {ts_file}")
            } else {
              to_log(lg, "info", "Writing subject {sub_id} extracted time series to {ts_file}")
            }
            data.table::fwrite(ts_df, ts_file, sep = "\t")
          }
        } else {
          ts_file <- NULL
        }

        enough_timepoints <- TRUE
        if (nrow(ts_mat) < 20L) {
          to_log(lg, "warn", "Only {nrow(ts_mat)} timepoints in timeseries. Cannot compute valid correlations")
          enough_timepoints <- FALSE
        }

        cor_files <- NULL
        if (enough_timepoints && compute_correlation) {
          cor_files <- lapply(cor_method, function(cmeth) {
            nacols <- which(apply(ts_mat, 2, function(col) all(is.na(col))))
            ts_use <- if (length(nacols) > 0L) ts_mat[, -nacols, drop = FALSE] else ts_mat

            if (ncol(ts_use) == 0L) {
              cmat <- matrix(NA_real_, 0, 0)
            } else {
              cmat <- if (cmeth == "cor.shrink") {
                corpcor::cor.shrink(ts_use)
              } else {
                stats::cor(ts_use, method = cmeth, use = "pairwise.complete.obs")
              }

              if (isTRUE(rtoz)) {
                to_log(lg, "debug", "Applying the Fisher z transformation to correlation coefficients.")
                cmat <- atanh(cmat)
                diag(cmat) <- NA_real_
              }

              if (length(nacols) > 0L) {
                full <- matrix(NA_real_, ncol(ts_mat), ncol(ts_mat))
                keep <- setdiff(seq_len(ncol(ts_mat)), nacols)
                full[keep, keep] <- cmat
                cmat <- full
              }
            }

            cor_bids <- modifyList(bids_info, list(
              rois = bids_camelcase(atlas_label), correlation = cmeth, suffix = "connectivity", ext = ".tsv"
            ))

            cor_file <- file.path(out_dir_atlas, construct_bids_filename(cor_bids, full.names = FALSE))
            write_file <- TRUE
            if (file.exists(cor_file)) {
              if (overwrite) {
                to_log(lg, "info", "Overwriting subject {sub_id} {cmeth} correlations to {cor_file}")
              } else {
                write_file <- FALSE
                to_log(lg, "info", "Not writing subject {sub_id} {cmeth} correlations to {cor_file} because file exists and overwrite=FALSE")
              }
            } else {
              to_log(lg, "info", "Writing subject {sub_id} {cmeth} correlations to {cor_file}")
            }

            zero_roi <- ncol(cmat) == 0L || nrow(cmat) == 0L
            if (write_file) {
              dir.create(dirname(cor_file), recursive = TRUE, showWarnings = FALSE)
              if (zero_roi) {
                to_log(lg, "warn", "No usable ROIs remain after filtering; creating an empty file at {cor_file}")
                if (file.exists(cor_file)) unlink(cor_file)
                file.create(cor_file)
              } else {
                data.table::fwrite(as.data.frame(cmat), cor_file, sep = "\t")
              }
            } else if (zero_roi) {
              to_log(lg, "warn", "No usable ROIs remain after filtering; correlations not written because overwrite=FALSE for {cor_file}")
            }

            cor_file
          })
          names(cor_files) <- cor_method
        }

        list(timeseries = ts_file, correlation = cor_files)
      },
      atlas_path = atlas,
      atlas_label = atlas_name,
      logger = lg,
      fun_label = glue::glue("extract_rois[{atlas_name}]")
    )

    outputs[[atlas_name]] <- atlas_result
  }

  return(outputs)
}

#' Huber M-estimator of Location and Scale
#' Borrowed from the `MASS` package to avoid dependency
#'
#' Computes a robust estimate of the mean (`mu`) and scale (`s`) of a numeric
#' vector using Huber's Proposal 2 (Huber, 1964). The estimator iteratively
#' down-weights values that are further than \eqn{k} times the median absolute
#' deviation (MAD) from the current location estimate, yielding resistance to
#' outliers.
#'
#' @param y A numeric vector of observations. Missing values (`NA`) are removed.
#' @param k Positive numeric tuning constant controlling the amount of
#'   winsorization. Larger values of \code{k} make the estimate closer to the
#'   arithmetic mean, while smaller values increase robustness.
#'   The default is \code{1.5}.
#' @param tol Numeric convergence tolerance for the iterative updates, expressed
#'   relative to the MAD. Defaults to \code{1e-6}.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{mu}{The robust location estimate (Huber M-estimator of mean).}
#'     \item{s}{The robust scale estimate, given by the MAD of the input sample.}
#'   }
#'
#' @details
#' The algorithm starts from the sample median and the MAD, then iteratively
#' updates the location by winsorizing values outside the interval
#' \eqn{[mu - k s, mu + k s]} until convergence within \code{tol}.
#' If the MAD is zero, the function stops with an error since a scale estimate
#' cannot be computed.
#'
#' @references
#' Huber, P. J. (1964). Robust Estimation of a Location Parameter.
#' \emph{Annals of Mathematical Statistics}, 35(1), 73–101.
#'
#' @examples
#' set.seed(123)
#' x <- c(rnorm(100), 10)  # outlier
#' huber(x)
#'
#' @seealso [stats::median], [stats::mad]
#'
#' @keywords internal
#' @noRd
#' @importFrom stats mad median
huber <- function(y, k = 1.5, tol = 1.0e-6) {
  y <- y[!is.na(y)]
  n <- length(y)
  mu <- median(y)
  s <- mad(y)
  if (s == 0) stop("cannot estimate scale: MAD is zero for this sample")
  repeat{
    yy <- pmin(pmax(mu - k * s, y), mu + k * s)
    mu1 <- sum(yy) / n
    if (abs(mu - mu1) < tol * s) break
    mu <- mu1
  }
  list(mu = mu, s = s)
}



#' Compute ROI voxel number requirement given a parsed specification
#' @param roi_voxels the number of voxels in an ROI to be potentially extracted
#' @keywords internal
#' @noRd
compute_min_vox_required <- function(spec, roi_voxels) {
  checkmate::assert_list(spec, any.missing = FALSE)
  checkmate::assert_number(roi_voxels, lower = 0, finite = TRUE)

  if (spec$type == "count") {
    req <- spec$value
  } else if (spec$type == "fraction") {
    req <- ceiling(spec$value * roi_voxels)
  } else {
    stop("Unknown specification type for min_vox_per_roi", call. = FALSE)
  }

  req <- as.integer(req)
  if (is.na(req) || req < 1L) req <- 1L
  return(req)
}

#' Internal utilities shared across ROI extraction functions.
#' @keywords internal
#' @noRd
parse_min_vox_per_roi <- function(spec) {
  if (length(spec) != 1L) {
    stop("min_vox_per_roi must be a single value", call. = FALSE)
  }

  if (is.na(spec)) {
    stop("min_vox_per_roi cannot be NA", call. = FALSE)
  }

  # Numeric input: allow integer counts or proportions in (0, 1]
  if (is.numeric(spec)) {
    if (spec >= 1 && checkmate::test_integerish(spec, tol = 1e-8)) {
      return(list(type = "count", value = as.integer(round(spec))))
    } else if (spec > 0 && spec <= 1) {
      return(list(type = "fraction", value = as.numeric(spec)))
    }
  }

  if (is.character(spec)) {
    trimmed <- trimws(spec)
    if (trimmed == "") stop("min_vox_per_roi cannot be an empty string", call. = FALSE)

    if (grepl("%$", trimmed)) {
      pct <- suppressWarnings(as.numeric(sub("%$", "", trimmed)))
      if (is.na(pct)) stop("Percentage min_vox_per_roi must contain a valid number before '%'", call. = FALSE)
      frac <- pct / 100
      if (frac <= 0 || frac > 1) stop("Percentage min_vox_per_roi must be between 0% and 100%", call. = FALSE)
      return(list(type = "fraction", value = frac))
    }

    # Handle numeric strings recursively
    num <- suppressWarnings(as.numeric(trimmed))
    if (!is.na(num)) {
      return(parse_min_vox_per_roi(num))
    }
  }

  stop("min_vox_per_roi must be a positive integer, a proportion in (0, 1], or a percentage string such as '80%'", call. = FALSE)
}



#' Format min_vox_per_roi requirement for display or storage
#' @keywords internal
#' @noRd
format_min_vox_requirement <- function(spec, roi_voxels = NULL, digits = 1) {
  checkmate::assert_list(spec, any.missing = FALSE)
  checkmate::assert_number(digits, lower = 0, finite = TRUE)

  if (spec$type == "count") {
    return(glue::glue("{spec$value} voxels"))
  }

  pct <- round(spec$value * 100, digits)
  if (is.null(roi_voxels)) {
    return(glue::glue("{pct}% of ROI voxels"))
  }

  req <- compute_min_vox_required(spec, roi_voxels)
  return(glue::glue("{pct}% of {roi_voxels} voxels (>= {req})"))
}
