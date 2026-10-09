#' Extract ROI timeseries and connectivity matrices
#'
#' Given a postprocessed BOLD NIfTI file and one or more atlas images,
#' this function summarizes the voxel time series within each ROI and
#' optionally computes ROI-to-ROI correlation matrices.
#'
#' Voxels with missing, all-zero, or constant BOLD time series are excluded.
#' This signal-validity mask is not an anatomical brain segmentation. Supply
#' `mask_file` to restrict extraction to a chosen spatial mask.
#'
#' @param bold_file Path to a 4D NIfTI file containing postprocessed BOLD data.
#' @param atlas_files Character vector of atlas NIfTI files with integer ROI labels.
#' @param out_dir Existing writable directory where output files should be
#'   written. Create it before calling this function.
#' @param log_file If not `NULL`, the log file to which details should be written.
#' @param cor_method Correlation method(s) to use when computing functional
#'   connectivity. Supported options include "pearson", "spearman",
#'   "kendall", and "cor.shrink". Use "none" to skip correlation
#'   computation. Multiple correlation methods may be supplied, but "none"
#'   must be used by itself and requires `save_ts = TRUE`. Connectivity is
#'   skipped when fewer than 20 timepoints remain after censoring.
#' @param roi_reduce Method used to summarize voxel time series within each
#'   ROI. Options are "mean" (default), "median", "pca", or "huber".
#' @param mask_file Optional path to a mask NIfTI file. Voxels outside of this mask
#'   are excluded from ROI extraction and connectivity calculation. Note that
#'   constant and zero voxels are always automatically removed by extract_rois.
#'   All positive atlas labels remain in the outputs; fully masked ROIs have
#'   all-`NA` time series and connectivity rows and columns.
#' @param min_vox_per_roi Minimum ROI size requirement. Supply a positive integer
#'   to require at least that many ROI voxels survive masking and are non-zero, or provide 
#'   a proportion (e.g., `0.8`) or percentage string (e.g., `80%`) to require that fraction of
#'   the ROI voxels to remain. ROIs failing this check are set to `NA`, preserving
#'   consistent ROI matrix size. Default: `5`.
#' @param save_ts If `TRUE`, save the ROI time series (aggregated using the
#'   `roi_reduce` method) to `_timeseries.tsv` files. Useful for running
#'   external analyses on the ROIs. Default: `TRUE`.
#' @param save_diagnostics If `TRUE`, write a per-ROI voxel-retention table to
#'   `_roidiagnostics.tsv`. The table distinguishes atlas voxels excluded by an
#'   optional spatial mask from voxels rejected because their BOLD time series
#'   are missing, zero, or constant. Default: `FALSE`.
#' @param rtoz If `TRUE`, apply Fisher's z (atanh) transformation to
#'   correlations. Untransformed correlations range from `-1` to `1`; transformed
#'   values are unbounded. Fisher transformation would map a diagonal correlation
#'   of `1` to `Inf`, so transformed output matrices use `NA` on the diagonal.
#' @param overwrite If `TRUE`, overwrite existing time-series, connectivity,
#'   or ROI-diagnostics TSV files.
#' @param allow_atlas_resampling If `TRUE`, an atlas whose spatial grid differs
#'   from the BOLD image may be resampled onto the BOLD grid using
#'   nearest-neighbour interpolation. Resampling occurs only for a verified grid
#'   mismatch, and requires the atlas coordinate space to match the BOLD
#'   filename's `space` entity. Default: `FALSE`.
#' @param atlas_space Optional fallback coordinate-space label for atlas files
#'   that do not contain a formal BIDS `space-<label>` filename entity, such as
#'   `"MNI152NLin2009cAsym"`. A filename entity is used when present; a
#'   conflicting fallback is an error. A space declaration is required only
#'   when atlas resampling is enabled and a grid mismatch is encountered.
#'   BrainGnomes does not register images between coordinate spaces.
#' 
#' @return A named list. Each element corresponds to an atlas and contains
#'   paths to the written timeseries (\code{timeseries}) and correlation
#'   matrix (\code{correlation}, or \code{NULL} if not computed), plus the
#'   voxel-retention table (\code{diagnostics}) when requested. Output ROI
#'   columns and connectivity dimensions include every positive atlas label,
#'   including labels with no usable voxels.
#'   Each written derivative also receives a portable provenance sidecar and
#'   methods/bibliography companions. Their paths are returned in `provenance`.
#' @importFrom checkmate assert_file_exists assert_character assert_directory_exists assert_flag
#' @export
extract_rois <- function(bold_file, atlas_files, out_dir, log_file = NULL,
                         cor_method = c("pearson", "spearman", "kendall", "cor.shrink"),
                         roi_reduce = c("mean", "median", "pca", "huber"),
                         mask_file = NULL, min_vox_per_roi = 5, save_ts = TRUE,
                         save_diagnostics = FALSE, rtoz = FALSE,
                         overwrite = FALSE, allow_atlas_resampling = FALSE,
                         atlas_space = NULL) {
  checkmate::assert_file_exists(bold_file)
  checkmate::assert_character(atlas_files, any.missing = FALSE, min.len = 1)
  checkmate::assert_directory_exists(out_dir, access = "w")
  if ("none" %in% cor_method) {
    if (length(cor_method) != 1L) {
      stop("'none' cannot be combined with correlation methods.", call. = FALSE)
    }
    cor_method <- NULL
  } else {
    cor_method <- match.arg(cor_method, several.ok = TRUE)
  }
  roi_reduce <- match.arg(roi_reduce)
  checkmate::assert_string(mask_file, null.ok = TRUE, na.ok = TRUE)
  if (isTRUE(is.na(mask_file[1L]))) mask_file <- NULL

  min_vox_spec <- parse_min_vox_per_roi(min_vox_per_roi)
  checkmate::assert_flag(save_ts)
  checkmate::assert_flag(save_diagnostics)
  if (is.null(cor_method) && !save_ts) {
    stop("cor_method = 'none' requires save_ts = TRUE.", call. = FALSE)
  }
  checkmate::assert_flag(rtoz)
  checkmate::assert_flag(overwrite)
  checkmate::assert_flag(allow_atlas_resampling)
  checkmate::assert_string(atlas_space, null.ok = TRUE, min.chars = 1L)

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
  bold_valid_vec <- apply(mat, 1L, function(v) {
    var_ts <- stats::var(v)
    !anyNA(v) && # no NAs
      !all(abs(v) < 2 * .Machine$double.eps) && # not all zero
      !is.na(var_ts) && # variance is defined
      var_ts > 2 * .Machine$double.eps # variance is positive
  })

  provided_mask_vec <- rep(TRUE, length(bold_valid_vec))

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
    if (length(provided_mask_vec) != length(bold_valid_vec)) {
      to_log(lg, "fatal", "Mask voxel count ({length(provided_mask_vec)}) does not match BOLD grid ({length(bold_valid_vec)})")
    }
  }

  provided_mask_vec[is.na(provided_mask_vec)] <- FALSE
  provided_mask_vec <- as.logical(provided_mask_vec)
  mask_vec <- bold_valid_vec & provided_mask_vec
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
        atlas_path <- prepare_atlas_for_extraction(
          atlas_file = atlas_path,
          bold_file = bold_file,
          allow_atlas_resampling = allow_atlas_resampling,
          atlas_space = atlas_space,
          cache_dir = file.path(out_dir, ".atlas_cache"),
          lg = lg
        )
        atlas_img <- RNifti::readNifti(atlas_path)
        out_dir_atlas <- file.path(out_dir, atlas_label)
        if (!dir.exists(out_dir_atlas)) dir.create(out_dir_atlas, recursive = TRUE)

        ts_bids <- modifyList(bids_info, list(rois = bids_camelcase(atlas_label), suffix = "timeseries", ext = ".tsv"))
        ts_file <- file.path(out_dir_atlas, construct_bids_filename(ts_bids, full.names = FALSE))
        diagnostics_file <- NULL
        if (isTRUE(save_diagnostics)) {
          diagnostics_bids <- modifyList(bids_info, list(
            rois = bids_camelcase(atlas_label), suffix = "roidiagnostics", ext = ".tsv"
          ))
          diagnostics_file <- file.path(
            out_dir_atlas,
            construct_bids_filename(diagnostics_bids, full.names = FALSE)
          )
        }

        cor_paths <- NULL
        if (compute_correlation) {
          cor_paths <- vapply(cor_method, function(cmeth) {
            cor_entity <- bids_camelcase(cmeth)
            cor_bids <- modifyList(bids_info, list(
              rois = bids_camelcase(atlas_label), correlation = cor_entity,
              suffix = "connectivity", ext = ".tsv"
            ))
            file.path(out_dir_atlas, construct_bids_filename(cor_bids, full.names = FALSE))
          }, FUN.VALUE = character(1), USE.NAMES = FALSE)

          if (anyDuplicated(cor_paths)) {
            stop("Correlation methods must produce unique output paths.", call. = FALSE)
          }
          names(cor_paths) <- cor_method
        }

        atlas_vec <- as.vector(atlas_img)

        if (!checkmate::test_integerish(atlas_vec, tol = 1e-6)) stop("Atlas ", atlas_path, " contains non-integer labels (outside tolerance).")
        roi_vals <- sort(unique(atlas_vec[atlas_vec > 0]))
        if (length(roi_vals) == 0L) {
          stop("Atlas '", atlas_path, "' contains no positive ROI labels.", call. = FALSE)
        }
        roi_names <- paste0("roi", roi_vals)

        roi_index_vec <- match(atlas_vec, roi_vals, nomatch = 0L)
        n_rois <- length(roi_vals)
        n_vox_atlas <- tabulate(roi_index_vec, nbins = n_rois)
        n_vox_in_mask <- tabulate(
          roi_index_vec[provided_mask_vec],
          nbins = n_rois
        )
        n_vox_usable <- tabulate(roi_index_vec[mask_vec], nbins = n_rois)
        min_vox_required <- vapply(n_vox_atlas, function(n_vox) {
          compute_min_vox_required(min_vox_spec, n_vox)
        }, integer(1))
        retained <- n_vox_usable >= min_vox_required
        exclusion_reason <- ifelse(
          retained,
          NA_character_,
          ifelse(
            n_vox_in_mask == 0L,
            "outside_mask",
            ifelse(n_vox_usable == 0L, "invalid_bold", "below_threshold")
          )
        )
        diagnostics <- data.frame(
          roi = roi_names,
          atlas_value = roi_vals,
          n_vox_atlas = n_vox_atlas,
          n_vox_in_mask = n_vox_in_mask,
          n_vox_usable = n_vox_usable,
          min_vox_required = min_vox_required,
          proportion_in_mask = n_vox_in_mask / n_vox_atlas,
          proportion_usable = n_vox_usable / n_vox_atlas,
          proportion_usable_in_mask = ifelse(
            n_vox_in_mask > 0L,
            n_vox_usable / n_vox_in_mask,
            NA_real_
          ),
          retained = retained,
          exclusion_reason = exclusion_reason,
          stringsAsFactors = FALSE
        )

        # Initialize only after atlas/grid validation, so invalid inputs cannot
        # leave behind apparent derivative records. Each TSV gets its own ID.
        provenance <- derivative_provenance_new(
          if (save_ts) ts_file else cor_paths[[1L]], bold_file, "ROI extraction",
          list(roi_reduce = roi_reduce, cor_method = cor_method,
               min_vox_per_roi = min_vox_per_roi, rtoz = rtoz,
               allow_atlas_resampling = allow_atlas_resampling, atlas_space = atlas_space)
        )
        derivative_provenance_sources(provenance, list(atlas = atlas, resolved_atlas = atlas_path,
                                     spatial_mask = mask_file))
        provenance$record$Resolved <- list(
          Atlas = list(
            Original = derivative_provenance_fingerprint(atlas, "atlas"),
            Resolved = derivative_provenance_fingerprint(atlas_path, "resolved_atlas"),
            SpaceDeclaration = atlas_space,
            InputSpace = bids_info$space,
            AtlasSpace = as.list(extract_bids_info(atlas))$space,
            Resampled = !identical(normalizePath(atlas), normalizePath(atlas_path)),
            Interpolation = "nearest-neighbour only when resampled; no inter-space registration",
            Labels = derivative_provenance_array(roi_vals), Geometry = derivative_provenance_geometry(atlas_path)
          ),
          SpatialMask = if (!is.null(mask_file)) derivative_provenance_fingerprint(mask_file, "mask") else NULL,
          VoxelValidity = "Exclude missing, all-zero, and constant BOLD time series",
          RegionDiagnostics = diagnostics,
          IntensityConvention = "Inherit input scale; PCA uses standardized voxel series and sign alignment to the regional mean"
        )
        derivative_provenance_operation(provenance, "roi_extraction", "running",
                      list(reduction = roi_reduce, min_vox_per_roi = min_vox_per_roi))
        provenance_files <- character()
        extraction_completed <- FALSE
        on.exit({
          if (!extraction_completed) derivative_provenance_attempt(provenance, "failed", "ROI extraction did not complete.")
        }, add = TRUE)

        if (isTRUE(save_diagnostics)) {
          reused_diagnostics <- file.exists(diagnostics_file) && !overwrite
          if (file.exists(diagnostics_file) && isFALSE(overwrite)) {
            to_log(lg, "info", "Not overwriting existing ROI diagnostics file {diagnostics_file}")
          } else {
            to_log(lg, "info", "Writing subject {sub_id} ROI voxel-retention diagnostics to {diagnostics_file}")
            data.table::fwrite(diagnostics, diagnostics_file, sep = "\t", na = "NA")
          }
          provenance_files <- c(provenance_files, derivative_provenance_roi_output(
            provenance, diagnostics_file, reused_diagnostics, "roi_diagnostics"
          ))
        }

        ts_mat <- sapply(seq_along(roi_vals), function(roi_index) {
          lbl <- roi_vals[[roi_index]]
          required_vox <- min_vox_required[[roi_index]]
          roi_idx <- which((roi_index_vec == roi_index) & mask_vec)
          if (length(roi_idx) < required_vox) {
            req_txt <- as.character(format_min_vox_requirement(
              min_vox_spec,
              n_vox_atlas[[roi_index]]
            ))
            to_log(lg, "info", "ROI {lbl} has {length(roi_idx)} usable voxels but requires {req_txt}. Marking as missing")
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
        colnames(ts_mat) <- roi_names

        ts_df <- as.data.frame(ts_mat)
        ts_df$volume <- seq_len(n_time)
        ts_df <- ts_df[, c("volume", roi_names)]
        surviving_idx <- which(colSums(!is.na(ts_mat)) > 0L)
        surviving_labels <- if (length(surviving_idx) > 0L) paste(head(roi_names[surviving_idx], 5L), collapse = ", ") else "<none>"
        to_log(lg, "debug", "Atlas {atlas_label}: retained {length(surviving_idx)} of {length(roi_vals)} ROIs after masking/min_vox (examples: {surviving_labels})")

        censor_file <- get_censor_file(bids_info)
        if (file.exists(censor_file)) {
          censor <- as.integer(readLines(censor_file))
          if (anyNA(censor) || any(!censor %in% c(0L, 1L))) {
            stop("Censor file must contain only 0 and 1 values: ", censor_file,
                 call. = FALSE)
          }
          n_kept <- sum(censor == 1L)
          if (length(censor) == n_time) {
            to_drop <- which(censor == 0L)
            if (any(to_drop)) {
              to_log(lg, "info", "Dropping volumes {paste(to_drop, collapse=', ')}")
              ts_df <- ts_df[-to_drop, , drop = FALSE]
              ts_mat <- ts_mat[-to_drop, , drop = FALSE]
            }
          } else if (n_kept == n_time) {
            to_log(
              lg, "info",
              "Censor file describes {length(censor) - n_kept} volumes already removed from the {n_time}-volume BOLD input; not applying it twice."
            )
          } else {
            stop(
              "Censor file is incompatible with the BOLD time dimension: ",
              length(censor), " censor values (", n_kept, " retained) for ",
              n_time, " BOLD volumes: ", censor_file,
              call. = FALSE
            )
          }
        }

        provenance$record$Resolved$Censor <- derivative_provenance_censor(censor_file, n_time)
        derivative_provenance_sources(provenance, list(censor = censor_file))
        provenance$record$Resolved$Censor$Application <- if (!file.exists(censor_file)) {
          "unavailable"
        } else if (length(censor) == n_time) "applied_to_input" else "already_applied_to_input"
        provenance$record$Resolved$Censor$OriginalVolumeIndices <- if (file.exists(censor_file)) {
          derivative_provenance_array(which(censor == 1L))
        } else NULL
        provenance$record$Resolved$OutputRows <- nrow(ts_mat)
        provenance$record$Resolved$InputVolumeIndices <- derivative_provenance_array(ts_df$volume)
        derivative_provenance_operation(provenance, "roi_extraction", "executed",
                      list(reduction = roi_reduce, min_vox_per_roi = min_vox_per_roi))

        if (isTRUE(save_ts)) {
          reused_timeseries <- file.exists(ts_file) && !overwrite
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
          provenance_files <- c(provenance_files, derivative_provenance_roi_output(
            provenance, ts_file, reused_timeseries
          ))
        } else {
          ts_file <- NULL
        }

        enough_timepoints <- TRUE
        if (compute_correlation && nrow(ts_mat) < 20L) {
          to_log(lg, "warn", "Only {nrow(ts_mat)} timepoints in timeseries. Cannot compute valid correlations")
          enough_timepoints <- FALSE
        }

        cor_files <- NULL
        if (enough_timepoints && compute_correlation) {
          cor_files <- lapply(cor_method, function(cmeth) {
            nacols <- which(apply(ts_mat, 2, function(col) all(is.na(col))))
            ts_use <- if (length(nacols) > 0L) ts_mat[, -nacols, drop = FALSE] else ts_mat
            no_usable_rois <- ncol(ts_use) == 0L

            if (no_usable_rois) {
              cmat <- matrix(NA_real_, ncol(ts_mat), ncol(ts_mat))
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
            dimnames(cmat) <- list(roi_names, roi_names)

            cor_file <- cor_paths[[cmeth]]
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

            if (write_file) {
              dir.create(dirname(cor_file), recursive = TRUE, showWarnings = FALSE)
              if (no_usable_rois) {
                to_log(lg, "warn", "No usable ROIs remain after filtering; writing an all-NA connectivity matrix to {cor_file}")
              }
              data.table::fwrite(
                as.data.frame.matrix(cmat),
                cor_file,
                sep = "\t",
                na = "NA"
              )
            } else if (no_usable_rois) {
              to_log(lg, "warn", "No usable ROIs remain after filtering; all-NA connectivity matrix not written because overwrite=FALSE for {cor_file}")
            }

            provenance_files <<- c(provenance_files, derivative_provenance_roi_output(
              provenance, cor_file, !write_file, "connectivity",
              list(method = cmeth, fisher_z = rtoz && !no_usable_rois,
                   all_missing = no_usable_rois, usable_regions = ncol(ts_use),
                   missing_values = if (cmeth == "cor.shrink") "corpcor::cor.shrink" else "pairwise.complete.obs")
            ))

            cor_file
          })
          names(cor_files) <- cor_method
        }

        if (!enough_timepoints && compute_correlation) {
          derivative_provenance_operation(provenance, "connectivity", "skipped",
            message = "Fewer than 20 timepoints; connectivity was not generated.")
          for (cor_file in cor_paths) {
            provenance$file <- cor_file
            provenance_files <- c(provenance_files, derivative_provenance_attempt(
              provenance, "skipped", "Fewer than 20 timepoints; connectivity was not generated."
            ))
          }
        }
        extraction_completed <- TRUE

        list(
          timeseries = ts_file,
          correlation = cor_files,
          diagnostics = diagnostics_file,
          provenance = unique(provenance_files)
        )
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
