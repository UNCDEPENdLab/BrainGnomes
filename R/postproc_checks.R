### Postprocessing validation functions

#' Validate that a brain mask was correctly applied to 4D fMRI data
#'
#' Checks that voxels outside the mask are zero (no signal leakage) and
#' optionally reports voxels inside the mask that are all zero (potentially
#' problematic).
#'
#' @param mask_file Path to the binary mask NIfTI file (1s = brain, 0s = non-brain).
#' @param data_file Path to the masked 4D fMRI data file (after `apply_mask` was run).
#'
#' @return A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
#'   Attributes:
#'   - `message`: Character string describing the validation result.
#'   - `external_violations`: Integer count of voxels outside mask with non-zero signal.
#'   - `internal_zeros`: Integer count of voxels inside mask that are all zero.
#'
#' @details
#' This function verifies that the masking step was applied correctly by checking:
#' - External violations: voxels where mask == 0 (outside brain) but data has non-zero signal.
#' - Internal zeros: voxels where mask > 0 (inside brain) but all timepoints are zero.
#'
#' Validation passes if `external_violations == 0`. Internal zeros are reported
#' but do not cause validation to fail.
#'
#' @keywords internal
#' @importFrom RNifti readNifti
#' @importFrom matrixStats rowMins
#' @importFrom dplyr mutate case_when
validate_apply_mask <- function(mask_file, data_file) {
  checkmate::assert_file_exists(mask_file)
  checkmate::assert_file_exists(data_file)

  mask <- RNifti::readNifti(mask_file)
  data4d <- RNifti::readNifti(data_file)

  img_dims <- dim(data4d)
  n_vox <- prod(img_dims[1:3])
  n_t <- img_dims[4]

  # reshape 4D time series into voxels x time matrix
  img_matrix <- array(data4d, dim = c(n_vox, n_t))
  min_vec <- matrixStats::rowMins(img_matrix) # minimum across time for each voxel

  mask_vec <- as.vector(mask)

  mask_result <- cbind(mask_vec, min_vec)
  mask_result_df <- as.data.frame(mask_result)

  mask_result_df <- mask_result_df %>%
    dplyr::mutate(
      externalposvox = dplyr::case_when(
        .$mask_vec > 0 ~ 0L,
        .$mask_vec == 0 & min_vec != 0 ~ 1L,
        .$mask_vec == 0 & min_vec == 0 ~ 0L
      ),
      internalzerovox = dplyr::case_when(
        .$mask_vec == 0 ~ 0L,
        .$mask_vec > 0 & min_vec == 0 ~ 1L,
        .$mask_vec > 0 & min_vec > 0 ~ 0L
      )
    )

  external_violations <- sum(mask_result_df$externalposvox)
  internal_zeros <- sum(mask_result_df$internalzerovox)

  passed <- external_violations == 0L

  msg <- paste0(
    internal_zeros, " voxels = 0 inside the mask. ",
    external_violations, " voxels > 0 outside the mask."
  )

  result <- passed
  attr(result, "message") <- msg
  attr(result, "external_violations") <- external_violations
  attr(result, "internal_zeros") <- internal_zeros

  return(result)
}


