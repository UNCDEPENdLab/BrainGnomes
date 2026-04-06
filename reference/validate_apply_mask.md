# Validate that a brain mask was correctly applied to 4D fMRI data

Checks that voxels outside the mask are zero (no signal leakage) and
optionally reports voxels inside the mask that are all zero (potentially
problematic).

## Usage

``` r
validate_apply_mask(mask_file, data_file)
```

## Arguments

- mask_file:

  Path to the binary mask NIfTI file (1s = brain, 0s = non-brain).

- data_file:

  Path to the masked 4D fMRI data file (after `apply_mask` was run).

## Value

A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
Attributes:

- `message`: Character string describing the validation result.

- `external_violations`: Integer count of voxels outside mask with
  non-zero signal.

- `internal_zeros`: Integer count of voxels inside mask that are all
  zero.

## Details

This function verifies that the masking step was applied correctly by
checking:

- External violations: voxels where mask == 0 (outside brain) but data
  has non-zero signal.

- Internal zeros: voxels where mask \> 0 (inside brain) but all
  timepoints are zero.

Validation passes if `external_violations == 0`. Internal zeros are
reported but do not cause validation to fail.
