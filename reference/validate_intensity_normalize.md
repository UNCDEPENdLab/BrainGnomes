# Validate intensity normalization (global median in mask)

Global median of in-mask values vs `target` within `tolerance`.

## Usage

``` r
validate_intensity_normalize(data_file, mask_file, target, tolerance = 0)
```

## Arguments

- data_file:

  4D NIfTI after `intensity_normalize`.

- mask_file:

  3D mask.

- target:

  Target median (`cfg$intensity_normalize$global_median`).

- tolerance:

  Absolute tolerance on `abs(median - target)`.

## Value

A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
Attributes: `message`, `details` (`global_median`, `target`,
`abs_diff`).
