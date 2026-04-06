# Validate temporal filtering (multitaper pre vs post)

Band power outside / inside the passband; needs `multitaper` and
`signal`.

## Usage

``` r
validate_temporal_filter(
  pre_file,
  post_file,
  tr,
  band_low_hz = NA_real_,
  band_high_hz = NA_real_,
  mask_file = NULL,
  n_voxels = 30L,
  passband_loss_fail_db = 3
)
```

## Arguments

- pre_file:

  Path to 4D BOLD before `temporal_filter`.

- post_file:

  Path to 4D BOLD after `temporal_filter`.

- tr:

  TR in seconds (`cfg$tr`).

- band_low_hz:

  Lower passband edge (Hz); `NA` if open.

- band_high_hz:

  Upper passband edge (Hz); `NA` if open.

- mask_file:

  Optional 3D mask; if unset, sample the whole volume.

- n_voxels:

  How many voxels to use.

- passband_loss_fail_db:

  Max allowed passband loss (dB) before fail (default 3).

## Value

A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
Attributes: `message`, `details` (numeric summaries and flags).
