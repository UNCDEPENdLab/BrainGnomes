# Apply a Butterworth Filter to a 4D NIfTI Image

This function performs voxelwise temporal filtering of a 4D fMRI image
using a Butterworth IIR filter (low-pass, high-pass, or bandpass).

## Usage

``` r
butterworth_filter_4d(
  infile,
  tr,
  low_hz = NULL,
  high_hz = NULL,
  outfile = "",
  internal = FALSE,
  order = 2L,
  padtype = "even",
  use_zi = TRUE,
  demean = TRUE,
  lg = NULL
)
```

## Arguments

- infile:

  Character string. Path to the input 4D NIfTI file.

- tr:

  Numeric. The repetition time (TR) in seconds.

- low_hz:

  Numeric or NULL. Low cutoff frequency in Hz for high-pass or bandpass
  filtering.

- high_hz:

  Numeric or NULL. High cutoff frequency in Hz for low-pass or bandpass
  filtering.

- outfile:

  Character string. If provided, the filtered image is written to this
  file.

- internal:

  Logical. If FALSE (default), returns a `niftiImage` object with voxel
  values; if TRUE, returns a minimal metadata internal object (see
  RNifti).

- order:

  Integer. Filter order (default = 4).

- padtype:

  Character string. Padding strategy: "even", "odd", "constant", or
  "zero". Default is "even".

- use_zi:

  Logical. Whether to use steady-state initial conditions (default =
  TRUE).

- demean:

  Logical. Whether to demean the timeseries prior to filtering. Usually
  a good to remove DC (mean) component (default = true).

- lg:

  Optional logger for status and debug messages.

## Value

A 4D NIfTI image, either written to `outfile` or returned as an object.

## Details

This function uses the `signal` package to compute IIR filter
coefficients, and then applies a zero-phase forward-backward filter to
each voxel using C++ code via Rcpp.

## Examples

``` r
if (FALSE) { # \dontrun{
butterworth_filter_4d("bold.nii.gz", tr = 2, low_hz = 0.01, high_hz = 0.1,
                       outfile = "bold_filtered.nii.gz")
} # }
```
