# Apply Butterworth Filter to 4D NIfTI Image

This function applies a temporal Butterworth filter to each voxel time
series in a 4D NIfTI image using forward-backward filtering.

## Usage

``` r
butterworth_filter_cpp(
  infile,
  b,
  a,
  outfile = "",
  internal = FALSE,
  padtype = "even",
  padlen = -1L,
  use_zi = TRUE,
  demean = TRUE
)
```

## Arguments

- infile:

  Character string. Path to the input 4D NIfTI file.

- b:

  Numeric vector. Numerator filter coefficients.

- a:

  Numeric vector. Denominator filter coefficients.

- outfile:

  Character string. Optional path to save the filtered image.

- internal:

  Logical. Whether to return an internal RNifti image object (default =
  false).

- padtype:

  String. Padding type: "even", "odd", "constant", or "zero" (default =
  "even").

- use_zi:

  Logical. Whether to use steady-state initial conditions (default =
  true).

- demean:

  Logical. Whether to demean the timeseries prior to filtering. Usually
  a good to remove

## Value

A 4D filtered NIfTI image as a niftiImage or internalImage object.
