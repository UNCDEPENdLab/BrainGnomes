# Convert a 4D NIfTI image to a matrix

Reads a 4D NIfTI file (with singleton y and z dimensions) and converts
it to a matrix with dimensions `time x variables`. This is the inverse
of
[`mat_to_nii()`](https://uncdependlab.github.io/BrainGnomes/reference/mat_to_nii.md).

## Usage

``` r
nii_to_mat(ni_in)
```

## Arguments

- ni_in:

  Path to a NIfTI file where the x dimension encodes variables and the
  4th (time) dimension encodes observations.

## Value

A numeric matrix of dimension `time x variables`.

## Details

Assumes the input image has shape `[x, 1, 1, time]` as produced by
[`mat_to_nii()`](https://uncdependlab.github.io/BrainGnomes/reference/mat_to_nii.md).
