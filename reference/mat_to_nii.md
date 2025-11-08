# Convert a matrix to a 4D NIfTI image

Writes a numeric matrix (e.g., confound regressors) to a 4D NIfTI file
with singleton y and z dimensions, suitable for processing with FSL
tools. Each column becomes a voxel in the x dimension, and each row
corresponds to a time point (t dimension).

## Usage

``` r
mat_to_nii(mat, ni_out = "mat")
```

## Arguments

- mat:

  A numeric matrix or data frame with dimensions `time x variables`.

- ni_out:

  Output filename (without extension) for the resulting NIfTI image.

## Value

The function invisibly returns `NULL`. A NIfTI file is written to
`ni_out`.

## Details

Missing values are replaced with zero.
