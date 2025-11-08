# Apply a brain mask to a 4D NIfTI image

Multiplies a NIfTI image by a binary mask using FSL's `fslmaths -mas` to
zero out non-brain voxels. This is typically used to restrict processing
to brain tissue.

## Usage

``` r
apply_mask(
  in_file,
  mask_file,
  out_file,
  overwrite = FALSE,
  lg = NULL,
  fsl_img = NULL
)
```

## Arguments

- in_file:

  Path to the input 4D NIfTI image.

- mask_file:

  Path to a binary mask NIfTI file (same dimensions as `in_file`).

- out_file:

  The full path for the file output by this step

- overwrite:

  Logical; whether to overwrite the output file if it already exists.

- lg:

  Optional lgr object used for logging messages

- fsl_img:

  Optional path to a Singularity image to execute the command in a
  container.

## Value

Path to the masked output NIfTI file.
