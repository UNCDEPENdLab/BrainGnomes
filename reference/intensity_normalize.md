# Apply a run-wise intensity multiplier to a 4D fMRI image

Multiplies every voxel and volume by one previously estimated positive
constant. BrainGnomes estimates this multiplier after masking and
smoothing and applies it before temporal denoising.

## Usage

``` r
intensity_normalize(
  in_file,
  out_file,
  scale_factor,
  overwrite = FALSE,
  lg = NULL,
  fsl_img = NULL
)
```

## Arguments

- in_file:

  Path to the input 4D NIfTI file.

- out_file:

  Full path for the intensity-normalized output file.

- scale_factor:

  Finite positive multiplier calculated as `target / L`, where `L` is
  the spatial median across reference voxels of their 10%-trimmed
  temporal means.

- overwrite:

  Logical; whether to overwrite the output file if it exists.

- lg:

  Optional lgr logger used for messages.

- fsl_img:

  Optional Singularity image used to execute FSL commands.

## Value

Path to the intensity-normalized output NIfTI file.

## Details

The input is rescaled using `fslmaths -mul`. This function only applies
the supplied multiplier; reference selection and estimation occur
upstream. The multiplier is not clipped, replaced, or re-estimated from
filtered or residualized data.
