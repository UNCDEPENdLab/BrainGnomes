# Apply run-scalar or denominator-guarded voxelwise PSC normalization

Applies either one previously estimated positive run multiplier or a
positive 3D voxelwise multiplier map. BrainGnomes estimates both after
masking and smoothing and applies them before temporal denoising.

## Usage

``` r
intensity_normalize(
  in_file,
  out_file,
  scale_factor = NULL,
  mode = "run_scalar",
  scale_file = NULL,
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
  temporal means. Required for `run_scalar`.

- mode:

  Either `"run_scalar"` (default) or `"voxel_psc"`.

- scale_file:

  Path to the 3D denominator-guarded PSC multiplier map. Reliable voxels
  use `100 / local_baseline`; floor and fallback multipliers are already
  encoded in this map. Required for `voxel_psc`.

- overwrite:

  Logical; whether to overwrite the output file if it exists.

- lg:

  Optional lgr logger used for messages.

- fsl_img:

  Optional Singularity image used to execute FSL commands.

## Value

Path to the intensity-normalized output NIfTI file.

## Details

The input is rescaled using `fslmaths -mul`; FSL broadcasts a 3D PSC map
across the 4D series. This function only applies the supplied
multiplier. "Guarded" describes how the map's denominators were bounded
or replaced during calibration; this application step does not clip
observations, apply the reference core as a mask, or re-estimate a
baseline from filtered or residualized data.
