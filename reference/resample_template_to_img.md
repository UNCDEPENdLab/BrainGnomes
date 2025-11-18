# Resample TemplateFlow Mask to fMRIPrep Image Using Python

This function uses Python (via `reticulate`) to identify and resample a
TemplateFlow mask to match the resolution and spatial dimensions of an
fMRIPrep BOLD image.

## Usage

``` r
resample_template_to_img(
  in_file,
  output = NULL,
  template_resolution = 1,
  suffix = "mask",
  desc = "brain",
  extension = ".nii.gz",
  interpolation = "nearest",
  install_dependencies = TRUE,
  overwrite = FALSE,
  lg = NULL
)
```

## Arguments

- in_file:

  Path to the BIDS-compliant NIfTI file (e.g., an fMRIPrep preprocessed
  BOLD image).

- output:

  Optional path to write the resampled image. If NULL, a BIDS-style
  filename is constructed.

- template_resolution:

  Integer specifying the TemplateFlow resolution index (e.g., 1 = 1mm).

- suffix:

  TemplateFlow suffix (e.g., "mask", "T1w").

- desc:

  TemplateFlow descriptor (e.g., "brain").

- extension:

  File extension for the template image (default is ".nii.gz").

- interpolation:

  Interpolation method to use during resampling. Options are "nearest",
  "linear", or "continuous".

- install_dependencies:

  Logical. If `TRUE` (default), attempts to automatically install
  required Python packages (nibabel, nilearn, templateflow) if they are
  missing from the active environment. If `FALSE`, the function will
  raise an error if dependencies are not found.

- overwrite:

  Logical. If `TRUE`, overwrite the existing output file (if present).

- lg:

  Optional lgr logger for emitting warnings/info to the postprocess log.

## Value

Invisibly returns `TRUE` on success. A new NIfTI file is written to
`output`.

## Details

The appropriate template is inferred from the `space-` entity of the
BIDS-formatted input filename. For example, an input such as:
`sub-221256_task-trust_run-1_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz`
will lead to selection of the MNI152NLin2009cAsym template.

This function depends on a companion Python script
(`fetch_matched_template_image.py`) that is bundled with the BrainGnomes
package and sourced at runtime.
