# Extract ROI timeseries and connectivity matrices

Given a postprocessed BOLD NIfTI file and one or more atlas images, this
function summarizes the voxel time series within each ROI and optionally
computes ROI-to-ROI correlation matrices.

## Usage

``` r
extract_rois(
  bold_file,
  atlas_files,
  out_dir,
  log_file = NULL,
  cor_method = c("pearson", "spearman", "kendall", "cor.shrink"),
  roi_reduce = c("mean", "median", "pca", "huber"),
  mask_file = NULL,
  min_vox_per_roi = 5,
  save_ts = TRUE,
  save_diagnostics = FALSE,
  rtoz = FALSE,
  overwrite = FALSE,
  allow_atlas_resampling = FALSE,
  atlas_space = NULL
)
```

## Arguments

- bold_file:

  Path to a 4D NIfTI file containing postprocessed BOLD data.

- atlas_files:

  Character vector of atlas NIfTI files with integer ROI labels.

- out_dir:

  Existing writable directory where output files should be written.
  Create it before calling this function.

- log_file:

  If not `NULL`, the log file to which details should be written.

- cor_method:

  Correlation method(s) to use when computing functional connectivity.
  Supported options include "pearson", "spearman", "kendall", and
  "cor.shrink". Use "none" to skip correlation computation. Multiple
  correlation methods may be supplied, but "none" must be used by itself
  and requires `save_ts = TRUE`. Connectivity is skipped when fewer than
  20 timepoints remain after censoring.

- roi_reduce:

  Method used to summarize voxel time series within each ROI. Options
  are "mean" (default), "median", "pca", or "huber".

- mask_file:

  Optional path to a mask NIfTI file. Voxels outside of this mask are
  excluded from ROI extraction and connectivity calculation. Note that
  constant and zero voxels are always automatically removed by
  extract_rois. All positive atlas labels remain in the outputs; fully
  masked ROIs have all-`NA` time series and connectivity rows and
  columns.

- min_vox_per_roi:

  Minimum ROI size requirement. Supply a positive integer to require at
  least that many ROI voxels survive masking and are non-zero, or
  provide a proportion (e.g., `0.8`) or percentage string (e.g., `80%`)
  to require that fraction of the ROI voxels to remain. ROIs failing
  this check are set to `NA`, preserving consistent ROI matrix size.
  Default: `5`.

- save_ts:

  If `TRUE`, save the ROI time series (aggregated using the `roi_reduce`
  method) to `_timeseries.tsv` files. Useful for running external
  analyses on the ROIs. Default: `TRUE`.

- save_diagnostics:

  If `TRUE`, write a per-ROI voxel-retention table to
  `_roidiagnostics.tsv`. The table distinguishes atlas voxels excluded
  by an optional spatial mask from voxels rejected because their BOLD
  time series are missing, zero, or constant. Default: `FALSE`.

- rtoz:

  If `TRUE`, apply Fisher's z (atanh) transformation to correlations.
  Untransformed correlations range from `-1` to `1`; transformed values
  are unbounded. Fisher transformation would map a diagonal correlation
  of `1` to `Inf`, so transformed output matrices use `NA` on the
  diagonal.

- overwrite:

  If `TRUE`, overwrite existing time-series, connectivity, or
  ROI-diagnostics TSV files.

- allow_atlas_resampling:

  If `TRUE`, an atlas whose spatial grid differs from the BOLD image may
  be resampled onto the BOLD grid using nearest-neighbour interpolation.
  Resampling occurs only for a verified grid mismatch, and requires the
  atlas coordinate space to match the BOLD filename's `space` entity.
  Default: `FALSE`.

- atlas_space:

  Optional fallback coordinate-space label for atlas files that do not
  contain a formal BIDS `space-<label>` filename entity, such as
  `"MNI152NLin2009cAsym"`. A filename entity is used when present; a
  conflicting fallback is an error. A space declaration is required only
  when atlas resampling is enabled and a grid mismatch is encountered.
  BrainGnomes does not register images between coordinate spaces.

## Value

A named list. Each element corresponds to an atlas and contains paths to
the written timeseries (`timeseries`) and correlation matrix
(`correlation`, or `NULL` if not computed), plus the voxel-retention
table (`diagnostics`) when requested. Output ROI columns and
connectivity dimensions include every positive atlas label, including
labels with no usable voxels. Each written derivative also receives a
portable provenance sidecar and methods/bibliography companions. Their
paths are returned in `provenance`.

## Details

Voxels with missing, all-zero, or constant BOLD time series are
excluded. This signal-validity mask is not an anatomical brain
segmentation. Supply `mask_file` to restrict extraction to a chosen
spatial mask.
