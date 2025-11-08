# Extract ROI timeseries and connectivity matrices

Given a postprocessed BOLD NIfTI file and one or more atlas images, this
function computes the mean timeseries within each ROI and optionally
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
  rtoz = FALSE,
  overwrite = FALSE
)
```

## Arguments

- bold_file:

  Path to a 4D NIfTI file containing postprocessed BOLD data.

- atlas_files:

  Character vector of atlas NIfTI files with integer ROI labels.

- out_dir:

  Directory where output files should be written.

- log_file:

  If not `NULL`, the log file to which details should be written.

- cor_method:

  Correlation method(s) to use when computing functional connectivity.
  Supported options include "pearson", "spearman", "kendall", and
  "cor.shrink". Use "none" to skip correlation computation. Multiple
  methods may be supplied.

- roi_reduce:

  Method used to summarize voxel time series within each ROI. Options
  are "mean" (default), "median", "pca", or "huber".

- mask_file:

  Optional path to a mask NIfTI file. Voxels outside of this mask are
  excluded from ROI extraction and connectivity calculation. Note that
  constant and zero voxels are always automatically removed by
  extract_rois.

- min_vox_per_roi:

  Minimum ROI size requirement. Supply a positive integer to require at
  least that many ROI voxels survive masking and are non-zero, or
  provide a proportion (e.g., `0.8`) or percentage string (e.g., `80%`)
  to require that fraction of the ROI voxels to remain. ROIs failing
  this check are set to `NA`, preserving consistent ROI matrix size.
  Default: `5`.

- save_ts:

  If `TRUE`, save the ROI time series (aggregated using `roi_reduce`
  method) to `_timeseries.tsv`. files. Useful for running external
  analyses on the ROIs. Default: `TRUE`.

- rtoz:

  If `TRUE`, using Fisher's z (aka atanh) transformation on correlations
  to make them continuous and unbounded, rather than `[0,1]`. The
  diagonal of the correlation matrices beccomes 15 to approximate the
  1.0 correlation, rather than making it `Inf`.

- overwrite:

  If `TRUE`, overwrite existing timeseries.tsv or connectivity.tsv
  files.

## Value

A named list. Each element corresponds to an atlas and contains paths to
the written timeseries (`timeseries`) and correlation matrix
(`correlation`, or `NULL` if not computed).

## Details

Voxels labelled in the atlas but lying outside the brain are
automatically excluded by intersecting with a brain mask derived from
the input timeseries.
