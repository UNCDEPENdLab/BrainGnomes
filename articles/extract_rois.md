# Extracting ROI Timeseries and Connectivity

## Overview

The
[`extract_rois()`](https://uncdependlab.github.io/BrainGnomes/reference/extract_rois.md)
function – called via `run_project` – summarizes postprocessed BOLD data
within atlas‑defined regions and can also compute ROI‑to‑ROI
connectivity matrices. It is run after postprocessing completes and
writes its outputs to the project’s `data_rois` directory. At present,
ROI extraction requires that postprocessing be included in the pipeline
because postprocessed files serve as the inputs to ROI extraction.

ROI extraction is enabled during
[`setup_project()`](https://uncdependlab.github.io/BrainGnomes/reference/setup_project.md)
or
[`edit_project()`](https://uncdependlab.github.io/BrainGnomes/reference/edit_project.md).
During the setup, you will be asked about how to reduce voxels within
ROIs to an aggregated time series and how to compute correlations among
them. Once configured,
[`run_project()`](https://uncdependlab.github.io/BrainGnomes/reference/run_project.md)
will schedule extraction jobs.

## ROI extraction setup

During `setup_project`, if you enable ROI extraction, you will be asked
to create one or more extraction streams. These consist of a name (for
identifying the stream), the postprocess streams that should serve as
inputs to extraction (i.e., what files from postprocessing should be
used), and which atlases or ROI masks should be used for extraction.

Note that multiple postprocessing streams can serve as inputs for an
extraction stream.

Also, multiple atlases/ROI masks can be included in a single extraction
stream. As detailed belwo, the output files are named according to the
mask name. All atlas/ROI mask files should be integer-valued NIfTI files
having the same stereotaxic space and spatial resolution as the
postprocessed data. BrainGnomes performs simple checks on these files,
but please check these yourself, too!

## ROI reduction methods

Each atlas region contains many voxels. The `roi_reduce` argument
controls how those voxel time series are combined into a single ROI
signal:

- **mean** – averages all voxels (default).
- **median** – takes the median to reduce sensitivity to outliers.
- **pca** – extracts the first principal component and aligns its sign
  with the mean, capturing the dominant pattern of variation.
- **huber** – applies a Huber M‑estimator for a robust trimmed mean.

### Removal of missing voxels

Note that ROI extraction removes any constant voxels (e.g., all zero)
prior to calculating the aggregated time series.

### Removal of scrubbed timepoints

If scrubbing was enabled for the relevant postprocessing stream, ROI
extraction will then use the `_censor.1D` file corresponding to each
NIfTI. More specifically, any timepoints identified by the scrubbing
expression will be dropped from the timeseries outputs and functional
connectivity calculations.

## Choosing correlation methods

Connectivity matrices are optional and are governed by the `cor_method`
argument. Multiple methods may be supplied. If you choose `'none'`, then
no connectivity calculations will be done. Supported options include:

- **pearson** – standard product–moment correlation.
- **spearman** – rank‑based correlation that is robust to non‑linear but
  monotonic relationships.
- **kendall** – Kendall’s $\tau$ for ordinal or small sample data.
- **cor.shrink** – shrinkage estimator from the `corpcor` package,
  useful when the number of ROIs is large relative to the number of time
  points.

## Other settings

BrainGnomes can export the time series from each ROI (aggregating voxels
in the region) to a .tsv file that is volumes x rois in size. This can
be helpful if you want to run external analyses on ROI time series. If
you answer “yes” to `Output ROI time series?`, then BrainGnomes will
output timeseries files ending in `_timeseries.tsv`.

The `rtoz` flag applies Fisher’s $z$ (aka `atanh`) transform to
correlations, producing unbounded values better suited for group
analysis.

You can also specify the minimum number of voxels that must be present
for an ROI to be considered valid. The default is 5. If an ROI has fewer
voxels, then it will be set to NA in the timeseries and connectivity
output files.

## Output files and naming

Outputs are organised by atlas within `data_rois/<atlas_name>/`.
Filenames use extended BIDS entities: the atlas appears as
`rois-<Atlas>` and correlation methods are labelled `cor-<method>`. For
example:

    sub-01_task-rest_desc-clean_rois-Schaefer400_timeseries.tsv
    sub-01_task-rest_desc-clean_rois-Schaefer400_cor-pearson_connectivity.tsv

These naming conventions ensure that time‑series and connectivity files
can be matched to their originating runs and analysis choices.

### Note about atlas naming

For a BIDS ‘entity’ to be valid, it must not contain underscores or
hyphens since these serve as field delimiters in BIDS. Consequently, if
your atlas/ROI file contains hyphens or underscores, these will be
removed and the next character will be capitalized (i.e., camel case).

For example, an atlas `schaefer_444_resampled.nii.gz` will become
`rois-schaefer444Resampled` in ROI outputs.

### timeseries TSV format

Columns in timeseries files are tab-separated. A header row is included,
and `volume` is one of the variables. The other variables are named
`roi<xx>` according to their integer value in the ROI mask. Here is a
short example of such a file:

    volume  roi1  roi2  roi3
         1  10.2  15.2    NA
         2  10.5  15.1    NA
         3  11.1  15.0    NA
         6  10.4  15.1    NA
         7  10.2  15.5    NA

Note how volume skips from 3 to 6. This reflects that volumes 4 and 5
were scrubbed from the output. `roi3` is all NA because it had fewer
than the minimum number of valid voxel time series (default: 5).

### connectivity TSV format

Connectivity matrices are output as tab-separated files. They contain a
square correlation matrix with a header row denoting the corresponding
integer value in the ROI mask. Here is a short example:

    roi1   roi2   roi3
       1    0.7     NA
     0.7      1     NA
      NA     NA      1

Here, the correlations of `roi3` with the other ROIs are NA because the
time series was NA.

## Summary

[`extract_rois()`](https://uncdependlab.github.io/BrainGnomes/reference/extract_rois.md)
provides a flexible way to derive ROI signals and functional
connectivity from postprocessed fMRI data. By selecting appropriate
reduction and correlation methods, you can tailor ROI analyses to the
needs of your study.

*Note*: You can technically call
[`extract_rois()`](https://uncdependlab.github.io/BrainGnomes/reference/extract_rois.md)
directly, but this is recommended only for testing because it runs the
compute directly within the R session, rather than scheduling jobs on
the HPC.

``` r
library(BrainGnomes)
extract_rois(
  bold_file = "sub-01_task-rest_desc-clean_bold.nii.gz",
  atlas_files = "Schaefer400.nii.gz",
  out_dir = "data_rois",
  roi_reduce = "mean",
  cor_method = c("pearson", "cor.shrink")
)
```
