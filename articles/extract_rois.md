# Extracting ROI Timeseries and Connectivity

## Overview

The
[`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md)
function – called via `run_project` – summarizes postprocessed BOLD data
within atlas‑defined regions and can also compute ROI‑to‑ROI
connectivity matrices. It is run after postprocessing completes and
writes its outputs to the project’s `data_rois` directory. At present,
ROI extraction requires that postprocessing have configured streams
because their output names determine extraction inputs. When those
outputs already exist, run only `steps = "extract_rois"`; they do not
need to be postprocessed again in the same submission.

ROI extraction is enabled during
[`setup_project()`](https://hallquistlab.github.io/BrainGnomes/reference/setup_project.md)
or
[`edit_project()`](https://hallquistlab.github.io/BrainGnomes/reference/edit_project.md).
During the setup, you will be asked about how to reduce voxels within
ROIs to an aggregated time series and how to compute correlations among
them. Once configured,
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
will schedule extraction jobs.

Each successful scheduled extraction writes a job-specific manifest
containing the exact timeseries and connectivity files returned by
[`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md).
Status checks verify those paths and file sizes beneath the shared
`data_rois` directory. Outputs from other subjects, sessions, or
extraction streams are not used as evidence that the job completed.

## ROI extraction setup

During `setup_project`, if you enable ROI extraction, you will be asked
to create one or more extraction streams. These consist of a name (for
identifying the stream), the postprocess streams that should serve as
inputs to extraction (i.e., what files from postprocessing should be
used), and which atlases or ROI masks should be used for extraction.

Multiple postprocessing streams can serve as inputs for one extraction
stream. For example, an extraction stream can combine a resting-state
postprocessing stream whose outputs use `desc-clean` with a task stream
whose outputs use `desc-denoised`:

``` yaml
postprocess:
  rest_clean:
    input_regex: "task:rest desc:preproc suffix:bold"
    bids_desc: clean
  nback_denoised:
    input_regex: "task:nback desc:preproc suffix:bold"
    bids_desc: denoised

extract_rois:
  connectivity:
    input_streams: [rest_clean, nback_denoised]
```

BrainGnomes preserves these associations positionally: the resting-state
specification is matched only with `desc-clean`, and the n-back
specification only with `desc-denoised`. It does not search the
cross-product of all input specifications and descriptions. The
lower-level
[`get_postproc_output_files()`](https://hallquistlab.github.io/BrainGnomes/reference/get_postproc_output_files.md)
function follows the same rule when supplied equal-length vectors. A
single `bids_desc` may be applied to several input specifications; all
other length mismatches are errors.

Also, multiple atlases/ROI masks can be included in a single extraction
stream. As detailed below, the output files are named according to the
mask name. All atlas/ROI mask files should be integer-valued NIfTI files
in the same coordinate space as the postprocessed data. By default,
their complete spatial grids must also match exactly.

If an atlas is in the correct coordinate space but has a different
resolution, field of view, or grid origin, an extraction stream can opt
into nearest-neighbour resampling. For a BIDS-named atlas, BrainGnomes
infers its coordinate space from the formal `space-<label>` entity:

``` yaml
extract_rois:
  connectivity:
    input_streams: [rest_clean]
    atlases: [/path/to/space-MNI152NLin2009cAsym_atlas-Schaefer444_dseg.nii.gz]
    allow_atlas_resampling: true
```

For a legacy atlas filename without that entity, provide `atlas_space`
as a fallback:

``` yaml
extract_rois:
  connectivity:
    input_streams: [rest_clean]
    atlases: [/path/to/Schaefer_444_1mm.nii.gz]
    allow_atlas_resampling: true
    atlas_space: MNI152NLin2009cAsym
```

BrainGnomes compares the full NIfTI grid, including dimensions, voxel
sizes, spatial units, and qform/sform transforms and codes. If the atlas
already matches, it is used directly even when `allow_atlas_resampling`
is `true`. If the grid differs, a filename-derived or configured atlas
space must exactly match the BOLD filename’s `space-<label>` entity. If
both the atlas filename and `atlas_space` declare a space, they must
agree. BrainGnomes does not guess from informal fragments such as
`2009c` embedded in a legacy filename. This is resampling within a
coordinate space, not registration between spaces. The resampled atlas
must retain every positive label; otherwise extraction stops rather than
silently changing the ROI set. Validated resampled atlases are cached
under `data_rois/.atlas_cache` by atlas content and target grid, so
subjects sharing a grid reuse the same image.

## ROI reduction methods

Each atlas region contains many voxels. The `roi_reduce` argument
controls how those voxel time series are combined into a single ROI
signal:

- **mean** – averages all voxels (default).
- **median** – takes the median to reduce sensitivity to outliers.
- **pca** – extracts the first principal component and aligns its sign
  with the mean, capturing the dominant pattern of variation.
- **huber** – applies a Huber M‑estimator of location, reducing the
  influence of extreme voxel values.

### Removal of missing voxels

Note that ROI extraction removes any constant voxels (e.g., all zero)
prior to calculating the aggregated time series. Voxels containing
missing values are also excluded; this signal-validity mask is not an
anatomical brain segmentation.

### Removal of scrubbed timepoints

If scrubbing was enabled for the relevant postprocessing stream, ROI
extraction will then use the `_censor.1D` file corresponding to each
NIfTI. More specifically, any timepoints identified by the scrubbing
expression will be dropped from the timeseries outputs and functional
connectivity calculations. A censor value of `1` retains a volume and
`0` excludes it. If postprocessing already removed those volumes,
extraction recognizes the shorter BOLD series and does not remove them
twice. In that case, the `volume` column indexes the input BOLD series;
original retained indices are recorded in the provenance sidecar.

## Choosing correlation methods

Connectivity matrices are optional and are governed by the `cor_method`
argument. Multiple methods may be supplied. If you choose `'none'`, then
no connectivity calculations will be done. Select `'none'` by itself and
keep `save_ts = TRUE`; the returned `correlation` value will be `NULL`.
Supported correlation options include:

- **pearson** – standard product–moment correlation.
- **spearman** – rank‑based correlation that is robust to non‑linear but
  monotonic relationships.
- **kendall** – Kendall’s $`\tau`$ for ordinal or small sample data.
- **cor.shrink** – shrinkage estimator from the `corpcor` package,
  useful when the number of ROIs is large relative to the number of time
  points.

For scheduled extraction,
[`setup_project()`](https://hallquistlab.github.io/BrainGnomes/reference/setup_project.md)
stores the selected methods under
`extract_rois/<stream>/correlation/method`;
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
passes those nested settings to the extraction helper. The direct
[`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md)
interface uses the `cor_method` argument instead.

## Other settings

BrainGnomes can export the time series from each ROI (aggregating voxels
in the region) to a .tsv file that is volumes x rois in size. This can
be helpful if you want to run external analyses on ROI time series. If
you answer “yes” to `Output ROI time series?`, then BrainGnomes will
output timeseries files ending in `_timeseries.tsv`.

Set `save_diagnostics = TRUE` to write an additional
`_roidiagnostics.tsv` file for each atlas and input image. It contains
one row per atlas label and reports the total atlas voxels, voxels
surviving the optional spatial mask, voxels with usable BOLD time
series, the applicable minimum-voxel threshold, and whether the ROI was
retained. The `exclusion_reason` column distinguishes `outside_mask`,
`invalid_bold`, and `below_threshold`. Proportions are reported relative
to both the complete atlas ROI and, where applicable, the spatially
masked ROI. This makes a fully masked ROI distinguishable from one that
was present spatially but contained only zero, constant, or missing BOLD
signals.

The `rtoz` flag applies Fisher’s $`z`$ (aka `atanh`) transform to
correlations, producing unbounded values for subsequent analysis.
Transformed matrices use `NA` on the diagonal because `atanh(1)` is
infinite.

You can also specify the minimum number of voxels that must be present
for an ROI to be considered valid. The default is 5. If an ROI has fewer
voxels, then it will be set to NA in the timeseries and connectivity
output files. You can alternatively require a fraction of the complete
atlas ROI, such as `min_vox_per_roi = 0.8` or `"80%"`. Connectivity is
skipped when fewer than 20 timepoints remain; time-series output can
still be written.

ROI labels are determined from the complete atlas before applying the
BOLD-derived mask, an optional user-provided mask, or the minimum-voxel
requirement. Consequently, every positive atlas label remains in the
same output position for every participant. A fully masked ROI is
represented by an all-`NA` time-series column and an all-`NA`
connectivity row and column.

## Output files and naming

Outputs are organised by atlas within `data_rois/<atlas_name>/`.
Filenames use extended BIDS entities: the atlas appears as
`rois-<Atlas>` and correlation methods are labelled `cor-<method>`. For
example:

    sub-01_task-rest_desc-clean_rois-Schaefer400_timeseries.tsv
    sub-01_task-rest_desc-clean_rois-Schaefer400_cor-pearson_connectivity.tsv
    sub-01_task-rest_desc-clean_rois-Schaefer400_cor-corShrink_connectivity.tsv
    sub-01_task-rest_desc-clean_rois-Schaefer400_roidiagnostics.tsv

These naming conventions ensure that time‑series and connectivity files
can be matched to their originating runs and analysis choices.
Method-name separators are converted to camelCase to keep BIDS entities
alphanumeric; for example, the `cor.shrink` estimator is encoded as
`cor-corShrink`.

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
than the minimum number of valid voxel time series (default: 5); the
same representation is used if the ROI is completely masked.

### connectivity TSV format

Connectivity matrices are output as tab-separated files. They contain a
square correlation matrix with a header row denoting the corresponding
integer value in the ROI mask. Here is a short example:

    roi1   roi2   roi3
       1    0.7     NA
     0.7      1     NA
      NA     NA     NA

Here, every correlation involving `roi3`, including its diagonal, is NA
because its time series was NA. If every ROI is masked or fails the
minimum-voxel requirement, the connectivity file still contains the full
atlas-sized square matrix, with every value set to `NA`.

## Summary

[`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md)
provides a flexible way to derive ROI signals and functional
connectivity from postprocessed fMRI data. By selecting appropriate
reduction and correlation methods, you can tailor ROI analyses to the
needs of your study.

Direct
[`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md)
calls are useful for local analyses or custom workflows and run within
the current R session. Create a writable output directory first, and
ensure the atlas grid matches the BOLD image (or explicitly enable the
validated resampling option described above). For scheduled study
processing, use configured extraction streams through
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md).

``` r

library(BrainGnomes)
dir.create("data_rois", recursive = TRUE, showWarnings = FALSE)
extract_rois(
  bold_file = "sub-01_task-rest_desc-clean_bold.nii.gz",
  atlas_files = "Schaefer400.nii.gz",
  out_dir = "data_rois",
  roi_reduce = "mean",
  cor_method = c("pearson", "cor.shrink")
)
```
