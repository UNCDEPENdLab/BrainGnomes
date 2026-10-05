# Motion Quality Control and Framewise Displacement Summaries

## Overview

Framewise displacement (FD) reduces the change in six rigid-body motion
parameters between consecutive volumes to one displacement value in
millimetres.
[`calculate_motion_outliers()`](https://hallquistlab.github.io/BrainGnomes/reference/calculate_motion_outliers.md)
summarizes an fMRIPrep confounds file as one row per BOLD run, including
maximum FD, mean FD, and the percentage of volumes above one or more
user-supplied thresholds.

This is an interactive quality-control helper. It reads confounds files
and can write a run-level summary table, but it does **not** submit
jobs, change a project configuration, create censor files, or alter BOLD
data. Later sections explain how its summaries can inform a separately
configured scrubbing policy.

## Choose the confounds files

There are three ways to select input files:

``` r

# Search the fMRIPrep directory recorded in a project configuration.
scfg <- load_project("/project/my_study")
qc <- calculate_motion_outliers(scfg = scfg)

# Search a directory recursively.
qc <- calculate_motion_outliers(input_dir = "/project/my_study/data_fmriprep")

# Summarize an explicit set of files.
qc <- calculate_motion_outliers(confounds_files = c(file1, file2))
```

An explicit `confounds_files` vector takes precedence. Directory
searches match fMRIPrep files ending in `_desc-confounds_timeseries.tsv`
or `_desc-confounds_regressors.tsv`, including gzip-compressed variants.
Subject, session, task, and run identifiers are parsed from each
BIDS-style filename.

## A reproducible example

The following synthetic files make this vignette executable without
study data, a scheduler, or a container. They contain a slow component,
an oscillation in a respiratory-frequency band, and isolated motion
changes. The demonstration FD column uses the same 50 mm rotational
radius used by the helper when it must recompute raw FD.

``` r

demo_dir <- tempfile("braingnomes_motion_qc_")
dir.create(demo_dir)

make_demo_confounds <- function(subject, spike_size) {
  n_volumes <- 120L
  volume <- seq_len(n_volumes)
  respiration <- sin(2 * pi * volume / 4)
  slow_drift <- sin(2 * pi * volume / 48)

  motion <- data.frame(
    rot_x = 0.0015 * respiration + 0.0005 * slow_drift,
    rot_y = 0.0010 * cos(2 * pi * volume / 4),
    rot_z = 0.0004 * slow_drift,
    trans_x = 0.06 * respiration + 0.02 * slow_drift,
    trans_y = 0.03 * cos(2 * pi * volume / 4),
    trans_z = 0.01 * slow_drift
  )
  motion$trans_x[c(45L, 88L)] <-
    motion$trans_x[c(45L, 88L)] + c(spike_size, -spike_size)

  differences <- rbind(0, diff(as.matrix(motion)))
  motion$framewise_displacement <-
    rowSums(abs(differences[, 1:3, drop = FALSE])) * 50 +
    rowSums(abs(differences[, 4:6, drop = FALSE]))

  output <- file.path(
    demo_dir,
    sprintf(
      "sub-%s_ses-01_task-rest_run-01_desc-confounds_timeseries.tsv",
      subject
    )
  )
  utils::write.table(
    motion,
    file = output,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE,
    na = "n/a"
  )
  output
}

demo_files <- c(
  make_demo_confounds("01", spike_size = 0.35),
  make_demo_confounds("02", spike_size = 0.80)
)
basename(demo_files)
#> [1] "sub-01_ses-01_task-rest_run-01_desc-confounds_timeseries.tsv"
#> [2] "sub-02_ses-01_task-rest_run-01_desc-confounds_timeseries.tsv"
```

## Raw FD threshold summaries

Supply several thresholds when you want to inspect the distribution
without committing to one rule. Thresholds are in millimetres, and
comparisons are strictly greater than the requested value. Thus,
`fd_gt_0p5` is the percentage of non-missing FD observations for which
`FD > 0.5`, not `FD >= 0.5`.

``` r

raw_qc <- calculate_motion_outliers(
  confounds_files = demo_files,
  thresholds = c(0.2, 0.3, 0.5)
)

raw_qc[, c(
  "subject", "session", "task", "run", "fd_max", "fd_mean",
  "fd_gt_0p2", "fd_gt_0p3", "fd_gt_0p5"
)]
#>   subject session task run    fd_max   fd_mean fd_gt_0p2 fd_gt_0p3 fd_gt_0p5
#> 1      01      01 rest  01 0.5737987 0.2262741  99.16667  3.333333  2.500000
#> 2      02      01 rest  01 1.0237987 0.2412741  99.16667  3.333333  3.333333
```

The unfiltered columns have these meanings:

- `fd_max` and `fd_mean` summarize the available per-volume FD values.
- `fd_gt_<threshold>` reports the percentage above each threshold;
  decimal points become `p` so the names remain syntactically
  convenient.
- `confounds_file` preserves the source path for audit and follow-up.
- BIDS identifier columns make it straightforward to join the table to
  other run-level metadata.

When a source `framewise_displacement` column exists, it is used for
these raw summaries. If it is missing or entirely unavailable,
BrainGnomes recomputes FD when all six canonical rotation and
translation columns are present. If neither source is usable, raw FD
statistics are `NA`.

## Compare raw and motion-filtered FD

Respiration can introduce oscillation into rigid-body motion estimates
and therefore into FD. Set `include_filtered = TRUE` to retain the raw
summaries and also recompute FD after filtering all six motion
parameters. The filtered columns start with `fd_filt_`.

For a notch filter, specify the stop band in breaths per minute (BPM):

``` r

filtered_qc <- calculate_motion_outliers(
  confounds_files = demo_files,
  thresholds = c(0.2, 0.3, 0.5),
  include_filtered = TRUE,
  filter_method = "notch",
  tr = 1,
  bandstop_min_bpm = 12,
  bandstop_max_bpm = 18,
  filter_order = 4L
)

filtered_qc[, c(
  "subject", "fd_max", "fd_filt_max", "fd_gt_0p5",
  "fd_filt_gt_0p5"
)]
#>   subject    fd_max fd_filt_max fd_gt_0p5 fd_filt_gt_0p5
#> 1      01 0.5737987   0.3004677  2.500000       0.000000
#> 2      02 1.0237987   0.6754552  3.333333       3.333333
```

For a low-pass filter, supply the cutoff in hertz:

``` r

lowpass_qc <- calculate_motion_outliers(
  scfg = scfg,
  thresholds = c(0.3, 0.5),
  include_filtered = TRUE,
  filter_method = "lowpass",
  tr = 0.8,
  low_pass_hz = 0.1,
  filter_order = 2L
)
```

Raw and filtered FD answer different questions. Filtering can help
separate motion-parameter oscillation in a targeted frequency range from
abrupt changes, but filtered FD is not automatically a better measure
for every acquisition or analysis. Inspect both, record the filter
settings, and establish the study’s decision rule before applying
exclusions.

### What happens when filtering cannot be applied?

The helper never copies raw FD into the filtered columns. If the six
motion columns are missing or the requested filter is invalid for the
sampling rate, the `fd_filt_*` columns remain present but contain `NA`.
This distinguishes an unavailable filtered measurement from a run with
genuinely low filtered motion.

The next example requests a 12–18 BPM notch band for a TR of 4 seconds.
That band cannot be represented usefully at this sampling rate, so
filtering is skipped:

``` r

skipped_qc <- suppressWarnings(calculate_motion_outliers(
  confounds_files = demo_files[1],
  thresholds = 0.5,
  include_filtered = TRUE,
  filter_method = "notch",
  tr = 4,
  bandstop_min_bpm = 12,
  bandstop_max_bpm = 18
))

skipped_qc[, c("fd_max", "fd_gt_0p5", "fd_filt_max", "fd_filt_gt_0p5")]
#>      fd_max fd_gt_0p5 fd_filt_max fd_filt_gt_0p5
#> 1 0.5737987       2.5          NA             NA
stopifnot(all(is.na(skipped_qc[grep("^fd_filt_", names(skipped_qc))])))
```

Treat these `NA` values as a QC finding. Check the TR, filter cutoffs,
filter order, and presence of all six motion columns before using
filtered results.

## Export a QC or exclusion table

Use `output_file` to write the returned run-level summary while
calculating it. Files ending in `.csv` or `.csv.gz` are comma-separated;
other supported names, including `.tsv` and `.tsv.gz`, are
tab-separated.

``` r

summary_file <- file.path(demo_dir, "motion_qc_summary.tsv")
exported_qc <- calculate_motion_outliers(
  confounds_files = demo_files,
  thresholds = c(0.2, 0.3, 0.5),
  output_file = summary_file
)

file.exists(summary_file)
#> [1] TRUE
utils::read.delim(summary_file, check.names = FALSE)
#>   subject session task run
#> 1       1       1 rest   1
#> 2       2       1 rest   1
#>                                                                                                    confounds_file
#> 1 /tmp/RtmpoRMlMm/braingnomes_motion_qc_24ed66379fd4/sub-01_ses-01_task-rest_run-01_desc-confounds_timeseries.tsv
#> 2 /tmp/RtmpoRMlMm/braingnomes_motion_qc_24ed66379fd4/sub-02_ses-01_task-rest_run-01_desc-confounds_timeseries.tsv
#>      fd_max   fd_mean fd_gt_0p2 fd_gt_0p3 fd_gt_0p5
#> 1 0.5737987 0.2262741  99.16667  3.333333  2.500000
#> 2 1.0237987 0.2412741  99.16667  3.333333  3.333333
```

[`calculate_motion_outliers()`](https://hallquistlab.github.io/BrainGnomes/reference/calculate_motion_outliers.md)
deliberately does not decide which runs to exclude. Add a decision
column only after choosing and documenting a protocol. The rule below is
illustrative, not a recommended universal threshold:

``` r

example_percent_limit <- 5
exclusion_table <- raw_qc
exclusion_table$exclude_example <-
  exclusion_table$fd_gt_0p5 > example_percent_limit
exclusion_table$reason_example <- ifelse(
  exclusion_table$exclude_example,
  sprintf("more than %s%% of volumes have FD > 0.5 mm", example_percent_limit),
  ""
)

exclusion_file <- file.path(demo_dir, "motion_exclusion_example.tsv")
utils::write.table(
  exclusion_table,
  file = exclusion_file,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  na = "NA"
)
exclusion_table[, c("subject", "fd_gt_0p5", "exclude_example", "reason_example")]
#>   subject fd_gt_0p5 exclude_example reason_example
#> 1      01  2.500000           FALSE               
#> 2      02  3.333333           FALSE
```

Keep the continuous measurements even when a binary decision is
required. They make sensitivity analyses and later review possible. A
practical exclusion table may also include usable duration, task
performance, imaging artifacts, and a manual-review field; FD should not
be the only evidence considered by default.

## Relationship to postprocessing scrubbing

Run-level QC and volume-level scrubbing are related but distinct:

- [`calculate_motion_outliers()`](https://hallquistlab.github.io/BrainGnomes/reference/calculate_motion_outliers.md)
  reports how much of each run exceeds a threshold.
- A postprocessing `scrubbing$expression` identifies the individual
  volumes that receive censor or spike-regressor treatment.
- Scrubbing can interpolate flagged volumes, append spike regressors,
  remove volumes, or combine these actions. The QC helper performs none
  of them.

For example, a postprocessing stream might contain:

``` yaml
postprocess:
  resting_state:
    motion_filter:
      enable: true
      filter_type: notch
      bandstop_min_bpm: 12
      bandstop_max_bpm: 18
    scrubbing:
      enable: true
      expression: "framewise_displacement > 0.5"
      add_to_confounds: true
      interpolate: true
      apply: false
```

When motion filtering is enabled for a stream, BrainGnomes filters the
motion parameters and recomputes FD before evaluating motion-based
scrubbing by default. To preview comparable summaries, use the same TR,
filter type, cutoffs, order, rotation units, and threshold in
[`calculate_motion_outliers()`](https://hallquistlab.github.io/BrainGnomes/reference/calculate_motion_outliers.md).
Two details require attention:

1.  The standalone helper’s `low_pass_hz` is in hertz, whereas the
    stream setting `motion_filter$lowpass_bpm` is in BPM. Convert with
    `low_pass_hz = lowpass_bpm / 60`.
2.  The helper uses a 50 mm head radius when recomputing FD. That
    matches the postprocessing default, but a stream with a non-default
    scrubbing head radius will not produce identical filtered FD values.

The [Postprocessing
Walkthrough](https://hallquistlab.github.io/BrainGnomes/articles/postprocessing.md)
explains interpolation, spike regressors, physical volume removal,
confound processing, and the order of these operations in more detail.

## Suggested QC workflow

1.  Summarize several plausible raw thresholds without setting
    exclusions.
2.  If respiration-related contamination is a concern, rerun the summary
    with acquisition-appropriate filtering and inspect raw and filtered
    columns together.
3.  Investigate any `NA` filtered metrics rather than treating them as
    zero.
4.  Join the run-level table to other QC evidence and document the
    exclusion rule before creating a final decision column.
5.  Configure scrubbing separately when individual volumes need
    treatment, and ensure its FD and filter settings match the policy
    evaluated during QC.
