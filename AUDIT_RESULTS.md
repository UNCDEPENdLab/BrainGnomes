# Audit Remediation Results

## F01 — Release tests depend on an untracked calibration helper

**Status:** Fixed and verified on 2026-08-25.

### Resolution

The smoothness-calibration workflow is an internal development process
used to derive and diagnose calibration coefficients. It is not part of
the package’s supported runtime or release-test surface. The reviewed
coefficient tables in `inst/extdata/` remain tracked package inputs and
continue to be tested through the production calibration-selection code.

### Changes

- Remove the two release tests that source and exercise the private
  `inst/dev/smoothness_calibration/calibration_helpers.R`
  implementation.
- Retain the unrelated production automask regression test in
  `tests/testthat/test-postprocess-subject.R`.
- Ignore `/inst/dev/` in `.gitignore` so the local calibration workflow
  and diagnostics cannot be added inadvertently.
- Exclude `inst/dev/` explicitly in `.Rbuildignore`, so a source package
  built from a developer working tree is identical in this respect to
  one built from a clean checkout.
- Correct the adjacent `.codex` build-ignore expression identified by
  F12.

### Regression evidence

The package continues to test the shipped calibration coefficients
without depending on the private derivation workflow:

``` sh
Rscript -e 'devtools::test(filter = "postproc_checks|postprocess-subject", reporter = "summary", stop_on_failure = TRUE)'
```

``` text
AUDIT_F01_FOCUSED_TOTALS: files=2 contexts=37 passed=123 failed=0 errors=0 warnings=0 skipped=0
```

An isolated release staging tree was given explicit probe files beneath
both `.codex/` and `inst/dev/` before running `R CMD build`. Neither
probe directory nor the removed calibration test entered the generated
tarball. The reviewed coefficient tables and relocated production test
remained present:

``` text
AUDIT_F01_TARBALL: forbidden=0 retained=3
BrainGnomes/inst/extdata/spatial_smooth_calibration.csv
BrainGnomes/inst/extdata/spatial_smooth_calibration_validation.csv
BrainGnomes/tests/testthat/test-postprocess-subject.R
```

A full vignette-enabled source build followed by an installed-package
check completed cleanly:

``` sh
R CMD build BrainGnomes
R CMD check --no-manual BrainGnomes_0.8-2.tar.gz
```

``` text
* checking for hidden files and directories ... OK
* checking tests ... OK
* DONE
Status: OK
```

Full-suite verification from the development source tree:

``` text
AUDIT_TEST_TOTALS: files=64 contexts=337 passed=1319 failed=0 errors=0 warnings=0 skipped=1
```

The skipped test is the explicitly opt-in, Slurm-backed TemplateFlow
integration test and does not concern smoothness calibration.

## F02 — Connectivity methods overwrite or reuse the same output file

**Status:** Fixed and verified on 2026-08-25.

### Changes

- `R/bids_functions.R`: include the normalized `correlation` entity in
  BIDS filename ordering, so both `cor` and `correlation` inputs produce
  `cor-*` filename entities.
- `R/extract_rois.R`: construct and validate every requested
  connectivity path before writing output; reject duplicate paths
  instead of silently reusing or overwriting another estimator’s matrix.
- Extend
  [`bids_camelcase()`](https://hallquistlab.github.io/BrainGnomes/reference/bids_camelcase.md)
  to treat periods, hyphens, and underscores as word boundaries, and
  normalize every correlation-method name with that shared helper. This
  encodes `cor.shrink` as `cor-corShrink`, supports future punctuated
  method names without special cases, and retains `cor.shrink` as the
  method name in returned results.
- `vignettes/extract_rois.Rmd`: document the shrinkage-estimator naming
  convention.
- Add regression coverage in
  `tests/testthat/test-construct_bids_filename.R` and
  `tests/testthat/test-extract_rois.R`.

### Regression evidence

Before applying the implementation fix, the new tests reproduced F02:
Pearson and Spearman returned one shared path, and the nonlinear two-ROI
fixture read the wrong estimator’s matrix under both `overwrite = FALSE`
and `overwrite = TRUE`.

After the fix, the same deterministic 30-volume fixture verifies
separate files:

``` text
sub-01_task-rest_desc-clean_rois-DemoAtlas_cor-pearson_connectivity.tsv
sub-01_task-rest_desc-clean_rois-DemoAtlas_cor-spearman_connectivity.tsv
```

The persisted Pearson matrix matches
`stats::cor(..., method = "pearson")` (`0.9171955` off-diagonal), while
the Spearman matrix matches `stats::cor(..., method = "spearman")`
(`1.0000000` off-diagonal). Both overwrite settings are tested. The
default method vector creates four distinct existing files for
`pearson`, `spearman`, `kendall`, and `cor.shrink`; the shrinkage file
ends in `_cor-corShrink_connectivity.tsv`. Duplicate requested methods
are rejected before either time-series or connectivity files are
written.

Focused verification:

``` sh
Rscript -e 'results <- devtools::test(filter = "construct_bids_filename|extract_rois", reporter = "summary", stop_on_failure = TRUE); totals <- as.data.frame(results); cat(sprintf("AUDIT_F02_TOTALS: files=%d contexts=%d passed=%d failed=%d errors=%d warnings=%d skipped=%d\n", length(unique(totals$file)), nrow(totals), sum(totals$passed), sum(totals$failed), sum(totals$error), sum(totals$warning), sum(totals$skipped)))'
```

``` text
AUDIT_F02_TOTALS: files=2 contexts=17 passed=60 failed=0 errors=0 warnings=0 skipped=0
```

Full-suite verification:

``` sh
Rscript -e 'results <- devtools::test(reporter = "summary", stop_on_failure = TRUE); totals <- as.data.frame(results); cat(sprintf("AUDIT_TEST_TOTALS: files=%d contexts=%d passed=%d failed=%d errors=%d warnings=%d skipped=%d\n", length(unique(totals$file)), nrow(totals), sum(totals$passed), sum(totals$failed), sum(totals$error), sum(totals$warning), sum(totals$skipped)))'
```

``` text
AUDIT_TEST_TOTALS: files=62 contexts=312 passed=1139 failed=0 errors=0 warnings=0 skipped=1
```

The skipped test is the explicitly opt-in, Slurm-backed TemplateFlow
integration test (`BG_RUN_SRUN_TEMPLATEFLOW_INTEGRATION=true`) and does
not concern F02.

## F03 — Scheduled extraction ignores the configured correlation estimator

**Status:** Fixed and verified on 2026-08-25.

### Changes

- `inst/extract_cli.R`: read the canonical project setting from
  `cfg$correlation$method` and pass it directly to
  `extract_rois(cor_method = ...)`, including vectors of requested
  estimators.
- Preserve compatibility with legacy `cfg$cor_method` configurations,
  accept matching nested and legacy settings, and reject contradictory
  or missing method settings before creating extraction outputs.
- Forward the configured `save_ts` setting to
  [`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md)
  while retaining its existing `TRUE` default when older configurations
  omit the option.
- `R/extract_rois.R`: serialize connectivity matrices with
  [`as.data.frame.matrix()`](https://rdrr.io/r/base/as.data.frame.html)
  so `corpcor` shrinkage matrices remain square instead of being
  flattened by their special `shrinkage` class.
- `tests/testthat/test-extract_cli.R`: add installed-resource-safe
  helper integration tests using real YAML configuration, deterministic
  NIfTI images, actual connectivity files, and the scheduler’s
  serialized arguments.
- `vignettes/extract_rois.Rmd`: distinguish scheduled
  `extract_rois/<stream>/correlation/method` configuration from the
  direct `extract_rois(cor_method = ...)` interface.

### Regression evidence

Before applying the fix, the new helper tests reproduced F03: nested
Spearman configuration wrote a Pearson connectivity file,
`save_ts = FALSE` still wrote a time-series file, and contradictory or
missing method settings silently ran. Expanding the fixture to all
estimators also exposed a previously undetected shrinkage serialization
defect: a two-ROI connectivity matrix was written as a 4-by-1 table
instead of a 2-by-2 matrix.

After the fix, helper-level regression tests verify:

- Pearson, Spearman, Kendall, and shrinkage each produce a
  method-specific, numerically correct 2-by-2 connectivity matrix.
- A nested four-method vector creates four distinct connectivity files.
- `save_ts = FALSE` suppresses time-series output without changing the
  requested connectivity estimator.
- Legacy flat settings remain supported; matching nested and flat
  settings are accepted; conflicting or missing settings fail before
  writing outputs.
- `submit_extract_rois()` preserves nested method vectors and `save_ts`
  when it serializes scheduled extraction arguments.

Focused verification:

``` sh
Rscript -e 'results <- devtools::test(filter = "extract_cli", reporter = "summary", stop_on_failure = TRUE); totals <- as.data.frame(results); cat(sprintf("AUDIT_F03_TOTALS: files=%d contexts=%d passed=%d failed=%d errors=%d warnings=%d skipped=%d\n", length(unique(totals$file)), nrow(totals), sum(totals$passed), sum(totals$failed), sum(totals$error), sum(totals$warning), sum(totals$skipped)))'
```

``` text
AUDIT_F03_TOTALS: files=1 contexts=5 passed=26 failed=0 errors=0 warnings=0 skipped=0
```

The package was also freshly installed into a temporary library and its
installed `extract_cli.R` was launched in a separate `Rscript` process
using a YAML configuration containing `correlation: {method: spearman}`
and `save_ts: false`:

``` text
AUDIT_INSTALLED_F03: status=0 method=spearman observed=1.0000000 spearman=1.0000000 pearson=0.9171955 timeseries_files=0
```

Full-suite verification after both F02 and F03 fixes:

``` sh
Rscript -e 'results <- devtools::test(reporter = "summary", stop_on_failure = TRUE); totals <- as.data.frame(results); cat(sprintf("AUDIT_TEST_TOTALS: files=%d contexts=%d passed=%d failed=%d errors=%d warnings=%d skipped=%d\n", length(unique(totals$file)), nrow(totals), sum(totals$passed), sum(totals$failed), sum(totals$error), sum(totals$warning), sum(totals$skipped)))'
```

``` text
AUDIT_TEST_TOTALS: files=63 contexts=317 passed=1165 failed=0 errors=0 warnings=0 skipped=1
```

The skipped test is the explicitly opt-in, Slurm-backed TemplateFlow
integration test. The separate `cor_method = "none"` issue is addressed
under F05 below.

## F04 — Documented multiple-input-stream extraction cannot run

**Status:** Fixed and verified on 2026-08-25.

### Changes

- `R/setup_postprocess.R`: allow
  [`get_postproc_output_files()`](https://hallquistlab.github.io/BrainGnomes/reference/get_postproc_output_files.md)
  to accept either one `bids_desc` for all input specifications or one
  description per specification. Equal-length vectors are matched
  positionally, not as a cross-product, and ambiguous length
  combinations fail with a targeted error.
- `R/process_subject.R`: replace independent
  [`sapply()`](https://rdrr.io/r/base/lapply.html) simplification with
  an explicit source-flattening step. Each selected postprocessing
  stream retains its own `input_regex`/`bids_desc` association when the
  extraction CLI arguments are serialized. If a source supplies several
  specifications, its description is repeated for each one.
- Preserve backward compatibility for callers that provide several
  `input_regex` values and one shared `bids_desc`; the scalar
  description is intentionally recycled.
- `man/get_postproc_output_files.Rd` documents positional pairing,
  scalar recycling, the absence of cross-product matching, and mismatch
  errors.
- `vignettes/extract_rois.Rmd` now includes a complete two-stream YAML
  example using `rest_clean` and `nback_denoised` and explains exactly
  how those sources are associated.
- `tests/testthat/test-get_postproc_output_files.R` and
  `tests/testthat/test-extract_cli.R` cover pairwise matching,
  deliberately swapped-description decoys, scalar recycling, invalid
  lengths, scheduler serialization, installed-helper extraction, and
  exact manifest contents.

### Regression evidence

Before the fix, two selected streams produced two `bids_desc` values and
failed at `checkmate::assert_string(bids_desc)` before any files were
examined:

``` text
Assertion on 'bids_desc' failed: Must have length 1.
```

Focused verification:

``` sh
Rscript -e 'results <- devtools::test(filter = "get_postproc_output_files|extract_cli", reporter = "summary", stop_on_failure = TRUE); totals <- as.data.frame(results); cat(sprintf("AUDIT_F04_TOTALS: files=%d contexts=%d passed=%d failed=%d errors=%d warnings=%d skipped=%d\n", length(unique(totals$file)), nrow(totals), sum(totals$passed), sum(totals$failed), sum(totals$error), sum(totals$warning), sum(totals$skipped)))'
```

``` text
AUDIT_F04_TOTALS: files=2 contexts=16 passed=85 failed=0 errors=0 warnings=0 skipped=0
```

The package was freshly installed into an isolated library. The
installed public matcher and `extract_cli.R` were run against four NIfTI
inputs: the two intended pairs (`rest/clean` and `nback/denoised`) plus
both swapped-description decoys. Only the intended files were extracted
and recorded in the explicit manifest:

``` text
AUDIT_INSTALLED_F04: matched=2 expected_outputs=2 decoy_outputs=0 manifest_files=2 paired=TRUE
```

Full-suite verification after F04 and the other recorded audit fixes:

``` sh
Rscript -e 'results <- devtools::test(reporter = "summary", stop_on_failure = TRUE); totals <- as.data.frame(results); cat(sprintf("AUDIT_TEST_TOTALS: files=%d contexts=%d passed=%d failed=%d errors=%d warnings=%d skipped=%d\n", length(unique(totals$file)), nrow(totals), sum(totals$passed), sum(totals$failed), sum(totals$error), sum(totals$warning), sum(totals$skipped)))'
```

``` text
AUDIT_TEST_TOTALS: files=64 contexts=339 passed=1324 failed=0 errors=0 warnings=0 skipped=1
```

The skipped test is the explicitly opt-in, Slurm-backed TemplateFlow
integration test and does not concern ROI input matching.

## F05 — Documented time-series-only extraction is rejected

**Status:** Fixed and verified on 2026-08-25.

### Changes

- `R/extract_rois.R`: recognize `cor_method = "none"` before calling
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html), normalize it
  to a no-correlation state, write the requested ROI time-series file,
  and return `correlation = NULL`.
- Reject mixtures such as `c("none", "pearson")` before any extraction
  output is written; reject `cor_method = "none", save_ts = FALSE`
  because that configuration cannot produce any output.
- Skip minimum-timepoint correlation warnings when connectivity
  calculation is disabled, allowing short time-series-only runs without
  irrelevant warnings.
- `tests/testthat/test-extract_rois.R`: cover direct time-series-only
  extraction, returned results, 12-volume inputs, and invalid
  no-correlation combinations.
- `tests/testthat/test-extract_cli.R`: cover nested and legacy scheduled
  settings, invalid scheduled combinations, interactive setup’s forced
  `save_ts = TRUE`, and scheduler argument serialization.
- `man/extract_rois.Rd` and `vignettes/extract_rois.Rmd`: document that
  `"none"` must be selected alone, requires time-series output, and
  returns a `NULL` correlation result.

### Regression evidence

Before the fix, the new direct and nested-helper tests failed with:

``` text
'arg' should be one of "pearson", "spearman", "kendall", "cor.shrink"
```

The mixed `c("none", "pearson")` case was also silently reduced to
Pearson and wrote unwanted connectivity output. After normalization,
direct extraction writes one 12-volume time-series file, returns
`correlation = NULL`, writes no connectivity files, and emits no
minimum-timepoint warning. Scheduled helpers produce the same
time-series-only result for both canonical nested and legacy flat
configurations. Interactive setup retains `correlation$method = "none"`
and forces `save_ts = TRUE`; scheduler serialization preserves both
settings.

Focused verification:

``` sh
Rscript -e 'results <- devtools::test(filter = "extract_cli|extract_rois", reporter = "summary", stop_on_failure = TRUE); totals <- as.data.frame(results); cat(sprintf("AUDIT_F05_TOTALS: files=%d contexts=%d passed=%d failed=%d errors=%d warnings=%d skipped=%d\n", length(unique(totals$file)), nrow(totals), sum(totals$passed), sum(totals$failed), sum(totals$error), sum(totals$warning), sum(totals$skipped)))'
```

``` text
AUDIT_F05_TOTALS: files=2 contexts=14 passed=80 failed=0 errors=0 warnings=0 skipped=0
```

The package was freshly installed into a temporary library. Its public
[`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md)
function and installed `extract_cli.R`, launched in a separate `Rscript`
process with `correlation: {method: none}`, both completed successfully:

``` text
AUDIT_INSTALLED_F05: status=0 direct_correlation_null=TRUE scheduled_timeseries=1 scheduled_connectivity=0 volumes=12
```

Full-suite verification after the F02, F03, and F05 fixes:

``` sh
Rscript -e 'results <- devtools::test(reporter = "summary", stop_on_failure = TRUE); totals <- as.data.frame(results); cat(sprintf("AUDIT_TEST_TOTALS: files=%d contexts=%d passed=%d failed=%d errors=%d warnings=%d skipped=%d\n", length(unique(totals$file)), nrow(totals), sum(totals$passed), sum(totals$failed), sum(totals$error), sum(totals$warning), sum(totals$skipped)))'
```

``` text
AUDIT_TEST_TOTALS: files=63 contexts=321 passed=1189 failed=0 errors=0 warnings=0 skipped=1
```

The skipped test is the explicitly opt-in, Slurm-backed TemplateFlow
integration test and does not concern F05.

## F06 — Interactive ROI-mask edits are silently discarded

**Status:** Fixed and verified on 2026-08-25.

### Changes

- `R/setup_extract.R`: assign the interactive mask prompt directly to
  `excfg$mask_file` instead of discarding it in a temporary local
  variable.
- Normalize empty strings, whitespace, missing values, and YAML
  missing-value sentinels with `validate_char(..., empty_value = NULL)`,
  allowing an existing mask to be intentionally removed consistently
  with
  [`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md).
- `tests/testthat/test-setup_extract.R`: verify mask selection for both
  existing and newly created extraction streams, actual project YAML
  save/load, and all supported empty-mask responses.
- `tests/testthat/test-extract_cli.R`: verify scheduler argument
  serialization preserves `mask_file` and that the configured mask
  changes actual ROI voxel averages when the extraction helper reads the
  generated YAML.

### Regression evidence

Before the fix, the focused regression tests reproduced all reported F06
failures: existing streams retained `old-mask.nii.gz`, newly created
streams had no `mask_file`, YAML persisted the wrong or missing value,
empty responses did not clear existing masks, and the helper attempted
to use the stale mask path.

After the fix, both new and existing extraction configurations retain
the selected mask, and saved/reloaded YAML contains the same path.
`NA_character_`, `""`, whitespace, and `.na.character` all clear a
previous mask to `NULL`. An end-to-end deterministic fixture excludes
one high-amplitude voxel from ROI 1: the first unmasked ROI value is
`93.5`, while the configured mask changes it to `98.0`; ROI 2 is
unchanged.

Focused verification:

``` sh
Rscript -e 'results <- devtools::test(filter = "setup_extract|extract_cli", reporter = "summary", stop_on_failure = TRUE); totals <- as.data.frame(results); cat(sprintf("AUDIT_F06_TOTALS: files=%d contexts=%d passed=%d failed=%d errors=%d warnings=%d skipped=%d\n", length(unique(totals$file)), nrow(totals), sum(totals$passed), sum(totals$failed), sum(totals$error), sum(totals$warning), sum(totals$skipped)))'
```

``` text
AUDIT_F06_TOTALS: files=2 contexts=11 passed=55 failed=0 errors=0 warnings=0 skipped=0
```

The package was freshly installed into a temporary library. Its
installed interactive setup function, actual project YAML persistence,
and installed `extract_cli.R` subprocess were exercised together:

``` text
AUDIT_INSTALLED_F06: configured_mask_persisted=TRUE yaml_mask_persisted=TRUE unmasked_roi1_first=93.5 masked_roi1_first=98.0 roi2_unchanged=TRUE
```

Full-suite verification after the F02, F03, F05, and F06 fixes:

``` sh
Rscript -e 'results <- devtools::test(reporter = "summary", stop_on_failure = TRUE); totals <- as.data.frame(results); cat(sprintf("AUDIT_TEST_TOTALS: files=%d contexts=%d passed=%d failed=%d errors=%d warnings=%d skipped=%d\n", length(unique(totals$file)), nrow(totals), sum(totals$passed), sum(totals$failed), sum(totals$error), sum(totals$warning), sum(totals$skipped)))'
```

``` text
AUDIT_TEST_TOTALS: files=64 contexts=324 passed=1203 failed=0 errors=0 warnings=0 skipped=1
```

The skipped test is the explicitly opt-in, Slurm-backed TemplateFlow
integration test and does not concern F06.

## F07 — Fully masked ROI labels disappear; an entirely masked atlas crashes

**Status:** Fixed and verified on 2026-08-25.

### Changes

- `R/extract_rois.R`: derive the sorted ROI-label list from every
  positive atlas voxel before applying BOLD-derived masks, explicit
  brain masks, or minimum-voxel thresholds.
- Preserve fully masked and below-threshold ROIs as all-`NA` time-series
  columns, retaining identical ROI order and output dimensions across
  participants with different masks.
- Always restore full atlas-sized connectivity matrices, including an
  all-`NA` square matrix when no ROI survives; retain ROI names as
  connectivity headers and use explicit `NA` serialization so even fully
  masked 1-by-1 matrices round-trip correctly.
- Replace the former empty-file behavior with an informative warning
  when a full-sized all-`NA` connectivity matrix is written; reject
  truly empty atlases with a clear `contains no positive ROI labels`
  error.
- `tests/testthat/test-extract_rois.R`: replace the obsolete
  label-pruning expectation and cover partial masks, completely empty
  masks, different participant masks, nonconsecutive atlas labels,
  percentage thresholds, time-series-only extraction, single-ROI
  atlases, and atlases without labels.
- `tests/testthat/test-extract_cli.R`: verify scheduled extraction
  preserves atlas labels and square connectivity dimensions under
  partial and full masks.
- `man/extract_rois.Rd` and `vignettes/extract_rois.Rmd`: document the
  stable atlas-label contract and correct the missing-ROI connectivity
  example.

### Regression evidence

Before the fix, the new regressions reproduced all reported F07
failures:

``` text
partially masked time-series columns: volume,roi1
partially masked connectivity dimensions: 1 x 1
connectivity headers: V1
fully masked atlas: replacement has 30 rows, data has 0
percentage-threshold failure: no lines available in input
```

After the fix, atlas labels `1, 3, 7` produce identical
`volume,roi1,roi3,roi7` time-series columns and identically ordered
3-by-3 connectivity files for participants retaining different ROI
subsets, as well as for participants with no usable voxels. Fully masked
ROI rows, columns, and diagonals are `NA`. Percentage-threshold failures
also produce readable, correctly labeled all-`NA` matrices, and a fully
masked single-label atlas round-trips as a readable 1-by-1 matrix
instead of an empty table.

Focused verification:

``` sh
Rscript -e 'results <- devtools::test(filter = "extract_rois|extract_cli", reporter = "summary", stop_on_failure = TRUE); totals <- as.data.frame(results); cat(sprintf("AUDIT_F07_TOTALS: files=%d contexts=%d passed=%d failed=%d errors=%d warnings=%d skipped=%d\n", length(unique(totals$file)), nrow(totals), sum(totals$passed), sum(totals$failed), sum(totals$error), sum(totals$warning), sum(totals$skipped)))'
```

``` text
AUDIT_F07_TOTALS: files=2 contexts=17 passed=137 failed=0 errors=0 warnings=0 skipped=0
```

The package was freshly installed into an isolated temporary library.
Its public
[`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md)
function was exercised with partially masked, completely masked, and
single-ROI atlases; the installed `extract_cli.R` was also launched in a
separate `Rscript` process with a fully masked atlas and both Pearson
and shrinkage estimators:

``` text
AUDIT_INSTALLED_F07: partial_columns=volume,roi1,roi7 partial_matrix=2x2 fully_masked_matrix=2x2 fully_masked_all_na=TRUE single_roi_matrix=1x1 scheduled_methods=2 scheduled_matrix=2x2
```

Full-suite verification after the F02, F03, F05, F06, and F07 fixes:

``` sh
Rscript -e 'results <- devtools::test(reporter = "summary", stop_on_failure = TRUE); totals <- as.data.frame(results); cat(sprintf("AUDIT_TEST_TOTALS: files=%d contexts=%d passed=%d failed=%d errors=%d warnings=%d skipped=%d\n", length(unique(totals$file)), nrow(totals), sum(totals$passed), sum(totals$failed), sum(totals$error), sum(totals$warning), sum(totals$skipped)))'
```

``` text
AUDIT_TEST_TOTALS: files=64 contexts=326 passed=1255 failed=0 errors=0 warnings=0 skipped=1
```

The skipped test is the explicitly opt-in, Slurm-backed TemplateFlow
integration test and does not concern F07.

## F08 — Project and subject status never include ROI-extraction completion

**Status:** Fixed and verified on 2026-08-25.

### Changes

- `R/status_functions.R`: include every enabled ROI-extraction stream in
  [`get_subject_status()`](https://hallquistlab.github.io/BrainGnomes/reference/get_subject_status.md)
  and therefore
  [`get_project_status()`](https://hallquistlab.github.io/BrainGnomes/reference/get_project_status.md),
  with logical completion and POSIXct timestamp columns.
- Namespace extraction columns as `extract_rois_<stream>_complete` and
  `extract_rois_<stream>_time`. Existing postprocessing columns retain
  their names, so postprocessing and extraction streams named `shared`
  remain independently visible as `shared_*` and
  `extract_rois_shared_*`.
- `R/pipeline_functions.R`: extend
  [`is_step_complete()`](https://hallquistlab.github.io/BrainGnomes/reference/is_step_complete.md)
  to recognize `step_name = "extract_rois"`, accept `ex_stream`, query
  stream/session-specific SQLite job names, and check the corresponding
  completion and failure markers.
- `R/process_subject.R`: schedule each extraction stream and session
  with a distinct tracking name and marker such as
  `extract_rois_demo_sub-01_ses-A`, preventing one extraction stream or
  session from being mistaken for another.
- `inst/hpc_scripts/extract_rois_subject.sbatch` and `.pbs`: write
  successful completion timestamps after extraction and before marking
  the tracked job complete. The Slurm helper no longer records a
  manifest from the postprocessing input directory as if it were the
  extraction output.
- Avoid duplicate `bg_status_df` classes when project status combines
  subject status frames.
- `README.md` and generated help pages now state that status includes
  enabled ROI-extraction streams and document the extraction column
  naming contract.
- `tests/testthat/test-status_functions.R`: cover marker and database
  states, complete and incomplete streams, two sessions, overlapping
  stream names, disabled extraction, summary counts, scheduler naming,
  and both batch scripts’ success markers.

### Regression evidence

Before the fix, a configuration containing enabled extraction streams
returned only `sub_id` and `ses_id`, exactly as reported in F08. The new
tests also showed that production scheduling used the ambiguous name
`extract_rois_sub-01` for every stream and did not write a successful
`.complete` timestamp.

After the fix, a two-session fixture with postprocessing stream `shared`
and extraction streams `shared` and `alternate` returns all of:

``` text
sub_id
ses_id
shared_complete
shared_time
extract_rois_shared_complete
extract_rois_shared_time
extract_rois_alternate_complete
extract_rois_alternate_time
```

The `shared` extraction state is `TRUE, FALSE` across sessions A and B,
while the `alternate` state is `FALSE, TRUE`.
[`summary()`](https://rdrr.io/r/base/summary.html) reports one
completion for each extraction stream. SQLite `COMPLETED` and `FAILED`
states remain distinct, and scheduling two streams over two sessions
creates four distinct tracking names and completion markers.

Focused verification:

``` sh
Rscript -e 'results <- devtools::test(filter = "status_functions|preflight-permissions|run_project|extract_cli", reporter = "summary", stop_on_failure = TRUE); totals <- as.data.frame(results); cat(sprintf("AUDIT_F08_TOTALS: files=%d contexts=%d passed=%d failed=%d errors=%d warnings=%d skipped=%d\n", length(unique(totals$file)), nrow(totals), sum(totals$passed), sum(totals$failed), sum(totals$error), sum(totals$warning), sum(totals$skipped)))'
```

``` text
AUDIT_F08_TOTALS: files=6 contexts=62 passed=224 failed=0 errors=0 warnings=0 skipped=0
```

Both extraction batch scripts pass shell syntax validation:

``` sh
bash -n inst/hpc_scripts/extract_rois_subject.sbatch inst/hpc_scripts/extract_rois_subject.pbs
```

The package was freshly installed into an isolated temporary library.
Its public subject/project status functions, summary method, SQLite
reconciliation, and installed Slurm/PBS resources were then exercised
together:

``` text
AUDIT_INSTALLED_F08: columns=sub_id,ses_id,shared_complete,shared_time,extract_rois_shared_complete,extract_rois_shared_time,extract_rois_alternate_complete,extract_rois_alternate_time project_rows=2 shared_states=TRUE,FALSE alternate_states=FALSE,TRUE summary_counts=1/1 batch_markers=TRUE
```

Full-suite verification after the F02, F03, F05, F06, F07, and F08
fixes:

``` sh
Rscript -e 'results <- devtools::test(reporter = "summary", stop_on_failure = TRUE); totals <- as.data.frame(results); cat(sprintf("AUDIT_TEST_TOTALS: files=%d contexts=%d passed=%d failed=%d errors=%d warnings=%d skipped=%d\n", length(unique(totals$file)), nrow(totals), sum(totals$passed), sum(totals$failed), sum(totals$error), sum(totals$warning), sum(totals$skipped)))'
```

``` text
AUDIT_TEST_TOTALS: files=64 contexts=331 passed=1284 failed=0 errors=0 warnings=0 skipped=1
```

The skipped test is the explicitly opt-in, Slurm-backed TemplateFlow
integration test and does not concern F08.

## F08 follow-up — Explicit ROI-extraction output manifests

**Status:** Implemented and verified on 2026-08-25.

### Changes

- `R/job_tracking_functions.R`: extend
  [`capture_output_manifest()`](https://hallquistlab.github.io/BrainGnomes/reference/capture_output_manifest.md)
  with an explicit-file mode. The selected files must exist beneath the
  configured output root and are stored as deterministic relative paths
  with sizes and modification times. Directory scanning remains the
  default for existing callers.
- `inst/extract_cli.R`: collect the exact timeseries and connectivity
  paths returned by every
  [`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md)
  call in the job, reject a requested manifest when no outputs were
  produced or a reported path is missing, and install the completed JSON
  manifest via a same-directory temporary file.
- `inst/hpc_scripts/extract_rois_subject.sbatch` and `.pbs`: use a
  stream/subject/session-specific manifest beside the completion marker,
  remove stale manifests before non-debug runs, pass the manifest
  destination to the extraction helper, fail successful commands that
  did not create a manifest, and pass the JSON file to
  `upd_job_status.R` before writing `.complete`.
- Preserve the existing shared `data_rois` organization. The manifest
  lists only the current job’s outputs, so files from other subjects,
  sessions, or extraction streams are treated as unrelated extras and
  cannot establish completion.
- `README.md`, `vignettes/extract_rois.Rmd`, and the generated manifest
  help document the job-specific verification behavior.
- `tests/testthat/test-job_tracking.R`, `test-extract_cli.R`, and
  `test-status_functions.R`: cover explicit subsets, paths outside the
  output root, installed-helper manifest creation, shared-root
  isolation, missing-file invalidation, SQLite status verification, and
  Slurm/PBS manifest handoff.

### Regression evidence

Focused verification:

``` sh
Rscript -e 'results <- devtools::test(filter = "job_tracking|extract_cli|status_functions", reporter = "summary", stop_on_failure = TRUE); totals <- as.data.frame(results); cat(sprintf("MANIFEST_FOCUSED_TOTALS: files=%d contexts=%d passed=%d failed=%d errors=%d warnings=%d skipped=%d\n", length(unique(totals$file)), nrow(totals), sum(totals$passed), sum(totals$failed), sum(totals$error), sum(totals$warning), sum(totals$skipped)))'
```

``` text
MANIFEST_FOCUSED_TOTALS: files=3 contexts=52 passed=220 failed=0 errors=0 warnings=0 skipped=0
```

Both extraction scheduler resources pass shell syntax validation:

``` sh
bash -n inst/hpc_scripts/extract_rois_subject.sbatch inst/hpc_scripts/extract_rois_subject.pbs
```

A fresh package installation was exercised through the installed
`extract_cli.R` and `upd_job_status.R` scripts. The extraction created
two outputs while an unrelated file was already present in `data_rois`;
only the two extraction outputs entered SQLite. Status was
manifest-verified before a listed file was deleted and became incomplete
afterward:

``` text
AUDIT_INSTALLED_EXPLICIT_MANIFEST: scope=explicit files=2 unrelated_recorded=FALSE status_before=TRUE verified_before=TRUE status_after=FALSE verified_after=FALSE batch_scripts=TRUE
```

Full-suite verification after the explicit-manifest follow-up:

``` sh
Rscript -e 'results <- devtools::test(reporter = "summary", stop_on_failure = TRUE); totals <- as.data.frame(results); cat(sprintf("AUDIT_TEST_TOTALS: files=%d contexts=%d passed=%d failed=%d errors=%d warnings=%d skipped=%d\n", length(unique(totals$file)), nrow(totals), sum(totals$passed), sum(totals$failed), sum(totals$error), sum(totals$warning), sum(totals$skipped)))'
```

``` text
AUDIT_TEST_TOTALS: files=64 contexts=335 passed=1310 failed=0 errors=0 warnings=0 skipped=1
```

The skipped test remains the explicitly opt-in Slurm-backed TemplateFlow
integration test and does not concern extraction manifests.

## F09 — Scheduled extraction ignores `save_ts = FALSE`

**Status:** Fixed and verified on 2026-08-25.

### Changes

- `inst/extract_cli.R` forwards the configured `save_ts` value to
  [`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md).
  Configurations that predate this field retain the documented `TRUE`
  default.
- The installed-helper regression fixture now covers all output modes
  relevant to this setting: explicit `FALSE`, explicit `TRUE`, an
  omitted legacy value, and time-series-only extraction with
  `correlation/method: none`.
- An invalid configuration that disables time-series output while also
  choosing `none` is rejected before files are written.

### Regression evidence

With `save_ts = FALSE`, the real extraction helper writes the requested
Spearman connectivity matrix and no `_timeseries.tsv`. Explicit `TRUE`
and a missing legacy setting each write one time-series and one
connectivity file. The separate time-series-only fixture writes one
time-series and no connectivity file.

The focused F09–F11 verification completed with:

``` text
AUDIT_F09_F11_FOCUSED: files=3 contexts=22 passed=143 failed=0 errors=0 warnings=0 skipped=0
```

## F10 — CLI help recommends an unsupported step and omits stream selection

**Status:** Fixed and verified on 2026-08-25.

### Changes

- `inst/BrainGnomes` no longer includes `bids_validation` in any
  `run_project --steps` example. The help identifies BIDS validation as
  the standalone
  [`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md)
  operation it actually is.
- Add and document `--extract_streams=<streams>` alongside
  `--postprocess_streams`.
- Move
  command-line-to-[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
  translation into the testable internal `.run_project_cli()` adapter.
  The adapter forwards selected extraction streams as well as steps,
  subject filters, postprocessing streams, and flags.
- Extend the real CLI help tests to reject unsupported step examples and
  require both extraction-stream help and the standalone validation
  guidance.

### Regression evidence

A parsed command containing:

``` text
--steps=extract_rois --extract_streams=rest task --dry-run
```

reaches
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
with `extract_streams = c("rest", "task")` and `dry_run = TRUE`. Both
global and command-specific help exit successfully, contain no `--steps`
example mentioning `bids_validation`, and expose the new option. These
assertions are included in the 143-assertion focused result reported
under F09.

## F11 — Public help omits signatures or contains nonworking examples

**Status:** Fixed and verified on 2026-08-25.

### Changes

- Add explicit usage signatures at the C++ roxygen source for all seven
  affected exported native-backed functions:
  [`automask()`](https://hallquistlab.github.io/BrainGnomes/reference/automask.md),
  [`filtfilt_cpp()`](https://hallquistlab.github.io/BrainGnomes/reference/filtfilt_cpp.md),
  [`image_quantile()`](https://hallquistlab.github.io/BrainGnomes/reference/image_quantile.md),
  [`lmfit_residuals_4d()`](https://hallquistlab.github.io/BrainGnomes/reference/lmfit_residuals_4d.md),
  [`natural_spline_4d()`](https://hallquistlab.github.io/BrainGnomes/reference/natural_spline_4d.md),
  [`natural_spline_interp()`](https://hallquistlab.github.io/BrainGnomes/reference/natural_spline_interp.md),
  and
  [`remove_nifti_volumes()`](https://hallquistlab.github.io/BrainGnomes/reference/remove_nifti_volumes.md);
  regenerate `R/RcppExports.R` and their `.Rd` files.
- Make `automask(outfile = "")` genuinely optional, correct its argument
  name from `image` to `img`, and replace the nonexistent
  `automask_rcpp()` example.
- Correct the
  [`image_quantile()`](https://hallquistlab.github.io/BrainGnomes/reference/image_quantile.md)
  median example to use the named `quantiles = 0.5` argument.
- Replace the nonexistent `run_project(prompt = TRUE)` argument with a
  supported `steps = "fmriprep"` example and repair malformed stream
  documentation. The `steps` help now enumerates the seven stages
  accepted by the implementation, distinguishes the `"all"` selector
  from those stages, and cross-links standalone
  [`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md)
  from both the parameter text and `\seealso`.
- Correct
  [`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md)
  scientific-output documentation: ordinary correlations range from `-1`
  to `1`, Fisher-transformed values are unbounded, and transformed
  diagonals are written as `NA`, not `15`.
- Add installed-safe documentation-contract tests that require all seven
  usage sections, match their argument names to real function formals,
  and reject the broken examples and output descriptions identified by
  the audit.

### Regression evidence

`R CMD check` reports all documentation gates and examples as clean:

``` text
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking examples ... OK
```

The usage/example assertions are included in the focused result under
F09. The documentation contract additionally requires every supported
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
stage, rejects the nonexistent `prompt` argument, and verifies the
[`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md)
`\seealso` cross-reference. A separate runtime regression also calls
[`automask()`](https://hallquistlab.github.io/BrainGnomes/reference/automask.md)
without `outfile` and confirms that it returns the expected
three-dimensional image.

The focused help-contract rerun after this correction completed with:

``` text
AUDIT_F11_HELP_TOTALS: files=1 contexts=2 passed=39 failed=0 errors=0 warnings=0 skipped=0
```

`R CMD Rd2txt man/run_project.Rd` also completed successfully and
rendered the seven supported stages, standalone validation guidance,
corrected example, and See Also link.

## F12 — Hidden local artifacts enter the release tarball

**Status:** Fixed and verified on 2026-08-25.

### Changes

- Correct `.Rbuildignore` to exclude `.codex` itself and every
  descendant with `^\\.codex(?:/|$)`.
- Exclude the private calibration workflow with `^inst/dev(?:/|$)` and
  ignore `/inst/dev/` in Git. This preserves it as local development
  material while keeping release artifacts independent of local
  working-tree state.
- Retain only the reviewed calibration coefficient tables in
  `inst/extdata/`, which remain package resources covered by production
  tests.

### Regression evidence

An isolated release tree was populated deliberately with
`.codex/f12-probe` and `inst/dev/smoothness_calibration/f12-probe`
before a full vignette-enabled source build. Inspection of the resulting
tarball found neither probe tree nor the removed private calibration
test, while all required public resources were present:

``` text
AUDIT_F12_TARBALL: forbidden=0 required_present=4/4
BrainGnomes/inst/BrainGnomes
BrainGnomes/tests/testthat/test-documentation-contracts.R
BrainGnomes/tests/testthat/test-extract_cli.R
BrainGnomes/inst/extdata/spatial_smooth_calibration.csv
```

The built source package then passed the isolated installed-package
check:

``` sh
R CMD build BrainGnomes
R CMD check --no-manual BrainGnomes_0.8-2.tar.gz
```

``` text
* checking for hidden files and directories ... OK
* checking tests ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK
```

Full development-suite verification after F09–F12:

``` text
AUDIT_TEST_TOTALS: files=65 contexts=341 passed=1365 failed=0 errors=0 warnings=0 skipped=1
```

The skipped test is the explicitly opt-in, Slurm-backed TemplateFlow
integration test and does not concern F09–F12.

## F13 — Empty-input and missing-value contracts are inconsistent

**Status:** Fixed and verified on 2026-08-25.

### Changes

- `extract_bids_info(character())` now returns a zero-row data frame
  with the complete 18-column BIDS schema. Every column is character,
  including `suffix`, `ext`, and `directory`. Empty input retains this
  schema when `drop_unused = TRUE`, because there are no observations
  from which unused entities can be inferred.
- Centralize the configured status specification shared by
  [`get_subject_status()`](https://hallquistlab.github.io/BrainGnomes/reference/get_subject_status.md)
  and the empty-project path in
  [`get_project_status()`](https://hallquistlab.github.io/BrainGnomes/reference/get_project_status.md).
  An empty project now returns a zero-row `bg_status_df` containing
  character `sub_id`/`ses_id`, logical completion columns, and POSIXct
  time columns for every enabled ordinary stage, postprocessing stream,
  and ROI-extraction stream.
- Validate
  [`image_quantile()`](https://hallquistlab.github.io/BrainGnomes/reference/image_quantile.md)
  probabilities before image I/O. Empty vectors, `NA`, `NaN`, `Inf`, and
  `-Inf` now produce argument-specific errors; finite values outside
  `[0, 1]` retain the existing range error.
- Document the empty schemas and finite, non-missing quantile
  requirement in the generated public help.

### Regression evidence

The empty BIDS tests require exact column order, dimensions, and
character types with both `drop_unused` settings. The empty-project
status test uses ordinary stages plus same-named postprocessing and
extraction streams and requires exact column names and types, including
the `extract_rois_` prefix. It also verifies that
[`summary()`](https://rdrr.io/r/base/summary.html) returns zero counts
rather than failing. Quantile tests exercise an empty vector and each
missing/non-finite class against a real NIfTI fixture and assert the
corresponding error message.

``` text
AUDIT_F13_FOCUSED: files=3 contexts=21 passed=119 failed=0 errors=0 warnings=0 skipped=0
```

The complete development test suite also completed successfully. Its
only skip was the explicitly opt-in Slurm-backed TemplateFlow
integration test.

## F14 — An installed legacy ROI script cannot start

**Status:** Fixed and verified on 2026-08-25.

### Preserved functionality

- Adapt the legacy script’s useful ROI voxel-retention concept into the
  supported
  [`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md)
  implementation as the opt-in `save_diagnostics` setting.
- Write a per-atlas `_roidiagnostics.tsv` with stable rows for every
  positive atlas label. It separately reports total atlas voxels, voxels
  surviving the optional spatial mask, BOLD-valid voxels, the computed
  minimum requirement, retention status, and explicit `outside_mask`,
  `invalid_bold`, or `below_threshold` exclusion reasons.
- Expose the option through interactive extraction setup, project
  validation, scheduled extraction, return values, and explicit output
  manifests. Older project configurations default to `FALSE`.
- Remove `inst/ROI_TempCorr.R` now that its useful diagnostic concept is
  available through the supported extraction workflow. The broken legacy
  script and its missing `R_helper_functions.R` dependency are no longer
  installed.
- Add an installed-safe script-inventory contract. The test explicitly
  classifies the public `BrainGnomes` command, three scheduler helpers,
  and two internal workers; it requires usable `--help` output from
  every supported entry point and rejects any reappearance of
  `ROI_TempCorr.R`.

### Regression evidence

Focused extraction, CLI, setup, validation, and manifest tests completed
with:

``` text
ROI_DIAGNOSTICS_FOCUSED: files=3 contexts=26 passed=190 failed=0 errors=0 warnings=0 skipped=0
```

The complete package test suite after the diagnostic migration completed
with:

``` text
ROI_DIAGNOSTICS_FULL: files=65 contexts=344 passed=1392 failed=0 errors=0 warnings=0 skipped=1
```

The skip is the explicitly opt-in Slurm-backed TemplateFlow integration
test.

After removing the legacy script, the focused inventory and dry-run
regression tests completed with 29 passing assertions and no failures,
errors, warnings, or skips. A fresh source tarball contained exactly the
expected top-level R entry points and workers:

``` text
BrainGnomes
add_parent.R
extract_cli.R
insert_tracked_job.R
postprocess_cli.R
upd_job_status.R
```

`ROI_TempCorr.R` was absent from that release artifact. The complete
development test suite was also rerun successfully after this removal.

## F15 — External runtime prerequisites and local limitations are not represented in package metadata

**Status:** Fixed and verified on 2026-08-25.

### Changes

- Replace the one-sentence package description with metadata that
  distinguishes locally usable R functionality from scheduler-dependent
  full pipeline execution and names Python/TemplateFlow involvement.
- Add a stage-dependent `SystemRequirements` field covering SLURM or
  TORQUE/PBS, Bash, the package’s Singularity command contract
  (including compatible Apptainer installations), container images,
  Flywheel, BIDS validation, HeuDiConv heuristics, the FreeSurfer
  license, and the optional Python imaging modules.
- Add prerequisites matrices to the README and Quickstart. These
  separate the common scheduler/storage requirements from each stage’s
  executable, container, license, cache, and input requirements. They
  also identify which configuration, BIDS, status, native imaging, and
  direct ROI operations can be used without a scheduler.
- Install `extdata/example_project_config.yaml`, a deliberately
  disabled, placeholder-path configuration that demonstrates the
  complete top-level project shape without masquerading as a runnable
  study.
- Add the evaluated *Local onboarding and prerequisites* vignette. It
  loads the installed example without validation, performs a BIDS
  filename round trip, calculates quantiles from a synthetic NIfTI
  image, and explains that a `run_project(dry_run = TRUE)` does not
  submit jobs but still requires an otherwise real and accessible
  project configuration.

### Regression evidence

- Metadata contracts require `SystemRequirements` to identify the
  scheduler, container, Python, and licensing dependencies, and require
  the expanded description to state both the HPC scope and locally
  usable boundary.
- The installed-resource test locates the example through
  [`system.file()`](https://rdrr.io/r/base/system.file.html), loads it
  with `load_project(validate = FALSE)`, checks its project/scheduler
  identity, and confirms that all eight configured or standalone stages
  are disabled.
- Documentation contracts require the local vignette’s executable
  configuration, BIDS, and image examples, its dry-run boundary, and the
  separately submitted
  [`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md)
  guidance.
- The vignette rendered successfully with all executable chunks:

``` text
LOCAL_ONBOARDING_RENDERED: /tmp/mnhallq/local_onboarding.html
```

The focused metadata, installed-resource, vignette-boundary, and
existing documentation-contract tests completed with:

``` text
AUDIT_F15_FOCUSED: files=2 contexts=6 passed=64 failed=0 errors=0 warnings=0 skipped=0
```

The complete development test suite also passed after the F15 changes.
The only skip remained the explicitly opt-in Slurm-backed TemplateFlow
integration test.

## Post-audit documentation and dry-run usability follow-up

**Status:** Implemented and verified on 2026-08-25.

### Changes

- Describe BIDS validation consistently as configured with the project
  but submitted separately through
  [`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md).
  This boundary is now explicit in the README, Quickstart, interactive
  setup guidance, CLI help, and the generated help for
  [`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
  and
  [`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md).
- Replace the Quickstart’s copied August 2025 CLI transcript with an
  evaluated vignette chunk that locates
  `system.file("BrainGnomes", package = "BrainGnomes")` and executes its
  real `--help` output. The Quickstart now lists all seven supported
  [`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
  stages and documents the current double-dash options, `status`,
  command-specific help, and a portable PATH expression.
- Expand `run_project(dry_run = TRUE)` into a resolved plan. For each
  selected postprocessing stream it reports the input query, BIDS output
  description, processing order, output root, and overwrite setting. For
  each extraction stream it reports its resolved postprocessing sources,
  atlases, mask, ROI reducer, correlation methods, minimum-voxel
  threshold, time-series, diagnostic and Fisher-transform flags, output
  root, and overwrite setting. Existing subject/session planning still
  runs in dry-run mode, and no submission behavior changed.

### Regression evidence

- The Quickstart rendered successfully to HTML while executing the real
  CLI-help chunk (`QUICKSTART_RENDERED`).
- Documentation-contract tests require the live
  [`system.file()`](https://rdrr.io/r/base/system.file.html)/[`system2()`](https://rdrr.io/r/base/system2.html)
  fragment and reject the obsolete copied single-dash syntax.
- Real CLI tests require the standalone validation wording and reject
  any `run_project --steps` example containing `bids_validation`.
- Dry-run tests assert resolved postprocessing order,
  source-to-description mapping, multiple atlas paths, multiple
  correlation methods, percentage ROI thresholds, output flags, and the
  extraction output root.

## BrainGnomes 0.9 release audit

**Status:** Package implementation and release artifact verified on
2026-08-25.

### Scope and disposition

- Re-reviewed every original finding, F01 through F15, against the
  current implementation, documentation, and regression tests. Every
  finding remains fixed and has focused coverage recorded above.
- Promoted the complete unreleased change set directly to version `0.9`;
  no separate `0.8-2` release boundary was retained.
- Updated `DESCRIPTION` to `Version: 0.9` and `Date: 2026-08-25`.
- Made `NEWS.md` the canonical changelog for the release (the repository
  has no separate `CHANGELOG` file), added the release date, and
  expanded the 0.9 notes to identify the scientific-correctness,
  workflow, native-interface, documentation, onboarding, and
  release-hygiene changes explicitly.
- Updated the README’s tagged-install example to `ref = "0.9"`.

### Additional release-hygiene corrections

- Exclude audit reports, prior BrainGnomes source tarballs, check
  directories, Python bytecode caches, and the previously identified
  local/development trees from source packages.
- Set `PYTHONDONTWRITEBYTECODE=1` for the test harness because Python
  subprocesses import the installed TemplateFlow helper. The final
  installed check library contains no `__pycache__` directory or `.pyc`
  file after the complete test suite.
- Build from a compact release staging tree. The working repository
  contains approximately 10.7 GB under ignored `local/`, `tmp/`, and
  `.cache/` directories; although these paths are excluded from the
  artifact, enumerating them makes a direct working-tree `R CMD build`
  unnecessarily slow.

### Verification evidence

The complete development suite passed:

``` text
RELEASE_0_9_TEST_TOTALS: files=67 contexts=353 passed=1464 failed=0 errors=0 warnings=0 skipped=1
```

The one skip is the explicitly opt-in Slurm/TemplateFlow integration
test, which requires `BG_RUN_SRUN_TEMPLATEFLOW_INTEGRATION=true` and a
real compute node/container runtime.

A full vignette-enabled source build completed successfully with GNU tar
and no build warning. The resulting artifact was
`BrainGnomes_0.9.tar.gz` with SHA-256:

``` text
77391d8d268afc3f1e318334e4433ba779f62154c5b9c4dcbe42f7d6f84eb05b
```

Tarball inspection found 335 entries, all seven R Markdown vignettes,
all required onboarding/configuration/test resources, and zero forbidden
audit, Codex, cache, private-calibration, legacy ROI-script,
prior-tarball, or removed calibration-test entries:

``` text
FINAL_TARBALL files=335 forbidden=0 required=5/5 vignettes=7
```

The final source artifact passed the ordinary installed-package release
gate:

``` sh
R CMD check --no-manual BrainGnomes_0.9.tar.gz
```

``` text
* checking tests ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK

[ FAIL 0 | WARN 0 | SKIP 1 | PASS 1464 ]
```

An additional `R CMD check --as-cran --no-manual` completed all package
tests, examples, compiled-code checks, and vignette rebuilds
successfully. It reported one WARNING and three NOTEs attributable to
the audit environment rather than package source:

- URL and current-time verification could not run because outbound
  DNS/network access was unavailable.
- `qpdf` was not installed, so PDF size-reduction checks could not run.
- `-Wconversion` came from this installation’s global
  `R CMD config CXXFLAGS`; BrainGnomes does not add that flag in
  `src/Makevars`.

### Tracked-release verification

The complete audited change set, including every new test and onboarding
resource, was committed before tagging. A new source tree was then
created with `git archive HEAD`, preventing ignored or untracked
working-tree files from contributing to the release candidate.

The tracked-only build reproduced the expected artifact inventory:

``` text
TRACKED_TARBALL files=335 forbidden=0 required=5/5 vignettes=7
```

That tracked-only tarball passed the installed-package release gate with
the same result as the working-tree candidate:

``` text
* checking tests ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK

[ FAIL 0 | WARN 0 | SKIP 1 | PASS 1464 ]
```

The source-control/reproducibility gate identified earlier in this
report is therefore complete. No unresolved F01-F15 implementation or
artifact blocker remains for the 0.9 tag.

## BrainGnomes 0.9-2 release audit

**Status:** Release candidate prepared and verified on 2026-08-31.

### Scope and findings

- Audited the eleven commits after tag `0.9` through `7d750da`, together
  with the release-candidate corrections made during this audit. The
  reviewed delta spans 98 tracked paths and covers the 0.9-1 safety
  fixes, postprocessing validation and orchestration, smoothness
  recalibration, project lifecycle and run provenance, atomic guided
  saves, fMRIPrep 25.2.5 E2E evidence, TemplateFlow transform
  preservation,
  [`image_quantile()`](https://hallquistlab.github.io/BrainGnomes/reference/image_quantile.md)
  memory reduction, and motion-QC guidance.
- Confirmed that the established
  [`setup_project()`](https://hallquistlab.github.io/BrainGnomes/reference/setup_project.md)
  -\>
  [`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
  workflow remains the primary user journey. The validation, planning,
  provenance, status, retry, and cancellation interfaces are additive;
  CLI submission and cancellation remain guarded. The package has no
  graphical UI surface.
- Reviewed the committed fMRIPrep 25.2.5 E2E record. Its 28
  postprocessing checks passed, and the two release-qualification issues
  it identified are resolved in this candidate: the
  transform-preservation patch is committed and `multitaper` is now a
  required runtime dependency. The external Slurm/container E2E was not
  rerun during this local release audit.
- Accepted deterministic distributed sampling in image validators as an
  intentional performance and memory tradeoff. The public documentation
  now states the exact default sampling limits and retains an exhaustive
  option where supported.

### Release-candidate corrections

- Make the R recovery APIs match their documented safe behavior:
  [`retry_project_run()`](https://hallquistlab.github.io/BrainGnomes/reference/retry_project_run.md)
  and
  [`cancel_project_run()`](https://hallquistlab.github.io/BrainGnomes/reference/cancel_project_run.md)
  now default to preview mode, while an explicit `dry_run = FALSE` is
  required for scheduler mutation.
- Preserve complete postprocessing and ROI-extraction stream names,
  including underscores, when reconstructing retry requests from
  scheduler job names.
- Promote `multitaper` from `Suggests` to `Imports`, because strict
  temporal filter validation uses it in ordinary runtime execution.
- Update `NEWS.md`, generated lifecycle and mask-validation help, the
  README tagged-install example, the fMRIPrep E2E disposition, and
  release metadata. `NEWS.md` remains the repository’s canonical
  changelog; no separate `CHANGELOG` file is maintained.
- Fix a source-package-only Quickstart asset failure. The pipeline
  graphic now ships from `inst/extdata`, is loaded through
  [`system.file()`](https://rdrr.io/r/base/system.file.html), retains
  its rendered placement, and has explicit alternative text. This avoids
  both a missing installed-vignette asset and an `inst/doc` packaging
  note.
- Make the permission preflight test collect and assert all expected
  warnings, preventing environment-dependent warning leakage without
  weakening the production permission checks.

### Documentation and UX verification

- `DESCRIPTION` declares `Version: 0.9-2` and `Date: 2026-08-31`;
  `NEWS.md` declares the same release and date; the README installs tag
  `0.9-2`.
- Every `vignettes/*.Rmd` file has a literal `DD Mon YYYY` date. The
  edited Quickstart is dated `31 Aug 2026`; every clean vignette date
  matches its most recent Git commit date.
- The Quickstart rendered successfully after the asset relocation, and
  its documentation contract passed 119 assertions. Existing setup/run
  prompts, dry-run output, command help, and optional recovery guidance
  remain covered by the complete test suite.

### Verification evidence

The complete development suite passed:

``` text
[ FAIL 0 | WARN 0 | SKIP 2 | PASS 2046 ]
```

The skips are intentional: one requires the opt-in Slurm/TemplateFlow
integration environment, and one compares CLI exports against the older
package installed in the developer library. The installed-package check
runs the candidate exports and passed.

The final vignette-enabled source archive passed `R CMD check --as-cran`
through installation, examples, compiled-code checks, all tests, and
vignette rebuilding:

``` text
0 errors | 1 warning | 1 note
```

The warning is the absence of host utility `qpdf`; the note is that the
sandbox could not verify external current time. With the developer’s
global Makevars disabled, all package compilation-flag checks passed.
Neither finding comes from the BrainGnomes source.

The persistent release artifact is `BrainGnomes_0.9-2.tar.gz` (1,115,197
bytes), with SHA-256:

``` text
bf322c22bef3ecd59eaedd288ec14e8a61935015cf93e26d6ee62a5973f23190
```

Tarball inspection found 363 entries, all eight R Markdown vignettes,
release metadata, and `inst/extdata/braingnomes_flow.png`. It contained
zero audit, Codex, cache, local-data, temporary-tree, nested-tarball, or
misplaced `inst/doc/braingnomes_flow.png` entries.

### Release disposition

No unresolved implementation, documentation, CLI/UX, test, vignette, or
artifact blocker remains for 0.9-2. The candidate source changes still
need to be committed before creating the `0.9-2` tag; this audit did not
publish or tag the release.

## Release audit 09 Oct 2026

Audit date: 09 Oct 2026. Scope: the current development checkout, with
focused bug fixes and documentation changes. Release preparation sets
the package version to `1.0` and the release date to `2026-10-09`;
release tagging and publication remain separate maintainer steps.

The audit found reproducible numerical, configuration-saving, database
contention, and terminal interface bugs and added regression coverage
for their fixes. Release readiness still depends on the
environment-specific validation listed below. This audit does not
certify every supported external container or scheduler combination.

### Findings and fixes

P1 indicates incorrect numerical output, lost configuration changes, a
possible native crash, or unintended work selection. P2 indicates
robustness, documentation, or interface defects.

| Priority | Finding | Result after the fix | Coverage |
|----|----|----|----|
| P1 | Spline interpolation skipped voxels whose retained samples were constant, leaving censored spikes unchanged. | Requested samples, including edge samples, receive the retained constant value. | Synthetic spike and edge cases with both extrapolation modes. |
| P1 | Regression replaced the entire series with zeros when only fitted rows were constant. It also zeroed constant signals when the removed columns excluded the intercept. | Both native and matrix regression apply the fitted model to the full series. Only entirely zero signals take the shortcut. | Censored spikes, a design without an intercept, and partial regression retaining the intercept. |
| P1 | RNifti integer conversion can truncate in-range fractional values and mishandle constant or wide-integer scaling. Integer storage itself is intentional and supports fractional physical values through header scaling. | Processing remains floating point; saved files retain the input datatype. An explicit output scaling grid is recalculated from processed values, and rounded codes are stored without a narrowing integer intermediate. | All eight integer types, NIfTI-1/2, nontrivial input scaling, range expansion, unsigned negative results, zero backgrounds, fractional constants, tiny/large values, and floating-point controls. |
| P1 | FLOAT32 integer working buffers erased small differences at large offsets, and explicit FLOAT32 result casts narrowed even FLOAT64 images. | Decode integers into double-precision working images and let the image datatype control result storage. Preserve FLOAT64 results without a narrowing intermediate. | Independently calculated spline, regression, and identity-filter values; 2^24 offsets; heavily scaled 16-bit data; FLOAT64 fractions and large values; genuine NIfTI-2 scales of 1e-100 and 1e100. |
| P1 | Scalar filter coefficients accessed empty filter state; empty numerator coefficients could also access invalid memory. | Scalar gains work with and without steady-state initialization; empty coefficients fail before indexing. | Identity/gain filters and invalid coefficient tests. |
| P1 | Configuration comparison silently ignored changes inside two unnamed lists. | Unnamed, mixed-name, or duplicate-name lists are compared as complete values; confirmed saves retain edits. | Structural comparisons and an actual YAML save. |
| P1 | Cancelling an interactive multiple-stream selection could select all streams. | Cancellation stops the guided run before submission. | Both postprocessing and extraction, with both empty cancellation representations. |
| P2 | Installed-package tests reproduced a status read failing while a local worker held a SQLite write lock. Related inspection readers had the same missing busy handler. | Status, project inspection, submission-history, and table-existence reads wait up to 10 seconds for concurrent writes. | Exclusive-lock regression cases for all four readers, plus local-worker submission and inspection tests. |
| P2 | Nonzero constant voxels bypassed requested demeaning and filtering. | Constant signals follow the requested operation; zero background retains its fast path. | Demeaning and scalar-gain examples. |
| P2 | pkgbuild’s debug flags re-enabled RNifti pointer/reference-count diagnostics despite the package’s `-DNDEBUG`, flooding worker logs. | A shared include disables RNifti’s inline diagnostics while preserving the caller’s debug setting. All native image translation units use it. | Deliberate `-UNDEBUG` build, captured processing/destruction output, and inspection of the resulting binary. |
| P2 | Conversion temporaries were disowned after `replaceData()` had copied them, leaking a working buffer on every integer-image operation. | Retain ownership of each temporary and let its destructor free it after the copy. | RNifti ownership contracts reviewed; synthetic repeated-operation RSS comparison; output-lifetime tests. |
| P2 | Nonzero `set_mean` simply added an offset even when partial regression retained a nonzero mean. | Both regression implementations recenter over fitted rows before applying the requested nonzero target. The default `0` preserves existing residual behavior. | Mean checks for a design without an intercept. |
| P2 | Native helpers accepted images with additional dimensions but processed only the first block. | Interpolation, regression, filtering, and volume removal reject such images. | Five-dimensional input checks. |
| P2 | Missing regression selections and non-finite designs or quantile voxels could produce misleading output. Filter validation was skipped for zero images. | Invalid inputs fail explicitly; quantiles permit non-finite values only outside the supplied mask. | Missing selections, invalid designs, mask exclusion, and zero-image filter cases. |
| P2 | Menu titles were passed as the graphics argument; native single selection could return the wrong item. Numeric prefixes and EOF were mishandled. | Titles use named arguments; choices preserve the requested index; invalid suffixes are rejected; EOF terminates menus. | Mocked interactive calls, cancellation tests, and terminal checks. |
| P2 | Raw terminal Ctrl+D did not implement the documented cancellation behavior. | Escape, Ctrl+D, and EOF return cancellation; typed prompts stop cleanly and terminal settings are restored. | Prompt tests and terminal checks. |
| P2 | CLI parsing treated negative values as options and dropped trailing equals signs; empty values became missing values. | Negative vectors, trailing equals signs, and empty assignments retain their values. | Parser regression cases. |
| P2 | Numeric-looking CLI subject/session identifiers lost their leading zeros; invalid boolean strings silently disabled flags. | Identifiers remain text and invalid flags fail before acting. | Installed CLI subject filtering and invalid-flag checks. |
| P2 | Shell-quoted path options were split again, breaking paths containing spaces or apostrophes. | CLI and worker entry points preserve literal path arguments; generated vector-option blocks retain their established parsing. | Path adapter tests, real plan output, and worker tracking scripts. |
| P2 | BIDS parsing matched incidental substrings and truncated alphanumeric resolution labels. | Only complete filename entities are parsed; accepted resolution labels round-trip through filename construction. | Entity-boundary and resolution round-trip tests. |
| P2 | Motion QC coerced invalid filtering flags to `FALSE` and accepted infinite thresholds. | Flags and finite thresholds are validated before summarization. | Invalid argument cases. |
| P2 | The quickstart’s Bash activation command escaped quotes inside a single-quoted R expression and failed. The README omitted CLI activation and contradicted guided setup. | Correct activation instructions, an R entry-point alternative, and clear guided/prompt-free setup descriptions. | Documentation regeneration, vignette build, and installed CLI help. |
| P2 | The quickstart linked to a PNG in the installed R library, which pkgdown does not copy into its website. | Use an identical vignette-local PNG and a relative image path so pkgdown copies the asset beside the article. | Asset identity and documentation tests; an actual pkgdown render verifies the image URL and copied PNG. |

The corresponding source documentation and generated Rd files were
updated. The edited quickstart has the literal current date immediately
after its author. Unedited vignette dates matched their most recent
commits when inspected. Every subsequently edited vignette uses the
current literal date. `NEWS.md` records the user-visible changes.

### Correction of the initial storage-precision assessment

The initial proposal to force floating-point files was withdrawn after
review of the intended storage policy and NIfTI scaling. Exact equality
to the returned floating-point image is not the right criterion for
integer output. The relevant checks are preservation of the storage
datatype, controlled quantization under the written slope/intercept, and
absence of clipping, wraparound, or unintended loss of resolution.

Simply restoring RNifti’s original write path failed seven focused
expectations: in-range fractional values could be truncated rather than
rounded to the nearest storage point. The corrected writer explicitly
recalculates output scaling, accounts for the actual NIfTI-1/2 header
representation before encoding, and uses a binary grid that preserves
zero backgrounds. It rounds directly into the requested integer width,
reserves RNifti’s INT32 missing-value code, and handles constant outputs
with a nonzero slope. Non-finite processed values fail before writing an
integer file.

The expanded tests require decoding error no greater than half the
output step plus double-precision arithmetic rounding. They also cap the
step relative to the resolution available in the requested datatype, so
an excessively coarse scale cannot disguise a large error. Integer
quantization remains inherent in the chosen storage type. Integer
working images now use double precision to avoid premature quantization
at large offsets or outside FLOAT32’s range; FLOAT64 results also avoid
an unnecessary FLOAT32 cast. FLOAT32 inputs retain their existing
storage precision. Integer working buffers use twice the space of
FLOAT32 buffers, but conversion temporaries no longer leak after
replacement.

For NIfTI-2 tests, the written double-precision header fields are
inspected directly. RNifti’s R header accessor and update helper use a
NIfTI-1 header representation, which can otherwise narrow these values
and invalidate extreme scaling fixtures or quantization bounds. Tests
confirm that the fixtures really contain the intended physical values
before processing.

### RNifti diagnostics and runtime installation

The installed RNifti library itself did not contain the reported pointer
messages. They were compiled into BrainGnomes from RNifti’s inline C++
headers. The existing Makevars `-DNDEBUG` was overridden by pkgbuild’s
later `-UNDEBUG` in development builds. The shared include now
suppresses diagnostics at their source, including automasking, reference
measurement, image quantiles, and the other native image helpers. Normal
logging remains available.

The affected worker used a sealed project/run-owned runtime bundle
containing the old BrainGnomes binary. A clean reinstall
(`R CMD INSTALL --preclean`) and new runtime bundles are required for
future jobs to use this correction. The bundle key includes the package
binary’s checksum, so new submissions from a fresh R session detect the
rebuilt package. Jobs inheriting an existing `BG_RUNTIME_BUNDLE` keep
their pinned runtime. Reinstalling a shared library does not alter
existing sealed run bundles. Existing production bundles and logs were
not modified during this audit.

### Documentation review before the PR

A second review read the README, contribution guide, all ten vignettes,
and public workflow/image help, and compared usage against current
implementations. Corrections include reversed temporal-filter cutoffs;
supported AROMA modes; the actual fMRIPrep 23.1.0 removal of integrated
AROMA; CompCor zero-padding and whole-name regex matching;
scalar-versus-squared variance units; ROI signal validity versus
anatomical masking; existing extraction inputs, censor semantics,
minimum-voxel options, connectivity limits, and writable output
requirements. The review also finishes the intermediate-image paragraph,
clarifies debug jobs versus submission-free previews, corrects scheduler
polling defaults, fixes an undefined example configuration, adds missing
article links, makes shell run-ID placeholders syntactically safe, and
refreshes contribution/CI guidance. The setup prompts also use valid
CompCor regexes and zero-based ranges.

Container instructions now use portable shared paths, explicit
placeholder versions, and the site’s runtime policy. The upstream AROMA
example still uses `main`, so its downloaded image must be held fixed
and its checksum retained. Offline TemplateFlow and compute-visible
scratch requirements are explicit. Upstream HeuDiConv, NiPreps,
fMRIPost-AROMA, fMRIPrep, and validator documentation was consulted for
version- or runtime-dependent guidance. The quickstart first uses its
local PNG so pkgdown can copy the image. Installed-source rendering
falls back to the existing image in package `extdata`; no extra
installed PNG or `.install_extras` rule is needed. Confound regexes are
formatted as literal code in the help page, and the ROI guide has
matching YAML and vignette-index titles.

### Audit coverage

The review covered package metadata and build exclusions, public R entry
points, configuration and path handling, setup and recovery interfaces,
confound and native image operations, ROI extraction and geometry
checks, SQLite submission and worker status boundaries, run/derivative
provenance, QC export, installed scripts, Python helpers,
scheduler-script syntax, documentation contracts, and release workflows.
Code inspection concentrated on edge cases and interface boundaries; the
existing regression suite supplied broader coverage.

No scheduler jobs or imaging containers were launched. Container
integration tests remain opt-in. No major feature, database schema,
dependency, or publication was added. The subsequent release preparation
updates version/date metadata to 1.0.

### Verification

The local environment uses Linux, R **4.5.1**, RNifti **1.10.0**, Rcpp
**1.1.2**, and RcppArmadillo **15.6.0.1**. Native builds use GCC
**12.2.0**.

- `devtools::document()` completed and regenerated the affected Rd files
  and native R wrappers. All exported objects have Rd aliases.

- Full `devtools::test()` against the source and an isolated current
  installation before the final SQLite contention correction: **4,789
  passed, 0 failed, 0 errors, 0 warnings, 4 skipped** across 632 test
  cases in 98 files. The skips are two opt-in Slurm/container
  integrations, the opt-in Quarto rendering check, and a guarded preview
  requiring a supplied real project.

- After the SQLite correction, the focused contention, submission
  durability, project inspection, and worker-status suites passed
  without warnings or failures. Four exclusive-lock scenarios exercise
  all affected readers.

- Package installation completed in an isolated library under `/tmp`.

- The earlier vignette-enabled source build completed. Its
  1,558,701-byte tarball has 431 entries, retains the new tests and
  calibration tables, and excludes development data, caches, compiled
  objects, and the audit reports.

- Python syntax passed for both installed Python helpers; Bash syntax
  passed for all 27 shell/scheduler scripts; the shell-worker regression
  script passed. R syntax passed for 57 source and entry-point files.

- Seven installed terminal checks passed: correct single selection,
  rejection of partial numeric selections in both menus, EOF in both
  menus, and Ctrl+D and Escape cancellation with terminal-setting
  restoration.

- The README’s Bash activation command successfully ran installed CLI
  help. The changed vignette’s literal date and the final source
  snapshot’s agreement with workspace package files were verified.

- Installed-package `devtools::check()` before the storage-policy
  correction: **0 errors, 1 warning, 1 note**. The installed test suite
  reports **4,794 passed, 0 failed, 0 warnings, 5 skipped**. Four skips
  are the opt-in integrations/preview/rendering described above; the
  fifth is a source-only website configuration check, since that
  configuration is deliberately excluded from the installed package. All
  ten vignettes build and rebuild successfully; examples, documentation
  checks, compiled-code checks, and package loading checks pass.

- The package-check warning is the missing system `qpdf` utility for PDF
  size-reduction checks. The note is inability to verify the current
  time. Remote repository-index queries also fail in this
  network-restricted environment, while the installed dependencies pass
  the dependency checks. These environment diagnostics should be
  resolved in the release environment.

- Revised storage-focused suite: **847 passed, 0 failed, 0 errors, 0
  warnings**.

- Full installed-package check of the revised storage writer, before the
  diagnostic-suppression addition: **5,547 passed, 0 failed, 0 warnings,
  5 skipped**; **0 check errors, 1 warning, 1 note** (the same
  environment diagnostics described above).

- Deliberate pkgbuild debug build with `-UNDEBUG`, before the
  double-precision working-buffer correction: **892 passed, 0 failed, 0
  errors, 0 warnings** across storage, filtering, regression, reference,
  automasking, and diagnostic-suppression tests. The resulting binary
  contains none of the pointer acquisition, release, or image-creation
  diagnostic strings.

- A synthetic interpolation using the affected production runtime
  emitted nine pointer/image diagnostics; the same operation using the
  corrected isolated installation emitted zero. Only temporary images
  were written.

- Installed-package tests before the double-precision correction with
  GCC undefined-behavior and float-cast-overflow sanitizers: **892
  passed, 0 failed, 0 errors, 0 warnings**. The binary links `libubsan`
  and contains instrumentation; no sanitizer diagnostics occurred.

- The full installed-package check including diagnostic suppression,
  before the double-precision correction: **5,548 passed, 0 failed, 0
  warnings, 5 skipped**; **0 check errors, 1 warning, 1 note**
  (environment diagnostics).

- The earlier vignette-enabled source build produced a 1,563,375-byte
  tarball with 435 entries, including both new native headers and the
  storage/logging regression tests. It excludes local datasets, compiled
  objects, and audit reports.

- Final double-precision/storage/logging focused tests: **1,122 passed,
  0 failed, 0 errors, 0 warnings**; a forced clean pkgbuild debug build
  with `-UNDEBUG` passed the same **1,122 assertions** without RNifti
  diagnostics.

- The same final revision installed with GCC undefined-behavior and
  float-cast-overflow sanitizers passed **1,122 assertions, 0 failures,
  0 errors, 0 warnings**, with no sanitizer diagnostics.

- A synthetic FLOAT64 identity-filter comparison changed values by up to
  **5.920929e-08** in the affected production runtime and by **0** in
  the corrected installation. After warm-up, 20 operations on a
  synthetic 48-by-48-by-48-by-6 integer image increased RSS by **50.625
  MiB** in the old runtime and **0 MiB** in the corrected installation.
  This is a local memory-growth check, not a general peak-memory
  guarantee.

- Final clean installed-package `devtools::check()` for the
  double-precision, storage, and logging corrections: **5,778 passed, 0
  failed, 0 warnings, 5 skipped**; **0 check errors, 1 warning, 1
  note**. The five skips are the guarded integrations, rendering/preview
  checks, and installed-package website check described above. All ten
  vignettes build and rebuild; examples, documentation, compiled code,
  and package loading checks pass. The warning remains missing `qpdf`;
  the note remains inability to verify the current time.

- The final double-precision source build produced a **1,564,353-byte**
  tarball with **435 entries**. Both headers and storage/logging
  regression tests are retained; development datasets, compiled objects,
  and audit reports are excluded. Its contents match the corresponding
  numerical-fix revision; subsequent release documentation and packaging
  checks are recorded below.

- Release preparation sets `DESCRIPTION` to version **1.0**, dated
  **2026-10-09**, and consolidates all session fixes in the dated 1.0
  changelog. Documentation regeneration and **223 documentation/workflow
  assertions** pass without failures, errors, or warnings. Computational
  behavior matches the revision whose full suite passed above;
  subsequent documentation corrections also update assets, tests, and
  setup instructions.

- Release-metadata
  `devtools::check(args = c("--as-cran", "--no-tests"))` completed with
  **0 errors, 1 warning, 1 note**. All ten vignettes build and rebuild.
  The warning remains missing `qpdf`; the note remains inability to
  verify the current time. Tests were already run on the identical
  runtime code.

- After the quickstart image correction, **225 documentation/workflow
  assertions** pass without failures, errors, or warnings. An actual
  pkgdown article build succeeds without missing-image warnings; its
  HTML references `figures/braingnomes_flow.png`, and the copied PNG
  under `articles/figures/` is byte-for-byte identical to the original
  package asset.

- Final documentation regeneration and **235
  documentation/workflow/confound assertions** pass without failures,
  errors, or warnings. All **81 R examples** across the README and ten
  guides parse; all **19 fenced shell examples** pass Bash syntax
  checks. Corrected motion/CompCor/BIDS regex examples were checked
  directly against the current helpers.

- The full pkgdown site for the final release source builds without
  warnings. All ten articles render, and both local image references
  resolve. The quickstart HTML points to `braingnomes_flow.png` beside
  the article, whose bytes match the original package PNG. A standalone
  render without the local PNG verifies the installed-source fallback:
  the HTML embeds the exact original PNG. Intermediate attempts to
  install an additional vignette asset triggered an
  unused-file/directory note because the installed HTML is
  self-contained; the final implementation uses the existing `extdata`
  image instead. Both source paths are verified without a redundant
  installed copy.

- The full installed-package suite after the documentation audit passes
  **5,780 assertions**, with **0 failures, 0 test warnings, and 5
  guarded skips**. This run precedes only the final image-fallback
  packaging adjustment; the runtime code is unchanged. The final
  packaging check and installed/source documentation tests below
  validate that adjustment without repeating the runtime suite. The full
  check has 0 errors, 1 warning, and 2 notes: missing `qpdf`,
  unavailable clock verification, and the extra installed PNG from the
  intermediate asset layout. The final package check confirms that the
  PNG note is resolved.

- The final source package check after the asset fallback correction
  completed with **0 errors, 1 warning, 1 note**. All ten vignettes
  build/rebuild, and documentation, examples, package loading, compiled
  code, and installed assets pass. The warning is missing system `qpdf`;
  the note is inability to verify current time. This packaging check
  skips tests; runtime-suite verification and the separate final
  documentation tests are recorded here.

- The final installed package passes **130 documentation assertions**
  with no failures or warnings; the website-configuration test has its
  expected skip because that maintainer file is not installed.
  Source-only image checks pass in the 235-assertion source run, and
  both real rendering paths pass above.

- The final release tarball is **1,788,384 bytes** with **436 entries**.
  Its DESCRIPTION contains version 1.0 and date 2026-10-09. Both source
  PNGs match the original exactly; no redundant installed PNG, compiled
  objects, ignored development datasets, or audit report are included.
  All **405 intended source files** match the working tree, apart from
  this excluded audit report.

The direct checkout build encountered broken links in ignored comparator
data under `local/`. R copied the checkout before applying build
exclusions. For release checking, a clean staging directory contains the
tracked files and new audit tests, with the current edits, and omits
ignored local datasets and compiled objects. This checks the intended
package contents without modifying the local development data. The check
uses `R_MAKEVARS_USER=/dev/null` to avoid the site-specific optimization
and conversion-warning flags in the local user Makevars file; package
Makevars remain in force.

### Remaining release work

1.  Run the existing Windows, macOS, R release/devel, and older-R CI
    matrix on the final change. Include the declared R 4.1 minimum
    explicitly, since `oldrel-1` alone does not guarantee that coverage.
    The local audit uses Linux and R 4.5.1.
2.  Run focused opt-in Slurm/container integration tests for the
    production container versions, following the interactive-allocation
    instructions in `AGENTS.md`. Retain the real fMRIPrep/postprocessing
    smoke-test evidence.
3.  Render the optional QC dashboard with the release environment’s
    Quarto and optional widget packages; collection/export tests do not
    replace that check.
4.  Resolve environment-dependent package-check warnings and notes
    before claiming a clean release check. The verification results
    distinguish them from package defects.
5.  Review the 1.0 changelog’s API removals and changed return types
    against downstream scripts before tagging and publishing the
    release. Version and release-date metadata are set to 1.0 and 09 Oct
    2026.

The audit report is excluded from the source package by the existing
`.Rbuildignore` rule.
