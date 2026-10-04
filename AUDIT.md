# BrainGnomes pre-release audit

**Audit date:** 2026-08-25 **Package:** BrainGnomes 0.8-2 **R/platform
exercised:** R 4.5.1, x86_64-pc-linux-gnu, Red Hat Enterprise Linux 9.7
**Scope:** Scientific correctness, package installation and release
checks, installed-package behavior, workflow reliability, tests,
documentation, API consistency, portability, and user experience.
**Repository changes made during the audit:** This report only. Package
source, tests, documentation, and existing untracked files were not
modified.

## 1. Executive summary

BrainGnomes is an R package for configuring, submitting, monitoring, and
diagnosing containerized functional MRI workflows on high-performance
computing clusters. Its intended users are neuroimaging researchers, lab
maintainers, and research-computing staff operating Slurm or TORQUE
environments. Users ordinarily create or load a `bg_project_cfg`
configuration with
[`setup_project()`](https://hallquistlab.github.io/BrainGnomes/reference/setup_project.md)
or
[`load_project()`](https://hallquistlab.github.io/BrainGnomes/reference/load_project.md),
optionally refine it with
[`edit_project()`](https://hallquistlab.github.io/BrainGnomes/reference/edit_project.md),
submit selected stages using
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md),
and inspect progress with
[`get_project_status()`](https://hallquistlab.github.io/BrainGnomes/reference/get_project_status.md),
[`get_subject_status()`](https://hallquistlab.github.io/BrainGnomes/reference/get_subject_status.md),
or `diagnose_pipeline()`.

Supported stages include optional Flywheel synchronization,
DICOM-to-BIDS conversion, standalone BIDS validation, MRIQC, fMRIPrep,
ICA-AROMA, BOLD postprocessing, and atlas-based ROI
time-series/connectivity extraction. The package also exposes
lower-level BIDS, imaging, filtering, motion, scheduler, and
job-tracking helpers. Its implementation combines approximately 18,000
lines of R, native image-processing routines, installed Python and R
helpers, cluster job templates, SQLite tracking, 62 test files, 139 help
pages, and six installed vignettes.

The repository shows substantial useful engineering: most checks pass,
the vignettes build, package installation succeeds, and the installed
test run records **1,108 passing assertions**. However, the release is
not ready:

1.  `R CMD check` fails with two installed-package test errors.
2.  Direct ROI extraction can silently label Pearson results as Spearman
    or another requested estimator because all connectivity methods
    resolve to the same filename.
3.  Scheduled ROI extraction silently ignores the configured estimator
    and computes Pearson correlations instead.
4.  Several documented extraction workflows either fail outright or
    silently ignore configuration, including multiple input streams, the
    `"none"` option, configured masks, and disabled time-series output.
5.  The advertised project-status interface does not report ROI
    extraction at all.

These issues affect both release acceptance and the interpretation of
scientific outputs. The two connectivity defects are especially serious
because execution can appear successful while the returned or persisted
analysis results do not match the requested method.

### Audit evidence and execution summary

The following operations were executed against the actual repository and
isolated temporary directories:

``` text
R CMD build --no-manual /proj/mnhallqlab/users/michael/BrainGnomes
  Result: SUCCESS; all six vignettes were built.

R CMD INSTALL --library=/tmp/braingnomes-release-audit.fjQexh6E/audit-library \
  /tmp/braingnomes-release-audit.fjQexh6E/BrainGnomes_0.8-2.tar.gz
  Result: SUCCESS; the package loaded from its isolated installed location.

R CMD check --no-manual BrainGnomes_0.8-2.tar.gz
  Result: 1 ERROR, 1 NOTE.
  Installed-package test result: FAIL 2 | WARN 0 | SKIP 1 | PASS 1108.

Installed targeted tests covering ROI extraction, BIDS filenames, CLI help,
image quantiles, and project status
  Result: PASS, despite the independently reproduced defects below.

Git-tracked-only source snapshot and targeted installed-package tests
  Result: the same two calibration-helper tests fail; their helper is absent
  from the tracked release source.
```

The one skipped test is the deliberately opt-in, real-cluster
TemplateFlow integration test. No real containerized fMRI pipeline or
live cluster integration run was performed. Repository-index access was
unavailable in the execution environment, but dependency checks
completed successfully; the release-check failure is attributable to the
package tests, not missing R dependencies.

**DO NOT RELEASE YET**

## 2. Release blockers

| ID | Priority | Evidence | Finding | User impact |
|----|----|----|----|----|
| F01 | P0 | Confirmed | `R CMD check` fails because two tests depend on an untracked, source-relative development helper. | The release fails its basic package-quality gate and a clean checkout cannot reproduce the developer environment. |
| F02 | P0 | Confirmed | Correlation-method entities are omitted from connectivity filenames. | Different requested estimators share one file; returned paths can identify Pearson values as Spearman, Kendall, or shrinkage results. |
| F03 | P0 | Confirmed | Scheduled extraction reads `cfg$cor_method` even though setup stores `cfg$correlation$method`. | A configured Spearman workflow silently writes Pearson correlations. |
| F04 | P1 | Confirmed | Extraction from multiple postprocessing streams produces a vector rejected by the file-discovery helper. | An explicitly documented workflow fails before extraction. |
| F05 | P1 | Confirmed | The documented `cor_method = "none"` option is rejected. | Time-series-only extraction fails directly; the scheduled path can instead compute unwanted Pearson connectivity. |
| F06 | P1 | Confirmed | The interactive ROI-mask prompt never assigns its result to the project configuration. | Users believe a spatial mask was applied, but extraction runs with the previous mask or no mask. |
| F07 | P1 | Confirmed | Fully masked atlas labels disappear, and an atlas with no surviving labels crashes. | ROI identities vary across participants and complete mask exclusion fails with an opaque error. |
| F08 | P1 | Confirmed | Project and subject status omit ROI-extraction stages entirely. | Users cannot monitor completion of an enabled final analysis stage using the documented status API. |

## 3. Technical findings

### F01 — Installed-package tests fail and require an untracked development helper

**Severity:** P0 — release blocker. **Evidence:** Confirmed. **Affected
components:** `tests/testthat/test-smoothness-calibration-dev.R:1`,
`tests/testthat/test-smoothness-calibration-dev.R:22`, untracked
`inst/dev/smoothness_calibration/calibration_helpers.R`.

**Problem.** Two tests call:

``` r

helper_path <- testthat::test_path(
  "..", "..", "inst", "dev", "smoothness_calibration", "calibration_helpers.R"
)
sys.source(helper_path, envir = calibration_env)
```

That relative path does not exist when `R CMD check` runs the installed
package’s test directory. Independently, `inst/dev/` is untracked in the
current repository, so a Git checkout or GitHub installation does not
contain the helper at all.

**Direct reproduction.**

``` text
$ git status --short
?? inst/dev/

$ git ls-files inst/dev tests/testthat/test-smoothness-calibration-dev.R
tests/testthat/test-smoothness-calibration-dev.R

$ R CMD check --no-manual BrainGnomes_0.8-2.tar.gz
...
Error in sys.source(helper_path, envir = calibration_env):
  '../../inst/dev/smoothness_calibration/calibration_helpers.R'
  is not an existing file
...
[ FAIL 2 | WARN 0 | SKIP 1 | PASS 1108 ]
Status: 1 ERROR, 1 NOTE
```

The build made from the current working tree includes the locally
present helper, yet still fails because its source-relative location is
wrong during installed-package testing. A second build made from
`git archive HEAD` does not contain the helper at all, and targeted
installed-package tests produce the same two errors.

**Why it matters.** Public release artifacts and CI must be reproducible
from tracked files. A failed `R CMD check` is a hard release gate,
regardless of how many other tests pass.

**Recommended fix.** Decide whether the calibration development helpers
are intended to ship. If they are required by package tests, move the
necessary helper into an appropriate tracked package/test resource and
resolve it using an installed-package-safe mechanism. If they are
deliberately development-only, move these tests outside the release
suite or explicitly guard them when their optional development resource
is unavailable. Merely committing `inst/dev/` is insufficient because
the relative path also fails under `R CMD check`.

**Recommended regression validation.** Build from a pristine Git
checkout, install the resulting tarball into an empty temporary library,
and require `R CMD check --no-manual` to finish with zero errors and
warnings. Add a CI case that never relies on untracked files or
`devtools::load_all()`.

### F02 — Different connectivity methods overwrite or reuse the same output file

**Severity:** P0 — release blocker. **Evidence:** Confirmed. **Affected
components:** `R/bids_functions.R:143`, `R/bids_functions.R:155`,
`R/bids_functions.R:163`, `R/extract_rois.R:203`,
`R/extract_rois.R:230`.

**Problem.**
[`construct_bids_filename()`](https://hallquistlab.github.io/BrainGnomes/reference/construct_bids_filename.md)
normalizes the abbreviated `cor` entity to `correlation`, and its prefix
table maps `correlation` to `cor`. However, its entity-order vector
contains `"cor"`, not `"correlation"`. Consequently, the normalized
correlation entity is never reconstructed.

``` r

BrainGnomes::construct_bids_filename(
  list(
    subject = "01",
    rois = "DemoAtlas",
    correlation = "pearson",
    suffix = "connectivity",
    ext = ".tsv"
  )
)

# Observed:
# "sub-01_rois-DemoAtlas_connectivity.tsv"
#
# Expected:
# "sub-01_rois-DemoAtlas_cor-pearson_connectivity.tsv"
```

The extraction vignette explicitly promises method-specific names such
as
`sub-01_task-rest_desc-clean_rois-Schaefer400_cor-pearson_connectivity.tsv`.

**Installed-package reproduction.** A deterministic two-ROI fixture was
analyzed with `cor_method = c("pearson", "spearman")`. The two returned
method entries pointed to one identical file:

``` text
number of requested methods: 2
number of unique returned connectivity paths: 1
value read through the "spearman" path: 0.916869
actual Pearson correlation:             0.916869
actual Spearman correlation:            0.998665
```

With the default `overwrite = FALSE`, the first method is written and
later methods are silently skipped while their returned paths still
point to the first method’s file. With `overwrite = TRUE`, later methods
overwrite earlier ones and all returned entries point to the last
method’s result. The default argument advertises all four methods,
making this a default-path defect rather than an obscure edge case.

**Why it matters.** Users can unknowingly analyze or publish
connectivity matrices produced by a different estimator than the one
requested or reported. This is a scientific-correctness problem, not
merely a cosmetic filename issue.

**Recommended fix.** Use `"correlation"` consistently in `entity_order`,
abbreviation normalization, and `prefixes`. Validate that every
requested estimator maps to a unique path before writing. Decide how to
encode `cor.shrink` consistently with the intended output naming
convention.

**Recommended regression test.** Test both `cor` and `correlation`
inputs to
[`construct_bids_filename()`](https://hallquistlab.github.io/BrainGnomes/reference/construct_bids_filename.md).
On a nonlinear deterministic ROI fixture, request at least Pearson and
Spearman, assert distinct filenames containing the expected method
entities, and compare each persisted matrix with the corresponding
`stats::cor(..., method = ...)`. Repeat for both overwrite settings and
the default method vector.

### F03 — Scheduled extraction silently computes Pearson instead of the configured estimator

**Severity:** P0 — release blocker. **Evidence:** Confirmed. **Affected
components:** `R/setup_extract.R:241`, `R/setup_extract.R:264`,
`R/process_subject.R:887`, `inst/extract_cli.R:94`.

**Problem.** Project setup stores the selected estimator at:

``` r

scfg$extract_rois[[stream]]$correlation$method
```

The installed extraction helper instead passes:

``` r

cor_method = cfg$cor_method
```

Because `cfg$cor_method` is absent, `NULL` is supplied to
[`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md).
[`match.arg()`](https://rdrr.io/r/base/match.arg.html) then selects its
first default, `"pearson"`. The actual scheduled workflow therefore
ignores the user’s nested correlation configuration.

**Installed-package reproduction.** An actual installed `extract_cli.R`
run received a YAML configuration containing:

``` yaml
correlation:
  method: spearman
save_ts: false
```

The helper exited successfully. Its persisted connectivity matrix
contained:

``` text
requested estimator:  spearman
observed correlation: 0.916869
Pearson expectation:  0.916869
Spearman expectation: 0.998665
```

This reproduction crosses the real installed R-helper boundary; it is
not a mock of the extraction implementation. F02 independently removes
the method from the filename, making the wrong estimator even harder to
detect.

**Why it matters.**
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
is the recommended production workflow. Users can select Spearman,
Kendall, shrinkage, or `none` during interactive setup and receive
successful output produced by a different method.

**Recommended fix.** Define one canonical extraction configuration
schema and map `cfg$correlation$method` explicitly into
`extract_rois(cor_method = ...)`. Consider a backward-compatible
fallback to a legacy flat `cfg$cor_method`, but reject contradictory
settings rather than silently preferring one.

**Recommended regression test.** Run the installed extraction helper
against a tiny deterministic NIfTI fixture and a YAML configuration
generated in the same nested shape as
[`setup_project()`](https://hallquistlab.github.io/BrainGnomes/reference/setup_project.md).
Assert the requested estimator appears in the filename and that the
numeric matrix equals the requested method, not Pearson. Cover each
supported estimator and multiple requested methods.

### F04 — Documented multiple-input-stream extraction cannot run

**Severity:** P1 — fix before release. **Evidence:** Confirmed.
**Affected components:** `R/process_subject.R:898`,
`R/process_subject.R:903`, `R/setup_postprocess.R:1697`,
`inst/extract_cli.R:72`, `vignettes/extract_rois.Rmd:31`.

**Problem.** The extraction vignette states that multiple postprocessing
streams may feed one extraction stream. Submission constructs one
`input_regex` and one `bids_desc` for each selected input stream:

``` r

ex_cfg$input_regex <- sapply(ex_cfg$input_streams, ...)
ex_cfg$bids_desc <- sapply(ex_cfg$input_streams, ...)
```

The helper subsequently calls
`get_postproc_output_files(cfg$input, cfg$input_regex, cfg$bids_desc)`,
whose implementation insists that `bids_desc` be a single string.

**Installed-package reproduction.**

``` r

BrainGnomes::get_postproc_output_files(
  input_dir = existing_postprocess_directory,
  input_regex = c(
    "task:rest desc:preproc suffix:bold",
    "task:rest desc:preproc suffix:bold"
  ),
  bids_desc = c("clean", "clean2")
)

# Error: Assertion on 'bids_desc' failed: Must have length 1.
```

**Why it matters.** A workflow explicitly encouraged in project setup
and the extraction guide fails immediately when the selected streams
have their own output descriptions.

**Recommended fix.** Preserve the one-to-one mapping between
postprocessing input specifications and output descriptions. Either
iterate over paired `(input_regex, bids_desc)` values in the installed
helper, or extend
[`get_postproc_output_files()`](https://hallquistlab.github.io/BrainGnomes/reference/get_postproc_output_files.md)
to validate and process paired vectors without combining mismatched
stream settings.

**Recommended regression test.** Create two postprocessing streams with
distinct descriptions and one extraction stream referencing both. Run
the installed helper and assert that exactly the expected files from
both streams are extracted.

### F05 — Documented time-series-only extraction is rejected

**Severity:** P1 — fix before release. **Evidence:** Confirmed.
**Affected components:** `R/extract_rois.R:41`, `R/extract_rois.R:49`,
`R/setup_extract.R:249`, `man/extract_rois.Rd:30`,
`vignettes/extract_rois.Rmd:61`.

**Problem.** The public help, extraction vignette, and interactive setup
all advertise `"none"` as the way to disable connectivity and extract
only ROI time series. The public function calls
`match.arg(cor_method, several.ok = TRUE)` using defaults that do not
include `"none"`.

**Installed-package reproduction.**

``` r

BrainGnomes::extract_rois(
  bold_file,
  atlas_files = atlas_file,
  out_dir = existing_output_directory,
  cor_method = "none"
)

# Error: 'arg' should be one of "pearson", "spearman", "kendall", "cor.shrink"
```

The scheduled path is worse when combined with F03: setup stores
`correlation$method = "none"`, but the helper reads the nonexistent
`cor_method` field and can calculate Pearson connectivity instead.

**Why it matters.** A documented and explicitly offered project
configuration cannot be executed reliably. Users requesting only
extracted time series may get an error or unexpected connectivity output
depending on how they invoke the package.

**Recommended fix.** Accept `"none"` explicitly, normalize it to a
no-correlation state before
[`match.arg()`](https://rdrr.io/r/base/match.arg.html), reject mixtures
such as `c("none", "pearson")`, and preserve that state through
configuration, scheduling, and the installed helper.

**Recommended regression test.** Exercise direct extraction, interactive
configuration serialization, and the installed extraction helper with
`"none"`. Assert successful creation of time-series output, no
connectivity files, and an unambiguous returned `correlation = NULL`.

### F06 — Interactive ROI-mask edits are silently discarded

**Severity:** P1 — fix before release. **Evidence:** Confirmed.
**Affected components:** `R/setup_extract.R:220`,
`R/setup_extract.R:309`.

**Problem.** `setup_extract_stream()` reads the mask prompt into a
temporary local variable:

``` r

mask_file <- prompt_input(...)
```

It never assigns that result to `excfg$mask_file`. The unchanged stream
is then returned and later serialized.

**Installed-package reproduction.**

``` r

cfg <- structure(
  list(extract_rois = list(
    enable = TRUE,
    demo = list(mask_file = "old-mask.nii.gz")
  )),
  class = "bg_project_cfg"
)

updated <- testthat::with_mocked_bindings(
  BrainGnomes:::setup_extract_stream(
    cfg,
    fields = "extract_rois/demo/mask_file",
    stream_name = "demo"
  ),
  prompt_input = function(...) "new-mask.nii.gz",
  .package = "BrainGnomes"
)

updated$extract_rois$demo$mask_file
# Observed: "old-mask.nii.gz"
# Expected: "new-mask.nii.gz"
```

For a newly configured stream, the selected mask is absent altogether.

**Why it matters.** A user can believe anatomical masking constrained
the ROI analysis when it did not. This changes which voxels contribute
to scientific outputs without an informative error.

**Recommended fix.** Assign the prompt result to `excfg$mask_file`,
normalize an intentionally empty response consistently with the direct
API, and preserve the field during YAML save/load and helper invocation.

**Recommended regression test.** Mock the mask prompt for both a new
stream and an existing stream; assert the returned object and
saved/reloaded YAML contain the selected path. Add a small end-to-end
extraction fixture showing the configured mask changes the actual
retained ROI voxels.

### F07 — Fully masked ROI labels disappear; an entirely masked atlas crashes

**Severity:** P1 — fix before release. **Evidence:** Confirmed.
**Affected components:** `R/extract_rois.R:130`, `R/extract_rois.R:133`,
`R/extract_rois.R:159`, `tests/testthat/test-extract_rois.R:127`.

**Problem.** ROI labels are derived only after intersection with the
usable-voxel mask:

``` r

roi_vals <- sort(unique(atlas_vec[atlas_vec > 0 & mask_vec]))
```

An atlas label with no surviving voxels is therefore deleted before the
documented minimum-voxel logic can represent it as an all-`NA` ROI.
Existing tests explicitly assert this deletion, even though the help
page and vignette promise consistent ROI matrix size and preserved
labels.

When every atlas label is removed,
[`sapply()`](https://rdrr.io/r/base/lapply.html) returns an empty
object, and subsequent data-frame construction fails instead of
producing a clear diagnostic or a well-defined all-`NA` result.

**Installed-package reproduction.** For an atlas containing labels 1 and
2 and a mask retaining only label 1:

``` text
actual timeseries columns: volume,roi1
documented consistent-label expectation: volume,roi1,roi2
```

For an all-zero mask:

``` text
Error running extract_rois[DemoAtlas]: replacement has 30 rows, data has 0
```

**Why it matters.** Atlas columns and connectivity dimensions can differ
across participants, making group-level comparisons unreliable or
requiring undocumented downstream reconciliation. A complete mask
mismatch produces a cryptic failure rather than an actionable
explanation.

**Recommended fix.** Establish whether atlas-label preservation is the
intended public contract. If so, derive labels from all positive atlas
voxels before masking, represent excluded labels with all-`NA` columns,
and produce a consistent all-`NA` matrix or an explicit, informative
error when no ROI survives. If label pruning is intentional, revise all
contradictory documentation and explain how consumers should align
subject matrices.

**Recommended regression test.** Cover partially excluded labels, a
completely excluded atlas, excluded labels that fail a percentage
threshold, and two participants with different masks. Assert stable ROI
order and matrix dimensions under the chosen public contract.

### F08 — Project and subject status never include ROI-extraction completion

**Severity:** P1 — fix before release. **Evidence:** Confirmed.
**Affected components:** `R/status_functions.R:32`,
`R/status_functions.R:92`, `README.md:50`.

**Problem.**
[`get_subject_status()`](https://hallquistlab.github.io/BrainGnomes/reference/get_subject_status.md)
constructs its tracked step list from BIDS conversion, MRIQC, fMRIPrep,
AROMA, and postprocessing. It never includes enabled `extract_rois`
streams.
[`get_project_status()`](https://hallquistlab.github.io/BrainGnomes/reference/get_project_status.md)
delegates to that same implementation.

**Installed-package reproduction.**

``` r

cfg <- structure(
  list(
    metadata = list(log_directory = log_dir, bids_directory = bids_dir),
    extract_rois = list(enable = TRUE, demo = list())
  ),
  class = "bg_project_cfg"
)

names(BrainGnomes::get_subject_status(cfg, "01"))
# Observed: c("sub_id", "ses_id")
# No extraction completion or timestamp columns are present.
```

**Why it matters.** The README presents these functions as the standard
way to check pipeline progress, yet they cannot report the final
ROI-analysis stage. A project may appear complete while extraction
remains queued, failed, or incomplete.

**Recommended fix.** Include enabled extraction streams in both subject
and project status, with completion and timestamp fields analogous to
postprocessing streams. Ensure stream names remain distinguishable when
postprocessing and extraction names overlap.

**Recommended regression test.** Configure an enabled extraction stream
and representative completion/incompletion markers or tracking rows.
Assert both status functions and their summary output expose the
stream’s actual state.

### F09 — Scheduled extraction ignores `save_ts = FALSE`

**Severity:** P2 — worth fixing. **Evidence:** Confirmed. **Affected
components:** `inst/extract_cli.R:94`, `R/setup_extract.R:267`,
`R/extract_rois.R:44`.

**Problem.** The installed extraction helper builds an argument list for
[`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md)
but never forwards `cfg$save_ts`. The direct function therefore uses its
default `save_ts = TRUE` regardless of project configuration.

**Installed-package reproduction.** The same real helper invocation used
for F03 received:

``` yaml
save_ts: false
```

It exited successfully and nevertheless wrote one `_timeseries.tsv`
file:

``` text
configured save_ts: false
number of timeseries files written: 1
```

**Why it matters.** Production behavior disagrees with both interactive
setup and the direct API. Large studies can create substantial unwanted
output, and users cannot predict which files a configured workflow will
produce.

**Recommended fix.** Forward `save_ts = cfg$save_ts`, apply an explicit
backward-compatible default only when the field is absent, and validate
combinations that would intentionally produce no output.

**Recommended regression test.** Invoke the installed extraction helper
with `save_ts = FALSE` and an enabled correlation method; assert that
connectivity exists and no time-series file is written. Repeat with
`TRUE` and with the supported time-series-only mode.

### F10 — CLI help recommends an unsupported processing step and omits stream selection

**Severity:** P2 — worth fixing. **Evidence:** Confirmed. **Affected
components:** `inst/BrainGnomes:22`, `inst/BrainGnomes:53`,
`inst/BrainGnomes:61`, `R/run_project.R:107`,
`R/run_bids_validation.R:3`.

**Problem.** Both global help and `run_project --help` recommend:

``` text
BrainGnomes run_project ... --steps='bids_validation fmriprep'
```

[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
does not support `bids_validation`; the package explicitly implements it
as standalone
[`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md).
The CLI also does not expose or document the R API’s `extract_streams`
selection capability.

**Installed-package reproduction.**

``` text
run_project(..., steps = "bids_validation", dry_run = TRUE)

Error: Unknown processing step(s): bids_validation. Valid choices are:
all, flywheel_sync, bids_conversion, mriqc, fmriprep, aroma,
postprocess, extract_rois
```

The installed command help contains the invalid example and has no
`--extract_streams` option.

**Why it matters.** A new user who copies the first substantive help
example receives an immediate failure. CLI users cannot select ROI
streams even though the README says extraction streams are selectable.

**Recommended fix.** Replace the invalid examples with supported stage
names, document standalone BIDS validation clearly, and add
`--extract_streams` if command-line parity with
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
is intended.

**Recommended regression test.** Parse every documented CLI example and
validate its processing-step names against the real supported-step list.
Add a CLI integration test showing that a selected extraction stream
reaches `run_project(extract_streams = ...)`.

### F11 — Multiple public help pages omit signatures or contain nonworking examples

**Severity:** P2 — worth fixing. **Evidence:** Confirmed. **Affected
components:** `R/RcppExports.R:43`, `R/RcppExports.R:70`,
`src/image_quantile.cpp:31`, `man/run_project.Rd:57`,
`man/extract_rois.Rd:51`, generated public help pages.

**Problem and reproduced examples.**

1.  Seven exported native-backed functions have help pages without a
    `\usage{}` section: `automask`, `filtfilt_cpp`, `image_quantile`,
    `lmfit_residuals_4d`, `natural_spline_4d`, `natural_spline_interp`,
    and `remove_nifti_volumes`.

2.  [`image_quantile()`](https://hallquistlab.github.io/BrainGnomes/reference/image_quantile.md)
    documents `image_quantile("bold.nii.gz", 0.5)`, but its second
    argument is `brain_mask`:

    ``` text
    image_quantile(existing_image, 0.5)
    Error: Expecting a single string value: [type=double; extent=1].
    ```

3.  [`automask()`](https://hallquistlab.github.io/BrainGnomes/reference/automask.md)
    documents `outfile` as optional with default `""`, but the actual R
    wrapper requires it:

    ``` text
    automask(existing_image)
    Error: argument "outfile" is missing, with no default
    ```

4.  The
    [`automask()`](https://hallquistlab.github.io/BrainGnomes/reference/automask.md)
    example calls nonexistent `automask_rcpp()`.

5.  The
    [`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
    example supplies nonexistent argument `prompt = TRUE`:

    ``` text
    run_project(cfg, prompt = TRUE)
    Error: unused argument (prompt = TRUE)
    ```

6.  [`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md)
    documents Fisher-transformed diagonal entries as `15`, but the
    actual written diagonal is `NA, NA`. The same help text incorrectly
    describes untransformed correlations as bounded by `[0, 1]` rather
    than `[-1, 1]`.

Most problematic examples are wrapped in `\dontrun{}`, so the passing
`R CMD check` example phase does not validate them.

**Why it matters.** Users cannot reliably discover signatures or
reproduce the examples for core imaging functions. Contradictory
scientific-output documentation increases downstream interpretation
mistakes.

**Recommended fix.** Regenerate or reorganize roxygen so exported
functions have real usage sections. Correct positional arguments, remove
nonexistent names and parameters, reconcile optional defaults with
implementation, and document the actual Fisher-diagonal convention.

**Recommended regression test.** Enumerate exported functions and assert
that each corresponding help topic contains a usage signature. Add tiny
executable image examples and test documentation examples against actual
function formals where external data or cluster access is not needed.

### F12 — A hidden local artifact enters the release tarball and produces a check NOTE

**Severity:** P2 — worth fixing. **Evidence:** Confirmed. **Affected
components:** `.Rbuildignore:13`, generated source tarball.

**Problem.** A source build from the working tree contains a top-level
`.codex` entry. `R CMD check` reports:

``` text
* checking for hidden files and directories ... NOTE
Found the following hidden files and directories:
  .codex
These were most likely included in error.
```

The final ignore expression does not match the actual hidden entry
correctly. The current source build also includes the untracked
`inst/dev/` tree, whereas the Git-clean build does not, producing
materially different release artifacts.

**Why it matters.** Release tarballs depend on local working-tree state
and contain unintended content. The resulting NOTE and nondeterministic
artifact contents obscure whether the checked tarball represents the
public source.

**Recommended fix.** Correct the `.Rbuildignore` expression for
`.codex`, decide explicitly whether `inst/dev/` is a tracked package
resource or a development-only exclusion, and build release candidates
from clean tracked checkouts.

**Recommended regression validation.** List the source tarball contents
during CI, reject unintended hidden entries, compare release resources
against tracked files, and require a check without the hidden-file NOTE.

### F13 — Empty-input and missing-value contracts are inconsistent

**Severity:** P2 — worth fixing. **Evidence:** Confirmed. **Affected
components:** `R/bids_functions.R:74`, `R/status_functions.R:92`,
`src/image_quantile.cpp:49`.

**Problem.** Several public helpers return surprising results on
ordinary empty or missing inputs:

``` r

BrainGnomes::extract_bids_info(character())
# Observed class: list
# Observed names: "directory"
# Documented return: data.frame

BrainGnomes::get_project_status(empty_project_cfg)
# Observed: data.frame with zero rows AND zero columns
# Expected useful schema: at least sub_id/ses_id and configured status columns

BrainGnomes::image_quantile(existing_image, quantiles = NA_real_)
# Observed: named NA with label "nan%"
# Documented input contract: probabilities in [0, 1]
```

**Why it matters.** Empty BIDS collections and projects with no
submitted jobs are realistic states. Unstable return schemas complicate
wrappers, reports, and dashboards; missing quantile probabilities are
accepted instead of being diagnosed.

**Recommended fix.** Return typed, schema-stable empty data frames;
define the expected status columns even when no subjects exist; and
reject missing/non-finite probability values before image calculations.

**Recommended regression test.** Cover `character(0)` BIDS input, a
project with no subject log directories, and `NA_real_`/`NaN` quantile
probabilities. Assert output class, column names, types, and informative
validation errors.

### F14 — An installed legacy ROI script cannot start

**Severity:** P2 — worth fixing. **Evidence:** Confirmed. **Affected
components:** `inst/ROI_TempCorr.R:64`, `inst/ROI_TempCorr.R:73`.

**Problem.** The installed legacy `ROI_TempCorr.R` helper immediately
sources `R_helper_functions.R` from its own directory. That file is not
installed with the package.

**Installed-package reproduction.** Running the installed helper with no
arguments exits with status 1 because `R_helper_functions.R` cannot be
found, before its own usage/help path executes.

**Why it matters.** The package ships a sizeable, discoverable ROI entry
point that is unusable. Users who encounter the installed script receive
an unrelated missing-file error rather than guidance toward the
supported
[`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md)
workflow.

**Recommended fix.** Remove the obsolete helper from shipped package
resources if unsupported, or supply its actual dependencies and make its
help path work without requiring unavailable files. Clearly identify the
supported ROI entry points.

**Recommended regression test.** Inventory installed user-facing helper
scripts and assert each supported entry point either displays usable
help or executes a minimal documented command successfully.

### F15 — External runtime prerequisites and local limitations are not represented in package metadata

**Severity:** P2 — UX/design concern. **Evidence:** Confirmed for
missing metadata; UX/design concern for impact. **Affected components:**
`DESCRIPTION:7`, `README.md:16`, six vignettes.

**Problem.** The README describes an HPC scheduler, container images,
licenses, and data prerequisites, but `DESCRIPTION` has no
`SystemRequirements` field. The package’s one-sentence `Description`
also omits the scheduler/container requirement, Python/TemplateFlow
involvement, and the distinction between a normally installable R
package and a cluster-dependent full pipeline. There is no installed
miniature configuration or runnable no-cluster onboarding example.

**Why it matters.** Users can install and load the package successfully
but cannot readily determine which workflows are usable in their current
environment, what prerequisites correspond to each stage, or how to
verify configuration without an existing study.

**Recommended fix.** Add accurate stage-dependent `SystemRequirements`,
expand the package description, and document a prerequisites matrix.
Provide a tiny installed example configuration and a clearly supported
dry-run or pure-R walkthrough that does not require an actual study
dataset.

**Recommended validation.** Review installation and dry-run instructions
from a clean R session and confirm that a new user can identify required
software, optional components, configuration shape, and the first usable
command without inspecting source code.

## 4. Usability findings

### What currently works well

The README accurately describes the broad purpose and intended HPC
audience, distinguishes the major pipeline stages, includes a plausible
setup/run/status sequence, and links to six substantive guides. The
package exposes dedicated diagnosis and status functions, and the
dry-run concept is discoverable. Installation, loading, vignette
creation, and most package checks work in an environment where
dependencies are already available.

### Highest-impact onboarding and workflow friction

1.  **A copied CLI help example fails.** Both installed help surfaces
    recommend `bids_validation` inside
    [`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md),
    but that step is standalone and rejected. Replace those examples and
    explicitly show
    [`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md).
2.  **The configuration UI promises behavior that production execution
    does not honor.** Selected ROI correlation methods, masks, and
    `save_ts` settings do not survive consistently from setup to
    execution. Show a human-readable resolved extraction plan during dry
    run, including selected estimators, masks, input streams, and
    expected outputs.
3.  **The advertised time-series-only option is not usable.** Setup and
    help offer `none`, but direct execution rejects it and the scheduled
    helper can silently choose Pearson. Make this a fully supported
    first-class configuration.
4.  **The status command omits the last stage.** Users following the
    README cannot tell whether ROI extraction has completed. Include
    per-extraction-stream status in project summaries and the CLI.
5.  **Public imaging help is not self-sufficient.** Seven exported help
    topics omit signatures, and examples call nonexistent names or pass
    incorrect arguments. Users must inspect source or function formals
    to use documented operations.
6.  **Several failure messages describe implementation artifacts rather
    than user mistakes.** An all-excluded atlas reports
    `replacement has 30 rows, data has 0`; multiple input streams report
    only `bids_desc` length. Explain which stream, atlas, mask, or
    setting is inconsistent and what action resolves it.
7.  **There is no low-friction first task independent of a real study.**
    Almost all workflow vignette code is not evaluated. Provide a
    minimal sample config, synthetic pure-R image example, or genuinely
    usable dry-run tutorial.
8.  **BIDS validation’s place in the workflow is inconsistent.** The
    README lists it alongside stages, the CLI implies it is a selectable
    stage, and its function help says it is standalone. State the
    distinction consistently in the README, Quickstart, CLI, and
    function reference.

## 5. API consistency review

| Area | Interface A | Interface B | Observed inconsistency | Recommended contract |
|----|----|----|----|----|
| ROI correlation configuration | Interactive setup stores `correlation$method`. | Installed extraction helper reads `cor_method`. | Scheduled execution defaults to Pearson and ignores user selection. | Define one canonical field and translate explicitly at every boundary. |
| Correlation filename entities | Abbreviation normalization produces `correlation`. | Filename ordering expects `cor`. | Method entities disappear and output files collide. | Use `correlation` internally and `cor-...` only as the serialized prefix. |
| Disable connectivity | Setup, help, and vignette accept `"none"`. | [`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md) rejects it. | Documented time-series-only workflow fails. | Normalize `"none"` once and propagate an explicit no-correlation state. |
| Multiple extraction inputs | Setup and vignette allow multiple postprocessing streams. | [`get_postproc_output_files()`](https://hallquistlab.github.io/BrainGnomes/reference/get_postproc_output_files.md) accepts only one `bids_desc`. | Production extraction fails for multiple streams. | Process paired specifications/descriptions or document a genuine one-stream restriction. |
| Time-series output | Setup stores `save_ts`. | Installed helper omits the argument. | `FALSE` becomes the public function’s default `TRUE`. | Preserve explicit output flags across setup, YAML, and installed execution. |
| ROI masks | Setup prompts for a mask. | Returned configuration retains the previous value. | User input is silently lost. | Save, reload, display, and apply the exact selected mask. |
| ROI labels | Help promises preserved labels and stable matrix size. | Implementation and one current test delete fully excluded labels. | Subject-level matrices may have different dimensions. | Choose and test one explicit cross-subject label-alignment contract. |
| Project stages | [`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md) supports ROI extraction. | Status functions omit it. | Final-stage completion is unobservable in the standard status API. | Expose every enabled stage/stream in project and subject status. |
| BIDS validation | CLI examples treat validation as a pipeline step. | R API documents standalone [`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md). | Copyable CLI examples fail. | Decide whether validation is standalone or integrated and document it uniformly. |
| Stream selection | R API exposes `extract_streams`. | CLI has no corresponding option. | CLI cannot express a documented R workflow. | Add the option or explicitly document the reduced CLI capability. |
| Native-backed functions | Function formals and actual defaults are available programmatically. | Seven help pages omit usage signatures and some describe different defaults. | Documentation is insufficient to call the public API correctly. | Generate accurate signatures directly from the exported wrappers. |
| Fisher transformation | Help promises diagonal values of `15`. | Written matrices contain diagonal `NA`. | Downstream expectations and example interpretations disagree. | Adopt and document one diagonal convention. |

## 6. Testing gaps

### Highest priority: protect scientific outputs and release artifacts

1.  **Clean-checkout installed-package release test.** Build only
    tracked files, install into a temporary library, and run
    `R CMD check`. Assert no helper is obtained from an adjacent source
    tree or untracked directory.
2.  **Method-specific BIDS filename reconstruction.** Cover both `cor`
    and `correlation`, every supported estimator, custom ROI entities,
    and uniqueness of all generated output paths.
3.  **Estimator-specific numeric integration tests.** Use deterministic
    ROI signals with intentionally different Pearson and Spearman
    values. Compare each written file with its actual requested
    estimator rather than testing only that a file exists.
4.  **Installed extraction-helper configuration integration.** Generate
    project-shaped YAML containing nested `correlation$method`, masks,
    `save_ts`, overwrite settings, and multiple estimators. Execute the
    installed helper and inspect both outputs and values.
5.  **Disabled-correlation workflow.** Test interactive configuration,
    YAML round trip, direct extraction, and installed execution for
    `"none"`; assert no unwanted connectivity output.
6.  **Multiple postprocessing input streams.** Use distinct input
    patterns and BIDS descriptions, then assert that each stream
    contributes exactly its intended files.
7.  **Extraction-mask setup and execution.** Mock setup prompts,
    save/reload the configuration, and assert that the actual extracted
    time series changes when the mask changes.
8.  **ROI identity across masking scenarios.** Exercise partially
    removed labels, labels with zero surviving voxels, all-excluded
    atlases, percentage thresholds, and multiple participant masks.
    Assert the documented matrix-shape policy.
9.  **Extraction-stage status integration.** Verify subject status,
    project status, summaries, and CLI output for queued, complete, and
    incomplete extraction streams.

### Secondary priority: user-facing contracts and edge cases

10. **Executable documentation smoke tests.** Ensure each public help
    topic has usage, and run tiny examples for `automask`,
    `image_quantile`, and other pure-R/native-backed helpers.
11. **CLI example consistency.** Validate every processing step
    mentioned in help against the actual supported step list; cover
    extraction-stream selection.
12. **Configuration-driven output flags.** Check both `save_ts = TRUE`
    and `FALSE` through the installed extraction helper.
13. **Empty return schemas and missing values.** Cover empty BIDS
    filename vectors, projects without subjects, and invalid/missing
    quantile probabilities.
14. **Fisher-transform output conventions.** Assert documented diagonal
    values, off-diagonal transformations, and preservation of missing
    ROIs.
15. **Installed helper resource inventory.** Identify which installed
    scripts are supported and verify their documented help or minimal
    invocation without relying on absent resources.

### Why the current suite misses the most important defects

Existing ROI tests request one correlation method at a time and assert
that a file exists, but do not verify the filename contains the method
or that different methods yield distinct numeric output. Current CLI
tests inspect help formatting, not whether its examples represent valid
workflows. There is no integration test connecting project-shaped
extraction configuration to the actual installed helper. One ROI test
explicitly encodes label deletion despite contradictory user
documentation. Consequently, focused installed tests can pass while
severe scientific-output and production-workflow problems remain.

## 7. Documentation improvements

1.  **README:** Clarify that BIDS validation is standalone unless the
    implementation changes; describe which steps are represented in
    project status; document command-line setup and extraction-stream
    limitations accurately.
2.  **Quickstart:** Include one internally consistent sequence showing
    configuration, dry run, standalone BIDS validation, selected
    processing stages, and final status checks. Add an example that can
    be followed without an existing lab-specific dataset.
3.  **ROI extraction vignette:** Update method-specific filename
    examples only after F02 is fixed; show direct and configured nested
    method selection; document the exact behavior of `none`, `save_ts`,
    masks, multiple input streams, and all-missing atlas labels.
4.  **ROI output documentation:** State whether ROI columns are
    preserved across subjects, define the Fisher-transform diagonal, and
    describe how missing ROIs appear in both time-series and
    connectivity files.
5.  **Native imaging help:** Restore usage signatures for all exported
    wrappers, correct `automask_rcpp` to the actual exported function,
    align `outfile` documentation with its real default, and call
    [`image_quantile()`](https://hallquistlab.github.io/BrainGnomes/reference/image_quantile.md)
    with `quantiles = 0.5`.
6.  **[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
    help:** Remove the nonexistent `prompt` argument from examples,
    accurately enumerate supported stages, and cross-link standalone
    [`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md).
7.  **CLI help:** Replace invalid validation examples, explain
    standalone validation, expose or document the absence of
    `--extract_streams`, and show the actual output/selection flags
    available to users.
8.  **DESCRIPTION:** Add a meaningful `SystemRequirements` entry and
    expand the package description to distinguish R installation from
    stage-specific HPC, container, Python, and imaging prerequisites.
9.  **Troubleshooting:** Add actionable examples for empty masks, absent
    atlas overlap, multiple-stream selection, failed release checks, and
    configurations that intentionally omit connectivity output.

## 8. Quick wins

1.  Change the BIDS entity-order entry from `cor` to `correlation` and
    add a one-line method-entity regression test.
2.  Pass `cfg$correlation$method` and `cfg$save_ts` into
    [`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md)
    from the installed extraction helper.
3.  Assign the mask prompt directly to `excfg$mask_file`.
4.  Handle `cor_method = "none"` explicitly before method matching.
5.  Replace invalid `bids_validation` examples in the installed CLI
    help.
6.  Correct the `.Rbuildignore` entry so the hidden local artifact does
    not enter source builds.
7.  Fix the stale `run_project(prompt = TRUE)` and
    `image_quantile(image, 0.5)` examples.
8.  Update the Fisher-diagonal documentation to match the chosen
    implementation.
9.  Relocate or guard development-only calibration tests so
    `R CMD check` does not depend on an untracked adjacent helper.
10. Add an extraction status column for each enabled stream, following
    the existing postprocessing-stream pattern.

## 9. Release checklist

### Must fix before release

Make `R CMD check` pass from a pristine Git checkout and an isolated
installed package.

Eliminate source-relative and untracked calibration-helper dependencies
from release tests.

Generate unique, correctly labeled connectivity paths for every
requested correlation estimator.

Preserve configured correlation methods from interactive setup through
YAML and installed extraction execution.

Verify each written connectivity matrix numerically against the
requested estimator.

Support or clearly reject multiple extraction input streams consistently
with project setup and documentation.

Make documented time-series-only extraction (`"none"`) behave
consistently in direct and scheduled workflows.

Persist and apply the ROI mask chosen during project setup/editing.

Resolve the documented ROI-label preservation contract and handle fully
excluded atlases explicitly.

Include enabled ROI extraction in project and subject progress
reporting.

### Should fix before release

Honor `save_ts = FALSE` in the installed extraction helper.

Replace unsupported CLI examples and expose/document extraction-stream
selection.

Restore usage signatures and repair demonstrably broken public examples.

Remove the hidden-file NOTE and make working-tree and Git-clean build
contents consistent.

Return schema-stable empty BIDS/status results and validate missing
quantile probabilities.

Reconcile Fisher-transform documentation with actual written diagonals.

Declare stage-specific external requirements and improve first-run
onboarding.

Remove, repair, or clearly classify the unusable legacy ROI helper.

### Safe to defer

Further reduce build-time compiler noise after correctness and
release-gate issues are resolved.

Improve minor vignette wording, typography, and cross-link polish.

Expand nonessential convenience examples beyond the core reproducible
workflows.

Run additional real-cluster/container integration scenarios once the
local installed-package defects are fixed; keep those tests opt-in and
environment-guarded.

## Appendix A. Minimal reusable ROI fixture

The following fixture recreates two ROI time series for which Pearson
and Spearman correlations are materially different. It can be used to
build regression tests for F02, F03, F05, F06, F07, and F09.

``` r

library(BrainGnomes)

root <- tempfile("braingnomes-audit-")
dir.create(root)
input_dir <- file.path(root, "input")
dir.create(input_dir)

bold_file <- file.path(input_dir, "sub-01_task-rest_desc-clean_bold.nii.gz")
atlas_file <- file.path(root, "DemoAtlas.nii.gz")

set.seed(2718)
dims <- c(4L, 3L, 2L, 30L)
x <- seq(-2, 2, length.out = dims[[4]])
bold <- array(0, dim = dims)

for (i in seq_len(dims[[4]])) {
  bold[1:2, , , i] <- 100 + x[[i]] + rnorm(12, sd = 0.03)
  bold[3:4, , , i] <- 100 + x[[i]]^3 + rnorm(12, sd = 0.03)
}

atlas <- array(0L, dim = dims[1:3])
atlas[1:2, , ] <- 1L
atlas[3:4, , ] <- 2L

RNifti::writeNifti(RNifti::asNifti(bold), bold_file)
RNifti::writeNifti(RNifti::asNifti(atlas), atlas_file)

result <- extract_rois(
  bold_file = bold_file,
  atlas_files = atlas_file,
  out_dir = root,
  cor_method = c("pearson", "spearman"),
  min_vox_per_roi = 1L
)[[1]]

paths <- unlist(result$correlation)
stopifnot(length(unique(paths)) == length(paths))

series <- read.delim(result$timeseries, check.names = FALSE)
for (method in names(result$correlation)) {
  observed <- as.matrix(
    read.delim(result$correlation[[method]], check.names = FALSE)
  )[1, 2]
  expected <- cor(series$roi1, series$roi2, method = method)
  stopifnot(isTRUE(all.equal(observed, expected, tolerance = 1e-7)))
}
```

At the audited revision, the unique-path assertion fails. If that
assertion is removed, the reported Spearman path contains approximately
`0.916869` even though the correct Spearman value is approximately
`0.998665`.

## Appendix B. Reproduction artifacts from this audit

Audit build products and fixture scripts were kept outside the
repository at:

``` text
/tmp/braingnomes-release-audit.fjQexh6E/
```

Useful artifacts, while that temporary directory remains available,
include:

``` text
BrainGnomes_0.8-2.tar.gz                Full working-tree source build.
BrainGnomes.Rcheck/00check.log          Complete release-check log.
BrainGnomes.Rcheck/tests/testthat.Rout.fail
                                         Installed test failure details.
audit-library/BrainGnomes/              Independently installed package.
clean-source/                           Git-tracked-only source snapshot.
clean-build/BrainGnomes_0.8-2.tar.gz    Git-clean source build.
audit_installed.R                       Installed-workflow comparison script.
fixtures/                               Synthetic NIfTI/YAML examples and outputs.
```

The report above contains the findings and reproductions needed for a
follow-up maintainer even if those temporary artifacts are later
removed.
