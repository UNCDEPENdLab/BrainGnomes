# Run-wise Intensity Normalization in BrainGnomes

## Overview

Functional MRI intensities are expressed in arbitrary scanner units. Two
runs can contain the same fractional BOLD response but have very
different raw values because of receiver gain, coil sensitivity,
reconstruction, or other acquisition details. This is inconvenient for
task-regression coefficients: the same underlying response may produce
coefficients with different numerical scales across runs and
participants.

BrainGnomes addresses the run-wide part of this problem by applying
**one positive multiplier to each 4D run**. At the point when scaling is
applied, the multiplier sets the robust intensity summary of stable
reference voxels to a user-selected target, conventionally 10,000. The
same multiplier is applied to every voxel and volume in that run.

The approach has three defining properties:

1.  Before postprocessing, BrainGnomes selects a conservative set of
    stable, positive-signal functional voxels to serve as the
    **reference region**. This is an intensity-calibration mask, not an
    anatomical ROI or an analysis mask.
2.  BrainGnomes holds those reference voxels fixed, but measures their
    intensity after masking and spatial smoothing. The multiplier
    therefore reflects the spatially processed image that will enter
    temporal denoising.
3.  The resulting factor is applied immediately, before AROMA,
    interpolation, temporal filtering, confound regression, or timepoint
    removal.

This makes intensity normalization straightforward for users while
protecting against division by a near-zero reference intensity, unstable
voxels, and variable external masks.

## Quick start

Intensity normalization is configured separately for each postprocessing
stream. Interactive project setup asks whether to enable it and prompts
for the target. The equivalent YAML structure is:

``` yaml
postprocess:
  task:
    intensity_normalize:
      enable: true
      target: 10000
      prefix: "n"
```

The default target is 10,000. Use the same target for every run and
participant that will contribute coefficients to the same group
analysis.

Existing configurations may contain `global_median` instead of `target`:

``` yaml
intensity_normalize:
  enable: true
  global_median: 10000
  prefix: "n"
```

BrainGnomes continues to accept this legacy field, but `target` is now
the canonical name. The older name is potentially misleading because the
current method does not target the median of every value in the 4D
image.

## What exactly does the target mean?

The target refers to a two-stage summary calculated at the
**normalization point**: after the enabled masking and smoothing steps,
but before temporal denoising. BrainGnomes first identifies which
volumes can contribute to this calculation. These **baseline-estimation
volumes** include all volumes by default; when the required metadata are
available, non-steady-state volumes and volumes marked for censoring are
excluded.

Let $`B`$ denote the indices of the baseline-estimation volumes. For
each voxel $`v`$ in the fixed reference region, BrainGnomes estimates
its temporal baseline from the image at the normalization point:

``` math
\hat\mu_v = \operatorname{trimmed\ mean}\{y_{vt}:t\in B\}.
```

The lowest and highest 10% of that voxel’s included observations are
removed before calculating the mean. BrainGnomes then takes the spatial
median of these voxelwise temporal baselines to obtain the run reference
intensity:

``` math
L = \operatorname{median}\{\hat\mu_v:v\in M_{\mathrm{ref}}\},
```

where $`M_{\mathrm{ref}}`$ is the fixed reference region. Given the
user’s target $`T`$, the run multiplier is

``` math
s = \frac{T}{L}.
```

At the normalization point, BrainGnomes calculates

``` math
y^*_{vt}=s\,y_{vt}
```

for every voxel and volume in the current postprocessed image.

Thus, at the instant normalization is applied, a target of 10,000 means:

> The spatial median across reference-region voxels of their 10%-trimmed
> temporal means equals 10,000 after multiplication.

It does **not** mean that a voxel’s temporal median, the whole-brain
mean, or the median of every value in the final 4D image must equal
10,000. Those quantities also depend on tissue composition, masks,
dropout, and later operations that change or remove the temporal mean.

Changing the target only changes the unit convention. For example,
changing the target from 10,000 to 1,000 divides every normalized value
and regression coefficient by 10. It does not improve signal quality or
statistical power.

## Where normalization occurs and why

BrainGnomes separates **choosing the reference voxels** from **measuring
their intensity**. It chooses the reference voxels and
baseline-estimation volumes from the input BOLD series, before
BrainGnomes postprocessing. This input must still have a finite,
positive temporal baseline; data that have already been demeaned or
residualized are unsuitable. The membership of the reference region is
then held fixed so that later processing cannot change which anatomical
locations determine the run’s scale.

The run reference intensity is measured only after spatial operations
that can change the included voxels or redistribute intensity. The
multiplier is then applied before temporal operations whose effects can
differ substantially by run:

``` text
input BOLD with a positive temporal baseline
        |
        +--> choose baseline-estimation volumes
        |                 |
        |                 +--> functional automask and temporal QA
        |                                      |
        |                                      +--> fixed reference voxels
        |
        +--> apply mask --> spatial smoothing
                              |
                              +--> measure run reference intensity L
                                   in the fixed reference voxels
                              +--> multiply immediately by T/L
                                         |
                                         +--> AROMA/interpolation/filtering/
                                              confound regression/scrubbing
```

If masking or smoothing is disabled, BrainGnomes simply normalizes after
the last enabled spatial operation. It does not permit a forced
processing order that places AROMA, interpolation, temporal filtering,
confound regression, or timepoint removal before normalization, or that
places masking or smoothing afterward.

This boundary prevents two different problems. Measuring before masking
or smoothing could produce a multiplier that no longer describes the
spatially processed image. Measuring after denoising could instead make
the multiplier depend on run-specific nuisance burden: a noisy run may
change substantially under regression while a clean run changes very
little. BrainGnomes therefore puts all runs into common units *before*
those run-specific temporal effects occur. It does not renormalize
afterward, because doing so could amplify the more strongly affected run
and obscure a meaningful data-quality difference.

For a linear processing operation $`F`$, scalar multiplication commutes
with that operation: $`F(sy)=sF(y)`$. Subsequent linear denoising can
therefore affect runs by different amounts while retaining the common
input units.

This ordering follows the design boundary in AFNI’s `afni_proc.py`: its
default blocks place `blur` and `mask` before `scale`, and `scale`
before `regress`; the script explicitly warns when blur follows scale or
regression precedes scale. The numerical operation differs, however.
AFNI’s default `scale` block performs voxelwise mean-100 scaling with an
upper cap, whereas BrainGnomes applies one uncapped run-wide multiplier,
estimated robustly, to the entire 4D run. The shared principle is the
placement of scaling between spatial preprocessing and temporal
modeling.

## How the reference region is constructed

The reference region is designed to identify a stable population of
functional voxels for estimating one number. It is intentionally
different from a mask used to retain every potentially analyzable brain
voxel. In output filenames and metadata, this region is called the
**reference core**.

The following subsections document BrainGnomes’ fixed internal
safeguards. Users do not need to tune these thresholds.

### 1. Select volumes for baseline estimation

All volumes contribute by default. When corresponding metadata are
available, BrainGnomes excludes:

- volumes marked by fMRIPrep `non_steady_state_outlier*` columns; and
- volumes excluded by the postprocessing censor vector.

Excluded volumes do not contribute to estimation of the temporal
baseline, noise, or intensity spikes. They still receive the same
multiplier as the rest of the run. Temporal differences used for QA are
calculated only between consecutive included observations with finite
values; BrainGnomes does not calculate a difference across an excluded
volume.

### 2. Generate a conservative functional automask

BrainGnomes runs
[`automask()`](https://uncdependlab.github.io/BrainGnomes/reference/automask.md)
on the input 4D BOLD image that retains its positive temporal baseline.
The 4D image is collapsed to a mean functional image and thresholded
using an AFNI-style iterative intensity-threshold estimator. Local
thresholds are estimated in eight overlapping spatial regions and
smoothly interpolated, reducing sensitivity to receive-field gradients
and regional signal differences.

The initial mask is reduced to its largest connected component and
cleaned with AFNI-style peeling:

- a voxel survives peeling when at least 17 of its 18 face- or
  edge-sharing neighbors survive;
- removed layers are restored only when they reconnect to the surviving
  core; and
- the largest face-connected component is retained after restoration.

For normalization, the fixed automask policy is deliberately
conservative:

``` text
clfrac        = 0.5
connectivity  = face-connected for component selection
peels         = 1
fill holes    = no
dilate        = no
```

Not filling holes is important because an enclosed region may represent
real susceptibility dropout rather than a segmentation defect. Not
dilating avoids bringing low-intensity boundary or background voxels
into the scale estimate.

Substantial nonfinite data (missing, infinite, or `NaN` values) should
be treated as a data-quality problem. Reference-region construction
explicitly measures finite coverage, but users should still investigate
runs containing widespread nonfinite BOLD values rather than assuming
masking can repair them.

### 3. Remove temporally unsuitable voxels

For every automask voxel, BrainGnomes calculates temporal QA from the
volumes selected for baseline estimation. A voxel must satisfy all of
the following:

1.  **Sufficient data.** At least 20 finite included observations and at
    least two valid consecutive differences are required.

2.  **Finite positive baseline.** The 10% trimmed temporal mean must be
    finite and greater than zero.

3.  **Adequate baseline.** The baseline must be at least 20% of the
    spatial median positive baseline in the candidate mask. This removes
    extreme low-signal voxels without attempting to classify tissue.

4.  **Non-extreme relative noise.** Robust relative noise is estimated
    as

    ``` math
    \frac{1.4826\;\operatorname{median}(|\Delta y|)}
         {\sqrt{2}\;\hat\mu_v}.
    ```

    Its cutoff is estimated across baseline-valid voxels on the log
    scale as the spatial median plus five scaled median absolute
    deviations.

5.  **Small spike fraction.** A temporal difference is extreme when its
    deviation from the median difference exceeds six scaled median
    absolute deviations. Both volumes adjacent to an extreme difference
    are marked, and no more than 5% of included volumes may be marked
    for a retained voxel.

These settings are internal guardrails, not user-facing tuning options.
A single versioned policy is easier to apply consistently across
projects than a large menu of poorly identified thresholds.

### 4. Enforce run-level safety checks

Normalization stops with an informative error when:

- the conservative automask contains fewer than 100 voxels;
- fewer than 50% of candidate voxels survive reference-region
  refinement;
- no finite positive run reference intensity can be estimated; or
- the requested target or resulting multiplier is not finite and
  positive.

BrainGnomes does not replace an invalid reference intensity with 1, add
an arbitrary offset, or silently switch to a different mask.

## Why no fMRIPrep or template mask is required

An external mask is not necessary to estimate a run multiplier. The
reference voxels need to represent a stable biological population, but
they do not need to occupy corresponding coordinates across
participants.

This is particularly useful for native-space data, where a template mask
may not match the BOLD grid, and for fMRIPrep data whose
`desc-brain_mask` quality varies across runs. BrainGnomes instead
derives its candidate mask directly from the functional image and
applies temporal stability criteria to that candidate.

The following mask roles remain separate:

| Mask | Purpose | BrainGnomes policy |
|----|----|----|
| Reference region (saved as the reference core) | Defines the run multiplier | Conservative functional automask plus temporal QA |
| Processing mask | Supports smoothing and other processing checks | More generous automask; may fill holes and dilate |
| Apply mask | Determines which voxels remain in the output | User, functional, anatomical, or matching template mask |

Changing `apply_mask` can appropriately change the measured run
reference intensity because normalization is performed after that mask
is applied. It does not redefine the reference region, which is still
selected from the input image. If masking leaves fewer than half of the
fixed reference voxels with a finite positive baseline, normalization
stops rather than switching reference populations silently. The
reference region is not applied to the 4D data as an analysis mask;
valid analysis voxels outside it remain in the output and receive the
same run multiplier.

## Outputs and provenance

When normalization is enabled, BrainGnomes saves two files beside the
final postprocessed BOLD output. Their BIDS entities match the input
run:

``` text
sub-01_task-example_run-1_space-..._desc-intensityReferenceCore_mask.nii.gz
sub-01_task-example_run-1_space-..._desc-intensityReferenceCore_mask.json
```

The NIfTI file is the final binary reference core. The JSON sidecar
records:

- method identifier (`automask_reference_core_v1`);
- original source BOLD file and the image present at the normalization
  point;
- normalization stage and the spatial steps completed before it;
- target, run reference intensity (`reference_location`), and applied
  scale factor;
- numbers of volumes used for baseline estimation and total volumes;
- candidate and core voxel counts and retained fraction;
- internal automask and temporal-QA settings;
- data-derived thresholds;
- counts surviving each reference-core criterion; and
- the number and fraction of fixed reference voxels usable when scaling
  is applied.

The temporary conservative automask is not retained. The core and its
sidecar contain the information needed for routine QA and factor
provenance.

If `validate_postproc_steps` is enabled, BrainGnomes also verifies that:

1.  `reference_location * scale_factor` equals the requested target;
2.  the pre- and post-normalization images have the same dimensions and
    finite value pattern;
3.  every finite output value equals its input value times the fixed
    factor, within numerical tolerance; and
4.  remeasurement using the same reference voxels and
    baseline-estimation volumes confirms that the image at the
    normalization point reaches the requested target.

## Interpreting normalized analyses

Suppose the signal at a voxel can be written approximately as

``` math
y_{rvt}=g_r b_{rv}\{1+p_{rv}x_t\}+e_{rvt},
```

where $`g_r`$ is run-wide acquisition gain, $`b_{rv}`$ is the local
baseline, and $`p_{rv}`$ is fractional BOLD response. A task coefficient
in raw units is approximately

``` math
\beta_{rv}=g_r b_{rv}p_{rv}.
```

A well-chosen run multiplier substantially removes $`g_r`$, making
coefficient units more consistent across runs. It does not remove the
local baseline term $`b_{rv}`$. Consequently:

- normalized task coefficients remain sensitive to regional
  receive-field variation, tissue mixture, partial volume, and
  susceptibility dropout;
- a single run-wide multiplier is not bias-field correction or scanner
  harmonization; and
- run-wise normalization is not the same as voxelwise percent signal
  change.

Multiplying a complete run by a positive scalar multiplies its beta
estimates, residual standard deviations, COPEs, and VARCOPEs in the
corresponding units. Within-run $`t`$ statistics are unchanged apart
from numerical precision. Correlations are also unchanged, so run-wise
intensity normalization is usually irrelevant for analyses based only on
correlation coefficients.

If the scientific estimand is local fractional signal change, a
carefully guarded voxelwise percent-signal-change transformation is more
direct. That is a different operation and requires stricter protection
against low local temporal baselines. The BrainGnomes `target` option
implements only the run-multiplier method described here.

## Choosing and evaluating the target

For most projects, keep the default target of 10,000. The exact number
is a convention; consistency is more important than whether the
convention is 10,000, 1,000, or 100.

Do not choose a different target for different subjects, tasks,
scanners, or runs. Doing so would reintroduce arbitrary unit
differences. If an external analysis pipeline expects a specific scale,
choose that scale once and document it.

Recommended project-level QA includes:

- plotting the multiplier (`scale_factor`) and run reference intensity
  (`reference_location`) by scanner, site, task, subject, and run;
- checking for associations between the factor and motion, mask size, or
  retained reference-voxel fraction;
- inspecting reference-core masks for severe truncation or extracranial
  inclusion;
- flagging unusually small reference regions or factors far from the
  project distribution; and
- confirming that first-level $`t`$ statistics remain unchanged while
  beta and uncertainty units change as expected.

A factor that differs from the rest of the project is not automatically
wrong: it may correctly remove a different acquisition gain. It is a
prompt to inspect the run, reference-region mask, and source
intensities.

## Troubleshooting

### The output’s global median is not 10,000

This is expected. At the point when normalization is applied, the target
applies to the spatial median across reference-region voxels of their
10%-trimmed temporal means, not to the global median of all output
values. Later temporal operations can also change summary statistics
while preserving the calibrated input units.

### The reference core is much smaller than the brain

The saved reference-core mask contains a stable population used only to
estimate the run multiplier; it is not an analysis mask. It may exclude
valid but low-signal or noisy analysis voxels. Those voxels remain in
the BOLD output unless removed by a separate apply mask.

### There is no fMRIPrep brain mask

No action is needed. The normalization reference is constructed from the
BOLD data and works in native or template space.

### Normalization fails because too few voxels survive

Inspect the input BOLD image and saved or logged diagnostics. Common
causes include severe dropout, partial or malformed coverage, widespread
nonfinite values, fewer than 20 usable volumes, or an input whose
temporal baseline was already removed. Do not repair this by adding a
constant or forcing scaling with a very small reference intensity.

### The scale factor is extremely large or small

Inspect `reference_location`, `core_fraction`, and the core mask in the
JSON sidecar. Confirm that the source is the preprocessed BOLD image
retaining a finite, positive temporal baseline and that all runs use the
same target. Extreme factors should be treated as QA flags rather than
automatically clipped.

### Should normalization occur before or after filtering?

Before filtering. BrainGnomes measures and applies the factor after the
final enabled spatial operation and before any temporal denoising. It
never estimates a reference intensity from filtered or residualized data
and does not apply a second corrective factor after temporal processing.

## Summary

BrainGnomes intensity normalization removes one arbitrary multiplicative
degree of freedom per run. It selects a stable set of reference voxels
from the input BOLD image, measures their run reference intensity after
spatial preprocessing, and immediately multiplies the full run by
`target / reference_location` before temporal denoising. Users choose
only the target; reference-region construction, placement, and quality
thresholds follow a fixed internal policy.

This produces more consistent coefficient units without claiming to
equalize local tissue intensities, repair dropout, or create
percent-signal-change data. The saved reference-core mask and JSON
sidecar make the transformation explicit and auditable.
