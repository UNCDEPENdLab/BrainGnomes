# Intensity Normalization in BrainGnomes

## Overview

Functional MRI intensities are expressed in arbitrary scanner units. Two
runs can contain the same fractional BOLD response but have very
different raw values because of receiver gain, coil sensitivity,
reconstruction, or other acquisition details. This is inconvenient for
task-regression coefficients: the same underlying response may produce
coefficients with different numerical scales across runs and
participants.

BrainGnomes offers two explicit intensity conventions. `run_scalar`
applies one positive multiplier to the complete 4D run and maps a robust
reference intensity to a user-selected target, conventionally 10,000.
`voxel_psc` applies a positive, denominator-guarded multiplier at each
voxel so that reliable local temporal baselines equal 100 and task
coefficients can be interpreted approximately as local percent signal
change.

Here, **guarded PSC** has a specific meaning. BrainGnomes uses ordinary
local PSC scaling, `100 / local_baseline`, whenever that baseline is
reliable. It replaces a very low positive denominator with a fixed lower
bound and replaces an unidentified denominator with the conservative
run-level denominator. The guard therefore prevents division by a tiny,
missing, or invalid baseline. It does **not** clip BOLD observations,
repair noisy data, impute a local baseline, or decide which voxels
remain in the image. Voxels that receive a floor or fallback remain
present, but their values are not exact local PSC.

The approach has four defining properties:

1.  Before postprocessing, BrainGnomes selects a conservative set of
    stable, positive-signal functional voxels to serve as the
    **reference region**. This is an intensity-calibration mask, not an
    anatomical ROI or an analysis mask.
2.  BrainGnomes holds those reference voxels fixed, but measures their
    intensity after masking and spatial smoothing. The multiplier
    therefore reflects the spatially processed image that will enter
    temporal denoising.
3.  Both modes use the same eligible volumes, robust 10%-trimmed
    temporal location, filename prefix, and post-spatial/pre-temporal
    checkpoint.
4.  The reference core is calibration support, not an analysis or
    PSC-validity mask. Only `apply_mask` determines which voxels are
    removed.

This makes intensity normalization straightforward for users while
protecting against division by a near-zero reference intensity, unstable
voxels, and variable external masks.

## Quick start

Intensity normalization is configured separately for each postprocessing
stream. Interactive setup asks for the mode and uses the same prefix
setting for either choice. The practical decision is:

- Choose `run_scalar` for conventional FSL-style run scaling and
  comparable target-based coefficient units. This is the default and
  does not make the data percent signal change.
- Choose `voxel_psc` when local fractional response size is the intended
  estimand and coefficients should approximate percent signal change.

Use the same mode for every run and participant in an analysis.
BrainGnomes handles reference estimation and low-signal safeguards
automatically.

Run-scalar configuration is:

``` yaml
postprocess:
  task:
    intensity_normalize:
      enable: true
      mode: run_scalar
      target: 10000
      prefix: "n"
```

The default target is 10,000. Use the same target for every run and
participant that will contribute coefficients to the same group
analysis.

Denominator-guarded PSC configuration is:

``` yaml
postprocess:
  task:
    intensity_normalize:
      enable: true
      mode: voxel_psc
      prefix: "n"
```

PSC always targets 100; a configured scalar `target` is ignored in this
mode. The prefix deliberately does not change with the mode. The
selected mode is recorded in the JSON provenance and processing log.
Because both modes use the same output naming convention, set
`overwrite: true` when rerunning an existing stream after changing its
mode; otherwise the normal pipeline cache rules may reuse the previously
generated output.

Existing configurations may contain `global_median` instead of `target`:

``` yaml
intensity_normalize:
  enable: true
  global_median: 10000
  prefix: "n"
```

BrainGnomes continues to accept this legacy field, but `target` is now
the canonical name. The older name is potentially misleading because the
current scalar method does not target the median of every value in the
4D image.

## How the two modes are calculated

The run-scalar target refers to a two-stage summary calculated at the
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

where $`M_{\mathrm{ref}}`$ is the fixed reference region. For
`run_scalar`, given the user’s target $`T`$, the run multiplier is

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

### Why trim 10% from each tail?

AFNI’s voxelwise scale block uses the ordinary temporal mean and then
caps individual scaled observations. BrainGnomes instead protects the
denominator while retaining a time-constant linear multiplier. Censoring
removes known bad volumes, but it cannot identify every regional spike
or transient dropout; one remaining extreme observation can bias every
scaled value and coefficient at that voxel.

The internal value `trim = 0.10` means that 10% is removed from each
tail and the central 80% is averaged. Development checks compared 0%,
5%, 10%, and 15% trimming. In 5,000 sampled mask voxels from each of two
long fMRIPrep runs, the 10%-trimmed baseline differed from the ordinary
mean by less than about 0.5–0.6% for 95% of voxels, while the
spatial-median run reference changed by no more than 0.01%. Across eight
low-noise 480-volume synthetic runs, the 95th percentile difference was
about 0.01–0.02%. Injected-spike checks showed a clear gain over the
ordinary mean and only small additional benefit beyond 10%.

| Development dataset | Eligible volumes | 95th percentile absolute change, 10% trim versus mean | 99th percentile | Spatial-median reference change |
|----|---:|---:|---:|---:|
| fMRIPrep clock run | 1,323 | 0.466% | 1.293% | -0.003% |
| fMRIPrep trust run | 1,084 | 0.605% | 1.717% | -0.010% |
| Eight low-noise synthetic runs | 480 each | 0.013–0.021% | 0.026–0.083% | -0.004% to -0.006% |

In a separate stress test, 5% of observations in 1,000 positive-baseline
synthetic voxel series were replaced by positive 10-SD spikes. Median
absolute baseline error fell from 0.066% with the ordinary mean to
0.015% with 5% trimming, 0.012% with 10%, and 0.011% with 15%. The
corresponding 95th percentiles were 0.281%, 0.060%, 0.056%, and 0.056%.
Thus, moving from no trim to 10% is consequential under contamination,
while moving from 10% to 15% buys little.

As an end-to-end check, a supplied 1,325-volume fMRIPrep run produced a
60,231-voxel reference core from a 60,640-voxel automask; every automask
voxel received ordinary PSC, while denominator guards were confined to
low-signal or background grid voxels outside that conservative
functional mask.

Ten percent is therefore a useful fixed compromise: it has negligible
effect on clean baselines, protects against residual voxel-specific
contamination, and is less aggressive than 15–20% trimming. It is an
internal policy rather than a user tuning parameter.

For `voxel_psc`, BrainGnomes calculates the same 10%-trimmed baseline
$`\hat\mu_v`$ at every voxel in the image present at the normalization
point. A reliable positive denominator receives

``` math
s_v = \frac{100}{\hat\mu_v}, \qquad y^*_{vt}=s_vy_{vt}.
```

The robust temporal baseline, rather than necessarily the ordinary
arithmetic mean, is therefore 100. In clean data these summaries are
nearly identical. The fixed value 100 is what makes a regression
coefficient interpretable in percentage-point units when the regressor
amplitude has the intended meaning.

### What “guarded PSC” means mathematically

Local PSC requires division by a voxel-specific baseline, so it needs
stronger denominator protection than `run_scalar`. Let $`F=0.20L`$,
where $`L`$ is the stable reference-core location. BrainGnomes applies
the internal policy

``` math
d_v =
\begin{cases}
\max(\hat\mu_v,F), & \hat\mu_v\text{ is finite, positive, and identified},\\
L, & \hat\mu_v\text{ is nonfinite, nonpositive, or insufficiently observed},
\end{cases}
\qquad s_v=100/d_v.
```

This creates three interpretable outcomes:

- **Ordinary PSC:** a finite positive baseline at or above $`F`$ uses
  $`100/\hat\mu_v`$. Its robust baseline is 100 and its coefficient has
  the usual approximate local PSC interpretation.
- **Denominator floor:** a finite positive baseline below $`F`$ uses
  $`100/F`$. Amplification is bounded, but the voxel is not exact local
  PSC.
- **Run-reference fallback:** a baseline that is nonfinite, nonpositive,
  or based on fewer than 20 finite eligible frames uses $`100/L`$. The
  voxel remains in broadly comparable run units but is not local PSC.

The word *guarded* describes only these denominator substitutions. Every
resulting multiplier is positive and constant over time. BrainGnomes
does not cap the scaled time series, replace individual observations,
zero guarded voxels, or apply a PSC validity mask. Counts in all three
categories are saved so users can distinguish exact robust local PSC
from retained guard cases.

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
                              +--> run_scalar: multiply by T/L
                                   voxel_psc: derive and apply 100/d_v
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

For a voxelwise linear temporal operation $`F`$, either time-constant
multiplier commutes with that operation. Subsequent linear denoising can
therefore affect runs by different amounts while retaining the units
established at this checkpoint.

### Why voxelwise PSC follows spatial smoothing

This issue is specific to spatially varying multipliers. With
`run_scalar`, the same constant $`s`$ applies everywhere, so a linear
spatial smoother satisfies $`S(sy)=sS(y)`$ and the two orders are
equivalent apart from implementation and rounding details.

Spatial smoothing and voxelwise PSC are different because the PSC
multiplier varies across voxels. To see this, let $`S`$ denote a spatial
weighted average and let $`m_v`$ be voxel $`v`$’s baseline. Scaling
before smoothing would produce

``` math
S\left(100\frac{y_{vt}}{m_v}\right),
```

which averages the neighboring voxels’ fractional time series. Smoothing
first and then scaling instead produces, approximately,

``` math
100\frac{S(y_{vt})}{S(m_v)}.
```

The second expression is the fractional change of the *smoothed signal*.
It weights a neighbor’s fractional response partly according to how much
baseline signal that neighbor contributes to the smoothed voxel. The two
expressions are equal only in special cases, such as when neighboring
baselines are the same. BrainGnomes uses a trimmed baseline and
contrast-sensitive SUSAN smoothing, so the displayed equations are an
intuitive linear approximation, but the ordering distinction remains.

This can matter substantively, not just cosmetically. Smoothing after
PSC would give a low-baseline or dropout-prone voxel approximately the
same spatial weight as a high-signal neighbor once both had been
normalized to 100. It could also spread the influence of a denominator
guard into adjacent voxels. BrainGnomes instead smooths the acquired
signal first and derives the local baseline and any denominator guard
from that same smoothed series. This generally makes the denominator
more stable and ensures that the PSC coefficient describes the exact
spatially processed signal supplied to the first-level model.

The corresponding interpretation is important: after smoothing, a beta
of 1 means approximately a one-percentage-point change relative to the
local *smoothed* baseline. It is not the simple spatial average of the
pre-smoothing voxels’ PSC values. Differences between the two orderings
should be small in locally homogeneous tissue when neighboring baselines
and fractional responses are similar. They can be larger near gray/white
or brain/CSF boundaries, near susceptibility dropout, across strong
receive-field gradients, around large vessels or lesions, and with
larger smoothing kernels. In those locations, smoothing itself already
produces partial-volume estimates; post-smoothing PSC makes those
mixed-signal units explicit rather than restoring pre-smoothing voxel
specificity.

Consequently, the chosen order is conventional and internally coherent,
but it does define the estimand and can change both coefficients and
statistics where spatial signals are heterogeneous. A project that
requires unsmoothed voxelwise PSC should disable spatial smoothing or
create a separate unsmoothed postprocessing stream, rather than
interpreting post-smoothing PSC as pre-smoothing voxel percent change.

This ordering follows the design boundary in AFNI’s `afni_proc.py`: its
default blocks place `blur` and `mask` before `scale`, and `scale`
before `regress`; the script explicitly warns when blur follows scale or
regression precedes scale. AFNI’s default `scale` block performs
voxelwise ordinary-mean scaling and caps individual output observations,
normally at 200. BrainGnomes `voxel_psc` instead guards the denominator
while retaining a time-constant linear multiplier, so it does not clip
response peaks. `run_scalar` remains the compatible run-wide
alternative. All modes share the placement of scaling between spatial
preprocessing and temporal modeling.

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
- the requested target or resulting scalar/PSC multiplier is not finite
  and positive.

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
| Reference region (saved as the reference core) | Defines the run multiplier and, for PSC, the denominator floor and fallback | Conservative functional automask plus temporal QA |
| Processing mask | Supports smoothing and other processing checks | More generous automask; may fill holes and dilate |
| Apply mask | Determines which voxels remain in the output | User, functional, anatomical, or matching template mask |

Changing `apply_mask` can appropriately change the measured run
reference intensity because normalization is performed after that mask
is applied. It does not redefine the reference region, which is still
selected from the input image. If masking leaves fewer than half of the
fixed reference voxels with a finite positive baseline, normalization
stops rather than switching reference populations silently. The
reference region is not applied to the 4D data as an analysis mask;
valid analysis voxels outside it remain in the output and receive either
the run multiplier or their own denominator-guarded PSC multiplier. The
guard protects division; it does not construct or apply a binary
validity mask. Noisy voxels remain noisy; deciding whether to exclude
them belongs to `apply_mask` or downstream analysis masking.

## Outputs and provenance

When normalization is enabled, BrainGnomes saves the reference core and
its JSON provenance beside the final postprocessed BOLD output. Their
BIDS entities match the input run:

``` text
sub-01_task-example_run-1_space-..._desc-intensityReferenceCore_mask.nii.gz
sub-01_task-example_run-1_space-..._desc-intensityReferenceCore_mask.json
```

The NIfTI file is the final binary reference core. The JSON sidecar
records:

- reference-core and normalization method identifiers and the selected
  mode;
- original source BOLD file and the image present at the normalization
  point;
- normalization stage and the spatial steps completed before it;
- target, run reference intensity (`reference_location`), and the scalar
  factor or PSC guard policy;
- numbers of volumes used for baseline estimation and total volumes;
- candidate and core voxel counts and retained fraction;
- internal automask and temporal-QA settings;
- data-derived thresholds;
- counts surviving each reference-core criterion; and
- the number and fraction of fixed reference voxels usable when scaling
  is applied.

`voxel_psc` additionally saves the exact positive multiplier map:

``` text
sub-01_task-example_run-1_space-..._desc-intensityNormalizationScale_map.nii.gz
```

The JSON defines guarded PSC explicitly and records the denominator
floor, maximum multiplier, reference fallback multiplier, and counts
receiving ordinary PSC, the low-baseline floor, insufficient-frame
fallback, or invalid- baseline fallback. It also labels which categories
are exact robust local PSC. Counts are reported for the complete grid
and separately within the conservative functional automask. The latter
is more useful for QA when the user intentionally leaves background
voxels unmasked.

The same category counts and percentages within the conservative
automask are written to the routine info-level processing log. The
complete-grid counts are written at debug level because they often
contain many intentional background voxels and can obscure the
functional-tissue summary.

The temporary conservative automask is not retained. The core and its
sidecar contain the information needed for routine QA and factor
provenance.

If `validate_postproc_steps` is enabled, BrainGnomes also verifies that:

1.  the pre- and post-normalization images have the same dimensions and
    finite value pattern;
2.  every finite output value equals its input value times the scalar or
    mapped factor, within numerical tolerance; and
3.  scalar mode maps `reference_location` to its requested target and
    remeasurement confirms it; or
4.  PSC mode uses a finite positive 3D multiplier map matching the BOLD
    spatial grid and a fixed target of 100.

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

With `voxel_psc`, a reliable voxel has

``` math
\beta^{PSC}_{rv}\approx100p_{rv}.
```

This removes both the run gain and the local baseline from the
coefficient scale. A beta of 1 corresponds approximately to a
one-percentage-point response for a unit-amplitude regressor. This
interpretation applies to ordinary-PSC voxels. Floor and fallback voxels
are retained specifically to avoid silently changing the analysis mask,
but they do not have exact local PSC interpretation; the provenance
counts indicate how often this occurred. PSC does not repair dropout,
increase temporal SNR, or harmonize neurovascular response. A positive
voxelwise multiplier leaves each voxel’s $`t`$ statistic unchanged apart
from numerical precision.

## Choosing and evaluating the mode and target

For compatible run-wide scaling, use `run_scalar` and keep the default
target of 10,000. The exact number is a convention; consistency is more
important than whether the convention is 10,000, 1,000, or 100.

Do not choose a different target for different subjects, tasks,
scanners, or runs. Doing so would reintroduce arbitrary unit
differences. If an external analysis pipeline expects a specific scale,
choose that scale once and document it.

Use `voxel_psc` when local fractional task amplitudes are the intended
first-level estimand. Its target is always 100 and is not user-tunable.
Use the same mode throughout a group analysis; scalar and PSC
coefficients are not the same units.

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
  beta and uncertainty units change as expected; and
- for PSC, plotting the fractions of automask voxels receiving the
  denominator floor or run-scalar fallback.

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

### A PSC voxel’s ordinary mean is not exactly 100

BrainGnomes targets the 10%-trimmed mean of eligible volumes. Censored
and non-steady-state volumes do not define the denominator, and residual
extremes do not dominate it. Consequently, the ordinary mean over every
volume need not be exactly 100. In clean voxels the difference should be
small.

### The reference core is much smaller than the brain

The saved reference-core mask contains a stable population used only to
estimate the run multiplier or the reference location $`L`$ that sets
the PSC floor and fallback; it is not an analysis mask. It may exclude
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

For PSC, inspect the saved multiplier map and `psc_guard` counts. A
large floored or fallback fraction inside the conservative automask
indicates that local percentage units are poorly identified in
substantial functional tissue. BrainGnomes reports this condition but
does not silently remove those voxels.

### Should normalization occur before or after filtering?

Before filtering. BrainGnomes measures and applies the factor after the
final enabled spatial operation and before any temporal denoising. It
never estimates a reference intensity from filtered or residualized data
and does not apply a second corrective factor after temporal processing.

## Summary

BrainGnomes selects a stable reference core from the original
positive-scale BOLD image and measures its intensity after spatial
preprocessing. At that checkpoint, `run_scalar` applies
`target / reference_location`, while `voxel_psc` applies a
denominator-guarded local multiplier targeting 100. A reliable baseline
receives ordinary PSC scaling, a very low positive baseline uses the
denominator floor, and an unidentified baseline uses a run-reference
fallback. These guards prevent unstable division without clipping
observations or masking voxels. Both modes use the same prefix, eligible
volumes, robust 10%-trimmed baseline, and placement before temporal
denoising.

The reference core is never an analysis or PSC-validity mask. The user’s
`apply_mask` choice controls spatial removal, while denominator floors
and fallbacks prevent unstable division without hiding the affected
voxels. Saved reference, multiplier, and JSON outputs make either
transformation auditable.
