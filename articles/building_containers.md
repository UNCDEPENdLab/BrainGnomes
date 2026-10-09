# Building Singularity containers for BrainGnomes

## Before downloading images

Obtain images only for the stages you plan to enable. Use your cluster’s
provided images when suitable; otherwise download a version you have
tested with your study. Use explicit release tags where available. If an
upstream project only offers a moving tag, retain the downloaded image
and its checksum for the study rather than repeatedly replacing it from
`latest` or `main`.

BrainGnomes batch scripts invoke `singularity`. Apptainer is suitable
when your site supplies that compatible command. Load the site’s runtime
module as needed and check `singularity --version` before proceeding.

Some clusters require container commands to run on compute nodes. Follow
your site’s policy; for example, request an interactive Slurm
allocation:

``` bash
srun --pty -N 1 -n 1 --mem=16g --time=01:00:00 bash
```

Add your site’s account or partition options if required. Downloads need
internet access, enough disk space, and a destination readable by the
compute nodes that will run BrainGnomes. If compute nodes cannot access
the internet, download on a permitted host and copy the images to shared
storage.

The examples below use `/path/to/shared/containers` and
`REPLACE_WITH_VERSION` as placeholders. Replace them before running the
commands. `.sif` filenames make the selected versions visible; existing
compatible `.simg` images can also be configured.

``` bash
container_dir="/path/to/shared/containers"
mkdir -p "$container_dir"
```

## HeuDiConv

Needed for DICOM-to-BIDS conversion. Select a released tag from the
[HeuDiConv installation
guide](https://heudiconv.readthedocs.io/en/latest/installation.html).
You also need a study-specific Python heuristic; the image does not
supply that study configuration.

``` bash
heudiconv_version="REPLACE_WITH_VERSION"
singularity pull "$container_dir/heudiconv-${heudiconv_version}.sif" \
  "docker://nipy/heudiconv:${heudiconv_version}"
```

## fMRIPrep

Needed for preprocessing a BIDS dataset. Choose your tested release and
save the image path for setup. See the [NiPreps Singularity
guide](https://www.nipreps.org/apps/singularity/) for container
preparation, bind mounts, and environment variables.

``` bash
fmriprep_version="REPLACE_WITH_VERSION"
singularity pull "$container_dir/fmriprep-${fmriprep_version}.sif" \
  "docker://nipreps/fmriprep:${fmriprep_version}"
```

The image does not replace the FreeSurfer license or TemplateFlow cache.
Populate the necessary templates before submission when compute nodes
lack internet access; see [restricted-network
guidance](https://www.nipreps.org/apps/singularity/#restricted-internet-access).

## MRIQC

Needed when MRIQC is enabled. Choose a tested release rather than a
moving tag:

``` bash
mriqc_version="REPLACE_WITH_VERSION"
singularity pull "$container_dir/mriqc-${mriqc_version}.sif" \
  "docker://nipreps/mriqc:${mriqc_version}"
```

## fMRIPost-AROMA

Needed when the separate ICA-AROMA stage is enabled. It consumes
fMRIPrep outputs; denoising during BrainGnomes postprocessing is
configured separately. See the [fMRIPost-AROMA installation
guide](https://fmripost-aroma.readthedocs.io/latest/installation.html).

``` bash
aroma_tag="main"  # upstream installation example; use a tested release tag if available
singularity pull "$container_dir/fmripost-aroma-${aroma_tag}.sif" \
  "docker://nipreps/fmripost-aroma:${aroma_tag}"
md5sum "$container_dir/fmripost-aroma-${aroma_tag}.sif"
```

The upstream [installation
example](https://github.com/nipreps/fmripost-aroma#installation) uses
the moving `main` tag. Keep the downloaded file fixed for the study and
retain its checksum; the tag alone does not identify its contents.

## FSL

Needed for postprocessing. One image used in development is the
NeuroDesk FSL 6.0.7.16 image; this is a specific example, not a
requirement to update an existing tested installation. Download it to
your chosen shared directory:

``` bash
curl --fail --location \
  --output "$container_dir/fsl_6.0.7.16_20250131.simg" \
  https://neurocontainers.neurodesk.org/fsl_6.0.7.16_20250131.simg
```

## BIDS validator executable

BrainGnomes configures a validator **executable**, rather than a
container, and submits validation separately with
[`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md).
The current [BIDS validator CLI
guide](https://bids-validator.readthedocs.io/en/stable/user_guide/command-line.html)
uses Deno and provides a standalone compilation option. Install or load
Deno following your site’s policy, then compile on a host with network
access:

``` bash
deno compile -ERWN -o "$container_dir/bids-validator" jsr:@bids/validator
"$container_dir/bids-validator" --help
```

Record the validator version used by your study. Set
`compute_environment$bids_validator` to the executable’s absolute path
during setup. The validator’s `--outfile` controls its report
destination; an `.html` filename does not itself select HTML output.
Consult the chosen validator version’s help for its supported formats.

## Use the downloaded files in BrainGnomes

Enter the absolute image/executable paths during
[`setup_project()`](https://hallquistlab.github.io/BrainGnomes/reference/setup_project.md)
or update an existing configuration with
[`edit_project()`](https://hallquistlab.github.io/BrainGnomes/reference/edit_project.md).
Images and project paths must be accessible from compute nodes, not only
the submission host. `doctor_project(scfg)` provides an optional
read-only environment check before submission.

For container troubleshooting, reproduce the relevant batch script’s
bind mounts and environment in an interactive allocation. The files
under `inst/hpc_scripts/` define the runtime invocation; obtaining an
image alone does not establish that a site’s mounts, licenses, and
template cache are ready.
