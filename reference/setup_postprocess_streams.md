# Configure postprocessing settings for a study

This function enables and configures the postprocessing steps to be
applied after fMRIPrep. Postprocessing may include denoising, smoothing,
filtering, intensity normalization, and confound regression applied to
preprocessed BOLD data.

## Usage

``` r
setup_postprocess_streams(scfg = list(), fields = NULL)
```

## Arguments

- scfg:

  A project configuration object, as produced by
  [`setup_project()`](https://uncdependlab.github.io/BrainGnomes/reference/setup_project.md).

- fields:

  A character vector of field names to prompt for. If `NULL`, all
  postprocessing fields will be prompted.

## Value

A modified version of `scfg` with the `$postprocess` field populated.

Modified `scfg` with one or more postprocessing streams

## Details

The function interactively prompts the user (or selectively prompts
based on `fields`) to specify whether postprocessing should be
performed, and if so, how each step should be configured.

Postprocessing is applied to the outputs of fMRIPrep to prepare BOLD
time series for statistical modeling. This may include:

- Applying brain masks

- Spatial smoothing

- ICA-AROMA denoising

- Temporal filtering

- Intensity normalization

- Confound calculation and regression

Each step is optional and configurable. This function sets default
values for memory, runtime, and cores, and invokes a series of sub-setup
functions to collect postprocessing parameters.

Interactively manage multiple postprocessing configurations. Users can
add, edit, or delete postprocessing streams. This wrapper is called by
[`setup_project()`](https://uncdependlab.github.io/BrainGnomes/reference/setup_project.md)
and invokes `setup_postprocess_stream()` for each stream.
