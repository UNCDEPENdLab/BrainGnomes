# Compute spike regressors for volume censoring

Evaluates user-supplied expressions against a confounds data.frame to
generate spike (one-hot) regressors. Expressions may optionally include
a semicolon-separated range of volumes to also flag (e.g. "-1:1;
framewise_displacement \> 0.5").

## Usage

``` r
compute_spike_regressors(confounds_df = NULL, spike_volume = NULL, lg = NULL)
```

## Arguments

- confounds_df:

  Data frame of confounds with one row per volume.

- spike_volume:

  Character vector of expressions to evaluate.

- lg:

  Logger object for messages.

## Value

Matrix of spike regressors or NULL if none detected.
