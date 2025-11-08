# Setup the compute environment for a study

Setup the compute environment for a study

## Usage

``` r
setup_compute_environment(scfg = list(), fields = NULL)
```

## Arguments

- scfg:

  a project configuration object, as produced by `load_project` or
  `setup_project`

## Value

a modified version of `scfg` with `$compute_environment` populated
