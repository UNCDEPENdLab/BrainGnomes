# Summarize project status

Provides a tabular summary of completion counts for each step in the
pipeline.

## Usage

``` r
# S3 method for class 'bg_status_df'
summary(object, ...)
```

## Arguments

- object:

  A data.frame produced by
  [`get_project_status()`](https://uncdependlab.github.io/BrainGnomes/reference/get_project_status.md).

- ...:

  Additional arguments (unused)

## Value

data.frame summarizing number of subjects completed for each step.
