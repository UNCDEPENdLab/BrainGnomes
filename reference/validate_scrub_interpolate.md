# Validate scrub interpolate (dims + finite at filled TRs)

Same 4D shape as pre; interpolated TRs (censor 0) must be finite in
post.

## Usage

``` r
validate_scrub_interpolate(pre_file, post_file, censor_file)
```

## Arguments

- pre_file:

  Path to 4D BOLD before `scrub_interpolate`.

- post_file:

  Path to 4D BOLD after `scrub_interpolate`.

- censor_file:

  Censor file (1 = keep, 0 = interpolate).

## Value

A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
Attributes: `message`, `details`.
