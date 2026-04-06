# Validate scrub timepoints (output TR count vs censor)

Post TRs should equal pre TRs minus scrubbed count. Pass censor read
**before** the step if the file is overwritten.

## Usage

``` r
validate_scrub_timepoints(pre_file, post_file, censor_vec)
```

## Arguments

- pre_file:

  Path to 4D BOLD before `scrub_timepoints`.

- post_file:

  Path to 4D BOLD after `scrub_timepoints`.

- censor_vec:

  Censor vector length = pre TRs (1 = keep, 0 = drop).

## Value

A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
Attributes: `message`, `details` (`n_pre_t`, `n_post_t`, `n_removed`).
