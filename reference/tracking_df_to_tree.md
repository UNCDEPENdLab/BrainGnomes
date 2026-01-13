# helper for converting tracking data.frame into a multi-level data.tree hierarchy

helper for converting tracking data.frame into a multi-level data.tree
hierarchy

## Usage

``` r
tracking_df_to_tree(tracking_df)
```

## Arguments

- tracking_df:

  A data.frame returned by get_tracked_job_status()

## Value

A named list of data.tree Node objects (one per sequence_id)
