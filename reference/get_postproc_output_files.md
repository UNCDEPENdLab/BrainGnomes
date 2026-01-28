# List postprocessed output files for a stream based on its input spec

Converts a postprocess input specification into a pattern that targets
postprocessed outputs by ensuring the `desc` entity matches `bids_desc`.

## Usage

``` r
get_postproc_output_files(input_dir, input_regex, bids_desc)
```

## Arguments

- input_dir:

  Directory containing postprocessed outputs.

- input_regex:

  Specification used to match the input files for the stream. May be a
  space-separated set of BIDS entities or a regex prefixed with
  "regex:".

- bids_desc:

  The `desc` value used for postprocessed outputs.

## Value

A character vector of full paths to matching postprocessed outputs.
