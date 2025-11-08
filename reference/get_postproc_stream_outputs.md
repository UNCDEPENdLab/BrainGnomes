# Determine expected output files for a postprocessing stream

Given a directory of candidate input files and a specification for
matching those inputs, this function returns the full paths to the
corresponding postprocessed NIfTI files. The output filenames are
derived by replacing the `desc` entity in each input file with the
stream's `bids_desc` value.

## Usage

``` r
get_postproc_stream_outputs(input_dir, input_regex, bids_desc)
```

## Arguments

- input_dir:

  Directory containing the input NIfTI files to be postprocessed.

- input_regex:

  Specification used to match the input files. This may be a
  space-separated set of BIDS entities (e.g., "desc:preproc
  suffix:bold") or a regular expression prefixed with "regex:".

- bids_desc:

  The `desc` value to use for the output filenames.

## Value

A character vector of full paths to the expected postprocessed NIfTI
files. The vector is named with the corresponding input file.

## Examples

``` r
if (FALSE) { # \dontrun{
get_postproc_stream_outputs(
  input_dir = "/path/to/subject",
  input_regex = "desc:preproc suffix:bold",
  bids_desc = "clean"
)
} # }
```
