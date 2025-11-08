# Construct a Regular Expression for Matching BIDS Filenames

This function constructs a regular expression to match BIDS-compatible
filenames based on a set of entity-value pairs. The resulting pattern
allows for intermediate unspecified entities between matched ones and
ensures correct ordering based on the BIDS specification. It supports
both full entity names and their common abbreviations (e.g., `sub`,
`ses`, `task`, `desc`, etc.).

## Usage

``` r
construct_bids_regex(spec, add_niigz_ext = TRUE)
```

## Arguments

- spec:

  A character string specifying BIDS entities as `key:value` pairs
  separated by spaces. For example:
  `"sub:01 task:rest desc:preproc suffix:bold"`. Abbreviated keys are
  supported. To pass a custom regex directly, prefix the input with
  `"regex:"` (e.g., `"regex:^sub-.*_bold\\.nii\\.gz$"`).

- add_niigz_ext:

  Logical; if `TRUE` (default), automatically appends a regex that
  matches `.nii` or `.nii.gz` extensions (`\\.nii(\\.gz)?$`) when no
  extension is explicitly provided.

## Value

A character string containing a regular expression pattern that matches
BIDS filenames containing the specified entities in order, allowing
intermediate unspecified fields.

## Details

- If no `suffix` is provided, the regex will match any suffix (e.g.,
  `"bold"`, `"T1w"`, etc.).

- If no `ext` is provided and `add_niigz_ext = TRUE`, the pattern will
  match `.nii` or `.nii.gz`.

- Unescaped periods in user-supplied extensions are automatically
  escaped to avoid unintended matches.

- Intermediate fields (e.g., `_acq-lowres_`) are allowed between
  specified components using the `(_[^_]+)*_` pattern.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Match a bold preprocessed run for subject 01 with any other intermediate fields allowed
  construct_bids_regex("sub:01 task:rest desc:preproc suffix:bold")

  # Match any file that ends in .nii or .nii.gz
  construct_bids_regex("suffix:bold")

  # Use an explicit regex pattern
  construct_bids_regex("regex:^sub-[0-9]+_task-rest_.*\\.nii\\.gz$")
} # }
```
