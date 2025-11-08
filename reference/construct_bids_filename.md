# Construct BIDS-Compatible Filenames from Extracted Entity Data

Given a data frame of BIDS entities (as returned by
[`extract_bids_info()`](https://uncdependlab.github.io/BrainGnomes/reference/extract_bids_info.md)),
this function reconstructs filenames following the BIDS specification.
It supports standard BIDS entities including subject, session, task,
run, acquisition, space, resolution, and description, along with the
suffix and file extension.

## Usage

``` r
construct_bids_filename(bids_df, full.names = FALSE)
```

## Arguments

- bids_df:

  A `data.frame` containing one or more rows of BIDS entities. Must
  include at least the columns `suffix` and `ext`, and optionally:
  `subject`, `session`, `task`, `acquisition`, `run`, `modality`,
  `echo`, `direction`, `reconstruction`, `hemisphere`, `space`,
  `resolution`, `description`, and `fieldmap`.

- full.names:

  If TRUE, return the full path to the file using the `$directory` field
  of `bids_df`.

## Value

A character vector of reconstructed BIDS filenames, one per row of
`bids_df`.

## Details

Column names in `bids_df` may be provided either as full BIDS entity
names (e.g., `reconstruction`, `description`) or using their abbreviated
forms (`rec`, `desc`, etc.); abbreviated names are normalized
internally.

## See also

[`extract_bids_info()`](https://uncdependlab.github.io/BrainGnomes/reference/extract_bids_info.md)
for extracting BIDS fields from filenames.

## Examples

``` r
df <- data.frame(
  subject = "01", task = "rest", space = "MNI152NLin6Asym",
  resolution = "2", description = "preproc", suffix = "bold", ext = ".nii.gz",
  stringsAsFactors = FALSE
)
construct_bids_filename(df)
#> [1] "sub-01_task-rest_space-MNI152NLin6Asym_res-2_desc-preproc_bold.nii.gz"
# Returns: "sub-01_task-rest_space-MNI152NLin6Asym_res-2_desc-preproc_bold.nii.gz"
```
