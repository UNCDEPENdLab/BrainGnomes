# Helper function to obtain all subject and session directories from a root folder

Helper function to obtain all subject and session directories from a
root folder

## Usage

``` r
get_subject_dirs(
  root = NULL,
  sub_regex = "[0-9]+",
  sub_id_match = "([0-9]+)",
  ses_regex = NULL,
  ses_id_match = "([0-9]+)",
  full.names = FALSE
)
```

## Arguments

- root:

  The path to a root folder containing subject folders.

- sub_regex:

  A regex pattern to match the subject folders. Default: `"[0-9]+"`.

- sub_id_match:

  A regex pattern for extracting the subject ID from the subject folder
  name. Default: `"([0-9]+)"`.

- ses_regex:

  A regex pattern to match session folders. Default: `NULL`. If `NULL`,
  session folders are not expected.

- ses_id_match:

  A regex pattern for extracting the session ID from the session folder
  name. Default: `"([0-9]+)"`.

- full.names:

  If `TRUE`, return absolute paths to the folders; if `FALSE`, return
  paths relative to `root`. Default: `FALSE`.

## Value

A data frame with one row per subject (or per subject-session
combination) and columns:

- `sub_id`: Subject ID extracted from each folder name.

- `ses_id`: Session ID (or `NA` if no session level).

- `sub_dir`: Path to the subject folder.

- `ses_dir`: Path to the session folder (`NA` if no session).

## Details

This function is used to find all subject folders within a root folder.
It is used internally by the package to find the subject DICOM and BIDS
folders for processing. The function uses the `list.dirs` function to
list all directories within the folder and then filters the directories
based on the regex patterns provided. The function returns a character
vector of the subject folders found.

The function also extracts the subject and session IDs from the folder
names using the regex patterns provided. The IDs are extracted using the
`extract_capturing_groups` function, which uses the `regexec` and
`regmatches` functions to extract the capturing groups from the folder
names. The function returns a data frame with the subject and session
IDs and the corresponding folder paths.

## Examples

``` r
if (FALSE) { # \dontrun{
  get_subject_dirs(root = "/path/to/root", sub_regex = "[0-9]+", sub_id_match = "([0-9]+)",
                   ses_regex = "ses-[0-9]+", ses_id_match = "([0-9]+)", full.names = TRUE)
} # }
```
