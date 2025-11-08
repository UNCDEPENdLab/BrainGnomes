# Remove NIfTI files if they exist

Remove NIfTI files if they exist

## Usage

``` r
rm_niftis(files = NULL)
```

## Arguments

- files:

  A character vector of file paths (with or without `.nii` or `.nii.gz`
  extensions).

## Value

Invisibly returns `NULL`. Used for its side effect of deleting files.

## Details

Deletes one or more NIfTI files from disk. If a file path is provided
without an extension, `.nii.gz` is appended before checking for
existence.
