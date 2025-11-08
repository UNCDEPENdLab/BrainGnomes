# Check for Existence of a BIDS-Formatted Output File with a given description

This function constructs a BIDS-compliant filename based on an input
file, replacing the `desc` field with a specified `description`, and
checks whether the corresponding output file already exists. If the file
exists and `overwrite = FALSE`, the function returns `skip = TRUE`.

## Usage

``` r
out_file_exists(in_file, description, overwrite = TRUE)
```

## Arguments

- in_file:

  Path to the input BIDS file (e.g., a preprocessed BOLD image).

- description:

  Character string to use as the new `desc` field in the expected output
  file.

- overwrite:

  Logical. If `FALSE`, existing files will not be overwritten.

## Value

A list with elements:

- out_file:

  Path to the expected output file.

- skip:

  Logical indicating whether to skip writing due to file existence.
