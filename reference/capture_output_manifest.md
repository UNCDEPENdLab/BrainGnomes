# Capture output manifest for a completed step

Scans an output directory and creates a JSON manifest containing file
paths, sizes, and modification times. This manifest can be stored in the
job tracking database and later used to verify that outputs remain
intact.

## Usage

``` r
capture_output_manifest(output_dir, recursive = TRUE, pattern = NULL)
```

## Arguments

- output_dir:

  Directory to scan for output files

- recursive:

  Scan subdirectories (default TRUE)

- pattern:

  Optional regex pattern to filter files

## Value

JSON string containing the manifest, or NULL if directory doesn't exist
