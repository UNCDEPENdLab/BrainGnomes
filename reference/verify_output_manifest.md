# Verify current directory state against stored manifest

Compares the current contents of an output directory against a
previously captured manifest. This can detect missing files, size
changes, or other modifications that may indicate incomplete or
corrupted outputs.

## Usage

``` r
verify_output_manifest(output_dir, manifest_json, check_mtime = FALSE)
```

## Arguments

- output_dir:

  Directory to check

- manifest_json:

  JSON string from database (as returned by capture_output_manifest)

- check_mtime:

  Also verify modification times match (default FALSE)

## Value

List with:

- `verified`: logical indicating if all files match, or NA if no
  manifest

- `reason`: character string describing the result

- `missing`: character vector of missing file paths

- `changed`: character vector of files with size/mtime changes

- `extra`: character vector of files not in original manifest

- `manifest_time`: timestamp when manifest was captured
