# Extract fields from BIDS filenames

Extract fields from BIDS filenames

## Usage

``` r
extract_bids_info(filenames, drop_unused = FALSE)
```

## Arguments

- filenames:

  A character vector of BIDS file names (or paths).

- drop_unused:

  Logical; if `TRUE`, drop any BIDS entities that are not present in any
  of the filenames.

## Value

A data.frame containing the BIDS key-value fields extracted from each
filename (each row corresponds to an input filename).

## Details

Based on the BIDS specification for file naming (see BIDS documentation
appendix on entities). For more detail, see:
https://bids-specification.readthedocs.io/en/stable/appendices/entities.html

This function recognizes standard BIDS entities such as subject
(`sub-`), session (`ses-`), task (`task-`), acquisition (`acq-`), run,
modality (`mod-`), echo (`echo-`), direction (`dir-`), reconstruction
(`rec-`), hemisphere (`hemi-`), space (`space-`), resolution (`res-`),
description (`desc-`), and fieldmap (`fmap-`), as well as the file
suffix and extension.

## Examples

``` r
filenames <- c(
  "sub-01_ses-02_task-memory_space-MNI2009c_acq-highres_desc-preproc_bold.nii.gz",
  "acq-lowres_desc-smoothed_sub-02_task-attention_run-2_bold.nii.gz",
  "sub-03_space-MNI152NLin6Asym_task-motor_desc-raw_echo-2_dir-PA_bold.nii.gz"
)
extract_bids_info(filenames)
#>   subject session      task acquisition reconstruction direction  run modality
#> 1      01      02    memory     highres             NA      <NA> <NA>       NA
#> 2      02    <NA> attention      lowres             NA      <NA>    2       NA
#> 3      03    <NA>     motor        <NA>             NA        PA <NA>       NA
#>   echo hemisphere           space resolution description fieldmap suffix
#> 1 <NA>         NA        MNI2009c         NA     preproc       NA   bold
#> 2 <NA>         NA            <NA>         NA    smoothed       NA   bold
#> 3    2         NA MNI152NLin6Asym         NA         raw       NA   bold
#>       ext directory
#> 1 .nii.gz         .
#> 2 .nii.gz         .
#> 3 .nii.gz         .
```
