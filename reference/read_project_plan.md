# Read a saved execution plan

Read a saved execution plan

## Usage

``` r
read_project_plan(file)
```

## Arguments

- file:

  YAML plan path.

## Value

A `bg_project_plan` object.

## Details

Ordinary plans use schema v1; exact retry plans use v2 so older
BrainGnomes versions cannot silently ignore their work-unit
restrictions.
