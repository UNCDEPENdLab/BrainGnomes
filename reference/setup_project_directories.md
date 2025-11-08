# Ensure required project directories exist

Given a project configuration, create any missing directories referenced
in the metadata, including the project root and standard subdirectories.
A message is emitted for each directory created, and existing but
unreadable directories trigger a warning.

## Usage

``` r
setup_project_directories(scfg)
```

## Arguments

- scfg:

  A project configuration object.

## Value

Invisibly returns `scfg`.
