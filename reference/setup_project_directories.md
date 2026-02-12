# Ensure required project directories exist

Given a project configuration, create any missing directories referenced
in the metadata, including the project root and standard subdirectories.
A message is emitted for each directory created, and existing but
unreadable directories trigger a warning. After creation, every
directory is verified writable; for directories owned by the current
user an attempt is made to add the user-write bit via
`ensure_user_writable`.

## Usage

``` r
setup_project_directories(scfg, check_cache = NULL)
```

## Arguments

- scfg:

  A project configuration object.

- check_cache:

  Optional environment used to memoize write-permission results.
  Directories verified writable here will be recorded so that downstream
  preflight checks (e.g. in `collect_submit_permission_issues`) can skip
  redundant [`file.access()`](https://rdrr.io/r/base/file.access.html)
  calls.

## Value

Invisibly returns `scfg`.
