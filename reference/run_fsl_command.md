# Run an FSL command with optional Singularity container support and structured logging

Executes an FSL command in a clean shell environment, with support for
Singularity containers, optional logging via the `lgr` package, and
flexible control over execution and output.

## Usage

``` r
run_fsl_command(
  args,
  fsldir = NULL,
  echo = TRUE,
  run = TRUE,
  intern = FALSE,
  stop_on_fail = TRUE,
  log_file = NULL,
  use_lgr = TRUE,
  fsl_img = NULL,
  bind_paths = NULL
)
```

## Arguments

- args:

  A character vector specifying the FSL command and arguments to run
  (e.g., `"fslmaths input.nii.gz -add 1 output.nii.gz"`).

- fsldir:

  Optional. Path to the FSL installation directory. If `NULL`, the
  function attempts to infer it from the environment or system
  configuration.

- echo:

  Logical. Whether to print the command to standard output. Defaults to
  `TRUE`.

- run:

  Logical. Whether to execute the command. If `FALSE`, returns as if the
  command succeeded. Defaults to `TRUE`.

- intern:

  Logical. If `TRUE`, returns the standard output of the command as a
  character vector with `"retcode"` attribute. If `FALSE`, returns only
  the exit code. Defaults to `FALSE`.

- stop_on_fail:

  Logical. If `TRUE`, stops execution if the FSL command returns a
  non-zero exit code. Defaults to `TRUE`.

- log_file:

  Optional. Path to a log file for recording output. Ignored if
  `use_lgr = TRUE`.

- use_lgr:

  Logical. Whether to use the `lgr` logging framework. If `TRUE`,
  configures and logs to console and/or file appenders. Defaults to
  `TRUE`.

- fsl_img:

  Optional. Path to a Singularity image containing FSL. If provided, the
  command is executed within the container using `singularity exec`.

- bind_paths:

  Optional. Character vector of additional directories to bind inside
  the Singularity container. The current working directory and temp
  directory are automatically added.

## Value

If `intern = FALSE`, returns the exit code (integer) of the command. If
`intern = TRUE`, returns the standard output as a character vector. In
both cases, `"stdout"` and `"stderr"` are attached as attributes.

## Details

This function sets up the FSL environment by sourcing `fsl.sh`, and can
optionally run FSL commands inside a Singularity container. Logging can
be directed to both console and file using `lgr`, or to a file using
basic [`cat()`](https://rdrr.io/r/base/cat.html) logging if
`use_lgr = FALSE`.

If `intern = TRUE`, the command output is returned and tagged with
attributes `stdout`, `stderr`, and `retcode`. The `bind_paths` argument
ensures that relevant file paths are visible inside the container.

## Examples

``` r
if (FALSE) { # \dontrun{
run_fsl_command("fslmaths input.nii.gz -add 1 output.nii.gz")
run_fsl_command("fslhd input.nii.gz", intern = TRUE)
} # }
```
