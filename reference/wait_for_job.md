# Wait for scheduler jobs or local processes to finish

Poll Slurm, TORQUE/PBS, or local process status until every supplied job
reaches a known terminal state. This can coordinate a parent R script
with child jobs when scheduler dependencies alone are insufficient.

## Usage

``` r
wait_for_job(
  job_ids,
  repolling_interval = 60,
  max_wait = 60 * 60 * 24,
  scheduler = "local",
  quiet = TRUE,
  stop_on_timeout = TRUE
)
```

## Arguments

- job_ids:

  One or more job ids of existing PBS or slurm jobs, or process ids of a
  local process for `scheduler="sh"`.

- repolling_interval:

  How often to recheck the job status, in seconds. Default: 60.

- max_wait:

  How long to wait on the job before giving up, in seconds. Default: 24
  hours (86,400 seconds)

- scheduler:

  What scheduler is used for job execution. Options: c("torque", "qsub",
  "slurm", "sbatch", "sh", "local")

- quiet:

  If `TRUE`, `wait_for_job` will not print out any status updates on
  jobs. If `FALSE`, the function prints out status updates for each
  tracked job so that the user knows what's holding up progress.

- stop_on_timeout:

  Logical. If `TRUE`, the function throws an error if the `max_wait` is
  exceeded. If `FALSE`, it returns `FALSE` instead of stopping. Default
  is `TRUE`.

## Value

Invisibly returns `TRUE` if all jobs completed successfully, or `FALSE`
if any job failed or was cancelled. A timeout raises an error when
`stop_on_timeout = TRUE`; otherwise it returns `FALSE`.

## Details

Note that for the `scheduler` argument, "torque" and "qsub" are the
same; "slurm" and "sbatch" are the same, and "sh" and "local" are the
same. This function waits on scheduler observations, not the project's
SQLite tracking state. Missing Slurm or TORQUE records are not evidence
of success: waiting continues until a known terminal state or the
timeout. In particular, expired TORQUE records cannot establish
successful completion. Confirmed terminal states are retained for this
wait invocation while other jobs finish, so subsequent accounting expiry
does not erase that evidence.

## Author

Michael Hallquist

## Examples

``` r
if (FALSE) { # \dontrun{
# example on qsub/torque cluster
wait_for_job("7968857.torque01.util.production.int.aci.ics.psu.edu", scheduler = "torque")

# example of waiting for two jobs on slurm cluster
wait_for_job(c("24147864", "24147876"), scheduler = "slurm")

# example of waiting for two jobs on local machine
wait_for_job(c("9843", "9844"), scheduler = "local")
} # }
```
