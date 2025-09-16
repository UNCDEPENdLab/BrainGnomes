#!/bin/bash
# Default SLURM requests if not overridden on command line
# These values are typically overridden by get_job_sched_args in R.
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --time=1:00:00
#SBATCH --mem=8g

set -eE  # 'E' ensures ERR trap inherits in functions/subshells

# source bash functions used in pipeline shell scripts (logging, traps)
[ -z "$pkg_dir" ] && echo "pkg_dir not set. Cannot locate required helper scripts" && exit 1
source "${pkg_dir}/shell_functions"

# Set traps for robust error handling
trap 'trap_job_failure schedule_subjects SIGTERM' SIGTERM
trap 'trap_job_failure schedule_subjects SIGINT' SIGINT
trap 'trap_job_failure schedule_subjects SIGHUP' SIGHUP
trap 'trap_job_failure schedule_subjects SIGQUIT' SIGQUIT
trap 'traperror schedule_subjects $? $LINENO ${BASH_LINENO[0]} "$BASH_COMMAND" $(printf "::%s" ${FUNCNAME[@]:-})' ERR

[ -z "$snapshot_rds" ] && echo "snapshot_rds not set. Exiting." && exit 1
[ ! -f "$snapshot_rds" ] && echo "snapshot_rds $snapshot_rds not found. Exiting." && exit 1
[ ! -r "$snapshot_rds" ] && echo "snapshot_rds $snapshot_rds not readable. Exiting." && exit 1

log_message INFO "Starting schedule_subjects controller using snapshot $snapshot_rds"

# Prefer the invoking R's R_HOME if provided
RSCRIPT_BIN="Rscript"
if [ -n "$R_HOME" ] && [ -x "$R_HOME/bin/Rscript" ]; then
  RSCRIPT_BIN="$R_HOME/bin/Rscript"
fi

"$RSCRIPT_BIN" --vanilla -e '
  snap <- readRDS(Sys.getenv("snapshot_rds"));
  if (!requireNamespace("BrainGnomes", quietly = TRUE)) stop("BrainGnomes package is required.");
  do.call(BrainGnomes:::schedule_subjects, snap);
  try(unlink(Sys.getenv("snapshot_rds")), silent = TRUE)
'

log_message INFO "Finished schedule_subjects controller"
exit 0
