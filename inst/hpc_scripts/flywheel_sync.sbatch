#!/bin/bash
# Default SLURM requests if not overridden on command line
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --time=4:00:00
#SBATCH --mem=16g

set -eE  # 'E' ensures ERR trap inherits in functions/subshells

# source bash functions used in pipeline shell scripts
[ -z "$pkg_dir" ] && echo "pkg_dir not set. Cannot locate required helper scripts" && exit 1
source "${pkg_dir}/shell_functions"

# Set trap for common termination signals
trap 'trap_job_failure flywheel_sync SIGTERM' SIGTERM
trap 'trap_job_failure flywheel_sync SIGINT' SIGINT
trap 'trap_job_failure flywheel_sync SIGHUP' SIGHUP
trap 'trap_job_failure flywheel_sync SIGQUIT' SIGQUIT
trap 'traperror flywheel_sync $? $LINENO ${BASH_LINENO[0]} "$BASH_COMMAND" $(printf "::%s" ${FUNCNAME[@]:-})' ERR

####
#verify required arguments
[ -z "$flywheel_cmd" ] && echo "flywheel_cmd not set. Exiting" && exit 1
[ ! -f "$flywheel_cmd" ] && echo "flywheel_cmd $flywheel_cmd not found. Exiting" && exit 1
[ ! -r "$flywheel_cmd" ] && echo "flywheel_cmd $flywheel_cmd not readable. Exiting" && exit 1
[ -z "$flywheel_source_url" ] && echo "flywheel_source_url not set. Exiting." && exit 1
[ -z "$flywheel_sync_directory" ] && echo "flywheel_sync_directory not set. Exiting." && exit 1

if [[ "$debug_pipeline" == "TRUE" || "$debug_pipeline" -eq 1 ]]; then
  debug_pipeline=1
  log_message INFO "Running in debug mode; commands will not be executed"
  rel_flag=c
else
  debug_pipeline=0
fi

# add the job id to the log file variables in case of crash
[ -z "$stdout_log" ] && echo "stdout_log not set. Exiting." && exit 1
[ -z "$stderr_log" ] && echo "stderr_log not set. Exiting." && exit 1
stdout_log="${stdout_log//%j/$SLURM_JOB_ID}"
stderr_log="${stderr_log//%j/$SLURM_JOB_ID}"

mem=$((SLURM_MEM_PER_NODE / 1024))GB
start_time=$(date +%s)

log_message INFO Starting flywheel_sync with mem: $mem and cpus: $SLURM_JOB_CPUS_PER_NODE

"${R_HOME}/bin/Rscript" "$upd_job_status_path" --job_id "$SLURM_JOB_ID" --sqlite_db "$sqlite_db" --status "STARTED"

# to trap sigterm, we need to background the main compute and then wait (during which time signals are received)
flywheel_cmd_args="sync"
[ -n "$flywheel_cli_options" ] && flywheel_cmd_args="$flywheel_cmd_args $flywheel_cli_options"
flywheel_cmd_args="$flywheel_cmd_args $flywheel_source_url $flywheel_sync_directory"

rel $rel_flag "$flywheel_cmd" $flywheel_cmd_args &
cmd_pid=$!

run_bg_and_wait "$cmd_pid" cmd_status

log_message DEBUG "Exit status of flywheel_sync is $cmd_status"

if [[ $cmd_status -eq 0 ]]; then
  "${R_HOME}/bin/Rscript" "$upd_job_status_path" --job_id "$SLURM_JOB_ID" --sqlite_db "$sqlite_db" --status "COMPLETED"
  log_message INFO "Finished flywheel_sync (time elapsed: $(time_elapsed))"
else
  "${R_HOME}/bin/Rscript" "$upd_job_status_path" --job_id "$SLURM_JOB_ID" --sqlite_db "$sqlite_db" --status "FAILED" --cascade
  log_message ERROR "flywheel_sync failed (exit code $cmd_status)"
fi
exit $cmd_status
