#!/bin/bash
# Default SLURM requests if not overridden on command line
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --time=09:00
#SBATCH --mem=8g

set -eE  # 'E' ensures ERR trap inherits in functions/subshells

# source bash functions used in pipeline shell scripts
[ -z "$pkg_dir" ] && echo "pkg_dir not set. Cannot locate required helper scripts" && exit 1
source "${pkg_dir}/shell_functions"

# Set trap for common termination signals
trap 'trap_job_failure fsaverage_setup SIGTERM' SIGTERM
trap 'trap_job_failure fsaverage_setup SIGINT' SIGINT
trap 'trap_job_failure fsaverage_setup SIGHUP' SIGHUP
trap 'trap_job_failure fsaverage_setup SIGQUIT' SIGQUIT
trap 'traperror fsaverage_setup $? $LINENO ${BASH_LINENO[0]} "$BASH_COMMAND" $(printf "::%s" ${FUNCNAME[@]:-})' ERR

####
#verify required arguments
[ -z "$fmriprep_container" ] && echo "fmriprep_container not set. Exiting" && exit 1
[ ! -f "$fmriprep_container" ] && echo "fmriprep_container $fmriprep_container not found. Exiting" && exit 1
[ ! -r "$fmriprep_container" ] && echo "fmriprep_container $fmriprep_container not readable. Exiting" && exit 1
[ -z "$loc_mrproc_root" ] && echo "loc_mrproc_root not set. Exiting." && exit 1

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

log_message INFO Starting fsaverage_setup with mem: $mem and cpus: $SLURM_JOB_CPUS_PER_NODE

"${R_HOME}/bin/Rscript" "$upd_job_status_path" --job_id "$SLURM_JOB_ID" --sqlite_db "$sqlite_db" --status "STARTED"

# to trap sigterm, we need to background the main compute and then wait (during which time signals are received)
rel $rel_flag singularity exec --cleanenv --containall -B "$loc_mrproc_root" "$fmriprep_container" \
    rsync --mkpath -a --no-owner --no-group --no-times --no-perms /opt/freesurfer/subjects/fsaverage "${loc_mrproc_root}/sourcedata/freesurfer" &
cmd_pid=$!
run_bg_and_wait "$cmd_pid" cmd_status

log_message DEBUG "Exit status of fsaverage_setup is $cmd_status"

if [[ $cmd_status -eq 0 ]]; then
  "${R_HOME}/bin/Rscript" "$upd_job_status_path" --job_id "$SLURM_JOB_ID" --sqlite_db "$sqlite_db" --status "COMPLETED"
  log_message INFO "Finished fsaverage setup (time elapsed: $(time_elapsed))"
else
  "${R_HOME}/bin/Rscript" "$upd_job_status_path" --job_id "$SLURM_JOB_ID" --sqlite_db "$sqlite_db" --status "FAILED" --cascade
  log_message ERROR "fsaverage setup failed (exit code $cmd_status)"
fi
exit $cmd_status
