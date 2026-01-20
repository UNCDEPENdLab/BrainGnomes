#!/bin/bash
# Default SLURM requests if not overridden on command line
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --time=1:00:00
#SBATCH --mem=32g

## N.B. This script extracts ROIs for one subject

set -eE  # 'E' ensures ERR trap inherits in functions/subshells

# source bash functions used in pipeline shell scripts
[ -z "$pkg_dir" ] && echo "pkg_dir not set. Cannot locate required helper scripts" && exit 1
source "${pkg_dir}/shell_functions"

# Set trap for common termination signals
trap 'trap_job_failure extract_rois SIGTERM' SIGTERM
trap 'trap_job_failure extract_rois SIGINT' SIGINT
trap 'trap_job_failure extract_rois SIGHUP' SIGHUP
trap 'trap_job_failure extract_rois SIGQUIT' SIGQUIT
trap 'traperror extract_rois $? $LINENO ${BASH_LINENO[0]} "$BASH_COMMAND" $(printf "::%s" ${FUNCNAME[@]:-})' ERR

ncores=$SLURM_NTASKS

####
#verify required arguments
[ -z "$loc_postproc_root" ] && echo "loc_postproc_root not set. Exiting." && exit 1
[ ! -r "$loc_postproc_root" ] && echo "loc_postproc_root $loc_postproc_root not readable. Exiting" && exit 1
[ -z "$sub_id" ] && echo "sub_id not set. Exiting." && exit 1
[ -z "$extract_cli" ] && echo "extract_cli not set. Exiting." && exit 1
[ -z "$extract_rscript" ] && echo "extract_rscript not set. Exiting." && exit 1
[ ! -f "$extract_rscript" ] && echo "extract_rscript $extract_rscript not found. Exiting" && exit 1
[ ! -r "$extract_rscript" ] && echo "extract_rscript $extract_rscript not readable. Exiting" && exit 1
[ -z "$log_file" ] && echo "log_file not set. Exiting" && exit 1
[ -z "$R_HOME" ] && echo "R_HOME not set. Exiting" && exit 1

# add the job id to the log file variables in case of crash
[ -z "$stdout_log" ] && echo "stdout_log not set. Exiting." && exit 1
[ -z "$stderr_log" ] && echo "stderr_log not set. Exiting." && exit 1
stdout_log="${stdout_log//%j/$SLURM_JOB_ID}"
stderr_log="${stderr_log//%j/$SLURM_JOB_ID}"

###
# handle multi-session setup
if [ -n "$ses_id" ]; then
  out_dir=${loc_postproc_root}/sub-${sub_id}/ses-${ses_id}
  ses_str="session $ses_id"
else
  out_dir=${loc_postproc_root}/sub-${sub_id}
  ses_str=""
fi
mkdir -p "$out_dir"
cd $out_dir || { echo "Failed to change directory to $out_dir"; exit 1; }

####
if [[ "$debug_pipeline" == "TRUE" || "$debug_pipeline" -eq 1 ]]; then
  debug_pipeline=1
  log_message INFO "Running in debug mode; commands will not be executed"
  rel_flag=c
else
  debug_pipeline=0
fi

log_flag=""
[[ -n "$log_file" ]] && log_flag="-l $log_file" # if log_file is set, pass it to rel

mem=$((SLURM_MEM_PER_NODE / 1024))GB
start_time=$(date +%s)

log_message INFO Starting ROI extraction for subject $sub_id in $out_dir with mem: $mem

"${R_HOME}/bin/Rscript" "$upd_job_status_path" --job_id "$SLURM_JOB_ID" --sqlite_db "$sqlite_db" --status "STARTED"

rel $rel_flag $log_flag "${R_HOME}/bin/Rscript" --vanilla "$extract_rscript" $extract_cli &
cmd_pid=$!

cmd_status=0
wait $cmd_pid || cmd_status=$? # trap inline error code as exit status

[[ $cmd_status -eq 0 ]] && "${R_HOME}/bin/Rscript" "$upd_job_status_path" --job_id "$SLURM_JOB_ID" --sqlite_db "$sqlite_db" --status "COMPLETED"
[[ $cmd_status -eq 1 ]] && "${R_HOME}/bin/Rscript" "$upd_job_status_path" --job_id "$SLURM_JOB_ID" --sqlite_db "$sqlite_db" --status "FAILED" --cascade

log_message INFO Finished ROI extraction for subject $sub_id "(time elapsed: $(time_elapsed))"
exit $cmd_status
