#!/bin/bash
# Default SLURM requests if not overridden on command line
#SBATCH -N 1
#SBATCH -n 4
#SBATCH --time=8:00:00
#SBATCH --mem=32g

set -eE  # 'E' ensures ERR trap inherits in functions/subshells

# source bash functions used in pipeline shell scripts
[ -z "$pkg_dir" ] && echo "pkg_dir not set. Cannot locate required helper scripts" && exit 1
source "${pkg_dir}/shell_functions"

# Set trap for common termination signals
trap 'trap_job_failure mriqc SIGTERM' SIGTERM
trap 'trap_job_failure mriqc SIGINT' SIGINT
trap 'trap_job_failure mriqc SIGHUP' SIGHUP
trap 'trap_job_failure mriqc SIGQUIT' SIGQUIT
trap 'traperror mriqc $? $LINENO ${BASH_LINENO[0]} "$BASH_COMMAND" $(printf "::%s" ${FUNCNAME[@]:-})' ERR

#https://stackoverflow.com/questions/51026315/how-to-solve-unicodedecodeerror-in-python-3-6/51027262#51027262
#LANG=en_US.UTF-8
#export LANG

####
#verify required arguments
[ -z "$mriqc_container" ] && echo "mriqc_container not set. Exiting" && exit 1
[ ! -f "$mriqc_container" ] && echo "mriqc_container $mriqc_container not found. Exiting" && exit 1
[ ! -r "$mriqc_container" ] && echo "mriqc_container $mriqc_container not readable. Exiting" && exit 1
[ -z "$loc_mriqc_root" ] && echo "loc_mriqc_root not set. Exiting." && exit 1
[ -z "$loc_scratch" ] && echo "loc_scratch not set. Exiting." && exit 1
[ -z "$loc_bids_root" ] && echo "loc_bids_root not set. Exiting." && exit 1
[ ! -d "$loc_bids_root" ] && echo "loc_bids_root $loc_bids_root not found. Exiting" && exit 1
[ ! -r "$loc_bids_root" ] && echo "loc_bids_root $loc_bids_root not readable. Exiting" && exit 1
[ -z "$sub_id" ] && echo "sub_id not set. Exiting." && exit 1
[ -z "$complete_file" ] && echo "complete_file not set. Exiting." && exit 1
if [[ "$debug_pipeline" == "TRUE" || "$debug_pipeline" -eq 1 ]]; then
  debug_pipeline=1
else
  debug_pipeline=0
fi
if [[ "$debug_pipeline" -eq 1 ]]; then
  log_message INFO "Running in debug mode; commands will not be executed"
  rel_flag=c
fi

# add the job id to the log file variables in case of crash
[ -z "$stdout_log" ] && echo "stdout_log not set. Exiting." && exit 1
[ -z "$stderr_log" ] && echo "stderr_log not set. Exiting." && exit 1
stdout_log="${stdout_log//%j/$SLURM_JOB_ID}"
stderr_log="${stderr_log//%j/$SLURM_JOB_ID}"

###
out_dir="${loc_mriqc_root}/sub-${sub_id}"
mkdir -p "$out_dir"
cd $out_dir || { echo "Failed to change directory to $out_dir"; exit 1; }

####
log_flag=""
[[ -n "$log_file" ]] && log_flag="-l $log_file" # if log_file is set, pass it to rel

singularity_cmd="singularity run --cleanenv -B '$loc_bids_root' -B '$loc_scratch' \
  -B '$loc_mriqc_root' '$mriqc_container'"

log_message INFO Starting mriqc for subject $sub_id in $out_dir

# to trap sigterm, we need to background the main compute and then wait (during which time signals are received)
rel $rel_flag $log_flag $singularity_cmd \
    "$loc_bids_root" "$loc_mriqc_root/" participant \
    $cli_options & # all options setup by submit_mriqc()
cmd_pid=$!

set +e
wait $cmd_pid
cmd_status=$?
set -e

[[ $cmd_status -eq 0 && $debug_pipeline -eq 0 ]] && date +"%Y-%m-%d %H:%M:%S" > "$complete_file"

log_message INFO Finished mriqc for subject $sub_id
exit $cmd_status
