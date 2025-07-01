#!/bin/bash
# Default SLURM requests if not overridden on command line
#SBATCH -N 1
#SBATCH -n 4
#SBATCH --time=24:00:00
#SBATCH --mem=48g

set -eE  # 'E' ensures ERR trap inherits in functions/subshells

# source bash functions used in pipeline shell scripts
[ -z "$pkg_dir" ] && echo "pkg_dir not set. Cannot locate required helper scripts" && exit 1
source "${pkg_dir}/shell_functions"

# Set trap for common termination signals
trap 'trap_job_failure aroma SIGTERM' SIGTERM
trap 'trap_job_failure aroma SIGINT' SIGINT
trap 'trap_job_failure aroma SIGHUP' SIGHUP
trap 'trap_job_failure aroma SIGQUIT' SIGQUIT
trap 'traperror aroma $? $LINENO ${BASH_LINENO[0]} "$BASH_COMMAND" $(printf "::%s" ${FUNCNAME[@]:-})' ERR

# default nthreads to however many cores we have $SLURM_SOMETHING
[ -z "$aroma_nthreads" ] && echo "No aroma_nthreads environment variable. Defaulting to 4" && aroma_nthreads=4

ncores=$SLURM_NTASKS

####
#verify required arguments
[ -z "$aroma_container" ] && echo "aroma_container not set. Exiting" && exit 1
[ ! -f "$aroma_container" ] && echo "aroma_container $aroma_container not found. Exiting" && exit 1
[ ! -r "$aroma_container" ] && echo "aroma_container $aroma_container not readable. Exiting" && exit 1
[ -z "$loc_mrproc_root" ] && echo "loc_mrproc_root not set. Exiting." && exit 1
[ -z "$loc_scratch" ] && echo "loc_scratch not set. Exiting." && exit 1
[ -z "$loc_bids_root" ] && echo "loc_bids_root not set. Exiting." && exit 1
[ ! -d "$loc_bids_root" ] && echo "loc_bids_root $loc_bids_root not found. Exiting" && exit 1
[ ! -r "$loc_bids_root" ] && echo "loc_bids_root $loc_bids_root not readable. Exiting" && exit 1
[ -z "$sub_id" ] && echo "sub_id not set. Exiting." && exit 1
[ -z "$complete_file" ] && echo "complete_file not set. Exiting." && exit 1
[[ "$debug_pipeline" == "TRUE" ]] && debug_pipeline=1 || debug_pipeline=0 # set debug_pipeline to 1 if it is set to TRUE, otherwise set it to 0

# add the job id to the log file variables in case of crash
[ -z "$stdout_log" ] && echo "stdout_log not set. Exiting." && exit 1
[ -z "$stderr_log" ] && echo "stderr_log not set. Exiting." && exit 1
stdout_log="${stdout_log//%j/$SLURM_JOB_ID}"
stderr_log="${stderr_log//%j/$SLURM_JOB_ID}"

###
out_dir="${loc_mrproc_root}/sub-${sub_id}"
mkdir -p "$out_dir"
cd $out_dir || { echo "Failed to change directory to $out_dir"; exit 1; }

####
[[ "$debug_pipeline" -eq 1 ]] && rel_flag=c #if debug_pipeline is 1, only echo command to log, don't run it

log_flag=""
[[ -n "$log_file" ]] && log_flag="-l $log_file" # if log_file is set, pass it to rel

singularity_cmd="singularity run --cleanenv -B '$loc_bids_root' -B '$loc_scratch' \
  -B '$loc_mrproc_root' '$aroma_container'"

mem=$((SLURM_MEM_PER_NODE / 1024))GB
start_time=$(date +%s)

log_message INFO Starting aroma for subject $sub_id with mem: $mem and cpus: $SLURM_JOB_CPUS_PER_NODE

# to trap sigterm, we need to background the main compute and then wait (during which time signals are received)
rel $rel_flag $log_flag $singularity_cmd \
    "$loc_bids_root" "$loc_mrproc_root/" participant \
    $cli_options & # all options setup by submit_aroma()
cmd_pid=$!

set +e
wait $cmd_pid
cmd_status=$?
set -e

[[ $cmd_status -eq 0 && $debug_pipeline -eq 0 ]] && date +"%Y-%m-%d %H:%M:%S" > "$complete_file"

log_message INFO Finished aroma for subject $sub_id "(time elapsed: $(time_elapsed))"
exit $cmd_status
