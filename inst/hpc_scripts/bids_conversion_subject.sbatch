#!/bin/bash
#Default SLURM requests if not overridden on command line
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --time=2:00:00
#SBATCH --mem=16g

set -e

# source bash functions used in pipeline shell scripts
[ -z "$pkg_dir" ] && echo "pkg_dir not set. Cannot locate required helper scripts" && exit 1
source "${pkg_dir}/shell_functions"

# Set trap for common termination signals
trap 'trap_job_failure bids_conversion SIGTERM' SIGTERM
trap 'trap_job_failure bids_conversion SIGINT' SIGINT
trap 'trap_job_failure bids_conversion SIGHUP' SIGHUP
trap 'trap_job_failure bids_conversion SIGQUIT' SIGQUIT

####
#verify necessary arguments
[ -z "$heudiconv_container" ] && echo "heudiconv_container not set. Exiting" && exit 1
[ ! -f "$heudiconv_container" ] && echo "heudiconv_container $heudiconv_container not found. Exiting" && exit 1
[ ! -r "$heudiconv_container" ] && echo "heudiconv_container $heudiconv_container not readable. Exiting" && exit 1
[ -z "$loc_sub_dicoms" ] && echo "loc_sub_dicoms not set. Exiting." && exit 1
[ -z "$loc_bids_root" ] && echo "loc_bids_root not set. Exiting." && exit 1
[ -z "$heudiconv_heuristic" ] && echo "No heudiconv_heuristic variable set." && exit 1
[ ! -f "$heudiconv_heuristic" ] && echo "Heuristic file $heudiconv_heuristic not found." && exit 1
[ ! -r "$heudiconv_heuristic" ] && echo "Heuristic file $heudiconv_heuristic not readable." && exit 1
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

heuristic_dir=$(dirname "$heudiconv_heuristic")

# add the job id to the log file variables in case of crash
[ -z "$stdout_log" ] && echo "stdout_log not set. Exiting." && exit 1
[ -z "$stderr_log" ] && echo "stderr_log not set. Exiting." && exit 1
stdout_log="${stdout_log//%j/$SLURM_JOB_ID}"
stderr_log="${stderr_log//%j/$SLURM_JOB_ID}"

###
# handle multi-session setup
if [ -n "$ses_id" ]; then
  out_dir=${loc_bids_root}/sub-${sub_id}/ses-${ses_id}
  ses_flag="--ses $ses_id"
  ses_str="session $ses_id"
else
  out_dir=${loc_bids_root}/sub-${sub_id}
  ses_flag=""
  ses_str=""
fi
mkdir -p "$out_dir"
cd $out_dir || { echo "Failed to change directory to $out_dir"; exit 1; }

####
#run heudiconv
log_flag=""
[[ -n "$log_file" ]] && log_flag="-l $log_file" # if log_file is set, pass it to rel

mem=$((SLURM_MEM_PER_NODE / 1024))GB
start_time=$(date +%s)

log_message INFO Starting heudiconv for subject $sub_id $ses_str in $out_dir with mem: $mem and cpus: $SLURM_JOB_CPUS_PER_NODE

singularity_cmd="singularity run --cleanenv -B $loc_bids_root -B $loc_sub_dicoms -B $heuristic_dir $heudiconv_container"

# to trap sigterm, we need to background the main compute and then wait (during which time signals are received)
# we now use --files to specify the dicoms to convert rather than -d since we don't want to assume
# the dicoms are in a specific directory structure (esp. in terms of level of nesting).
rel $rel_flag $log_flag $singularity_cmd --files $loc_sub_dicoms -s $sub_id $ses_flag \
  -f $heudiconv_heuristic -c dcm2niix \
  -o $loc_bids_root --bids &
cmd_pid=$!

set +e
wait $cmd_pid
cmd_status=$?
set -e

# write complete file if the command was successful and debug_pipeline is not set
[[ $cmd_status -eq 0 && $debug_pipeline -eq 0 ]] && date +"%Y-%m-%d %H:%M:%S" > "$complete_file"

log_message INFO Finished heudiconv for subject $sub_id $ses_str "(time elapsed: $(time_elapsed))"
exit $cmd_status

#######
# example call
# singularity run --cleanenv -B /proj/mnhallqlab/projects/preproc_pipeline_test_data/dicoms \
#   -B /proj/mnhallqlab/projects/preproc_pipeline_test_data/data_BIDS \
#   /proj/mnhallqlab/users/michael/fmriprep_pipeline_setup/heudiconv_latest.sif \
#   --files /proj/mnhallqlab/projects/preproc_pipeline_test_data/dicoms/540311 -f heuristic.py -s 540311 -c dcm2niix --bids \
#   -o /proj/mnhallqlab/projects/preproc_pipeline_test_data/data_BIDS
