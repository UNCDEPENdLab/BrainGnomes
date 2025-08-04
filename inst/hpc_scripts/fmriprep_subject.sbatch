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
trap 'trap_job_failure fmriprep SIGTERM' SIGTERM
trap 'trap_job_failure fmriprep SIGINT' SIGINT
trap 'trap_job_failure fmriprep SIGHUP' SIGHUP
trap 'trap_job_failure fmriprep SIGQUIT' SIGQUIT
trap 'traperror fmriprep $? $LINENO ${BASH_LINENO[0]} "$BASH_COMMAND" $(printf "::%s" ${FUNCNAME[@]:-})' ERR

# default nthreads to however many cores we have $SLURM_SOMETHING
[ -z "$fmriprep_nthreads" ] && echo "No fmriprep_nthreads environment variable. Defaulting to 4" && fmriprep_nthreads=4

ncores=$SLURM_NTASKS

####
#verify required arguments
[ -z "$fmriprep_container" ] && echo "fmriprep_container not set. Exiting" && exit 1
[ ! -f "$fmriprep_container" ] && echo "fmriprep_container $fmriprep_container not found. Exiting" && exit 1
[ ! -r "$fmriprep_container" ] && echo "fmriprep_container $fmriprep_container not readable. Exiting" && exit 1
[ -z "$loc_mrproc_root" ] && echo "loc_mrproc_root not set. Exiting." && exit 1
[ -z "$loc_scratch" ] && echo "loc_scratch not set. Exiting." && exit 1
[ -z "$loc_bids_root" ] && echo "loc_bids_root not set. Exiting." && exit 1
[ ! -d "$loc_bids_root" ] && echo "loc_bids_root $loc_bids_root not found. Exiting" && exit 1
[ ! -r "$loc_bids_root" ] && echo "loc_bids_root $loc_bids_root not readable. Exiting" && exit 1
[ -z "$templateflow_home" ] && echo "templateflow_home not set. Exiting." && exit 1
if [ ! -d "$templateflow_home" ]; then
  log_message WARN templateflow_home directory $templateflow_home does not exist. Creating it inside fmriprep_subject.sbatch script.
  mkdir -p "$templateflow_home"
fi

[ -z "$sub_id" ] && echo "sub_id not set. Exiting." && exit 1
[ -z "$complete_file" ] && echo "complete_file not set. Exiting." && exit 1
if [[ "$debug_pipeline" == "TRUE" || "$debug_pipeline" -eq 1 ]]; then
  debug_pipeline=1
  log_message INFO "Running in debug mode; commands will not be executed"
  rel_flag=c
else
  debug_pipeline=0
fi

[ -z "$fs_license_file" ] && fs_license_file="$FS_LICENSE" # fall back to global freesurfer license file if not set
[ -z "$fs_license_file" ] && fs_license_file="$FREESURFER_HOME/license.txt" # fall back to freesurfer license file in $FREESURFER_HOME if not set
[ -z "$fs_license_file" ] && echo "fs_license_file not set and cannot find a fallback. Exiting." && exit 1
[ ! -f "$fs_license_file" ] && echo "FreeSurfer license file $fs_license_file not found. Exiting" && exit 1
[ ! -r "$fs_license_file" ] && echo "FreeSurfer license file $fs_license_file not readable. Exiting" && exit 1

fs_dir=$(dirname "$fs_license_file")

# add the job id to the log file variables in case of crash
[ -z "$stdout_log" ] && echo "stdout_log not set. Exiting." && exit 1
[ -z "$stderr_log" ] && echo "stderr_log not set. Exiting." && exit 1
stdout_log="${stdout_log//%j/$SLURM_JOB_ID}"
stderr_log="${stderr_log//%j/$SLURM_JOB_ID}"

###
# handle multi-session setup
if [ -n "$ses_id" ]; then
  out_dir="${loc_mrproc_root}/sub-${sub_id}/ses-${ses_id}"
  ses_str="session $ses_id"
else
  out_dir="${loc_mrproc_root}/sub-${sub_id}"
  ses_str=""
fi
mkdir -p "$out_dir"
cd $out_dir || { echo "Failed to change directory to $out_dir"; exit 1; }

####
log_flag=""
[[ -n "$log_file" ]] && log_flag="-l $log_file" # if log_file is set, pass it to rel

# ensure that all paths to singularity are absolute paths
vars=(loc_bids_root loc_scratch loc_mrproc_root templateflow_home fs_dir)

# Convert each to absolute path using abspath
for var in "${vars[@]}"; do
  abs=$(abspath "${!var}")
  eval "$var=\"\$abs\""
done

singularity_cmd="singularity run --cleanenv -B '$loc_bids_root' -B '$loc_scratch' \
  -B '$loc_mrproc_root' -B '$templateflow_home' -B '$fs_dir' '$fmriprep_container'"

mem=$((SLURM_MEM_PER_NODE / 1024))GB
start_time=$(date +%s)

log_message INFO Starting fmriprep for subject $sub_id $ses_str in $out_dir with mem: $mem and cpus: $SLURM_JOB_CPUS_PER_NODE

# export necessary environment variables for fmriprep in singularity
rel $rel_flag $log_flag export APPTAINERENV_TEMPLATEFLOW_HOME="$templateflow_home" # set the environment variable for templateflow in the singularity container
rel $rel_flag $log_flag export APPTAINERENV_FS_LICENSE="$fs_license_file" # set the environment variable for freesurfer license in the singularity container

# to trap sigterm, we need to background the main compute and then wait (during which time signals are received)
rel $rel_flag "$log_flag" "$singularity_cmd" \
    "$loc_bids_root" "$loc_mrproc_root/" participant \
    "$cli_options" & # all options setup by submit_fmriprep()
cmd_pid=$!

set +e
wait $cmd_pid
cmd_status=$?
set -e

[[ $cmd_status -eq 0 && $debug_pipeline -eq 0 ]] && date +"%Y-%m-%d %H:%M:%S" > "$complete_file"

log_message INFO "Finished fmriprep for subject $sub_id $ses_str (time elapsed: $(time_elapsed))"
exit $cmd_status
