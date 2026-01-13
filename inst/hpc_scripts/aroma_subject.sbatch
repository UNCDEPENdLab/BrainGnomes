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
if [[ "$debug_pipeline" == "TRUE" || "$debug_pipeline" -eq 1 ]]; then
  debug_pipeline=1
  log_message INFO "Running in debug mode; commands will not be executed"
  rel_flag=c
else
  debug_pipeline=0
fi

# default to no cleanup if variable not provided
[ -z "$aroma_cleanup" ] && aroma_cleanup=0

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

log_flag=""
[[ -n "$log_file" ]] && log_flag="-l $log_file" # if log_file is set, pass it to rel

mem=$((SLURM_MEM_PER_NODE / 1024))GB
start_time=$(date +%s)

# check for BOLD input files. fmripost aroma is pretty weak in its error messages.
# search recursively to support session-specific directories (e.g., ses-01/func)
if ! find "$out_dir" -type f -path '*/func/*_space-MNI152NLin6Asym_res-2_desc-preproc_bold.nii.gz' -print -quit | grep -q .; then
  log_message FATAL No MNI152NLin6Asym_res-2 NIfTI files found in $out_dir. AROMA cannot run. Ensure that fMRIPrep --output-spaces includes MNI152NLin6Asym:res-2.
  exit 1
fi

log_message INFO Starting aroma for subject $sub_id with mem: $mem and cpus: $SLURM_JOB_CPUS_PER_NODE

# protect existing sub-<id>.html file
[ -f "${loc_mrproc_root}/sub-${sub_id}.html" ] && mv "${loc_mrproc_root}/sub-${sub_id}.html" "${loc_mrproc_root}/sub-${sub_id}_fmriprep.html"

"${R_HOME}/bin/Rscript" "$upd_job_status_path" --job_id "$SLURM_JOB_ID" --sqlite_db "$sqlite_db" --status "STARTED"
# at present, fmripost-aroma does not support an offline mode, so we have to tolerate the risk of a race condition.
# rel $rel_flag $log_flag singularity run --cleanenv --containall --net --network none -B "$loc_bids_root" -B "$loc_scratch" \

# to trap sigterm, we need to background the main compute and then wait (during which time signals are received)
# disable ERR trap in the subshell so fmripost-aroma's non-zero exit (even on success) does not write a fail file
# via the inherited trap. We handle success/fail explicitly after wait.
(
  set +eE
  trap - ERR
  rel $rel_flag $log_flag singularity run --cleanenv --containall -B "$loc_bids_root" -B "$loc_scratch" \
    -B "$loc_mrproc_root" "$aroma_container" \
    "$loc_bids_root" "$loc_mrproc_root/" participant \
    $cli_options # all options setup by submit_aroma()
) &
cmd_pid=$!

cmd_status=0
wait $cmd_pid || cmd_status=$? # trap inline error code as exit status

# fmripost-aroma somehow decides to throw a non-zero exit status even when it completes successfully.
# fall back to looking for success string in fmriprep log file
success=$(grep -q "fMRIPost-AROMA finished successfully!" "$stdout_log" && echo 0 || echo 1)

log_message DEBUG "Exit status of fmripost-aroma is $cmd_status, success is $success"

[ -f "${loc_mrproc_root}/sub-${sub_id}.html" ] && mv "${loc_mrproc_root}/sub-${sub_id}.html" "${loc_mrproc_root}/sub-${sub_id}_aroma.html"
[ -f "${loc_mrproc_root}/sub-${sub_id}_fmriprep.html" ] && mv "${loc_mrproc_root}/sub-${sub_id}_fmriprep.html" "${loc_mrproc_root}/sub-${sub_id}.html"
# [[ $cmd_status -eq 0 && $debug_pipeline -eq 0 ]] && date +"%Y-%m-%d %H:%M:%S" > "$complete_file"

if [[ $success -eq 0 ]]; then
  if [[ "$aroma_cleanup" == "TRUE" || "$aroma_cleanup" -eq 1 ]]; then
    log_message INFO "Removing unneeded AROMA NIfTI/JSON files"
    rel $rel_flag $log_flag find "$out_dir" -type f -name "*space-MNI152NLin6Asym_res-2*" -delete
  fi
  [[ $debug_pipeline -eq 0 ]] && date +"%Y-%m-%d %H:%M:%S" > "$complete_file"
  "${R_HOME}/bin/Rscript" "$upd_job_status_path" --job_id "$SLURM_JOB_ID" --sqlite_db "$sqlite_db" --status "COMPLETED"
else
  fail_file="${complete_file%_complete}_fail"
  archive_file "$fail_file"
  [[ $debug_pipeline -eq 0 ]] && date +"%Y-%m-%d %H:%M:%S" > "$fail_file"
  "${R_HOME}/bin/Rscript" "$upd_job_status_path" --job_id "$SLURM_JOB_ID" --sqlite_db "$sqlite_db" --status "FAILED" --cascade
fi

log_message INFO Exiting aroma for subject $sub_id "(time elapsed: $(time_elapsed))"
exit $success
