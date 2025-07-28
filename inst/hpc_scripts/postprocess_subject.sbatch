#!/bin/bash
# Default SLURM requests if not overridden on command line
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --time=8:00:00
#SBATCH --mem=8g

# This script postprocesses all files in a subject input directory that match a given regular expression (input_regex).
# For each file it finds, it launches a separate child job, then waits for these child jobs to complete.

set -eE  # 'E' ensures ERR trap inherits in functions/subshells

# source bash functions used in pipeline shell scripts
[ -z "$pkg_dir" ] && echo "pkg_dir not set. Cannot locate required helper scripts" && exit 1
source "${pkg_dir}/shell_functions"

# Set trap for common termination signals
trap 'trap_job_failure postprocess SIGTERM' SIGTERM
trap 'trap_job_failure postprocess SIGINT' SIGINT
trap 'trap_job_failure postprocess SIGHUP' SIGHUP
trap 'trap_job_failure postprocess SIGQUIT' SIGQUIT
trap 'traperror postprocess $? $LINENO ${BASH_LINENO[0]} "$BASH_COMMAND" $(printf "::%s" ${FUNCNAME[@]:-})' ERR

ncores=$SLURM_NTASKS

####
#verify required arguments
[ -z "$loc_mrproc_root" ] && echo "loc_mrproc_root not set. Exiting." && exit 1
[ ! -r "$loc_mrproc_root" ] && echo "loc_mrproc_root $loc_mrproc_root not readable. Exiting" && exit 1
[ -z "$sub_id" ] && echo "sub_id not set. Exiting." && exit 1
[ -z "$complete_file" ] && echo "complete_file not set. Exiting." && exit 1
[ -z "$postprocess_cli" ] && echo "postprocess_cli not set. Exiting." && exit 1
[ -z "$postprocess_rscript" ] && echo "postprocess_rscript not set. Exiting." && exit 1
[ ! -f "$postprocess_rscript" ] && echo "postprocess_rscript $postprocess_rscript not found. Exiting" && exit 1
[ ! -r "$postprocess_rscript" ] && echo "postprocess_rscript $postprocess_rscript not readable. Exiting" && exit 1
[ -z "$input_dir" ] && echo "input_dir is not set. Exiting" && exit 1
[ ! -d "$input_dir" ] && echo "input_dir $input_dir cannot be found. Exiting" && exit 1
[ -z "$input_regex" ] && echo "input_regex is not set. Exiting" && exit 1
[  -z "$postprocess_image_sched_script" ] && echo "postprocess_image_sched_script is not set. Exiting" && exit 1
[ ! -f "$postprocess_image_sched_script" ] && echo "postprocess_image_sched_script $postprocess_image_sched_script not found. Exiting" && exit 1
[ ! -r "$postprocess_image_sched_script" ] && echo "postprocess_image_sched_script $postprocess_image_sched_script not readable. Exiting" && exit 1
[ -z "$stream_name" ] && echo "stream_name not set. Exiting" && exit 1

if [[ "$debug_pipeline" == "TRUE" || "$debug_pipeline" -eq 1 ]]; then
  debug_pipeline=1
  log_message INFO "Running in debug mode; commands will not be executed"
  rel_flag=c
else
  debug_pipeline=0
fi

[ -z "$sub_id" ] && echo "sub_id not set. Exiting." && exit 1

# add the job id to the log file variables in case of crash
[ -z "$stdout_log" ] && echo "stdout_log not set. Exiting." && exit 1
[ -z "$stderr_log" ] && echo "stderr_log not set. Exiting." && exit 1
stdout_log="${stdout_log//%j/$SLURM_JOB_ID}"
stderr_log="${stderr_log//%j/$SLURM_JOB_ID}"

###
# handle multi-session setup
if [ -n "$ses_id" ]; then
  out_dir=${loc_mrproc_root}/sub-${sub_id}/ses-${ses_id}
  ses_str="session $ses_id"
else
  out_dir=${loc_mrproc_root}/sub-${sub_id}
  ses_str=""
fi
mkdir -p "$out_dir"
cd $out_dir || { echo "Failed to change directory to $out_dir"; exit 1; }

####
log_flag=""
[[ -n "$log_file" ]] && log_flag="-l $log_file" # if log_file is set, pass it to rel

### Pass forward only sched_args and CLI options that don't conflict with the child arguments
sched_args="$(rm_cli_fields "$sched_args" job-name output error)"
postprocess_cli="$(rm_cli_fields "$postprocess_cli" input input_regex)"

### Look for all relevant input files in the input directory
full_regex="${input_dir%/}/$input_regex"  # Ensure slash between dir and file

cmd="find \"$input_dir\" -regextype posix-extended -regex \"$full_regex\""
log_message DEBUG Looking for files to postprocess using "$cmd"
files=($(find "$input_dir" -regextype posix-extended -regex "$full_regex"))

if [[ ${#files[@]} -eq 0 ]]; then
  log_message DEBUG No files to postprocess found in $out_dir.
  exit 0
fi

job_ids=()

for f in "${files[@]}"; do
  
  fname=$(remove_ext "$f")
  jobname="postprocess_${fname}"

  # create separate logs for each image -- place in the same location as the subject-level job
  postprocess_stdout_log="$(dirname $stdout_log)/postprocess_${stream_name}_${fname}_jobid-%j.out"
  postprocess_stderr_log="$(dirname $stdout_log)/postprocess_${stream_name}_${fname}_jobid-%j.err"
  
  jid=$(sbatch \
    --parsable \
    $sched_args \
    --job-name="$jobname" \
    --output="$postprocess_stdout_log" \
    --error="$postprocess_stderr_log" \
    --export=ALL,input_file="$f",stdout_log="$postprocess_stdout_log",stderr_log="$postprocess_stderr_log" \
    "$postprocess_image_sched_script")
  
  log_message INFO Submitted postprocessing job $jid for $(basename $f) in $out_dir
  job_ids+=("$jid")
done

# Wait for all child jobs to finish
echo "Waiting for child jobs to complete..."

while true; do
  active_jobs=0
  for jid in "${job_ids[@]}"; do
    squeue -j "$jid" &> /dev/null && ((active_jobs += 1))
  done

  if [[ $active_jobs -eq 0 ]]; then
    echo "All child jobs completed."
    break
  fi
  sleep 300 # poll every 5 minutes
done


# Check exit status of child jobs using sacct
failed=0
for jid in "${job_ids[@]}"; do
  # Get job state and exit code. sacct may return multiple lines; take the first
  state=$(sacct -n -j "$jid" -o State | head -n 1 | awk '{print $1}')
  exit_code=$(sacct -n -j "$jid" -o ExitCode | head -n 1 | awk '{print $1}')
  exit_main=${exit_code%%:*}

  if [[ "$state" != "COMPLETED" || "$exit_main" != "0" ]]; then
    log_message ERROR "Child job $jid failed with state $state exit code $exit_code"
    failed=1
  fi
done

[[ $failed -eq 0 && $debug_pipeline -eq 0 ]] && date +"%Y-%m-%d %H:%M:%S" > "$complete_file"

log_message INFO Finished postprocessing for subject $sub_id $ses_str
exit $failed
