#!/bin/bash
# Default SLURM requests if not overridden on command line
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --time=1:00:00
#SBATCH --mem=4g

# This script postprocesses all files in a subject input directory that match a given regular expression (input_regex).
# It discovers matching files, writes them to a manifest, then submits a single Slurm job array (with
# concurrency throttle) and a lightweight sentinel job that handles completion/failure bookkeeping.

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
[ -z "$out_dir" ] && echo "out_dir not set. Exiting." && exit 1
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
[ -z "$postprocess_sentinel_sched_script" ] && echo "postprocess_sentinel_sched_script is not set. Exiting" && exit 1
[ ! -f "$postprocess_sentinel_sched_script" ] && echo "postprocess_sentinel_sched_script $postprocess_sentinel_sched_script not found. Exiting" && exit 1

# Default concurrency limit if not set by caller
[ -z "$max_concurrent_images" ] && max_concurrent_images=4

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


ses_str=""
if [ -n "$ses_id" ]; then
  ses_str="session $ses_id"
fi
mkdir -p "$out_dir"
cd "$out_dir" || { echo "Failed to change directory to $out_dir"; exit 1; }
export out_dir

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
  log_message INFO "No files to postprocess found in $input_dir using input_regex '$input_regex' (stream $stream_name)."
  exit 0
fi

"${R_HOME}/bin/Rscript" "$upd_job_status_path" --job_id "$SLURM_JOB_ID" --sqlite_db "$sqlite_db" --status "STARTED"

n_files=${#files[@]}
last_idx=$((n_files - 1))

# Write discovered files to a manifest (one per line) for the array job to read
filelist_path="${out_dir}/.postprocess_${stream_name}_filelist.txt"
printf '%s\n' "${files[@]}" > "$filelist_path"
log_message INFO "Wrote $n_files files to manifest: $filelist_path"

task_status_dir="${out_dir}/.postprocess_${stream_name}_task_status"
summary_json="${out_dir}/.postprocess_${stream_name}_array_summary.json"
mkdir -p "$task_status_dir"
rm -f "$task_status_dir"/*.env "$summary_json" "${summary_json%.json}.tsv"
log_message DEBUG "Using task status directory $task_status_dir"

# Build per-task log paths using %a for the Slurm array task ID
array_stdout_log="$(dirname $stdout_log)/postprocess_${stream_name}_task-%a_jobid-%A.out"
array_stderr_log="$(dirname $stdout_log)/postprocess_${stream_name}_task-%a_jobid-%A.err"

# Submit a single job array with concurrency throttle
array_jid=$(sbatch \
  --parsable \
  $sched_args \
  --job-name="postprocess_${stream_name}_array" \
  --output="$array_stdout_log" \
  --error="$array_stderr_log" \
  --array="0-${last_idx}%${max_concurrent_images}" \
  --export=ALL,filelist_path="$filelist_path",task_status_dir="$task_status_dir",stdout_log="$array_stdout_log",stderr_log="$array_stderr_log" \
  "$postprocess_image_sched_script")

log_message INFO "Submitted array job $array_jid for $n_files images (max concurrent: $max_concurrent_images) in $out_dir"

# Track the array parent in the DB
"${R_HOME}/bin/Rscript" "$insert_tracked_job_path" --sqlite_db "$sqlite_db" --job_id "$array_jid" --job_name "postprocess_${stream_name}_array" --scheduler "slurm" --scheduler_options "$sched_args"
"${R_HOME}/bin/Rscript" "$add_parent_path" --sqlite_db "$sqlite_db" --job_id "$array_jid" --parent_job_id "$SLURM_JOB_ID" --child_level 2

# Read the sentinel job ID written by submit_postprocess() in R
ses_suffix=""
if [[ -n "$ses_id" ]]; then
  ses_suffix="_ses-${ses_id}"
fi
sentinel_jid_file="$(dirname "$stdout_log")/.postprocess_${stream_name}${ses_suffix}_sentinel_jid"

if [[ -f "$sentinel_jid_file" ]]; then
  sentinel_jid=$(cat "$sentinel_jid_file")
  log_message INFO "Read sentinel job ID $sentinel_jid from $sentinel_jid_file"

  # Update the sentinel's dependency from afterany:parent to afterany:array.
  # This ensures the sentinel runs after the array completes, not after the parent.
  scontrol update JobId="$sentinel_jid" Dependency="afterany:${array_jid}"
  log_message INFO "Updated sentinel $sentinel_jid dependency to afterany:$array_jid"

  # Pass array metadata to sentinel via shared file
  sentinel_meta_file="${out_dir}/.postprocess_${stream_name}_sentinel_meta.env"
  cat > "$sentinel_meta_file" <<EOF
array_job_id=$array_jid
n_tasks=$n_files
task_status_dir=$task_status_dir
summary_json=$summary_json
EOF
  log_message DEBUG "Wrote sentinel metadata to $sentinel_meta_file"
else
  log_message WARNING "Sentinel job ID file not found at $sentinel_jid_file. Downstream dependency may not work correctly."
fi

# Parent job is done — sentinel handles completion/failure bookkeeping
"${R_HOME}/bin/Rscript" "$upd_job_status_path" --job_id "$SLURM_JOB_ID" --sqlite_db "$sqlite_db" --status "COMPLETED"
log_message INFO "Postprocess parent job completed for subject $sub_id $ses_str. Array ($array_jid) and sentinel (${sentinel_jid:-unknown}) are running."
exit 0

