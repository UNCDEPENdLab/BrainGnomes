#!/bin/bash
# Default SLURM requests if not overridden on command line
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --time=6:00:00
#SBATCH --mem=48g

## N.B. This script postprocesses a single input provided via input_file (environment variable), whereas
## postprocess_subject.sbatch looks for all images matching the --input_regex and submits
## jobs pointing to this script for each image it finds. This allows parallelization over images.

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
[ -z "$postprocess_cli" ] && echo "postprocess_cli not set. Exiting." && exit 1
[ -z "$postprocess_rscript" ] && echo "postprocess_rscript not set. Exiting." && exit 1
[ ! -f "$postprocess_rscript" ] && echo "postprocess_rscript $postprocess_rscript not found. Exiting" && exit 1
[ ! -r "$postprocess_rscript" ] && echo "postprocess_rscript $postprocess_rscript not readable. Exiting" && exit 1
# If running as an array task, read the assigned file from the manifest
if [[ -n "$SLURM_ARRAY_TASK_ID" && -n "$filelist_path" ]]; then
  [ ! -f "$filelist_path" ] && echo "filelist_path $filelist_path not found. Exiting" && exit 1
  input_file=$(sed -n "$((SLURM_ARRAY_TASK_ID + 1))p" "$filelist_path")
  [ -z "$input_file" ] && echo "No file at array index $SLURM_ARRAY_TASK_ID in $filelist_path. Exiting" && exit 1
fi

[ -z "$input_file" ] && echo "input_file not set. Exiting." && exit 1
[ ! -f "$input_file" ] && echo "input_file $input_file not found. Exiting" && exit 1
[ ! -r "$input_file" ] && echo "input_file $input_file not readable. Exiting" && exit 1
[ -z "$log_file" ] && echo "log_file not set. Exiting" && exit 1
[ -z "$R_HOME" ] && echo "R_HOME not set. Exiting" && exit 1

# add the job id to the log file variables in case of crash
[ -z "$stdout_log" ] && echo "stdout_log not set. Exiting." && exit 1
[ -z "$stderr_log" ] && echo "stderr_log not set. Exiting." && exit 1
stdout_log="${stdout_log//%j/$SLURM_JOB_ID}"
stderr_log="${stderr_log//%j/$SLURM_JOB_ID}"
# Resolve array placeholders if running as an array task
is_array_task=0
task_idx=""
task_job_id="$SLURM_JOB_ID"
task_array_job_id=""
if [[ -n "$SLURM_ARRAY_TASK_ID" ]]; then
  is_array_task=1
  task_idx="$SLURM_ARRAY_TASK_ID"
  task_array_job_id="${SLURM_ARRAY_JOB_ID:-$SLURM_JOB_ID}"
  task_job_id="${task_array_job_id}_${SLURM_ARRAY_TASK_ID}"
  stdout_log="${stdout_log//%a/$SLURM_ARRAY_TASK_ID}"
  stderr_log="${stderr_log//%a/$SLURM_ARRAY_TASK_ID}"
  stdout_log="${stdout_log//%A/$SLURM_ARRAY_JOB_ID}"
  stderr_log="${stderr_log//%A/$SLURM_ARRAY_JOB_ID}"
fi

write_task_status() {
  local task_state="$1"
  local task_exit_code="$2"
  [[ $is_array_task -eq 1 && -n "$task_status_dir" && -n "$task_idx" ]] || return 0
  mkdir -p "$task_status_dir"
  local task_status_file="${task_status_dir}/task_${task_idx}.env"
  : > "$task_status_file"
  printf 'task_index=%q\n' "$task_idx" >> "$task_status_file"
  printf 'task_job_id=%q\n' "$task_job_id" >> "$task_status_file"
  printf 'task_array_job_id=%q\n' "$task_array_job_id" >> "$task_status_file"
  printf 'task_input_file=%q\n' "$input_file" >> "$task_status_file"
  printf 'task_status=%q\n' "$task_state" >> "$task_status_file"
  printf 'task_exit_code=%q\n' "$task_exit_code" >> "$task_status_file"
  printf 'task_stdout_log=%q\n' "$stdout_log" >> "$task_status_file"
  printf 'task_stderr_log=%q\n' "$stderr_log" >> "$task_status_file"
}

mkdir -p "$out_dir"
cd "$out_dir" || { echo "Failed to change directory to $out_dir"; exit 1; }

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

log_message INFO Starting postprocessing for image $input_file in $out_dir with mem: $mem

if [[ $is_array_task -eq 0 ]]; then
  "${R_HOME}/bin/Rscript" "$upd_job_status_path" --job_id "$SLURM_JOB_ID" --sqlite_db "$sqlite_db" --status "STARTED"
fi

rel $rel_flag $log_flag "${R_HOME}/bin/Rscript" --vanilla "$postprocess_rscript" --input="$input_file" $postprocess_cli &
cmd_pid=$!
run_bg_and_wait "$cmd_pid" cmd_status

if [[ $cmd_status -eq 0 ]]; then
  write_task_status "COMPLETED" "$cmd_status"
  if [[ $is_array_task -eq 0 ]]; then
    "${R_HOME}/bin/Rscript" "$upd_job_status_path" --job_id "$SLURM_JOB_ID" --sqlite_db "$sqlite_db" --status "COMPLETED" --output_dir "$out_dir"
  fi
  log_message INFO Finished postprocessing for image $input_file "(time elapsed: $(time_elapsed))"
else
  write_task_status "FAILED" "$cmd_status"
  if [[ $is_array_task -eq 0 ]]; then
    "${R_HOME}/bin/Rscript" "$upd_job_status_path" --job_id "$SLURM_JOB_ID" --sqlite_db "$sqlite_db" --status "FAILED"
  fi
  log_message ERROR "Postprocessing failed for image $input_file (exit code $cmd_status)"
fi
exit $cmd_status
