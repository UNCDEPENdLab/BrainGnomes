#!/bin/bash
# Default SLURM requests if not overridden on the command line
# These defaults are superseded by scheduler arguments built in submit_prefetch_templates()
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --time=0:30:00
#SBATCH --mem=16g

set -eE  # 'E' ensures ERR trap inherits in functions/subshells

# source bash functions used in pipeline shell scripts
[ -z "$pkg_dir" ] && echo "pkg_dir not set. Cannot locate required helper scripts" && exit 1
source "${pkg_dir}/shell_functions"

# Set trap for common termination signals and errors
trap 'trap_job_failure prefetch_templates SIGTERM' SIGTERM
trap 'trap_job_failure prefetch_templates SIGINT' SIGINT
trap 'trap_job_failure prefetch_templates SIGHUP' SIGHUP
trap 'trap_job_failure prefetch_templates SIGQUIT' SIGQUIT
trap 'traperror prefetch_templates $? $LINENO ${BASH_LINENO[0]} "$BASH_COMMAND" $(printf "::%s" ${FUNCNAME[@]:-})' ERR

####
# verify required arguments
[ -z "$prefetch_container" ] && echo "prefetch_container not set. Exiting." && exit 1
[ ! -f "$prefetch_container" ] && echo "prefetch_container $prefetch_container not found. Exiting." && exit 1
[ ! -r "$prefetch_container" ] && echo "prefetch_container $prefetch_container not readable. Exiting." && exit 1
[ -z "$prefetch_script" ] && echo "prefetch_script not set. Exiting." && exit 1
[ ! -f "$prefetch_script" ] && echo "prefetch_script $prefetch_script not found. Exiting." && exit 1
[ ! -r "$prefetch_script" ] && echo "prefetch_script $prefetch_script not readable. Exiting." && exit 1
[ -z "$templateflow_home" ] && echo "templateflow_home not set. Exiting." && exit 1
[ -z "$prefetch_spaces" ] && echo "prefetch_spaces not set. Exiting." && exit 1

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

log_flag=""
[[ -n "$log_file" ]] && log_flag="-l $log_file" # if log_file is set, pass it to rel

# ensure TemplateFlow home exists before mounting
if [ ! -d "$templateflow_home" ]; then
  log_message INFO "Creating missing templateflow_home directory: $templateflow_home"
  mkdir -p "$templateflow_home"
fi

# normalize important paths
prefetch_container=$(abspath "$prefetch_container")
prefetch_script=$(abspath "$prefetch_script")
templateflow_home=$(abspath "$templateflow_home")
prefetch_script_dir=$(dirname "$prefetch_script")

bind_args=()
bind_paths=("$templateflow_home" "$prefetch_script_dir")
for path in "${bind_paths[@]}"; do
  [ -z "$path" ] && continue
  bind_args+=("-B" "$path:$path")
done

log_message INFO "Prefetching TemplateFlow resources into $templateflow_home for spaces: $prefetch_spaces"
start_time=$(date +%s)

"${R_HOME}/bin/Rscript" "$upd_job_status_path" --job_id "$SLURM_JOB_ID" --sqlite_db "$sqlite_db" --status "STARTED"

# export necessary environment variables for the container
rel $rel_flag $log_flag export TEMPLATEFLOW_HOME="$templateflow_home"
rel $rel_flag $log_flag export APPTAINERENV_TEMPLATEFLOW_HOME="$templateflow_home"

# run the TemplateFlow prefetch helper inside the fMRIPrep container
rel $rel_flag $log_flag singularity exec --cleanenv --containall "${bind_args[@]}" \
  "$prefetch_container" python "$prefetch_script" --output-spaces "$prefetch_spaces" &
cmd_pid=$!
run_bg_and_wait "$cmd_pid" cmd_status

if [[ $cmd_status -eq 0 ]]; then
  if [[ -n "$prefetch_state_file" ]]; then
    state_dir=$(dirname "$prefetch_state_file")
    state_tmp="${prefetch_state_file}.tmp.$$"
    if mkdir -p "$state_dir"; then
      if {
        printf "status: COMPLETED\n"
        printf "templateflow_home: %s\n" "$templateflow_home"
        printf "spaces: %s\n" "$prefetch_spaces"
        printf "updated_at_utc: %s\n" "$(date -u '+%Y-%m-%dT%H:%M:%SZ')"
        printf "scheduler_job_id: %s\n" "$SLURM_JOB_ID"
      } > "$state_tmp" && mv -f "$state_tmp" "$prefetch_state_file"; then
        log_message INFO "Updated TemplateFlow prefetch state: $prefetch_state_file"
      else
        rm -f "$state_tmp"
        log_message WARN "Unable to update TemplateFlow prefetch state: $prefetch_state_file"
      fi
    else
      log_message WARN "Unable to create prefetch state directory: $state_dir"
    fi
  fi
  "${R_HOME}/bin/Rscript" "$upd_job_status_path" --job_id "$SLURM_JOB_ID" --sqlite_db "$sqlite_db" --status "COMPLETED" --output_dir "$templateflow_home"
  log_message INFO "Finished TemplateFlow prefetch (time elapsed: $(time_elapsed))"
else
  "${R_HOME}/bin/Rscript" "$upd_job_status_path" --job_id "$SLURM_JOB_ID" --sqlite_db "$sqlite_db" --status "FAILED" --cascade
  log_message ERROR "TemplateFlow prefetch failed (exit code $cmd_status)"
fi

exit $cmd_status
