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

# Load the right instance of R to make this go -- TODO: need to abstract this as a configuration step
module use /proj/mnhallqlab/sw/modules
module load r/4.2.1

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
[ -z "$postproc_cli" ] && echo "postproc_cli not set. Exiting." && exit 1
[ -z "$postproc_rscript" ] && echo "postproc_rscript not set. Exiting." && exit 1
[ ! -f "$postproc_rscript" ] && echo "postproc_rscript $postproc_rscript not found. Exiting" && exit 1
[ ! -r "$postproc_rscript" ] && echo "postproc_rscript $postproc_rscript not readable. Exiting" && exit 1
[ -z "$input_file" ] && echo "input_file not set. Exiting." && exit 1
[ ! -f "$input_file" ] && echo "input_file $input_file not found. Exiting" && exit 1
[ ! -r "$input_file" ] && echo "input_file $input_file not readable. Exiting" && exit 1

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
[[ "$debug_pipeline" -eq 1 ]] && rel_flag=c #if debug_pipeline is 1, only echo command to log, don't run it

log_flag=""
[[ -n "$log_file" ]] && log_flag="-l $log_file" # if log_file is set, pass it to rel

mem=$((SLURM_MEM_PER_NODE / 1024))GB
start_time=$(date +%s)

log_message INFO Starting postprocessing for image $input_file in $out_dir with mem: $mem

rel $rel_flag $log_flag Rscript --vanilla "$postproc_rscript" --input="$input_file" $postproc_cli &
    
wait

log_message INFO Finished postprocessing for image $input_file "(time elapsed: $(time_elapsed))"
