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
trap 'trap_job_failure bids_validation SIGTERM' SIGTERM
trap 'trap_job_failure bids_validation SIGINT' SIGINT
trap 'trap_job_failure bids_validation SIGHUP' SIGHUP
trap 'trap_job_failure bids_validation SIGQUIT' SIGQUIT

# add the job id to the log file variables in case of crash
stdout_log="${stdout_log//%j/$SLURM_JOB_ID}"
stderr_log="${stderr_log//%j/$SLURM_JOB_ID}"

####
#verify necessary arguments
[ -z "$bids_validator" ] && echo "bids_validator not set. Exiting" && exit 1
[ ! -f "$bids_validator" ] && echo "bids_validator $bids_validator not found. Exiting" && exit 1
[ ! -r "$bids_validator" ] && echo "bids_validator $bids_validator not readable. Exiting" && exit 1
[ -z "$bids_dir" ] && echo "bids_dir not set. Exiting." && exit 1
[ ! -d "$bids_dir" ] && echo "bids_dir $bids_dir not found. Exiting" && exit 1
[ ! -r "$bids_dir" ] && echo "bids_dir $bids_dir not readable. Exiting" && exit 1
[ -z "$sub_id" ] && echo "sub_id not set. Exiting." && exit 1
[ -z "$outfile" ] && outfile="bids_validator_output.html" # default output file name
[[ "$debug_pipeline" == "TRUE" ]] && debug_pipeline=1 || debug_pipeline=0 # set debug_pipeline to 1 if it is set to TRUE, otherwise set it to 0

cd $bids_dir || { echo "Failed to change directory to $bids_dir"; exit 1; }

####
#run bids-validator
[ $debug_pipeline -eq 1 ] && rel_flag=c # if debug_pipeline is 1, only echo command to log, don't run it
[[ -n "$cmd_log" ]] && cmd_log="-l $cmd_log" # if cmd_log is set, pass it to rel

rel c $cmd_log Starting bids validation for subject $sub_id at $(date +"%Y-%m-%d %H:%M:%S") # log the start of the bids-validator command

rel $rel_flag $cmd_log ${bids_validator} ${bids_dir} --outfile ${bids_dir}/${outfile}

[[ $? -eq 0 && $debug_pipeline -eq 0 ]] && date +"%Y-%m-%d %H:%M:%S" > ${bids_dir}/.bids_validation_complete

rel c $cmd_log Finished bids validation for subject $sub_id at $(date +"%Y-%m-%d %H:%M:%S") # log the start of the bids-validator command