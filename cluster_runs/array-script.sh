#!/bin/bash
#SBATCH --job-name=CKMRpop
#SBATCH --output=ck_out_%A_%a
#SBATCH --output=ck_err_%A_%a
#SBATCH --array=1-60%9
#SBATCH --cpus-per-task=4
#SBATCH --mem=60G



module load R

# get the parameters
eval $(./line-assign.sh $SLURM_ARRAY_TASK_ID array_jobs.tsv)


Rscript --vanilla species_2_run_script.R \
  $CZ $SF $DS ${SLURM_JOB_ID} > stdout_${SLURM_JOB_ID} 2> stderr_${SLURM_JOB_ID}
