#!/usr/bin/env bash
#SBATCH --time=10:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=6
#SBATCH --job-name=bgcMetaBiasFits
#SBATCH --mail-type=ALL
#SBATCH --mail-user=hood.211@osu.edu


#COMMANDS TO RUN
module load intel
module load R/3.5.1
module load cxx17
R < 04_model_fitting_BiasTerms_JMH.R --no-save