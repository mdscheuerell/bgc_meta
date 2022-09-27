#!/usr/bin/env bash
#SBATCH --time=10:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=6
#SBATCH --mem=60gb
#SBATCH --job-name=bgcMetaBiasFits
#SBATCH --mail-type=ALL
#SBATCH --mail-user=hood.211@osu.edu


#COMMANDS TO RUN
module load gnu
module load R/4.0.2

R < 04_model_fitting_BiasTerms_JMH.R --no-save