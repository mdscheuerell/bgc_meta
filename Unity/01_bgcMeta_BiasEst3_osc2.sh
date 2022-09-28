#!/usr/bin/env bash
#SBATCH --account=PAS1621
#SBATCH --time=10:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=20
#SBATCH --mem=60gb
#SBATCH --job-name=bgcMetaBiasFits
#SBATCH --mail-type=ALL
#SBATCH --mail-user=hood.211@osu.edu


#COMMANDS TO RUN
ml R/4.2.1-gnu11.2

R < 04_model_fitting_BiasTerms_JMH2_OSC2.R --no-save