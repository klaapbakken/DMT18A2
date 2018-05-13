#!/bin/bash
#$ -cwd
#$ -S /bin/bash
#$ -N DataMining
#$ -l mem_free=64G
#$ -pe threaded 24
#$ -e error.txt
#$ -o stdout.txt

# Initialize environment
source activate datamining

# Run Rscript
Rscript preprocess.R

# Deactivate env
deactivate
