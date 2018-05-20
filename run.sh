#!/bin/bash
#$ -cwd
#$ -S /bin/bash
#$ -N DataMining
#$ -l mem_free=128G
#$ -pe threaded 16
#$ -e error.txt
#$ -o stdout.txt

# Initialize environment
source activate datamining

# Run Rscript
Rscript workflow.R

# Deactivate env
#deactivate
