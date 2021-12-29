#!/bin/sh
#SBATCH --nodes=1
#SBATCH --ntasks=8
#SBATCH --mem=16G
#SBATCH --time=10:00:00
#SBATCH --array=1-184
#SBATCH --output=./logs/GZIP_FASTQ.log
#SBATCH -p batch
#SBATCH --chdir=/bigdata/jialab/rli012/PCaDB/data/fromSRA/SRP119917/

N=$SLURM_ARRAY_TASK_ID
CPU=$SLURM_NTASKS

cd FASTQ
FILE=`ls SRR*\.fastq | head -n $N | tail -n 1`

echo $FILE

gzip $FILE

echo 'Done!'

### All FASTQ files
# gzip ./FASTQ/*.fastq


