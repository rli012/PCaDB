#!/bin/sh
#SBATCH --nodes=1
#SBATCH --ntasks=2
#SBATCH --mem=8G
#SBATCH --time=1:00:00
#SBATCH --array=1-92
#SBATCH --output=./logs/FastQC.log
#SBATCH -p intel
#SBATCH --chdir=/bigdata/jialab/rli012/PCaDB/data/fromSRA/SRP163173/

fastqc=/bigdata/jialab/rli012/software/FastQC/fastqc

N=$SLURM_ARRAY_TASK_ID
CPU=$SLURM_NTASKS

FILE=`ls FASTQ/SRR*\.fastq.gz | head -n $N | tail -n 1`
PREFIX=${FILE%.fastq.gz}
PREFIX=${PREFIX#FASTQ/}

fq1=$FILE
fq2=${FILE/_1/_2}

#PREFIX=SRR6170071
#bam=${PREFIX}.bam
#fq1=FASTQ/${PREFIX}_1.fastq.gz
#fq2=FASTQ/${PREFIX}_2.fastq.gz

echo 'Start QC...'
echo $PREFIX

### QC ###

$fastqc $fq1 --outdir=./FastQC/
#$fastqc $fq2 --outdir=./FastQC/ 

echo 'Done!'
