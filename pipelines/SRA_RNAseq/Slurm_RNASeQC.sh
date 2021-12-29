#!/bin/sh
#SBATCH --nodes=1
#SBATCH --ntasks=2
#SBATCH --mem=8G
#SBATCH --time=1:00:00
#SBATCH --array=1-92
#SBATCH --output=./logs/RNASeQC.log
#SBATCH -p intel
#SBATCH --chdir=/bigdata/jialab/rli012/PCaDB/data/fromSRA/SRP163173/

rnaseqc=/bigdata/jialab/rli012/software/rnaseqc.v2.4.2.linux

N=$SLURM_ARRAY_TASK_ID
CPU=$SLURM_NTASKS

annotation=/rhome/rli012/bigdata/PCaDB/data/Reference/gencode.v38.annotation.collapsed.gtf

BAM=`ls ./BAM/*.bam | grep -v 'Transcriptome' | head -n $N | tail -n 1`
SAMPLE=${BAM%.bam}
SAMPLE=${SAMPLE#./BAM/}

echo 'Start QC...'
echo $BAM

### QC ###

$rnaseqc $annotation $BAM ./RNASeQC \
	--coverage \
	--sample ${SAMPLE}

echo 'Done!'
