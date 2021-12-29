#!/bin/sh
#SBATCH --nodes=1
#SBATCH --ntasks=8
#SBATCH --mem=16G
#SBATCH --time=10:00:00
#SBATCH --output=./logs/featureCounts.log
#SBATCH -p intel
#SBATCH --chdir=/bigdata/jialab/rli012/PCaDB/data/fromSRA/SRP163173/

SRA=SRP163173

featureCounts=/rhome/rli012/bigdata/software/subread-2.0.3-Linux-x86_64/bin/featureCounts

annotation=/rhome/rli012/bigdata/PCaDB/data/Reference/gencode.v38.annotation.gtf # gtf annotation file
genomeFa=/rhome/rli012/bigdata/PCaDB/data/Reference/GRCh38.primary_assembly.genome.fa # fasta sequence file
genomeDir=/rhome/rli012/bigdata/PCaDB/data/Reference/GRCh38/ #output directory

N=$SLURM_ARRAY_TASK_ID
CPU=$SLURM_NTASKS

BAMS=`ls ./BAM/*.bam | grep -v 'Transcriptome'`
OUTPUT=./featureCounts/${SRA}.count.txt

echo 'Start Counting...'

### Quantification ###

$featureCounts -T $CPU -p --countReadPairs -g gene_id -a $annotation -o ./featureCounts/count.tmp $BAMS

## SE
#$featureCounts -T $CPU -g gene_id -a $annotation -o ./featureCounts/count.tmp $BAMS

tail -n+2 ./featureCounts/count.tmp | cut --complement -f2-5 > $OUTPUT

echo 'Done!'
