#!/bin/sh
#SBATCH --nodes=1
#SBATCH --ntasks=8
#SBATCH --mem=16G
#SBATCH --time=1:00:00
#SBATCH --array=1-92
#SBATCH --output=./logs/Salmon_Quant.log
#SBATCH -p intel
#SBATCH --chdir=/bigdata/jialab/rli012/PCaDB/data/fromSRA/SRP163173/

STAR=/rhome/rli012/bigdata/software/STAR-2.7.9a/bin/Linux_x86_64/STAR
samtools=/rhome/rli012/bigdata/software/samtools-1.9/bin/samtools
salmon=/rhome/rli012/bigdata/software/salmon-1.5.2_linux_x86_64/bin/salmon

annotation=/rhome/rli012/bigdata/PCaDB/data/Reference/gencode.v38.annotation.gtf # gtf annotation file
genomeFa=/rhome/rli012/bigdata/PCaDB/data/Reference/GRCh38.primary_assembly.genome.fa # fasta sequence file
genomeDir=/rhome/rli012/bigdata/PCaDB/data/Reference/GRCh38/ #output directory
transcripts=/rhome/rli012/bigdata/PCaDB/data/Reference/gencode.v38.transcripts.fa

N=$SLURM_ARRAY_TASK_ID
CPU=$SLURM_NTASKS

FILE=`ls BAM/*.Transcriptome.bam | head -n $N | tail -n 1`
PREFIX=${FILE%.Transcriptome.bam}
PREFIX=${PREFIX#BAM/}

bam=$FILE

#PREFIX=SRR2973290
#bam=${PREFIX}.bam

echo 'Salmon Quant...'
echo $PREFIX

### Quantification ###

$salmon quant --threads 8 \
	--targets $transcripts \
	--gencode \
	--libType A \
	--alignments $bam \
	--output ./Salmon/$PREFIX 

echo 'Done!'
