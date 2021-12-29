#!/bin/sh
#SBATCH --nodes=1
#SBATCH --ntasks=16
#SBATCH --mem=64G
#SBATCH --time=10:00:00
#SBATCH --array=1-92
#SBATCH --output=./logs/STAR.Alignment.log
#SBATCH -p batch
#SBATCH --chdir=/bigdata/jialab/rli012/PCaDB/data/fromSRA/SRP163173/

STAR=/rhome/rli012/bigdata/software/STAR-2.7.9a/bin/Linux_x86_64/STAR
samtools=/rhome/rli012/bigdata/software/samtools-1.9/bin/samtools

annotation=/rhome/rli012/bigdata/PCaDB/data/Reference/gencode.v38.annotation.gtf # gtf annotation file
genomeFa=/rhome/rli012/bigdata/PCaDB/data/Reference/GRCh38.primary_assembly.genome.fa # fasta sequence file
genomeDir=/rhome/rli012/bigdata/PCaDB/data/Reference/GRCh38/ #output directory

N=$SLURM_ARRAY_TASK_ID
CPU=$SLURM_NTASKS

FILE=`ls FASTQ/SRR*\.fastq.gz | grep _1.fastq.gz | head -n $N | tail -n 1`
#FILE=`ls FASTQ/SRR*\.fastq | grep _1.fastq | head -n $N | tail -n 1`
PREFIX=${FILE%_1.fastq.gz}
PREFIX=${PREFIX#FASTQ/}

fq1=$FILE
fq2=${FILE/_1/_2}

#PREFIX=SRR6170071
#bam=${PREFIX}.bam
#fq1=FASTQ/${PREFIX}_1.fastq.gz
#fq2=FASTQ/${PREFIX}_2.fastq.gz

echo 'Start Alignment...'
echo $PREFIX

### Alignment ###

$STAR --runThreadN $CPU \
	  --genomeDir $genomeDir \
      --twopassMode Basic \
	  --readFilesIn $fq1 $fq2 \
	  --readFilesCommand zcat \
	  --quantMode TranscriptomeSAM \
	  --sjdbGTFfile $annotation \
	  --outSAMtype BAM Unsorted \
      --outFilterIntronMotifs RemoveNoncanonicalUnannotated \
      --limitBAMsortRAM 18982366129 \
	  --outFileNamePrefix BAM/${PREFIX}

$samtools sort -@ $CPU BAM/${PREFIX}Aligned.out.bam -T BAM/${PREFIX} -o BAM/${PREFIX}.bam
mv ./BAM/${PREFIX}Aligned.toTranscriptome.out.bam ./BAM/${PREFIX}.Transcriptome.bam

rm BAM/${PREFIX}Aligned.out.bam
rm -r ./BAM/${PREFIX}_STAR*

echo 'Done!'
