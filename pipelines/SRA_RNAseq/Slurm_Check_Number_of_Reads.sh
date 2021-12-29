#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=8
#SBATCH --mem=24G
#SBATCH --time=5:00:00
#SBATCH --output=./logs/Check_number_of_reads.log
#SBATCH -p batch
#SBATCH --chdir=/bigdata/jialab/rli012/PCaDB/data/fromSRA/SRP133573

#sbatch Slurm_Check_Number_of_Reads.sh

CPU=$SLURM_NTASKS
if [ ! $CPU ]; then
   CPU=2
fi

N=$SLURM_ARRAY_TASK_ID
if [ ! $N ]; then
    N=$1
fi

FILES=`cat SRR_Acc_List.txt`

for FILE in $FILES
do
	echo $FILE
	FQ1=./FASTQ/$FILE\_1.fastq.gz
	FQ2=./FASTQ/$FILE\_2.fastq.gz

	zcat $FQ1 | wc -l
	zcat $FQ2 | wc -l

done
