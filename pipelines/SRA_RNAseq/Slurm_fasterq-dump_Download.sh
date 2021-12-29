#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=8
#SBATCH --mem=16G
#SBATCH --time=20:00:00
#SBATCH --array=1-92
#SBATCH --output=./logs/SRA_Download.log
#SBATCH -p intel
#SBATCH --chdir=/bigdata/jialab/rli012/PCaDB/data/fromSRA/SRP163173/

#sbatch --array 1-101 Slurm_fasterq-dump_Download.sh

#module load sratoolkit/2.10.0

fasterqdump=/rhome/rli012/bigdata/software/sratoolkit.2.10.8-centos_linux64/bin/fasterq-dump

CPU=$SLURM_NTASKS
if [ ! $CPU ]; then
   CPU=2
fi

N=$SLURM_ARRAY_TASK_ID
if [ ! $N ]; then
    N=$1
fi

FILE=`cat SRR_Acc_List.txt | head -n $N | tail -n 1`

if [ ! -e $FILE ]; then
	echo "Start Downloading ..."
    
	$fasterqdump $FILE -t tmp/
	gzip ${FILE}*
	
	echo "Download Completed !"
fi
