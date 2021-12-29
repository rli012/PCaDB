#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=8
#SBATCH --mem=16G
#SBATCH --time=10:00:00
#SBATCH --output=./logs/MultiQC.log
#SBATCH -p intel
#SBATCH --chdir=/bigdata/jialab/rli012/PCaDB/data/fromSRA/SRP163173/

#source /rhome/rli012/bigdata/env/bin/activate
#export PYTHONPATH=/rhome/rli012/bigdata/G/env/lib/python2.7/site-packages

cd ./MultiQC
multiqc ../FastQC ../RNASeQC ../BAM ../featureCounts # Salmon

echo "done"


### Installation

#virtualenv env
#source env/bin/activate
#export PYTHONPATH=`pwd`/env/lib/python2.7/site-packages
# #pip install --upgrade pip
#pip install multiqc

#pip3 install multiqc --user

