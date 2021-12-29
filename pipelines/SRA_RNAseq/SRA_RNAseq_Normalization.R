
########################################################################
#######                 SRA RNAseq Normalization                 #######
########################################################################

library(rtracklayer)
library(tibble)
library(edgeR)

setwd('~/bigdata/PCaDB/')

#gtf.file <- '~/bigdata/PCaDB/data/Reference/gencode.v38.annotation.gtf'
#gtf <- readGFF(gtf.file, version=2L)
#gtf <- gtf[which(gtf$type=='gene'),]
#dim(gtf)

project <- 'SRP163173'

expr.file <- paste0('data/fromSRA/', project, '/featureCounts/', project, '.count.txt')
exprData <- read.table(expr.file, header = T, stringsAsFactors = F)
exprData[1:5,1:5]

filter <- grep('PAR_Y', exprData$Geneid)
filter

exprData <- exprData[-filter,]

rownames(exprData) <- unlist(lapply(exprData$Geneid, function(x) strsplit(x, '_|\\.')[[1]][1]))
colnames(exprData) <- gsub('\\.|bam', '', colnames(exprData), ignore.case = T)
exprData[1:5,]

exprData <- exprData[,-c(1:2)]

dge <-  DGEList(counts = exprData)

### TMM normalization
dge = calcNormFactors(dge, method = 'TMM')

exprLogCPM <- edgeR::cpm(dge,log = TRUE) ### for visualization
dim(exprLogCPM)

saveRDS(exprLogCPM, file=paste0('data/rData/', project, '_Expression.RDS'))
