
####################################################################
#######               cBioPortal Data Collection             #######
####################################################################

### helper function: select the most informative probe, MAX IQR
library(dplyr)

selectProbeFun <- function(expr) {
  expr$IQR <- apply(expr[,-which(colnames(expr)=='ID')], 1, IQR)
  
  expr <- expr %>% group_by(ID) %>% 
    filter(row_number() == which.max(IQR)) %>%
    column_to_rownames('ID')
  
  expr <- expr[,-which(colnames(expr)=='IQR')]
  
  return(expr)
  
}

### DKFZ dataset as an example
# wget https://cbioportal-datahub.s3.amazonaws.com/prostate_dkfz_2018.tar.gz -P data/cBioPortal/
# tar -xvzf prostate_dkfz_2018.tar.gz

dataset <- 'DKFZ'

exprData <- read.table('data/cBioPortal/prostate_dkfz_2018/data_mrna_seq_rpkm.txt',
                       sep='\t', header = T, stringsAsFactors = F)

## ENSEMBL 62; ftp://ftp.ensembl.org/pub/release-62/gtf/homo_sapiens/
gtf <- readGFF('data/Annotation/Homo_sapiens.GRCh37.62.gtf.gz', version=2L)
filter <- which(duplicated(gtf$gene_id))
gtf <- gtf[-filter,]

# exprData <- add_column(.data = exprData, .before = 3, Ensembl=NA)
# exprData$Ensembl <- gtf$gene_id[match(exprData$Hugo_Symbol, gtf$gene_name)]
# exprData[1:5,1:5]
# 
# filter <- which(duplicated(exprData$Ensembl))
# filter

exprData$ID <- gtf$gene_id[match(exprData$Hugo_Symbol, gtf$gene_name)]

idx <- which(!is.na(rowSums(exprData[,-ncol(exprData)])))
exprData <- selectProbeFun(exprData[idx,])

if (max(exprData) > 100) {
  exprData <- log2(exprData)
}

rownames(exprData) <- unlist(lapply(rownames(exprData), function(x) strsplit(x, '_|\\.', )[[1]][1]))

saveRDS(exprData, file=paste0('data/rData/', dataset, '_Expression.RDS'))
