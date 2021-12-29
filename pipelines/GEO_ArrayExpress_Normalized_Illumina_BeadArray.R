
#################################################################################
#######            GEO/ArrayExpress Data Collection (Normalized)          #######
#################################################################################

### Illumina HumanHT-12 V4.0 Expression Beadchip as an example

library(dplyr)
library(tibble)

### helper function: select the most informative probe, MAX IQR

selectProbeFun <- function(expr) {
  expr$IQR <- apply(expr[,-which(colnames(expr)=='ID')], 1, IQR)
  
  expr <- expr %>% group_by(ID) %>% 
    filter(row_number() == which.max(IQR)) %>%
    column_to_rownames('ID')
  
  expr <- expr[,-which(colnames(expr)=='IQR')]
  
  return(expr)
  
}

pcadb.anno <- readRDS('data/Homo_Sapiens_Gene_Annotation_ENSEMBL_HGNC_ENTRE.RDS')

# BiocManager::install('illuminaHumanv3.db')
library(illuminaHumanv4.db)
#columns(illuminaHumanv4.db)

library(illuminaHumanv3.db)
#columns(illuminaHumanv3.db)

### helper function: map the probe ids with Ensembl ids
IlluminaFun <- function(exprData, anno='illuminaHumanv4.db') {
  
  if (!exists('pcadb.anno')) {
    pcadb.anno <- readRDS('data/Homo_Sapiens_Gene_Annotation_ENSEMBL_HGNC_ENTRE.RDS')
  }
  
  if (anno=='illuminaHumanv4.db') {
    illumina.anno <- AnnotationDbi::select(illuminaHumanv4.db,
                                           keys = rownames(exprData),
                                           columns=c('ENSEMBL', 'GENETYPE', "SYMBOL","ENTREZID"), # "SYMBOL","ENTREZID",
                                           keytype="PROBEID")
    
  } else if (anno=='illuminaHumanv3.db') {
    illumina.anno <- AnnotationDbi::select(illuminaHumanv3.db,
                                           keys = rownames(exprData),
                                           columns=c('ENSEMBL', 'GENETYPE', "SYMBOL","ENTREZID"), # "SYMBOL","ENTREZID",
                                           keytype="PROBEID")
    
  }
  
  idx <- which(startsWith(illumina.anno$ENSEMBL, 'ENSG'))
  illumina.anno <- illumina.anno[idx,]
  
  filter <- which(!illumina.anno$ENSEMBL %in% pcadb.anno$ensembl_id)
  illumina.anno <- illumina.anno[-filter,]
  
  illumina.anno <- illumina.anno %>% group_by(PROBEID) %>% 
    filter(if (length(GENETYPE)>1) GENETYPE == 'protein-coding' else !is.na(GENETYPE))
  
  ### optional (if a probe mapped to multiple genes with some of them are novel proteins)
  
  illumina.anno$PROBE_ENTREZ_ID <- paste0(illumina.anno$PROBEID, '_', illumina.anno$ENTREZID)
  dup.probe.entrez <- illumina.anno$PROBE_ENTREZ_ID[which(duplicated(illumina.anno$PROBE_ENTREZ_ID))]
  
  idx <- which(illumina.anno$PROBE_ENTREZ_ID %in% dup.probe.entrez)
  illumina.anno$ENSEMBL_ENTREZ_ID <- paste0(illumina.anno$ENSEMBL, '_', illumina.anno$ENTREZID)
  
  i <- match(illumina.anno$ENSEMBL_ENTREZ_ID[idx], paste0(pcadb.anno$ensembl_id, '_', pcadb.anno$entrez_id))
  i <- i[which(!is.na(pcadb.anno$tax_id[i]))]
  
  filter <- idx[which(!illumina.anno$ENSEMBL_ENTREZ_ID[idx] %in% paste0(pcadb.anno$ensembl_id, '_', pcadb.anno$entrez_id)[i])]
  illumina.anno <- illumina.anno[-filter,]
  
  #### for probes mapped to multiple genes
  probes <- illumina.anno$PROBEID[which(duplicated(illumina.anno$PROBEID))]
  filter <- which(illumina.anno$PROBEID %in% probes)
  illumina.anno <- illumina.anno[-filter,]
  
  ###
  exprData <- data.frame(exprData[illumina.anno$PROBEID,])
  exprData$ID <- illumina.anno$ENSEMBL
  
  idx <- which(!is.na(rowSums(exprData[,-ncol(exprData)])))
  exprData <- selectProbeFun(exprData[idx,])
  
  if (max(exprData) > 100) {
    exprData <- log2(exprData)
  }
  
  rownames(exprData) <- unlist(lapply(rownames(exprData), function(x) strsplit(x, '_|\\.', )[[1]][1]))
  
  return (exprData)
  
}

gse <- 'GSE28680' # Illumina HumanHT-12 V4.0 Expression Beadchip
seriesMatrix <- getGEO(gse, AnnotGPL = FALSE, getGPL = FALSE, GSEMatrix = TRUE, destdir = 'data/fromGEO/') # AnnotGPL = TRUE
i.matrix <- 1
exprData <- exprs(seriesMatrix[[i.matrix]])

exprData <- IlluminaFun(exprData, anno = 'illuminaHumanv4.db')

saveRDS(exprData, file=paste0('data/rData/', gse, '_Expression.RDS'))