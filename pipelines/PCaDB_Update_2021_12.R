setwd('~/Documents/Publications/PCaDB/v1/')

datasets <- read_excel('www/downloads/PCaDB_Transcriptome_Datasets_Summary.xlsx')

View(datasets)

#datasets <- datasets[,1:13]

datasets <- data.frame(datasets, stringsAsFactors = F)
colnames(datasets)[1] <- 'PCaDB.ID'
colnames(datasets)[2] <- 'Dataset'
colnames(datasets)[5] <- 'GEO/ArrayExpress'
colnames(datasets)[6] <- 'SRA'
colnames(datasets)[8] <- 'N'

saveRDS(datasets, file='data/PCaDB_Datasets.RDS')


#######

meta.data <- expr.data <- list()

for (dt in datasets$Dataset) {
  print (dt)
  
  fl.name <- paste0('v1/www/downloads/', dt, '_eSet.RDS')
  
  eSet <- readRDS(fl.name)
  
  expr.data[[dt]] <- round(exprs(eSet),3)
  meta.data[[dt]] <- data.frame(pData(eSet), stringsAsFactors = F)
  
}

saveRDS(meta.data, file = 'v1/data/PCaDB_Metadata.RDS')
saveRDS(expr.data, file = 'v1/data/PCaDB_Expression.RDS')

saveRDS(expr.data, file = 'v1/data/PCaDB_Expression.UnCompressed.RDS', compress = FALSE)



###
setwd('~/Documents/Publications/PCaDB/v1/')
datasets <- readRDS('data/PCaDB_Datasets.RDS')
View(datasets)

meta.data <- readRDS('data/PCaDB_Metadata.RDS')
expr.data <- readRDS('data/PCaDB_Expression.RDS')

cpm.datasets <- datasets$Dataset[c(1,5,51:66)]
cpm.datasets

pca.list <- list()

for (prj in datasets$Dataset) {
  
  print (prj)
  
  expr <- expr.data[[prj]]
  meta <- meta.data[[prj]]
  
  if (prj %in% cpm.datasets) {
    keep <- rowSums(expr > log2(1)) >= 0.5*ncol(expr)
  } else if (prj %in% c('DKFZ')) {
    keep <- rowSums(expr > log2(0.5)) >= 0.5*ncol(expr)
  } else {
    keep <- rownames(expr)
  }
  
  dataForPCA <- expr[keep,]
  
  filter <- which(rowSums(is.na(dataForPCA))>0)
  
  if (length(filter)>0) {
    dataForPCA <- dataForPCA[-filter,]
  }
  
  filter <- which(apply(dataForPCA, 1, sd)==0)
  
  if (length(filter)>0) {
    dataForPCA <- t(scale(t(dataForPCA[-filter,])))
    
  } else {
    dataForPCA <- t(scale(t(dataForPCA)))
  }
  
  pcaResults <- prcomp(dataForPCA)
  sumpca <- summary(pcaResults)
  
  pc1 <- round(sumpca$importance[2,1]*100,2)
  pc2 <- round(sumpca$importance[2,2]*100,2)
  pc3 <- round(sumpca$importance[2,3]*100,2)
  
  dataForPCAPlot <- data.frame(PC1=pcaResults$rotation[,1],
                               PC2=pcaResults$rotation[,2],
                               PC3=pcaResults$rotation[,3],
                               pc1, pc2, pc3,
                               Sample=rownames(pcaResults$rotation),
                               Sample.Type=meta$pcadb_group,
                               #Group=factor(meta$pcadb_group), # , levels=group.levels
                               #Disease.Status=meta$sample_type,
                               stringsAsFactors = F)
  
  pca.list[[prj]] <- dataForPCAPlot
  
}


saveRDS(pca.list, file='data/PCaDB_PCA_Analysis.RDS')


### ???

lapply(meta.data, ncol)
meta.table <- data.frame(do.call(rbind, meta.data), stringsAsFactors = F)

sample.freq <- table(meta.table$sample_type)
dataForPiePlot <- data.frame(num=as.numeric(sample.freq), sam=names(sample.freq))
View(dataForPiePlot)
saveRDS(dataForPiePlot, file='data/PCaDB_Pie_Sample_Type.RDS')


###

i <- 50
for (dt in datasets$Dataset[51:77]) {
  i <- i + 1
  message (dt)
  message (i)
  
  meta <- meta.data[[dt]]
  
  #print (meta$preop_psa) # 51, 64, 65
  #print (meta$gleason_group) # 51, 72, 74
  #print (meta$bcr_status) # 51, 52
  #print (meta$time_to_bcr)
  print (meta$pcadb_group)
}


View(meta.data[[65]])


#### Multivariate CoxPH

for (dt in datasets$Dataset[1:10]) {
  message (dt)
  
  meta <- meta.data[[dt]]
  
  idx <- which(colSums(is.na(meta)) != nrow(meta))
  
  print (colnames(meta)[idx])
}



# PCaDB ID	Dataset	Age at Diagnosis	Preop PSA	Gleason Score	Clinical T Stage
# PCaDB0001	TCGA-PRAD	Y	Y	Y	Y
# PCaDB0002	CPC-Gene	Y	Y	N	Y
# PCaDB0003	Taylor	N	N	Y	Y
# PCaDB0004	DKFZ	Y	Y	Y	N
# PCaDB0005	GSE54460	N	Y	Y	N
# PCaDB0006	Cambridge	Y	Y	Y	Y
# PCaDB0007	Stockholm	N	Y	Y	Y
# PCaDB0008	CancerMap	N	Y	Y	N
# PCaDB0009	CIT	Y	Y	Y	N
# PCaDB0010	Belfast	Y	Y	Y	N
# Continous	Continous	Categorical	Categorical
# Low (≤ 7); High ( > 7)	Low (≤ T2); High ( > T2)

for (dt in datasets$Dataset[1:10]) {
  message (dt)
  
  meta <- meta.data[[dt]]
  
  idx <- which(colSums(is.na(meta)) != nrow(meta))
  
  print (colnames(meta)[idx])
}

library(cowplot)

meta.data <- readRDS('data/PCaDB_Metadata.RDS')
expr.data <- readRDS('data/PCaDB_Expression.RDS')

bcr.dataset <- datasets$Dataset[1:10]

gene.annotation <- readRDS('data/PCaDB_Gene_Annotation.RDS')

###
names(meta.data)
projects <- bcr.dataset
projects

coxph.list <- list()
#km.list <- list()

for (prj in projects) {
  
  print (prj)
  
  expr <- expr.data[[prj]]
  meta <- meta.data[[prj]]
  
  if (prj %in% c('TCGA-PRAD','GSE54460')) {
    keep <- rowSums(expr > log2(1)) >= 0.5*ncol(expr)
  } else if (prj %in% c('DKFZ')) {
    keep <- rowSums(expr > log2(0.5)) >= 0.5*ncol(expr)
  } else {
    keep <- rownames(expr)
  }
  
  samples <- which(meta$sample_type=='Tumor' | meta$sample_type=='Primary')
  
  expr <- expr[keep,samples]
  meta <- meta[samples,]
  
  gene.id <- rownames(expr)
  gene.name <- gene.annotation[gene.id, 'gene_name']
  
  time.to.bcr <- as.numeric(meta$time_to_bcr)
  bcr.status <- as.numeric(meta$bcr_status)
  
  age <- as.numeric(meta$age_at_diagnosis)
  psa <- as.numeric(meta$preop_psa)
  
  gleason <- rep(NA,length(samples))
  gleason[meta$gleason_score>7] <- 'High'
  gleason[meta$gleason_score<=7] <- 'Low'
  gleason <- factor(gleason, levels=c('Low','High'))

  stage <- rep(NA, length(samples))
  if (prj %in% c('TCGA-PRAD','CPC-Gene','Taylor')) {
    stage[grepl('T1|T2|T0|Tx', meta$clinical_t_stage, ignore.case = T)] <- 'Low'
    stage[grepl('T3|T4', meta$clinical_t_stage)] <- 'High'
  } else if (prj %in% c('Cambridge','Stockholm')) {
    stage[grepl('T1|T2|T0|Tx', meta$clinical_stage, ignore.case = T)] <- 'Low'
    stage[grepl('T3|T4', meta$clinical_stage)] <- 'High'
  }
  
  stage <- factor(stage, levels=c('Low','High'))
  
  coxTable <- c()
  for (g in gene.id) {
    
    g.name <- gene.annotation[g, 'gene_name']
    
    score <- as.numeric(expr[g,])
    risk.group <- score > median(score, na.rm = T)
    
    if (prj %in% c('TCGA-PRAD','Cambridge')) {
      coxtest <- coxph(Surv(time.to.bcr, bcr.status) ~ score+age+psa+gleason+stage)
    } else if (prj %in% c('CPC-Gene')) {
      coxtest <- coxph(Surv(time.to.bcr, bcr.status) ~ score+age+psa+stage)
    } else if (prj %in% c('Taylor')) {
      coxtest <- coxph(Surv(time.to.bcr, bcr.status) ~ score+gleason+stage)
    } else if (prj %in% c('DKFZ','CIT','Belfast')) {
      coxtest <- coxph(Surv(time.to.bcr, bcr.status) ~ score+age+psa+gleason)
    } else if (prj %in% c('GSE54460','CancerMap')) {
      coxtest <- coxph(Surv(time.to.bcr, bcr.status) ~ score+psa+gleason)
    } else if (prj %in% c('Stockholm')) {
      coxtest <- coxph(Surv(time.to.bcr, bcr.status) ~ score+psa+gleason+stage)
    } 

    summcph <- summary(coxtest)
    
    coeffs <- as.numeric(c(summcph$coefficients[1,2], summcph$conf.int[1,3:4], 
                           summcph$coefficients[1,5]))
    
    if (coeffs[4]>0.05| is.na(coeffs[4])) {
      coxTable <- coxTable
    } else {
      coxTable <- rbind(coxTable, c(g, g.name, coeffs))
    }
  }
  
  ###
  coxTable <- data.frame(coxTable, row.names = NULL, stringsAsFactors = F)
  colnames(coxTable) <- c('Ensembl','Symbol','Hazard.Ratio','Lower95','Upper95','P.Value')
  #coxTable$FDR <- p.adjust(coxTable$P.Value, method='BH')
  
  coxTable[,3:6] <- apply(coxTable[,3:6], 2, as.numeric)
  
  o <- order(coxTable$P.Value, decreasing = F)
  coxTable <- coxTable[o,]
  
  coxph.list[[prj]] <- coxTable
  
}

saveRDS(coxph.list, file='data/PCaDB_Survival_Multivariate_CoxPH_P0.05.RDS')



###
signature <- read_excel('www/downloads/PCaDB_Prognostic_Signatures_Gene_List.xlsx', skip = 2)
signature

signature <- data.frame(signature, stringsAsFactors = F)
saveRDS(signature, file='data/PCaDB_Signatures.RDS')
