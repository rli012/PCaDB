
##########################################################################################
# ===================================== Expression ===================================== #
##########################################################################################

### Affymetrix

# http://brainarray.mbni.med.umich.edu/Brainarray/Database/CustomCDF/CDF_download.asp
# R Source Package: O

# Affymetrix Human Exon 1.0 ST Array: pd.huex10st.hs.gencodeg
# Affymetrix Human Gene 2.0 ST Array: pd.huex20st.hs.gencodeg
# Affymetrix Human Transcriptome Array 2.0: pd.hta20.hs.gencodeg
# Affymetrix Human Genome U133A Array: pd.hgu133a.hs.gencodeg
# Affymetrix Human Genome U133 Plus 2.0 Array: pd.hgu133plus2.hs.gencodeg
# Affymetrix Human Genome U133A 2.0 Array: pd.hgu133a2.hs.gencodeg

# library(pd.hg.u133.plus.2) # from Bioconductor (NOT IN USE)

setwd('~/bigdata/rli012/PCaDB/')

install.packages("http://mbni.org/customcdf/24.0.0/gencodeg.download/pd.hgu133a2.hs.gencodeg_24.0.0.tar.gz",
                repos = NULL, type = "source")

library(pd.huex10st.hs.gencodeg)
library(pd.hgu133a.hs.gencodeg)
library(pd.hgu133plus2.hs.gencodeg)
library(pd.hgu133a2.hs.gencodeg)

library(GEOquery)
gse <- 'GSE12378'
gse <- 'GSE30521'
anno <- 'pd.huex10st.hs.gencodeg'

gse <- 'GSE2443'
anno <- 'pd.hgu133a.hs.gencodeg'

gse <- 'GSE26910' # 12 samples
gse <- 'GSE32448'
anno <- 'pd.hgu133plus2.hs.gencodeg'

gse <- 'GSE6956'
gse <- 'GSE7055'
anno <- 'pd.hgu133a2.hs.gencodeg'

filePaths = getGEOSuppFiles(gse, baseDir = 'data/fromGEO', makeDirectory = FALSE, filter_regex = 'RAW')
untar(paste0('data/fromGEO/', gse, '_RAW.tar'), exdir = paste0('data/fromGEO/', gse, '_RAW'))

celFiles = list.celfiles(paste0('data/fromGEO/', gse, '_RAW'), full.names=T, listGzipped=T)

rawData = read.celfiles(celFiles, pkgname = anno)

if (gse=='GSE26910') {
  rawData = read.celfiles(celFiles[1:12], pkgname = anno)
}

probesetData = oligo::rma(rawData)

exprData = exprs(probesetData)
exprData[1:5,1:5]

rownames(exprData) <- unlist(lapply(rownames(exprData), function(x) strsplit(x, '_|\\.')[[1]][1]))
colnames(exprData) <- unlist(lapply(colnames(exprData), function(x) strsplit(x, '_|\\.')[[1]][1]))

filter <- which(!startsWith(rownames(exprData), 'ENSG'))
filter

if (length(filter) > 0) {
  exprData <- exprData[-filter,]
}

dim(exprData)

saveRDS(exprData, file=paste0('data/rData/', gse, '_Expression.RDS'))


### Illumina Arrays

### select most informative probe, MAX IQR

selectProbeFun <- function(expr) {
  expr$IQR <- apply(expr[,-which(colnames(expr)=='ID')], 1, IQR)
  
  expr <- expr %>% group_by(ID) %>% 
    filter(row_number() == which.max(IQR)) %>%
    column_to_rownames('ID')
  
  expr <- expr[,-which(colnames(expr)=='IQR')]
  
  return(expr)
  
}


library(dplyr)
library(tibble)

pcadb.anno <- readRDS('data/Homo_Sapiens_Gene_Annotation_ENSEMBL_HGNC_ENTRE.RDS')
table(pcadb.anno$chromosome)

# BiocManager::install('illuminaHumanv3.db')
library(illuminaHumanv4.db)
#columns(illuminaHumanv4.db)

library(illuminaHumanv3.db)
#columns(illuminaHumanv3.db)

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
  illumina.anno$PROBE_ENTREZ_ID
  
  dup.probe.entrez <- illumina.anno$PROBE_ENTREZ_ID[which(duplicated(illumina.anno$PROBE_ENTREZ_ID))]
  dup.probe.entrez
  
  idx <- which(illumina.anno$PROBE_ENTREZ_ID %in% dup.probe.entrez)
  idx
  
  illumina.anno$ENSEMBL_ENTREZ_ID <- paste0(illumina.anno$ENSEMBL, '_', illumina.anno$ENTREZID)
  illumina.anno$ENSEMBL_ENTREZ_ID
  
  i <- match(illumina.anno$ENSEMBL_ENTREZ_ID[idx], paste0(pcadb.anno$ensembl_id, '_', pcadb.anno$entrez_id))
  i <- i[which(!is.na(pcadb.anno$tax_id[i]))]
  
  filter <- idx[which(!illumina.anno$ENSEMBL_ENTREZ_ID[idx] %in% paste0(pcadb.anno$ensembl_id, '_', pcadb.anno$entrez_id)[i])]
  
  illumina.anno <- illumina.anno[-filter,]

  #### probe mapped to multiple genes
  
  probes <- illumina.anno$PROBEID[which(duplicated(illumina.anno$PROBEID))]
  filter <- which(illumina.anno$PROBEID %in% probes)
  illumina.anno <- illumina.anno[-filter,]
  illumina.anno
  
  
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
gse <- 'GSE29650' # Illumina HumanHT-12 V3.0 Expression Beadchip
gse <- 'GSE32571' # Illumina HumanHT-12 V3.0 Expression Beadchip

seriesMatrix <- getGEO(gse, AnnotGPL = FALSE, getGPL = FALSE, GSEMatrix = TRUE, destdir = 'data/fromGEO/') # AnnotGPL = TRUE
i.matrix <- 1
exprData <- exprs(seriesMatrix[[i.matrix]])
exprData

exprData <- IlluminaFun(exprData, anno = 'illuminaHumanv4.db')
exprData

exprData <- IlluminaFun(exprData, anno = 'illuminaHumanv3.db')
exprData

saveRDS(exprData, file=paste0('data/rData/', gse, '_Expression.RDS'))


### RNAseq

library(rtracklayer)
library(tibble)
library(edgeR)

setwd('~/bigdata/PCaDB/')

#gtf.file <- '~/bigdata/PCaDB/data/Reference/gencode.v38.annotation.gtf'
#gtf <- readGFF(gtf.file, version=2L)
#gtf <- gtf[which(gtf$type=='gene'),]
#dim(gtf)

datasets <- c('SRP133573','ERP023321','SRP163173','ERP006077','SRP119917',
              'SRP118614','SRP212704','SRP073789','SRP157215','SRP151104',
              'SRP002628','SRP148500','ERP017433','SRP100706','SRP026387',
              'SRP030027')

project <- datasets[3]

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

datasets <- read_excel('manuscript/Cancer Research/Resubmission/Supplementary Table 1.xlsx')

for (dt in datasets$`Dataset Name`[51:76]) {
  message (dt)
  file <- paste0('data/rData/', dt, '_Expression.RDS')
  
  if (file.exists(file)) {
    expr <- readRDS(file)
    print (ncol(expr))
  }
  
}

### Other arrays

gse <- 'GSE38241'
seriesMatrix <- getGEO(gse, AnnotGPL = TRUE, getGPL = TRUE, GSEMatrix = TRUE, destdir = 'data/fromGEO/') # AnnotGPL = TRUE
names(seriesMatrix)

i.matrix <- 1

gseData <- seriesMatrix[[i.matrix]]
gpl <- gseData@annotation
gpl

exprData <- exprs(gseData)
View(exprData)

annoData <- gseData@featureData@data
annoData

all(rownames(exprData)==annoData$ID)

annoData$`Gene ID` <- as.character(annoData$`Gene ID`)
#annoData$`Gene ID` <- as.character(annoData$GENE)

annoData$`Gene ID`[which(is.na(annoData$`Gene ID`))] <- ''
sum(annoData$`Gene ID`=='', na.rm = T)

idx <- match(annoData$`Gene ID`, pcadb.anno$entrez_id)
ensembl <- pcadb.anno$ensembl_id[idx]

filter <- which(is.na(ensembl))
exprData <- data.frame(exprData[-filter,], stringsAsFactors = F)
exprData$ID <- ensembl[-filter]

idx <- which(!is.na(rowSums(exprData[,-ncol(exprData)])))
exprData <- selectProbeFun(exprData[idx,])

saveRDS(exprData, file=paste0('data/rData/', gse, '_Expression.RDS'))


##########################################################################################
# ====================================== Metadata ====================================== #
##########################################################################################

library(stringr)

generatePhenoFun <- function(samples) {
  ### Harmonized metadata
  traits <- c('sample_id','patient_id','tissue','batch','platform','sample_type',
              'age_at_diagnosis','ethnicity','race','clinical_stage','clinical_t_stage',
              'clinical_n_stage','clinical_m_stage','pathological_stage','pathological_t_stage',
              'pathological_n_stage','pathological_m_stage','preop_psa','gleason_primary_pattern',
              'gleason_secondary_pattern','gleason_tertiary_pattern','gleason_group','gleason_score',
              'time_to_death','os_status','time_to_bcr','bcr_status','time_to_metastasis',
              'metastasis_status','risk_group','treatment','additional_treatment')
  
  phenoData <- data.frame(matrix(NA, nrow=length(samples), ncol=length(traits)), stringsAsFactors = F)
  
  colnames(phenoData) <- traits
  rownames(phenoData) <- samples
  
  return (phenoData)
}

######

# GSE111177   SRP133573
gse <- 'GSE111177'
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

keep <- c('title','geo_accession','source_name_ch1',
          colnames(pheno)[grep('\\.ch1|description', colnames(pheno))],
          'contact_institute')
keep
pheno <- pheno[,keep]

colnames(pheno) <- gsub('\\.ch1', '', colnames(pheno))
colnames(pheno) <- gsub(' |-|\\.', '_', colnames(pheno))

pheno[pheno=='-'] <- NA
pheno[pheno=='unknown'] <- NA
View(pheno)

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$patient_id <- unlist(sapply(pheno$title, function(x) strsplit(x, '-')[[1]][1]))
phenoData$tissue[pheno$sample.type.ch1=='Biopsy'] <- 'FFPE Biopsy'
phenoData$tissue[pheno$sample.type.ch1=='Radical Prostatectomy'] <- 'FFPE RP'
phenoData$tissue[pheno$sample.type.ch1=='TURP'] <- 'FFPE TURP'
phenoData$platform <- 'Illumina HiSeq 2500'
phenoData$sample_type <- 'Primary'
phenoData$treatment <- pheno$treatment.ch1
phenoData$age_at_diagnosis <- as.numeric(pheno$age_at_diagnosis__years_)
phenoData$metastasis_status <- as.numeric(gsub(':\\s+', '', str_extract(pheno$metastasis, ':\\s+\\d+')))

phenoData$gleason_primary_pattern <- as.numeric(gsub(':\\s+', '', str_extract(pheno$primary_gleason_grade, ':\\s+\\d+')))
phenoData$gleason_primary_pattern[phenoData$gleason_primary_pattern==9] <- NA
phenoData$gleason_primary_pattern[phenoData$gleason_primary_pattern==888] <- NA
phenoData$gleason_secondary_pattern <- as.numeric(gsub(':\\s+', '', str_extract(pheno$secondary_gleason_grade, ':\\s+\\d+')))
phenoData$gleason_secondary_pattern[phenoData$gleason_secondary_pattern==9] <- NA
phenoData$gleason_secondary_pattern[phenoData$gleason_secondary_pattern==888] <- NA
phenoData$gleason_score <- phenoData$gleason_primary_pattern + phenoData$gleason_secondary_pattern
phenoData$gleason_group <- ifelse(is.na(phenoData$gleason_score), NA, paste(phenoData$gleason_primary_pattern, phenoData$gleason_secondary_pattern, sep='+'))

phenoData$preop_psa <- as.numeric(gsub(':\\s+', '', str_extract(pheno$psa_at_time_of_diagnosis, ':\\s+\\d+')))
phenoData$preop_psa[phenoData$preop_psa==999] <- NA

phenoData$race <- pheno$race
phenoData$bcr_status <- as.numeric(gsub(':\\s+', '', str_extract(pheno$recurrence, ':\\s+\\d+')))
phenoData$bcr_status[phenoData$bcr_status==2] <- 'Local recurrence'
phenoData$bcr_status[phenoData$bcr_status==3] <- 'Distant recurrence'

phenoData$pcadb_group <- paste0(gsub('-', '', phenoData$treatment), gsub('FFPE ', '_', phenoData$tissue))
phenoData$pcadb_group

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))



# ERP023321 *******
# https://www.nature.com/articles/s41467-021-26840-5



# GSE120741   SRP163173
gse <- 'GSE120741'
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$patient_id <- pheno$patient.id.ch1
phenoData$tissue <- 'Fresh frozen RP'
phenoData$platform <- 'Illumina HiSeq 2500'
phenoData$sample_type <- 'Primary'

phenoData$bcr_status[pheno$case.control.ch1=='Case'] <- 1 # 5 years
phenoData$bcr_status[pheno$case.control.ch1=='Control'] <- 0 # 10 years

phenoData$pcadb_group[phenoData$bcr_status==1] <- 'Primary (BCR within 5 years)'
phenoData$pcadb_group[phenoData$bcr_status==0] <- 'Primary (No BCR within 10 years)'

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))


# ERP006077
sra <- 'ERP006077'
pheno <- read.csv(paste0('data/Metadata/', sra, '.txt'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

keep <- which(pheno$Assay.Type=='RNA-Seq' & pheno$title=='Radical Prostatectomy Specimen')
keep

pheno <- pheno[keep,]

#filter <- which(duplicated(pheno$BioSample))
#pheno <- pheno[-filter,]
dim(pheno)

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$Alias
phenoData$platform <- pheno$Instrument
phenoData$tissue <- 'RP'
phenoData$sample_type <- ifelse(pheno$Description=='Primary Prostate Tumour', 
                                'Primary','Normal')

phenoData$pcadb_group <- phenoData$sample_type

saveRDS(phenoData, file=paste0('data/rData/', sra, '_Metadata.RDS'))


# SRP119917 *****
sra <- 'SRP119917'
pheno <- read.csv(paste0('data/Metadata/', sra, '.txt'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$Sample.Name
phenoData$patient_id <- pheno$Isolate
phenoData$platform <- 'Illumina HiSeq 2500'

phenoData$sample_type[pheno$tissue=='higher grade prostate tumor'] <- 'Primary'
phenoData$sample_type[pheno$tissue=='lower grade prostate tumor'] <- 'Primary'
phenoData$sample_type[pheno$tissue=='normal prostate tissue'] <- 'Normal'

phenoData$age_at_diagnosis <- pheno$AGE

phenoData$pcadb_group[pheno$tissue=='higher grade prostate tumor'] <- 'Higher grade tumor'
phenoData$pcadb_group[pheno$tissue=='lower grade prostate tumor'] <- 'Lower grade tumor'
phenoData$pcadb_group[pheno$tissue=='normal prostate tissue'] <- 'Normal'

saveRDS(phenoData, file=paste0('data/rData/', sra, '_Metadata.RDS'))


# GSE104131   SRP118614
gse <- 'GSE104131'
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$patient_id <- str_extract(pheno$source_name_ch1, '\\d+$')
phenoData$tissue <- 'FFPE'
phenoData$platform <- 'Illumina HiSeq 2500'
phenoData$sample_type[pheno$tissue.ch1=='prostate tumor'] <- 'Primary'
phenoData$sample_type[pheno$tissue.ch1=='normal prostate'] <- 'Normal'

phenoData$race <- pheno$ethnicity.ch1
phenoData$gleason_score <- '7 or higher'

phenoData$pcadb_group <- paste0(phenoData$sample_type, ' (', phenoData$race, ')')

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))


# GSE133626   SRP212704
gse <- 'GSE133626'
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$patient_id <- str_extract(pheno$title, '^\\d+')
phenoData$tissue <- 'Fresh frozen RP'
phenoData$platform <- 'Illumina HiSeq 2000'

phenoData$sample_type[grepl('Tumor', pheno$tissue.ch1)] <- 'Primary'
phenoData$sample_type[grepl('Normal', pheno$tissue.ch1)] <- 'Normal'

phenoData$pcadb_group <- phenoData$sample_type

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))


# GSE80609    SRP073789
gse <- 'GSE80609'
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
#phenoData$tissue <- 'Fresh frozen RP' # not sure (some are TURP)
phenoData$platform <- 'Illumina HiSeq 2000'
phenoData$sample_type[pheno$progression.step.ch1=='BPH'] <- 'Normal'
phenoData$sample_type[pheno$progression.step.ch1=='CaP'] <- 'Primary'
phenoData$sample_type[pheno$progression.step.ch1=='advanced CaP'] <- 'Advanced'
phenoData$sample_type[pheno$progression.step.ch1=='CRPC'] <- 'CRPC'

phenoData$pcadb_group <- phenoData$sample_type

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))

# GSE118435   SRP157215
gse <- 'GSE118435'
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

keep <- c('title','geo_accession','source_name_ch1',
          colnames(pheno)[grep('\\.ch1|description', colnames(pheno))],
          'contact_institute')
keep
pheno <- pheno[,keep]

colnames(pheno) <- gsub('\\.ch1', '', colnames(pheno))
colnames(pheno) <- gsub(' |-|\\.', '_', colnames(pheno))

pheno[pheno=='-'] <- NA
pheno[pheno=='unknown'] <- NA


samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$tissue <- paste('Fresh frozen', pheno$tumor_site)
phenoData$platform <- 'Illumina HiSeq 2500'
phenoData$sample_type <- 'mCRCP'

phenoData$pcadb_group <- paste0('mCRCP_', 
                               unlist(sapply(pheno$title, function(x) strsplit(x, '_')[[1]][2])))

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))


# SRP151104 *******
sra <- 'SRP151104'
pheno <- read.csv(paste0('data/Metadata/', sra, '.txt'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$Sample.Name
phenoData$patient_id <- pheno$Isolate
phenoData$platform <- 'NextSeq 500'

phenoData$age_at_diagnosis <- pheno$AGE
phenoData$sample_type <- 'Locally Advanced'

phenoData$pcadb_group <- phenoData$sample_type

saveRDS(phenoData, file=paste0('data/rData/', sra, '_Metadata.RDS'))


# GSE22260    SRP002628
gse <- 'GSE22260'
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

keep <- c('title','geo_accession','source_name_ch1',
          colnames(pheno)[grep('\\.ch1|description', colnames(pheno))],
          'contact_institute')
keep
pheno <- pheno[,keep]

colnames(pheno) <- gsub('\\.ch1', '', colnames(pheno))
colnames(pheno) <- gsub(' |-|\\.', '_', colnames(pheno))

pheno[pheno=='-'] <- NA
pheno[pheno=='unknown'] <- NA

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$patient_id <- str_extract(pheno$title, '\\d+$')
phenoData$tissue <- 'Fresh frozen RP' # not 100% sure
phenoData$platform <- 'Illumina Genome Analyzer II'

phenoData$sample_type[pheno$tissue=='Normal prostate tissue'] <- 'Normal'
phenoData$sample_type[pheno$tissue=='Prostate cancer tissue'] <- 'Primary'
phenoData$gleason_score <- as.numeric(gsub('Gleason score ', '', pheno$tumor_stage))

phenoData$pcadb_group <- phenoData$sample_type

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))


# GSE114740 SRP148500

gse <- 'GSE114740'
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$patient_id <- unlist(sapply(pheno$isolate.ch1, function(x) strsplit(x, ' ')[[1]][1]))
phenoData$tissue <- 'Fresh frozen RP' # not 100% sure
phenoData$platform <- 'Illumina HiSeq 2000'

phenoData$age_at_diagnosis <- as.numeric(gsub(' years old', '', pheno$age.ch1))
phenoData$sample_type <- ifelse(grepl('tumor', pheno$isolate.ch1), 'Primary','Normal')

phenoData$pcadb_group <- phenoData$sample_type

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))


# sra <- 'SRP148500'
# pheno <- read.csv(paste0('data/Metadata/', sra, '.txt'), header = T, row.names = 1, 
#                   stringsAsFactors = F)
# View(pheno)
# 
# samples <- rownames(pheno)
# phenoData <- generatePhenoFun(samples)
# 
# phenoData$sample_id <- pheno$Sample.Name
# phenoData$patient_id <- unlist(sapply(pheno$Isolate, function(x) strsplit(x, ' ')[[1]][1]))
# phenoData$platform <- 'Illumina HiSeq 2000'
# 
# phenoData$age_at_diagnosis <- as.numeric(gsub(' years old', '', pheno$AGE))
# phenoData$sample_type <- ifelse(grepl('tumor', pheno$Isolate), 'Primary','Normal')
# 
# phenoData$pcadb_group <- phenoData$sample_type
# 
# saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))


# E-MTAB-5021 ERP017433
gse <- 'E-MTAB-5021'
url <- paste0('https://www.ebi.ac.uk/arrayexpress/files/', gse, '/', gse, '.sdrf.txt')
cmd <- paste0 ('wget ', url, ' -P data/Metadata/')
system(cmd)
pheno <- read.table(paste0('data/Metadata/', gse, '.sdrf.txt'), header = T, sep = '\t',
                  stringsAsFactors = F)
View(pheno)

filter <- which(duplicated(pheno$Comment.ENA_RUN.))
filter
pheno <- pheno[-filter,]
pheno
rownames(pheno) <- pheno$Comment.ENA_RUN.

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$Source.Name
phenoData$patient_id <- pheno$Characteristics.individual.
phenoData$tissue <- 'FFPE RP'
phenoData$platform <- 'Illumina HiSeq 2500'

phenoData$sample_type <- ifelse(pheno$Characteristics.clinical.information.=='morphologically normal tissue',
                                'Normal', 'Normal adjacent to tumor')

phenoData$pcadb_group <- paste0(phenoData$sample_type,
                                ifelse(pheno$Characteristics.sampling.site.=='peripheral zone of prostate',
                                ' (Peripheral zone)', ' (Transition zone)'))

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))


# GSE95369    SRP100706
gse <- 'GSE95369'
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

keep <- c('title','geo_accession','source_name_ch1',
          colnames(pheno)[grep('\\.ch1|description', colnames(pheno))],
          'contact_institute')
keep
pheno <- pheno[,keep]

colnames(pheno) <- gsub('\\.ch1', '', colnames(pheno))
colnames(pheno) <- gsub(' |-|\\.', '_', colnames(pheno))

pheno[pheno=='-'] <- NA
pheno[pheno=='unknown'] <- NA

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$tissue <- 'FFPE RP'
phenoData$platform <- 'Illumina HiSeq 4000'

phenoData$sample_type <- 'Primary'

phenoData$gleason_score <- as.numeric(gsub('GS', '', str_extract(pheno$description, 'GS\\d+')))

phenoData$pcadb_group <- paste0(phenoData$sample_type, '_', gsub(' \\(GS\\d+\\)', '', pheno$description))
phenoData$pcadb_group <- gsub(' ', '_', phenoData$pcadb_group)

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))

# GSE48403    SRP026387
gse <- 'GSE48403'
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$patient_id <- str_extract(pheno$description, 'patient \\d+')
phenoData$tissue <- '	TRUSS biopsy'
phenoData$platform <- 'Illumina HiSeq 2000'

phenoData$sample_type <- 'Locally-advanced or Metastatic'
phenoData$treatment <- 'ADT'

phenoData$pcadb_group <- paste0('Advanced/Metastatic_', ifelse(pheno$time.point.ch1=='Pre-treament', 'PreADT', 'PostADT'))


pheno <- read.csv('data/Metadata/GSE48403_Table1.csv', header = T, stringsAsFactors = F)
pheno

pheno$Patient <- paste('patient', pheno$Patient)

idx <- match(phenoData$patient_id, pheno$Patient)
idx

phenoData$age_at_diagnosis <- as.numeric(pheno$Age..yr[idx])
phenoData$gleason_score <- as.numeric(pheno$GSS[idx])
phenoData$preop_psa <- as.numeric(pheno$iPSA..ng.ml[idx])

phenoData$clinical_t_stage <- pheno$T[idx]
phenoData$clinical_n_stage <- pheno$N[idx]
phenoData$clinical_m_stage <- pheno$M[idx]

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))

# GSE51005    SRP030027
gse <- 'GSE51005'
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$patient_id <- str_extract(pheno$description, 'patient \\d+')
phenoData$tissue <- '	TRUSS biopsy'
phenoData$platform <- 'Illumina HiSeq 2000'

phenoData$sample_type <- 'Locally-advanced or Metastatic'
phenoData$treatment <- 'docetaxel plus ADT'

phenoData$pcadb_group <- paste0('Advanced/Metastatic_', ifelse(pheno$time.point.ch1=='Pre-treament', 'Pre_ADT_docetaxel', 'Post_ADT_docetaxel'))


pheno <- read.csv('data/Metadata/GSE51005_Table1.csv', header = T, stringsAsFactors = F)
pheno

pheno$Patient <- paste('patient', pheno$Patient)

idx <- match(phenoData$patient_id, pheno$Patient)
idx

phenoData$gleason_score <- as.numeric(pheno$GSS[idx])
phenoData$preop_psa <- as.numeric(pheno$iPSA..ng.ml.[idx])

phenoData$clinical_t_stage <- pheno$T[idx]
phenoData$clinical_n_stage <- pheno$N[idx]
phenoData$clinical_m_stage <- pheno$M[idx]

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))


# GSE126078	SRP183532
gse <- 'GSE126078'
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

keep <- grep('CRPC', pheno$source_name_ch1)
keep

pheno <- pheno[keep,]

keep <- c('title','geo_accession','source_name_ch1',
          colnames(pheno)[grep('\\.ch1|description', colnames(pheno))],
          'contact_institute')
keep
pheno <- pheno[,keep]

colnames(pheno) <- gsub('\\.ch1', '', colnames(pheno))
colnames(pheno) <- gsub(' |-|\\.', '_', colnames(pheno))

pheno[pheno=='-'] <- NA
pheno[pheno=='unknown'] <- NA


samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$tissue <- paste('Fresh frozen', pheno$tumor_site)
phenoData$platform <- 'Illumina HiSeq 2500'
phenoData$sample_type <- 'mCRCP'

phenoData$pcadb_group <- paste0('mCRCP', str_extract(pheno$title, '_\\D+\\S+'))
View(phenoData)

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))


##################
# GSE12378
gse <- 'GSE12378'
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1,
                  stringsAsFactors = F)
View(pheno)

# keep <- c('title','geo_accession','source_name_ch1',
#           colnames(pheno)[grep('\\.ch1|description', colnames(pheno))],
#           'contact_institute')
# keep
# pheno <- pheno[,keep]
# 
# colnames(pheno) <- gsub('\\.ch1', '', colnames(pheno))
# colnames(pheno) <- gsub(' |-|\\.', '_', colnames(pheno))
# 
# pheno[pheno=='-'] <- NA
# pheno[pheno=='unknown'] <- NA
# View(pheno)

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$tissue <- 'Fresh frozen RP'
phenoData$platform <- 'Affymetrix Human Exon 1.0 ST Array'
phenoData$sample_type <- ifelse(pheno$Cancer.Status.ch1=='Cancer', 'Primary', 'Normal')
phenoData$pcadb_group <- phenoData$sample_type

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))


# GSE2443
gse <- 'GSE2443'
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$tissue <- ifelse(grepl('AD', pheno$title), 
                           'Fresh frozen RP', 'Snap-frozen biopsy')
phenoData$platform <- 'Affymetrix Human Genome U133A Array'
phenoData$sample_type <- 'Primary'
phenoData$gleason_score <- as.numeric(gsub('=\\s+', '', str_extract(pheno$description, '=\\s+\\d')))

phenoData$pcadb_group <- ifelse(grepl('AD', pheno$title), 
                                'Androgen dependent primary PCa', 'Androgen independent primary PCa')

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))

# GSE26910
gse <- 'GSE26910'
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

pheno <- pheno[grep('prostate', pheno$title),]

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$tissue <- 'Fresh frozen'
phenoData$platform <- 'Affymetrix Human Genome U133 Plus 2.0 Array'
phenoData$sample_type <- ifelse(grepl('Normal', pheno$source_name_ch1), 
                                'Normal stroma', 'Tumor stroma')
phenoData$pcadb_group <- phenoData$sample_type

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))

# GSE28680
gse <- 'GSE28680'
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$tissue <- 'Fresh frozen TURP'
phenoData$platform <- 'Illumina HumanHT-12 V4.0 Expression Beadchip'
phenoData$sample_type <- ifelse(grepl('Benign', pheno$group.ch1), 
                                'Normal', 'Primary')
phenoData$treatment <- pheno$type.ch1

phenoData$pcadb_group <- NA
phenoData$pcadb_group[pheno$type.ch1=='benign'] <- 'Normal'
phenoData$pcadb_group[pheno$type.ch1=='untreated'] <- 'Untreated'
phenoData$pcadb_group[pheno$type.ch1=='untreated'] <- 'Untreated'
phenoData$pcadb_group[pheno$type.ch1!='untreated' & pheno$group.ch1=='Responding'] <- 'ADT responsive'
phenoData$pcadb_group[pheno$type.ch1!='untreated' & pheno$group.ch1=='Not_Responding'] <- 'Castrate resistant'

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))


# GSE29650
gse <- 'GSE29650'
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1, 
                  stringsAsFactors = F)
View(pheno)

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$patient_id <- pheno$source_name_ch1
#phenoData$tissue <- 'Fresh frozen'
phenoData$platform <- 'Illumina HumanHT-12 V3.0 Expression Beadchip'
phenoData$sample_type <- 'Metastatic CRCP (Bone)'

phenoData$pcadb_group <- ifelse(grepl('low', pheno$ar.v.expression.level.ch1), 
                                'Low AR-V', 'High AR-V')
  
saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))


# GSE30521
gse <- 'GSE30521'		#Affymetrix Human Exon 1.0 ST Array
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1,
                  stringsAsFactors = F)
View(pheno)

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$tissue <- 'Fresh frozen RP'
phenoData$platform <- 'Affymetrix Human Exon 1.0 ST Array'
phenoData$sample_type <- ifelse(grepl('Normal', pheno$gleason.score.ch1), 'Normal','Primary')

phenoData$gleason_primary_pattern <- as.numeric(lapply(pheno$gleason.score.ch1, function(x) strsplit(x, '\\+|\\=')[[1]][1]))
phenoData$gleason_secondary_pattern <- as.numeric(lapply(pheno$gleason.score.ch1, function(x) strsplit(x, '\\+|\\=')[[1]][2]))
phenoData$gleason_score <- phenoData$gleason_primary_pattern + phenoData$gleason_secondary_pattern
phenoData$gleason_group <- ifelse(is.na(phenoData$gleason_score), NA, paste(phenoData$gleason_primary_pattern, phenoData$gleason_secondary_pattern, sep='+'))

phenoData$pcadb_group <- phenoData$sample_type

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))


# GSE32448
gse <- 'GSE32448' #Affymetrix Human Genome U133 Plus 2.0 Array
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1,
                  stringsAsFactors = F)
View(pheno)

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$patient_id <- gsub('HomoProstate_T_|HomoProstate_N_', '', pheno$title)
phenoData$tissue <- 'Fresh frozen RP'
phenoData$platform <- 'Affymetrix Human Genome U133 Plus 2.0 Array'
phenoData$sample_type <- ifelse(pheno$tissue.ch1=='prostate tumor', 'Primary','Normal')

phenoData$pcadb_group <- NA
phenoData$pcadb_group[pheno$differentiation.ch1=='Well Differentiated'] <- 'Well_Differentiated_Primary'
phenoData$pcadb_group[pheno$differentiation.ch1=='Well Differentiated Matched'] <- 'Well_Differentiated_Normal'
phenoData$pcadb_group[pheno$differentiation.ch1=='Poorly Differentiated'] <- 'Poorly_Differentiated_Primary'
phenoData$pcadb_group[pheno$differentiation.ch1=='Poorly Differentiated Matched'] <- 'Poorly_Differentiated_Normal'

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))

# GSE32571
gse <- 'GSE32571' #Illumina HumanHT-12 V3.0 Expression Beadchip
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1,
                  stringsAsFactors = F)
View(pheno)

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$tissue <- 'Fresh frozen RP'
phenoData$platform <- 'Illumina HumanHT-12 V3.0 Expression Beadchip'
phenoData$sample_type <- ifelse(pheno$disease.stage.ch1=='Cancer', 'Primary','Normal')
phenoData$gleason_group[pheno$gleason.pattern.group.ch1=='high'] <- '4+3 and higher'
phenoData$gleason_group[pheno$gleason.pattern.group.ch1=='low'] <- '3+4 and lower'
phenoData$gleason_group[pheno$gleason.pattern.group.ch1=='N/A'] <- NA

phenoData$pcadb_group <- phenoData$sample_type

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))

# GSE6956
gse <- 'GSE6956' #Affymetrix Human Genome U133A 2.0 Array
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1,
                  stringsAsFactors = F)
View(pheno)

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$patient_id <- str_extract(pheno$title, 'patient\\s+\\S+')

phenoData$tissue <- ifelse(grepl('pool', pheno$title, ignore.case = T), 'Fresh frozen needle biopsy', 'Fresh frozen RP')
phenoData$platform <- 'Affymetrix Human Genome U133A 2.0 Array'
phenoData$sample_type <- ifelse(grepl('Prostate tumor', pheno$title, ignore.case = T), 'Primary','Normal')

phenoData$gleason_score <- as.numeric(pheno$gleason.sum.ch1)
phenoData$race <- pheno$race.ch1
phenoData$pathological_t_stage <- ifelse(is.na(pheno$pt.stage.ch1), 'NA', paste0('pT', pheno$pt.stage.ch1))

phenoData$pcadb_group <- ifelse(grepl('pool', pheno$title, ignore.case = T), 'Pooled normal (Needle biopsy)', paste(phenoData$sample_type, ' (RP)'))

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))

# GSE7055
gse <- 'GSE7055' #Affymetrix Human Genome U133A 2.0 Array
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1,
                  stringsAsFactors = F)
View(pheno)

keep <- grep('HG-U133A 2.0 Array', pheno$title)
pheno <- pheno[keep,]

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$title
phenoData$patient_id <- str_extract(pheno$title, 'patient\\s+\\S+')

phenoData$tissue <- 'Fresh frozen RP'
phenoData$platform <- 'Affymetrix Human Genome U133A 2.0 Array'
phenoData$sample_type <- 'Primary'

phenoData$gleason_score <- as.numeric(pheno$gleason.sum.ch1)
phenoData$race <- pheno$race.ch1
phenoData$pathological_t_stage <- ifelse(is.na(pheno$pt.stage.ch1), 'NA', paste0('pT', pheno$pt.stage.ch1))

phenoData$pcadb_group <- ifelse(pheno$perineural.invasion.ch1=='Yes', 'PNI Tumor', 'NonPNI Tumor')

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))

# GSE38241
gse <- 'GSE38241' #Agilent-014850 Whole Human Genome Microarray 4x44K G4112F
pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1,
                  stringsAsFactors = F)
View(pheno)

samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$sample.id.ch1
phenoData$patient_id <- pheno$subject.id.ch1

phenoData$tissue <- 'Fresh frozen tissue'
phenoData$platform <- 'Agilent-014850 Whole Human Genome Microarray 4x44K G4112F'
phenoData$sample_type <- ifelse(pheno$tissue.type.ch1=='Normal Prostate', 'Healthy', 'Metastatic')

phenoData$pcadb_group <- phenoData$sample_type

saveRDS(phenoData, file=paste0('data/rData/', gse, '_Metadata.RDS'))


### SRP133573
# metadata <- readxl::read_excel('SRP133573/Metadata/GSE111177_Sample-key.xlsx')
# View(metadata)
# 
# sra <- read.csv('SRP133573/Metadata/SRP133573.txt', header = T, stringsAsFactors = F)
# 
# idx <- which(!duplicated(sra$BioSample))
# idx
# 
# sra <- sra[idx,]
# colnames(sra)
# 
# gse <- read.csv('SRP133573/Metadata/GSE111177.csv', header = T, stringsAsFactors = F)


##########################################################################################
# ==================================== ExpressionSet =================================== #
##########################################################################################

setwd('~/Documents/Publications/PCaDB/')

datasets <- unique(gsub('_\\w+.RDS', '', list.files('data/rData/')))
datasets

gse <- datasets[27]
exprData <- readRDS(file=paste0('data/rData/', gse, '_Expression.RDS'))
colnames(exprData)

phenoData <- readRDS(file=paste0('data/rData/', gse, '_Metadata.RDS'))
rownames(phenoData)

colnames(exprData) == rownames(phenoData)

ovlp <- intersect(rownames(phenoData), colnames(exprData))
ovlp

exprData <- exprData[,ovlp]
phenoData <- phenoData[ovlp,]

all(rownames(phenoData) == colnames(exprData))

eSet <- ExpressionSet(assayData = as.matrix(exprData),
                      phenoData = AnnotatedDataFrame(phenoData))

saveRDS(eSet, file=paste0('data/rData/', gse, '_eSet.RDS'))


###### 3, 4, 5, 6, 7, 9, 10, 11, 20, 21, 24, 25
gse <- datasets[5]
gse

exprData <- readRDS(file=paste0('data/rData/', gse, '_Expression.RDS'))
colnames(exprData)

phenoData <- readRDS(file=paste0('data/rData/', gse, '_Metadata.RDS'))
rownames(phenoData)
View(phenoData)

## 3,4 -----------------------------------------------
sra <- read.csv('data/Metadata/SRP118614.txt', header = T, stringsAsFactors = F)
sra <- read.csv('data/Metadata/SRP133573.txt', header = T, stringsAsFactors = F)
filter <- which(duplicated(sra$BioSample))
filter
sra <- sra[-filter,]

samples <- sra$GEO_Accession..exp.[match(colnames(exprData), sra$BioSample)]
samples
## -----------------------------------------------------

## 5
sra <- read.csv('data/Metadata/SRP148500.txt', header = T, stringsAsFactors = F)
sample_id <- gsub('prostate ', '', phenoData$sample_id)
sample_id

gse_sra <- data.frame(gse=rownames(phenoData),
                      sra=sra$Run[match(sample_id, sra$Isolate)],
                      stringsAsFactors = F)
gse_sra

samples <- gse_sra$gse[match(colnames(exprData), gse_sra$sra)]
samples

## -----------------------------------------------------

# 6, 7, 9, 10, 11, 20, 21, 24, 25
sra <- read.csv('data/Metadata/SRP157215.txt', header = T, stringsAsFactors = F)
sra <- read.csv('data/Metadata/SRP163173.txt', header = T, stringsAsFactors = F)
sra <- read.csv('data/Metadata/SRP183532.txt', header = T, stringsAsFactors = F)
sra <- read.csv('data/Metadata/SRP212704.txt', header = T, stringsAsFactors = F)
sra <- read.csv('data/Metadata/SRP002628.txt', header = T, stringsAsFactors = F)
sra <- read.csv('data/Metadata/SRP026387.txt', header = T, stringsAsFactors = F)
sra <- read.csv('data/Metadata/SRP030027.txt', header = T, stringsAsFactors = F)
sra <- read.csv('data/Metadata/SRP073789.txt', header = T, stringsAsFactors = F)
sra <- read.csv('data/Metadata/SRP100706.txt', header = T, stringsAsFactors = F)

samples <- sra$GEO_Accession..exp.[match(colnames(exprData), sra$Run)]
samples

##
colnames(exprData) <- samples

colnames(exprData) == rownames(phenoData)
samples %in% rownames(phenoData)

ovlp <- intersect(rownames(phenoData), colnames(exprData))
ovlp

exprData <- exprData[,ovlp]
phenoData <- phenoData[ovlp,]

all(rownames(phenoData) == colnames(exprData))

eSet <- ExpressionSet(assayData = as.matrix(exprData),
                      phenoData = AnnotatedDataFrame(phenoData))

saveRDS(eSet, file=paste0('data/rData/', gse, '_eSet.RDS'))




####

### RNAseq
# datasets <- c('GSE111177','GSE120741','ERP006077','SRP119917','GSE104131',
#               'GSE133626','GSE80609','GSE118435','SRP151104','GSE22260',
#               'GSE114740','E-MTAB-5021','GSE95369','GSE48403','GSE51005',
#               'GSE126078')
