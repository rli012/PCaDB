
###########################################################################
#######                Harmonization of Sample Metadata             #######
###########################################################################

#**********************************#
#         Helper Functions         #
#**********************************#

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


#*******************************************#
#         TCGA-PRAD Sample Metadata         #
#*******************************************#

library(GDCRNATools)

project <- 'TCGA-PRAD'

### Parse RNAseq metadata
meta.rna <- gdcParseMetadata(project.id = project, 
                             data.type  = 'RNAseq', 
                             write.meta = FALSE)

### Filter duplicated samples
meta.rna <- gdcFilterDuplicate(meta.rna)

### Filter non-Primary Tumor and non-Solid Tissue Normal samples
meta.rna <- gdcFilterSampleType(meta.rna)

### Download clinical data
clinical.dir <- paste('data/Clinical', sep='/')
gdcClinicalDownload(project.id     = project, 
                    write.manifest = FALSE,
                    method         = 'gdc-client',
                    directory      = clinical.dir)

### Merge clinical data
clinical.data <- gdcClinicalMerge(path = clinicaldir, key.info = TRUE)
clinical.data[1:6,5:10]

### Meatadata
rownames(meta.rna) <- substr(rownames(meta.rna),start = 1, stop = 15)

pheno1 <- clinical.data
rownames(pheno1) <- paste0(rownames(pheno1), '-01')

pheno1 <- pheno1[match(rownames(meta.rna), rownames(pheno1)),]
rownames(pheno1) <- rownames(meta.rna)

pheno2 <- readRDS('data/rData/Clinical_TCGA_PRAD_With_PreopPSA_and_BCR.RDS')
pheno2 <- pheno2[match(rownames(meta.rna), rownames(pheno2)),]

rownames(pheno2) <- rownames(meta.rna)

#######
samples <- rownames(pheno1)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- rownames(pheno1)
phenoData$patient_id <- substr(phenoData$sample_id, start = 1, stop = 12)
phenoData$platform <- 'Illumina HiSeq 2000'
phenoData$sample_type <- ifelse(grepl('-11', phenoData$sample_id), 'Normal', 'Primary')
phenoData$age_at_diagnosis <- pheno1$age_at_initial_pathologic_diagnosis
phenoData$ethnicity <- pheno1$ethnicity
phenoData$race <- pheno1$race
phenoData$clinical_t_stage <- pheno1$clinical_T
phenoData$clinical_n_stage <- pheno1$clinical_N
phenoData$clinical_m_stage <- pheno1$clinical_M
phenoData$pathological_t_stage <- pheno1$pathologic_T
phenoData$pathological_n_stage <- pheno1$pathologic_N
phenoData$pathological_m_stage <- pheno1$pathologic_M
phenoData$preop_psa <- pheno2$preop_psa
phenoData$gleason_primary_pattern <- pheno2$primary_pattern
phenoData$gleason_secondary_pattern <- pheno2$secondary_pattern
phenoData$gleason_tertiary_pattern <- pheno2$tertiary_pattern
phenoData$gleason_score <- phenoData$gleason_primary_pattern + phenoData$gleason_secondary_pattern
phenoData$gleason_group <- ifelse(is.na(phenoData$gleason_score), NA, paste(phenoData$gleason_primary_pattern, phenoData$gleason_secondary_pattern, sep='+'))

phenoData$time_to_death <- ifelse(!is.na(pheno2$days_to_death), 
                                  round(as.numeric(pheno2$days_to_death/365*12),2), 
                                  round(as.numeric(pheno2$days_to_last_followup/365*12),2))
phenoData$os_status <- ifelse(!is.na(pheno2$days_to_death), 1, 0)

phenoData$time_to_bcr <- ifelse(!is.na(pheno2$days_to_first_biochemical_recurrence),
                                round(as.numeric(pheno2$days_to_first_biochemical_recurrence/365*12),2), 
                                round(as.numeric(pheno2$days_to_last_followup/365*12),2))
                                
phenoData$bcr_status <- ifelse(!is.na(pheno2$days_to_first_biochemical_recurrence), 1, 0)

phenoData$pcadb_group <- phenoData$sample_type

saveRDS(phenoData, file=paste0('data/rData/', project, '_Metadata.RDS'))


#*************************************#
#         GEO Sample Metadata         #
#*************************************#

library(GEOquery)

# GSE2443
gse <- 'GSE2443'
seriesMatrix <- getGEO(gse, AnnotGPL = FALSE, getGPL = FALSE, 
                       GSEMatrix = TRUE, destdir = 'data/fromGEO/') # AnnotGPL = TRUE

pheno <- pData(seriesMatrix[[1]])

# if downloaded manually from WebGEO (http://bioinfo.jialab-ucr.org/WebGEO/)
#pheno <- read.csv(paste0('data/Metadata/', gse, '.csv'), header = T, row.names = 1, 
#                  stringsAsFactors = F)
#View(pheno)

#######
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


#*************************************#
#         SRA Sample Metadata         #
#*************************************#

# SRP119917 (https://www.ncbi.nlm.nih.gov/Traces/study/?acc=SRP119917&o=acc_s%3Aa)
sra <- 'SRP119917'
pheno <- read.csv(paste0('data/Metadata/', sra, '.txt'), header = T, row.names = 1, 
                  stringsAsFactors = F)

https://www.ncbi.nlm.nih.gov/Traces/solr-proxy-be/solr-proxy-be.cgi?core=run_sel_index
#######
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

#********************************************#
#         cBioPortal Sample Metadata         #
#********************************************#

# DFKZ (https://www.cbioportal.org/study/clinicalData?id=prostate_dkfz_2018)
# wget https://cbioportal-datahub.s3.amazonaws.com/prostate_dkfz_2018.tar.gz -P data/cBioPortal/
# tar -xvzf prostate_dkfz_2018.tar.gz

dt <- 'DFKZ'
pheno <- read.table('data/cBioPortal/prostate_dkfz_2018/prostate_dkfz_2018_clinical_data.tsv', 
                    sep='\t', header=T, stringsAsFactors = F)

rownames(pheno) <- pheno$Sample.ID

#######
samples <- rownames(pheno)
phenoData <- generatePhenoFun(samples)

phenoData$sample_id <- pheno$Sample.ID
phenoData$patient_id <- pheno$Patient.ID
phenoData$platform <- 'Illumina HiSeq 2000'
phenoData$sample_type <- 'Primary'
phenoData$age_at_diagnosis <- pheno$Diagnosis.Age
phenoData$pathological_stage <- pheno$Stage
phenoData$preop_psa <- pheno$Preop.PSA
phenoData$gleason_primary_pattern <- as.numeric(lapply(pheno$Radical.Prostatectomy.Gleason.Score.for.Prostate.Cancer, function(x) strsplit(x, '+', fixed=T)[[1]][1]))
phenoData$gleason_secondary_pattern <- as.numeric(lapply(pheno$Radical.Prostatectomy.Gleason.Score.for.Prostate.Cancer, function(x) strsplit(x, '+', fixed=T)[[1]][2]))
phenoData$gleason_score <- phenoData$gleason_primary_pattern + phenoData$gleason_secondary_pattern
phenoData$gleason_group <- ifelse(is.na(phenoData$gleason_score), NA, paste(phenoData$gleason_primary_pattern, phenoData$gleason_secondary_pattern, sep='+'))
phenoData$time_to_bcr <- as.numeric(pheno$Time.from.Surgery.to.BCR.Last.Follow.Up)
phenoData$bcr_status <- as.numeric(pheno$BCR.Status)

phenoData$pcadb_group <- phenoData$sample_type

saveRDS(phenoData, file=paste0('data/rData/', dt, '_Metadata.RDS'))


#**********************************************#
#         ArrayExpress Sample Metadata         #
#**********************************************#

# E-MTAB-5021 ERP017433 (https://www.ebi.ac.uk/arrayexpress/files/E-MTAB-5021/E-MTAB-5021.sdrf.txt)
dt <- 'E-MTAB-5021'

url <- paste0('https://www.ebi.ac.uk/arrayexpress/files/', dt, '/', dt, '.sdrf.txt')
cmd <- paste0 ('wget ', url, ' -P data/Metadata/')
system(cmd)

pheno <- read.table(paste0('data/Metadata/', dt, '.sdrf.txt'), header = T, sep = '\t',
                    stringsAsFactors = F)

filter <- which(duplicated(pheno$Comment.ENA_RUN.))
pheno <- pheno[-filter,]
rownames(pheno) <- pheno$Comment.ENA_RUN.

#######
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

saveRDS(phenoData, file=paste0('data/rData/', dt, '_Metadata.RDS'))

