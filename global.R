
library(RColorBrewer)
cols = brewer.pal(4, "Reds")
cols = colorRampPalette(cols)(10)
col_fun = colorRampPalette(rev(c(cols[10],cols[1])), space = "Lab")(2)


# gene.annotation <- readRDS('data/Homo_Sapiens_Gene_Annotation_ENSEMBL_HGNC_ENTREZ.RDS')
# gene.annotation$alias_symbol <- gsub('"', '', gene.annotation$alias_symbol, fixed=T)
# 
# filter <- which(duplicated(gene.annotation$ensembl_id) | is.na(gene.annotation$ensembl_id))
# gene.annotation <- gene.annotation[-filter,]
# 
# gene.annotation$ensembl_id <- as.character(gene.annotation$ensembl_id)
# gene.annotation$entrez_id <- as.character(gene.annotation$entrez_id)
# gene.annotation$hgnc_id <- as.character(gene.annotation$hgnc_id)
# gene.annotation$gene_name <- as.character(gene.annotation$gene_name)
# gene.annotation$description <- as.character(gene.annotation$description)
# gene.annotation$alias_symbol <- gsub('|', ', ', as.character(gene.annotation$alias_symbol), fixed = T)
# 
# rownames(gene.annotation) <- gene.annotation$ensembl_id
# 
# gene.annotation <- gene.annotation[order(gene.annotation$gene_name, decreasing = F),]
# 
# saveRDS(gene.annotation, file='data/PCaDB_Gene_Annotation.RDS')

gene.annotation <- readRDS('data/PCaDB_Gene_Annotation.RDS')
gene.annotation <- gene.annotation[-c(1:16),]

idx1 <- which(gene.annotation$gene_biotype=='protein_coding')
idx2 <- which(!1:nrow(gene.annotation) %in% idx1)
gene.annotation <- gene.annotation[c(idx1,idx2),]


gene.default <- 'ENSG00000142515' # KLK3

datasets <- readRDS('data/PCaDB_Datasets.RDS')
#pcadb.dataset <- datasets[,c(2,1,4,5,7,8,11,12,13)]
pcadb.dataset <- datasets[,c(2,1,8,12,13)]

# meta.data <- readRDS('data/PCaDB_Metadata.RDS')
# expr.data <- readRDS('data/PCaDB_Expression.RDS')

# scDataHenry <- readRDS('data/PCaDB_scDataHenry.RDS')

bcr.dataset <- pcadb.dataset$Dataset[1:10]
# 
# for (i in 1:nrow(pcadb.dataset)) {
# 
#   dt <- pcadb.dataset$Dataset[i]
#   message (dt)
#   print (unique(meta.data[[dt]]$sample_type))
#   #print (unique(meta.data[[dt]]$tissue))
# 
# }


# for (i in 1:nrow(pcadb.dataset)) {
# 
#   dt <- pcadb.dataset$Dataset[i]
#   message (dt)
#   print (i)
#   print (unique(meta.data[[dt]]$gleason_group))
#   #print (unique(meta.data[[dt]]$tissue))
# 
# }

# 1, 3, 4, 5, 6, 7, 8, 9, 12, 17, 43, 47, 50

# for (i in 1:nrow(pcadb.dataset)) {
# 
#   dt <- pcadb.dataset$Dataset[i]
#   message (dt)
#   print (i)
#   print (unique(meta.data[[dt]]$preop_psa))
#   #print (unique(meta.data[[dt]]$tissue))
# 
# }

# 1, 2, 4, 5, 6, 7, 8, 9, 10, 12, 23, 25, 26, 27, 28, 29, 41, 47, 50


expr.dataset <- c('TCGA-PRAD','Taylor','Cambridge','CancerMap','CIT','GSE59745','GSE79021',
  'GSE35988-GPL6480','GSE35988-GPL6848','GSE59745','SU2C-PCF-2019-Capture','SU2C-PCF-2019-PolyA',
  'GSE3325','GSE32269','GSE6919-GPL8300','GSE6919-GPL92','GSE6919-GPL93','GSE6752','GSE8218',
  'E-TABM-26-U133A','E-TABM-26-U133B','GSE29079','GSE97284','GSE62872','GSE77930','Neuroendocrine')


## Taylor: some cell lines

expr.dataset.default <- expr.dataset[1]
expr.dataset.id <- selectizeInput(inputId = "expr.dataset.id", label=h5(strong('Select a Dataset:')),# h4(strong('miRNA'))
                             choices = NULL, selected = expr.dataset.default, 
                             multiple = FALSE, width = 300,
                             options = list(placeholder = 'Select the dataset(s)',
                                            server = TRUE, selectOnTab=TRUE
                             ))

pca.signatures <- readRDS('data/PCaDB_Signatures.RDS')
#write_xlsx(pca.signatures, "www/data/PCaDB_Prognostic_Signatures_Gene_List.xlsx")

signature.name <- unique(pca.signatures$Signature)
signature.default <- signature.name[1]
overview.signature <- selectizeInput(inputId = "overview.signature", label=h5(strong('Select a Signature:')),# h4(strong('miRNA'))
                                     choices = NULL, selected = signature.default, 
                                     multiple = FALSE, width = 300,
                                     options = list(placeholder = 'Select a signature',
                                                    server = TRUE, selectOnTab=TRUE
                                     ))

signature.de.dataset <- c('TCGA-PRAD','Taylor','Cambridge','CancerMap','CIT',
                          'GSE79021','E-TABM-26-U133A','E-TABM-26-U133B','GSE29079')

de.dataset.default <- 'TCGA-PRAD'

de.dataset <- selectizeInput(inputId = "de.dataset", label=h5(strong('Select a Dataset:')),# h4(strong('miRNA'))
                                     choices = NULL, selected = de.dataset.default, 
                                     multiple = FALSE, width = 300,
                                     options = list(placeholder = 'Select a dataset',
                                                    server = TRUE, selectOnTab=TRUE
                                     ))


bcr.dataset.default <- bcr.dataset[1]
bcr.dataset.input <- selectizeInput(inputId = "bcr.dataset.input", label=h5(strong('Select a Dataset:')),# h4(strong('miRNA'))
                                       choices = NULL, selected = bcr.dataset.default, 
                                       multiple = FALSE, width = 300,
                                       options = list(placeholder = 'Select a dataset',
                                                      server = TRUE, selectOnTab=TRUE
                                       ))


Signature.Gene.Quant <- data.frame(sort(table(pca.signatures$Ensembl.ID)[-which(names(table(pca.signatures$Ensembl.ID))=='NA')], decreasing = T))
sum(Signature.Gene.Quant$Freq>=1) # 1032
sum(Signature.Gene.Quant$Freq>=2) # 142
sum(Signature.Gene.Quant$Freq>=3) # 40
sum(Signature.Gene.Quant$Freq>=4) # 13

signature.geneset <- list('Prognostic Signature'=signature.name,
                          'Common Genes'=c('All (1032 Genes)' = 'All_Signature_Genes',
                                           '>= 2 Signatures (142 Genes)' = 'Common_2orMore',
                                           '>= 3 Signatures (40 Genes)' = 'Common_3orMore',
                                           '>= 4 Signatures (13 Genes)' = 'Common_4orMore')
)

signature.geneset.default <- 'Common_3orMore'

signature.pathway.input <- selectizeInput(inputId = "signature.pathway.input", label=h5(strong('Select the Gene List:')),# h4(strong('miRNA'))
                                          choices = NULL, selected = signature.geneset.default, 
                                          multiple = FALSE, width = 300,
                                          options = list(placeholder = 'Select the gene list',
                                                         server = TRUE, selectOnTab=TRUE
                                          ))


signature.ontology <- c('Kyoto Encyclopedia of Genes and Genomes (KEGG)' = 'KEGG',
               'REACTOME' = 'REACTOME',
               'Disease Ontology (DO)' = 'DO',
               'Network of Cancer Gene (NCG)' = 'NCG',
               'DisGeNET' = 'DGN', 
               'Gene Ontology - Biological Process (GO-BP)' = 'GOBP',
               'Gene Ontology - Cellular Component (GO-CC)' = 'GOCC',
               'Gene Ontology - Molecular Function (GO-MF)' = 'GOMF',
               'MSigDB - H:HALLMARK' = 'MSigDBHALLMARK',
               'MSigDB - C4:Cancer Gene Neighborhoods' = 'MSigDBC4CGN',
               'MSigDB - C4:Cancer Modules' = 'MSigDBC4CM',
               'MSigDB - C6:Oncogenic Signatures' = 'MSigDBC6'#,
               #'MSigDB - C7:Immunologic Signatures' = 'MSigDBC7'
               )

signature.ontology.default <- signature.ontology[1]
signature.ontology.input <- selectizeInput(inputId = "signature.ontology.input", label=h5(strong('Gene Sets:')),# h4(strong('miRNA'))
                             choices = NULL, selected = signature.ontology.default, 
                             multiple = FALSE, width = 410,
                             options = list(placeholder = 'Select a gene set',
                                            server = TRUE, selectOnTab=TRUE
                             ))


#####

signature.comp.signature.default <- 'Klein'
#signature.comp.signature.default <- 'All Signatures'
signature_comp_signature_input <- selectizeInput(inputId = "signature_comp_signature_input", label=h5(strong('Select a Signature:')),# h4(strong('miRNA'))
                                           choices = NULL, selected = signature.comp.signature.default, 
                                           multiple = FALSE, width = 300,
                                           options = list(placeholder = 'Select a Signature',
                                                          server = TRUE, selectOnTab=TRUE
                                           ))


signature.comp.training.default <- bcr.dataset[1]
signature.comp.training.input <- selectizeInput(inputId = "signature.comp.training.input", label=h5(strong('Select a Training Dataset:')),# h4(strong('miRNA'))
                                                 choices = NULL, selected = signature.comp.training.default, 
                                                 multiple = FALSE, width = 300,
                                                 options = list(placeholder = 'Select a Training Dataset',
                                                                server = TRUE, selectOnTab=TRUE
                                                 ))


signature.models <- c('CoxPH',
                      'CoxLasso',
                      'CoxRidge',
                      'SuperPC',
                      'plsRcox',
                      'RandomForest')

signature.comp.model.default <- 'CoxRidge'
signature.comp.model.input <- selectizeInput(inputId = "signature.comp.model.input", label=h5(strong('Select a Model:')),# h4(strong('miRNA'))
                                                 choices = NULL, selected = signature.comp.model.default, 
                                                 multiple = FALSE, width = 300,
                                                 options = list(placeholder = 'Select a Model',
                                                                server = TRUE, selectOnTab=TRUE
                                                 ))

###
signature.enrichment <- readRDS('data/PCaDB_Signature_Enrichment.RDS')

###
model.comparison <- readRDS('data/PCaDB_Signature_Model_Comparison.RDS')

###
pcadb.pca <- readRDS(file='data/PCaDB_PCA_Analysis.RDS')

###
pcadb.coxph <- readRDS(file='data/PCaDB_Survival_CoxPH_P0.05.RDS')
pcadb.km <- readRDS(file='data/PCaDB_Survival_KM_P0.05.RDS')


###
meta.data <- readRDS('data/PCaDB_Metadata.RDS')
expr.data <- readRDS('data/PCaDB_Expression.UnCompressed.RDS')

##
scDataHenry <<- readRDS('data/PCaDB_scDataHenry.UnCompressed.RDS')

signature.deg <- readRDS('data/PCaDB_Signature_DEGs.RDS')



