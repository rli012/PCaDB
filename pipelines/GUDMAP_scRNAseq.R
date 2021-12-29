
####################################################################
#######           Single-cell RNAseq Data Collection         #######
####################################################################

# https://www.gudmap.org/chaise/record/#2/RNASeq:Study/RID=W-RAHW

# mkdir -p data/SingleCell/GSE117403/
# wget -O data/SingleCell/GSE117403/DPrF_sc_huPr_2018CellReports.RDS https://www.gudmap.org/hatrac/resources/rnaseq/study/W-RAHW/study_files/134d4d3f97c9d446b03019ec84dcf020.rds:XCZTF5ZAKOY5S6JQCOJ4X6O4RY?uinit=1&cid=record
  
library(Seurat)
  
DPrF_sc_huPr_2018CellReports <- readRDS('data/SingleCell/GSE117403/DPrF_sc_huPr_2018CellReports.RDS')
expr <- DPrF_sc_huPr_2018CellReports@assays[['RNA']]@data
annotation <- DPrF_sc_huPr_2018CellReports@meta.data
tsne <- DPrF_sc_huPr_2018CellReports@reductions$tsne@cell.embeddings
umap <- DPrF_sc_huPr_2018CellReports@reductions$umap@cell.embeddings

scDataHenry <- list(expr=expr,annotation=annotation,tsne=tsne,umap=umap)

saveRDS(scDataHenry, file='data/Database/PCaDB_scDataHenry.RDS')
  