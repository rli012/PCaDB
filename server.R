
.libPaths(c(.libPaths(), '/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.6/'))

library(shiny)
#library(shinydashboard)
library(shinyjs)
library(shinyalert)
library(readxl)
library(ggplot2)
library(gridExtra)
library(Matrix)
library(stringr)
library(Biobase)
library(survival)
library(survminer)
library(limma)
library(edgeR)
library(reshape2)
library(htmlwidgets) # JS
library(DT) # formatStyle
library(plotly)
library(ggrepel)
library(dplyr)
library(digest)
library(glmnet)

#library(shinydashboardPlus)
#library(shinythemes)
#library(dashboardthemes)
#library(shinycssloaders)
library(shinybusy)
#library(waiter)

table.download.button <- JS('$("button.buttons-copy").css("font-size",11);
                            $("button.buttons-csv").css("font-size",11);
                            $("button.buttons-excel").css("font-size",11);
                            return table;')

signature.de.dataset <<- c('TCGA-PRAD','Taylor','Cambridge','CancerMap','CIT',
                          'GSE79021','E-TABM-26-U133A','E-TABM-26-U133B') # ,'GSE29079'
 

######## Server

server <- function(input, output, session) { 
  
  shinyjs::hide(selector = ".navbar > .sidebar-toggle")

  #show_modal_spinner(spin = 'semipolar', color = google.red, text = h4(strong('Loading data, please wait ...'))) # show the modal window
  
  updateSelectizeInput(session, 'gene.id', choices = gene.annotation, selected = gene.default, server = TRUE)
  updateSelectizeInput(session, 'expr.dataset.id', choices = expr.dataset, selected = expr.dataset.default, server = TRUE)
  updateSelectizeInput(session, 'overview.signature', choices = signature.name, selected = signature.default, server = TRUE)
  
  updateSelectizeInput(session, 'de.dataset', choices = signature.de.dataset, selected = de.dataset.default, server = TRUE)
  updateSelectizeInput(session, 'bcr.dataset.input', choices = bcr.dataset, selected = bcr.dataset.default, server = TRUE)
  updateSelectizeInput(session, 'signature.pathway.input', choices = signature.geneset, selected = signature.geneset.default, server = TRUE)
  updateSelectizeInput(session, 'signature.ontology.input', choices = signature.ontology, selected = signature.ontology.default, server = TRUE)
  
  updateSelectizeInput(session, 'signature_comp_signature_input', choices = c('All Signatures', signature.name), selected = 'Klein', server = TRUE)
  updateSelectizeInput(session, 'signature.comp.training.input', choices = bcr.dataset, selected = signature.comp.training.default, server = TRUE)
  updateSelectizeInput(session, 'signature.comp.model.input', choices = signature.models, selected = signature.comp.model.default, server = TRUE)
  
  if (! exists('expr.data')) {
    expr.data <<- readRDS('data/PCaDB_Expression.UnCompressed.RDS')
  }

  if (exists('expr.data')) {
    remove_modal_spinner()
  }
  
  if (! exists('scDataHenry')) {
    scDataHenry <<- readRDS('data/PCaDB_scDataHenry.UnCompressed.RDS')
  }
  
  ####
  # if (! exists('expr.data')) {
  #   readRDSAs('data/PCaDB_Expression.RDS', 'expr.data')
  # }
  # 
  # if (! exists('meta.data')) {
  #   readRDSAs('data/PCaDB_Metadata.RDS', 'meta.data')
  # }
  # 
  # if (! exists('scDataHenry')) {
  #   readRDSAs('data/PCaDB_scDataHenry.RDS', 'scDataHenry')
  # }
  
  
  ############################################################
  ###                         Query                        ###
  ############################################################
  
  observeEvent(input$gene.id, {
    
    gene.id <- input$gene.id
    
    ### Gene Information
    
    output$gene.symbol <- renderText({ 
      gene.symbol <- gene.annotation[gene.id, 'gene_name']
      ifelse(is.na(gene.symbol), '', gene.symbol)
    })
    
    output$gene.alias <- renderText({ 
      gene.alias <- gene.annotation[gene.id, 'alias_symbol']
      gene.alias <- paste0('Alias: ', ifelse(is.na(gene.alias), '', gene.alias))
      gene.alias
    })
    
    
    output$gene.ensembl.id <- renderText({ 
      gene.ensembl.id <- gene.annotation[gene.id, 'ensembl_id']
      gene.ensembl.id <- paste0('Ensembl ID: ', ifelse(is.na(gene.ensembl.id), '', gene.ensembl.id))
      gene.ensembl.id
    })
    
    output$gene.entrez.id <- renderText({ 
      gene.entrez.id <- gene.annotation[gene.id, 'entrez_id']
      gene.entrez.id <- paste0('Entrez ID: ', ifelse(is.na(gene.entrez.id), '', gene.entrez.id))
      gene.entrez.id
    })
    
    output$gene.description <- renderText({ 
      gene.description <- gene.annotation[gene.id, 'description']
      gene.description <- paste0('Description: ', ifelse(is.na(gene.description), '', gene.description))
      gene.description
    })
    
    
    output$gene.external <- renderUI({ 
      hgnc.id <- gene.annotation[gene.id, 'hgnc_id']
      entrez.id <- gene.annotation[gene.id, 'entrez_id']
      
      ensembl <- paste0('https://uswest.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g=', gene.id)
      ensembl.link <- a('ENSEMBL', href = ensembl, target="_blank", style = "font-size: 100%;")
      
      hgnc <- paste0('https://www.genenames.org/data/gene-symbol-report/#!/hgnc_id/', hgnc.id)
      hgnc.link <- a('HGNC', href = hgnc, target="_blank", style = "font-size: 100%;")
      
      ncbi <- paste0('https://www.ncbi.nlm.nih.gov/gene/', entrez.id)
      ncbi.link <- a('NCBI', href = ncbi, target="_blank", style = "font-size: 100%;")
      
      gtex <- paste0('https://gtexportal.org/home/gene/', gene.id)
      gtex.link <- a('GTEx', href = gtex, target="_blank", style = "font-size: 100%;")
      
      hpa <- paste0('https://www.proteinatlas.org/', gene.id)
      hpa.link <- a('HPA', href = hpa, target="_blank", style = "font-size: 100%;")
      
      kegg <- paste0('https://www.kegg.jp/entry/hsa:', entrez.id)
      kegg.link <- a('KEGG', href = kegg, target="_blank", style = "font-size: 100%;") #background-color: white;
      
      if (gene.id=='') {
        tagList("External links:")
      } else {
        tagList("External links:", ensembl.link, hgnc.link, ncbi.link, gtex.link, hpa.link, kegg.link)
      }
      })
  
  ### Expression
  
  expr.dataset.id <- reactive({
    input$expr.dataset.id
    
  })
  
  output$query_boxplot <- renderPlot({
    
    expr.dataset.id <- expr.dataset.id()
    
    req(length(expr.data[[expr.dataset.id]][gene.id,])>0)
    
    dataForBoxPlot <- data.frame(expr=expr.data[[expr.dataset.id]][gene.id,],
                                 group=meta.data[[expr.dataset.id]][,'pcadb_group'],
                                 stringsAsFactors = F)
    
    # if (expr.dataset.id=='Taylor') {
    #   filter <- grep('Cell Line', dataForBoxPlot$group)
    #   dataForBoxPlot <- dataForBoxPlot[-filter,]
    # }
    
    p <- ExprBoxPlotFun(dataForBoxPlot, colors)
    p
    
  })#, height = 400, width = 600)
  
  
  plotWidth <- reactive({
    expr.dataset.id <- expr.dataset.id()
    
    n.sample.type <- length(unique(meta.data[[expr.dataset.id]][,'pcadb_group']))
    
    if (n.sample.type<=3) {
      plotWidth <- 400
    } else if (n.sample.type > 3 & n.sample.type <= 6) {
      plotWidth <- 600
    } else if (n.sample.type > 6) {
      plotWidth <- 900
    }
    
    # if (expr.dataset.id=='Taylor') {
    #   plotWidth <- 400
    # }
    
    plotWidth
    
  })
  
  
  plotHeight <- reactive({
    expr.dataset.id <- expr.dataset.id()
    
    if (expr.dataset.id %in% c('Taylor','GSE32269','SU2C-PCF-2019-PolyA','SU2C-PCF-2019-Capture',
                               'GSE6752','GSE77930','Neuroendocrine',
                               'GSE6919-GPL8300','GSE6919-GPL92','GSE6919-GPL93')) {
      plotHeight <- 500
    } else if (expr.dataset.id %in% c('GSE59745', 'GSE97284','GSE3325',
                                      'GSE35988-GPL6480','GSE35988-GPL6848'))
    {
      plotHeight <- 450
    } else {
      plotHeight <- 350
    }
    
    plotHeight
    
  })
  
  
  output$query_boxplot_ui <- renderUI({
    
    expr.dataset.id <- expr.dataset.id()
    
    if (!startsWith(gene.id, 'ENSG')) {
      return ()
    }
    
    if(startsWith(gene.id, 'ENSG') & !gene.id %in% rownames(expr.data[[expr.dataset.id]])) {
      shinyalert(text=paste0('The selected gene ', gene.id, ' is not in the dataset ', expr.dataset.id));
      return()
    }
    
    plotOutput("query_boxplot", height = plotHeight(), width = plotWidth())
  })
  
  
  ### Survival
  
  # bcr.dataset <- reactive({
  #   bcr.dataset
  #   
  # })
  
  
  dataForSurvivalAnalysis <- reactive({
    #bcr.dataset <- bcr.dataset()
    
    dataForSurvivalAnalysis <- c()
    
    for (dt in bcr.dataset) {
      
      #message(dt)
      
      idx <- which(meta.data[[dt]][,'sample_type'] %in% c('Primary','Tumor'))
      
      if (! gene.id %in% rownames(expr.data[[dt]])) {
        next
      }
      
      survData <- data.frame(expr=expr.data[[dt]][gene.id,idx],
                             time.to.bcr=meta.data[[dt]][idx,'time_to_bcr'],
                             bcr.status=meta.data[[dt]][idx,'bcr_status'],
                             dataset=dt, stringsAsFactors = F)
      
      #print (survData)
      
      dataForSurvivalAnalysis <- rbind(dataForSurvivalAnalysis, survData)
      
    }
    
    #print (dataForSurvivalAnalysis)
    
    dataForSurvivalAnalysis <- data.frame(dataForSurvivalAnalysis, stringsAsFactors = F)
    #colnames(dataForSurvivalAnalysis) <- c('expr','time.to.bcr','bcr.status','dataset')
    dataForSurvivalAnalysis
    #print (dataForSurvivalAnalysis)
    
  })
  
  kmTable <- reactive({
    
    dataForSurvivalAnalysis <- dataForSurvivalAnalysis()
    datasets <- unique(dataForSurvivalAnalysis$dataset)
    
    kmTable <- c()
    for (dt in datasets) {
      
      idx <- which(dataForSurvivalAnalysis$dataset==dt)
      
      score <- as.numeric(dataForSurvivalAnalysis$expr[idx])
      time.to.bcr <- as.numeric(dataForSurvivalAnalysis$time.to.bcr[idx])
      bcr.status <- as.numeric(dataForSurvivalAnalysis$bcr.status[idx])
      
      # coxtest <- coxph(Surv(time.to.bcr, bcr.status) ~ score)
      # summcph <- summary(coxtest)
      # 
      # coeffs <- c(summcph$coefficients[,2], summcph$conf.int[,3:4], 
      #             summcph$coefficients[,5])
      # 
      # 
      # coxTable <- rbind(coxTable, coeffs)
      
      ###
      risk.group <- score > median(score, na.rm = T)
      
      if (length(unique(risk.group))==1) {
        next
      }
      
      n <- length(risk.group)
      
      n.high <- sum(risk.group, na.rm=T)
      n.low <- sum(!risk.group, na.rm=T)
      
      sdf <- survdiff(Surv(time.to.bcr, bcr.status) ~ risk.group)
      p.val <- pchisq(sdf$chisq, length(sdf$n)-1, lower.tail = FALSE)
      #p.val = 1 - pchisq(data.survdiff$chisq, length(data.survdiff$n) - 1)
      
      hr = (sdf$obs[2]/sdf$exp[2])/(sdf$obs[1]/sdf$exp[1])
      upper95 = exp(log(hr) + qnorm(0.975)*sqrt(1/sdf$exp[2]+1/sdf$exp[1]))
      lower95 = exp(log(hr) - qnorm(0.975)*sqrt(1/sdf$exp[2]+1/sdf$exp[1]))
      
      coeffs <- c(n, hr, lower95, upper95, p.val, dt)
      
      kmTable <- rbind(kmTable, coeffs)
      
      
    }
    
    
    ###
    # coxTable <- data.frame(mir.id, mir.name, coxTable, row.names = NULL, stringsAsFactors = F)
    # colnames(coxTable) <- c('miRNA.Accession','miRNA.ID','Hazard.Ratio','Lower95','Upper95','P.Value')
    # 
    # o <- order(coxTable$P.Value, decreasing = F)
    # coxTable <- coxTable[o,]
    # 
    # coxph.list[[prj]] <- coxTable
    
    
    ###
    
    #kmTable[,1:5] <- apply(kmTable[,1:5], 2, numeric)
    
    if (gene.id=='' | length(datasets)==0) {
      kmTable <- data.frame(matrix(ncol = 6, nrow = 0))
      colnames(kmTable) <- c('AUC','Lower95','Upper95','P.Value','miRNA.ID')
      return (kmTable)
    }
    
    kmTable <- data.frame(kmTable, stringsAsFactors = F) #row.names = NULL, 
    colnames(kmTable) <- c('N','HR','Lower95','Upper95','P.Value','Dataset')
    
    if (length(datasets)!=0) {
      rownames(kmTable) <- datasets
    }
    
    kmTable$HR <- as.numeric(kmTable$HR)
    kmTable$N <- as.numeric(kmTable$N)
    kmTable$Lower95 <- as.numeric(kmTable$Lower95)
    kmTable$Upper95 <- as.numeric(kmTable$Upper95)
    kmTable$P.Value <- as.numeric(kmTable$P.Value)
    
    o <- order(kmTable$P.Value, decreasing = F)
    kmTable <- kmTable[o,]
    
    kmTable
    
    #print (kmTable$HR)
    
    
  })
  
  
  plotHeight.KM <- reactive({
    
    dataForSurvivalAnalysis <- dataForSurvivalAnalysis()
    datasets <- unique(dataForSurvivalAnalysis$dataset)
    
    if (length(datasets)==0) {
      1
    } else {
      length(datasets)
    }
    
  })
  
  output$survival_forest <- renderPlot({
    
    dataForForestPlot <- kmTable()
    
    if (gene.id=='') {
      return()
    }
    
    if(startsWith(gene.id, 'ENSG') & nrow(dataForForestPlot)==0) {
      shinyalert(text=paste0('The selected gene ', gene.id, ' is not detected in any of 10 datasets with RFS data'));
      return()
    }
    
    
    p <- transcriptomeKMForestplotFunT(dataForForestPlot)
    p
    
  }, width=800, height=(plotHeight.KM()+3)/10*450)
  
  output$survival_km <- renderPlot({
    
    dataForSurvivalAnalysis <- dataForSurvivalAnalysis()
    datasets <- unique(dataForSurvivalAnalysis$dataset)
    
    if (gene.id=='') {
      return()
    }
    
    if(startsWith(gene.id, 'ENSG') & length(datasets)==0) {
      #shinyalert(text=paste0('The selected gene ', gene.id, ' is not in the single-cell RNAseq dataset'));
      return()
    }
    
    
    KMPlotList <- list()
    
    for (dt in datasets) {
      
      idx <- which(dataForSurvivalAnalysis$dataset==dt)
      #c('expr','time.to.bcr','bcr.status','dataset')
      
      dataForKMPlot <- dataForSurvivalAnalysis[idx,]
      
      KMPlotList[[dt]] <- KMPlotFun(dataForKMPlot, dt=dt)
      
    }
    
    # plot.height <- reactive(length(KMPlotList))
    # print (plot.height())
    p <- grid.arrange(grobs=KMPlotList, ncol = 2)
    
    p
    
    
  }, width=680, height=ceiling(plotHeight.KM()/2)*350)
  
  
  ### Single Cell
  
  dataForTSNEPlot <- reactive({

    dataForTSNEPlot <- data.frame(tSNE.1=scDataHenry$tsne[,1],
                                  tSNE.2=scDataHenry$tsne[,2],
                                  cell.type=as.character(scDataHenry$annotation$Population),
                                  expr=as.numeric(scDataHenry$expr[gene.id,]),
                                  gene=gene.annotation[gene.id, 'gene_name'],
                                  stringsAsFactors = F)
    
    dataForTSNEPlot
    
  })
  
  
  dataForUMAPPlot <- reactive({
    
    dataForUMAPPlot <- data.frame(UMAP.1=scDataHenry$umap[,1],
                                  UMAP.2=scDataHenry$umap[,2],
                                  cell.type=as.character(scDataHenry$annotation$Population),
                                  expr=as.numeric(scDataHenry$expr[gene.id,]),
                                  gene=gene.annotation[gene.id, 'gene_name'],
                                  stringsAsFactors = F)
    
    dataForUMAPPlot
    
  })
  
  output$henry_sc_tsne_cell <- renderPlot({
    
    if (!startsWith(gene.id, 'ENSG')) {
      return ()
    }
    
    if(startsWith(gene.id, 'ENSG') & !gene.id %in% rownames(scDataHenry$expr)) {
      shinyalert(text=paste0('The selected gene ', gene.id, ' is not detected in the scRNAseq dataset'));
      return()
    }
    
    dataForTSNEPlot <- dataForTSNEPlot()
    p <- scTSNEPlotFun(dataForTSNEPlot, expr=FALSE)
    p
    
  })
  
  output$henry_sc_tsne_expr <- renderPlot({
    
    if (!startsWith(gene.id, 'ENSG')) {
      return ()
    }
    
    if(startsWith(gene.id, 'ENSG') & !gene.id %in% rownames(scDataHenry$expr)) {
      #shinyalert(text=paste0('The selected gene ', gene.id, ' is not in the scRNAseq dataset'));
      return()
    }
    
    dataForTSNEPlot <- dataForTSNEPlot()
    p <- scTSNEPlotFun(dataForTSNEPlot, expr=TRUE)
    p
    
  })
  
  
  output$henry_sc_umap_cell <- renderPlot({
    
    if (!startsWith(gene.id, 'ENSG')) {
      return ()
    }
    
    if(startsWith(gene.id, 'ENSG') & !gene.id %in% rownames(scDataHenry$expr)) {
      #shinyalert(text=paste0('The selected gene ', gene.id, ' is not in the scRNAseq dataset'));
      return()
    }
    
    dataForUMAPPlot <- dataForUMAPPlot()
    p <- scUMAPPlotFun(dataForUMAPPlot, expr=FALSE)
    p
    
  })
  
  output$henry_sc_umap_expr <- renderPlot({
    
    if (!startsWith(gene.id, 'ENSG')) {
      return ()
    }
    
    if(startsWith(gene.id, 'ENSG') & !gene.id %in% rownames(scDataHenry$expr)) {
      #shinyalert(text=paste0('The selected gene ', gene.id, ' is not in the scRNAseq dataset'));
      return()
    }
    
    dataForUMAPPlot <- dataForUMAPPlot()
    p <- scUMAPPlotFun(dataForUMAPPlot, expr=TRUE)
    p
    
  })
  
  
  output$henry_sc_bubble <- renderPlot({
    
    if (!startsWith(gene.id, 'ENSG')) {
      return ()
    }
    
    if(startsWith(gene.id, 'ENSG') & !gene.id %in% rownames(scDataHenry$expr)) {
      #shinyalert(text=paste0('The selected gene ', gene.id, ' is not in the scRNAseq dataset'));
      return()
    }
    
    dataForBubblePlot <- dataForTSNEPlot() %>% group_by(gene, cell.type) %>% 
      summarise(mean.all=mean(expr), 
                mean.expressed=ifelse(sum(expr>0)>0, sum(expr)/sum(expr>0), 0),
                percent.expressed=sum(expr>0)/length(expr)*100)
    
    p <- scBubblePlotFun(dataForBubblePlot)
    p
    
  })
  
  
  output$henry_sc_violin <- renderPlot({
    
    if (!startsWith(gene.id, 'ENSG')) {
      return ()
    }
    
    if(startsWith(gene.id, 'ENSG') & !gene.id %in% rownames(scDataHenry$expr)) {
      #shinyalert(text=paste0('The selected gene ', gene.id, ' is not in the scRNAseq dataset'));
      return()
    }
    
    dataForViolinPlot <- dataForTSNEPlot()
    p <- scViolinPlotFun(dataForViolinPlot)
    p
    
  })
})
  
  
  ############################################################
  ###                 Prognostic Signatures                ###
  ############################################################
  
  observeEvent(input$overview.signature, {
    
    req(input$overview.signature)
    
    output$signature.gene.list <- DT::renderDataTable({
      
      .idx <- which(pca.signatures$Signature==input$overview.signature)
      signature.table <- pca.signatures[.idx,-1]
      signature.table
      
    }, 
    callback=table.download.button,
    options = list(#pageLength = 1000, 
      dom = 'lfrtBp', buttons = c('copy', 'csv', 'excel')), #i
    extensions = "Buttons",
    selection = list(mode='none', selected=1), ### === not selectable
    rownames = FALSE,
    server = FALSE
    )
    
    
  })
  
  
  ##########
  
  de.signature.table <- reactive({
    
    signature.summ <- pca.signatures %>% group_by(Ensembl.ID) %>%
      summarise(Count=length(Signature), 
                Signatures=paste(Signature, collapse = '; '),
                Symbol=HGNC.Symbol[1])
    
    signature.summ <- data.frame(signature.summ, stringsAsFactors = F)
    
    dataset <- input$de.dataset
    
    idx <- which(meta.data[[dataset]]$sample_type %in% c('Tumor','Primary','Normal'))
    
    expr <- expr.data[[dataset]][,idx]
    
    deg.group <- ifelse(meta.data[[dataset]]$sample_type[idx]=='Normal','Normal','Primary')
    deg.group <- factor(deg.group, levels=c('Normal','Primary'))
    
    design <- model.matrix(~0+deg.group)
    colnames(design) <- levels(deg.group)
    
    contrast.matrix <- makeContrasts(contrasts='Primary - Normal',
                                     levels=design)
    
    ### Differential gene expression analysis (limma)
    
    fit <- lmFit(expr, design)
    fit2 <- contrasts.fit(fit, contrast.matrix)
    fit2 <- eBayes(fit2)
    
    dgeTable <- topTable(fit2, coef=1, n=Inf, adjust.method='BH', sort.by='p')
    dgeTable <- dgeTable[signature.summ$Ensembl.ID,]
    
    dgeTable$Symbol <- signature.summ$Symbol
    dgeTable$Signature.Count <- signature.summ$Count
    dgeTable$Signatures <- signature.summ$Signatures
    
    dgeTable <- dgeTable[order(dgeTable$adj.P.Val, decreasing = F),]
    
    dgeTable
    
  })
  
  shinyjs::hide('signature.de.list')
  shinyjs::hide('signature.de.volcano')
  
  output$signature.de.list <- DT::renderDataTable({
    dgeTable <- de.signature.table()
    dgeTable[,c(1,2,3,6)] <- apply(dgeTable[,c(1,2,3,6)], 2, function(v) round(v,3))
    dgeTable[,c(4,5)] <- apply(dgeTable[,c(4,5)], 2, function(v) format(as.numeric(v), digits=3))
    
    dgeTable
    
  }, 
  callback=table.download.button,
  options = list(#pageLength = 1000, 
    dom = 'lfrtBp', buttons = c('copy', 'csv', 'excel')), #i
  extensions = "Buttons",
  selection = list(mode='none', selected=1), ### === not selectable
  rownames = FALSE,
  server = FALSE
  )
  
  
  output$signature.de.volcano <- renderPlot({
    
    dataForVolcanoPlot <- de.signature.table()
    
    logFcThreshold <- 1
    adjPvalThreshold <- 0.01
    
    dataForVolcanoPlot$Significance[with(dataForVolcanoPlot,
                                         logFC < logFcThreshold | adj.P.Val > adjPvalThreshold)] <- 'NS'
    dataForVolcanoPlot$Significance[with(dataForVolcanoPlot,
                                         logFC >= logFcThreshold & adj.P.Val <= adjPvalThreshold)] <- 'UP'
    dataForVolcanoPlot$Significance[with(dataForVolcanoPlot,
                                         logFC <= -logFcThreshold & adj.P.Val <= adjPvalThreshold)] <- 'DOWN'
    
    p <- DEvolcanoPlotFun(dataForVolcanoPlot, logFcThreshold, adjPvalThreshold)
    p
    
  })
  
  shinyjs::show('signature.de.list')
  shinyjs::show('signature.de.volcano')
  
  
  
  
  ###
  bcr.signature.table <- reactive({
    
    signature.summ <- pca.signatures %>% group_by(Ensembl.ID) %>%
      summarise(Count=length(Signature), 
                Signatures=paste(Signature, collapse = '; '),
                Symbol=HGNC.Symbol[1])
    
    signature.summ <- data.frame(signature.summ, stringsAsFactors = F)
    
    dataset <- input$bcr.dataset.input
    
    idx <- which(meta.data[[dataset]]$sample_type %in% c('Tumor','Primary'))
    
    exprData <- expr.data[[dataset]][,idx]
    
    time.to.event <- as.numeric(meta.data[[dataset]]$time_to_bcr)[idx]
    event.status <- as.numeric(meta.data[[dataset]]$bcr_status)[idx]
    
    kmTable <- c()
    
    for (.g in signature.summ$Ensembl.ID) {
      
      if (!.g %in% rownames(exprData)) {
        coeffs <- rep(NA,5)
        kmTable <- rbind(kmTable, coeffs)
        next
      }
      
      expr <- exprData[.g,]
      risk.group <- expr > median(expr, na.rm = T)
      
      if (length(unique(risk.group))==1) {
        coeffs <- rep(NA,5)
        kmTable <- rbind(kmTable, coeffs)
        next
      }
      
      n.high <- sum(risk.group, na.rm=T)
      n.low <- sum(!risk.group, na.rm=T)
      
      sdf <- survdiff(Surv(time.to.event, event.status) ~ risk.group)
      p.val <- pchisq(sdf$chisq, length(sdf$n)-1, lower.tail = FALSE)
      #p.val = 1 - pchisq(data.survdiff$chisq, length(data.survdiff$n) - 1)
      
      hr = (sdf$obs[2]/sdf$exp[2])/(sdf$obs[1]/sdf$exp[1])
      upper95 = exp(log(hr) + qnorm(0.975)*sqrt(1/sdf$exp[2]+1/sdf$exp[1]))
      lower95 = exp(log(hr) - qnorm(0.975)*sqrt(1/sdf$exp[2]+1/sdf$exp[1]))
      
      coeffs <- c(hr, lower95, upper95, p.val)
      
      kmTable <- rbind(kmTable, coeffs)
      
    }
    
    colnames(kmTable) <- c('HR','Lower95','Upper95','P')
    rownames(kmTable) <- signature.summ$Ensembl.ID
    
    kmTable <- data.frame(kmTable, stringsAsFactors = F)
    
    kmTable$adj.P.Val <- p.adjust(kmTable$P, method = 'BH')
    
    kmTable$Symbol <-  signature.summ$Symbol
    kmTable$Signature.Count <- signature.summ$Count
    kmTable$Signatures <- signature.summ$Signatures
    kmTable$N <- length(time.to.event)
    
    kmTable <- kmTable[order(kmTable$adj.P.Val, decreasing = F),]
    kmTable
    
  })
  
  shinyjs::hide('signature.bcr.list')
  shinyjs::hide('signature.bcr.forest')
  #shinyjs::hide('signature.bcr.volcano')
  
  output$signature.bcr.list <- DT::renderDataTable({
    kmTable <- bcr.signature.table()
    kmTable[,1:5] <- apply(kmTable[,1:5], 2, function(v) ifelse(v<=0.001, format(as.numeric(v), digits=3), round(v,3)))
    kmTable <- kmTable[,-ncol(kmTable)]
    kmTable
    
  }, 
  callback=table.download.button,
  options = list(#pageLength = 1000, 
    dom = 'lfrtBp', buttons = c('copy', 'csv', 'excel')), #i
  extensions = "Buttons",
  selection = list(mode='none', selected=1), ### === not selectable
  rownames = FALSE,
  server = FALSE
  )
  
  
  # output$signature.bcr.volcano <- renderPlot({
  #   
  #   dataForVolcanoPlot <- bcr.signature.table()
  #   dataForVolcanoPlot$logHR <- log2(dataForVolcanoPlot$HR)
  #   
  #   logHrThreshold <- 0
  #   adjPvalThreshold <- 0.01
  #   
  #   dataForVolcanoPlot$Significance[with(dataForVolcanoPlot,
  #                                        logHR < logHrThreshold | adj.P.Val > adjPvalThreshold)] <- 'NS'
  #   dataForVolcanoPlot$Significance[with(dataForVolcanoPlot,
  #                                        logHR >= logHrThreshold & adj.P.Val <= adjPvalThreshold)] <- 'UP'
  #   dataForVolcanoPlot$Significance[with(dataForVolcanoPlot,
  #                                        logHR <= -logHrThreshold & adj.P.Val <= adjPvalThreshold)] <- 'DOWN'
  #   
  #   p <- KMvolcanoPlotFun(dataForVolcanoPlot, logHrThreshold, adjPvalThreshold)
  #   p
  #   
  # })
  
  output$signature.bcr.forest <- renderPlot({
    
    dataForForestPlot <- bcr.signature.table()
    dataForForestPlot$Gene <- as.character(dataForForestPlot$Symbol)
    dataForForestPlot <- dataForForestPlot[which(dataForForestPlot$Signature.Count>=3 & !is.na(dataForForestPlot$HR)),]
    
    p <- signatureKMForestplotFunT(dataForForestPlot)
    p
    
  })
  
  
  
  shinyjs::show('signature.bcr.list')
  shinyjs::show('signature.bcr.forest')
  #shinyjs::show('signature.bcr.volcano')
  
  
  observe({
    req(input$signature.pathway.input, input$signature.ontology.input)
    
    signature.enrich <- reactive({
      
      signature <- input$signature.pathway.input
      ontology <- input$signature.ontology.input
      
      enrichTable <- signature.enrichment[[ontology]][[signature]]
      enrichTable
    })
    
    output$signature.enrich.table <- DT::renderDataTable({
      
      enrich.table <- signature.enrich()
      
      if (nrow(enrich.table)==0) {
        shinyjs::hide('signature_enrichment_bar_plot')
        shinyjs::hide('signature_enrichment_bubble_plot')
      } else {
        shinyjs::show('signature_enrichment_bar_plot')
        shinyjs::show('signature_enrichment_bubble_plot')
      }
      
      if (nrow(enrich.table)>0) {
        enrich.table$Count <- paste0(enrich.table$Count, '/', enrich.table$List.Total)
        enrich.table$Pop.Hits <- paste0(enrich.table$Pop.Hits, '/', enrich.table$Pop.Total)
      }
      
      enrich.table <- enrich.table[-c(4,6,7,10,11)]
      colnames(enrich.table)[c(3,4)] <- c('Count/List.Total','Pop.Hits/Pop.Total')
      
      enrich.table
      
    }, 
    callback=table.download.button,
    options = list(pageLength = 5, dom = 'lfrtBp', buttons = c('copy', 'csv', 'excel')), #i
    extensions = "Buttons",
    selection = list(mode='none', selected=1), ### === not selectable
    server = FALSE
    )
    
    
    plotHeight.Enrich.Bubble <- reactive({
      
      dataForBarPlot <- signature.enrich()
      
      if (nrow(dataForBarPlot)>=30) {
        500
      } else if (nrow(dataForBarPlot)>=20 & nrow(dataForBarPlot)<30) {
        nrow(dataForBarPlot)/30*500
      } else {
        20/30*500
      }

    })
    
    plotHeight.Enrich.Bar <- reactive({
      
      dataForBarPlot <- signature.enrich()
      
      if (nrow(dataForBarPlot)>=26) {
        500
      } else {
        nrow(dataForBarPlot)/30*500 + 90
      }
      
    })
    

    output$signature_enrichment_bar_plot <- renderPlot({
      
      dataForBarPlot <- signature.enrich()
      dataForBarPlot$BH.Adj.P <- as.numeric(dataForBarPlot$BH.Adj.P)
      dataForBarPlot$Count <- as.numeric(dataForBarPlot$Count)
      dataForBarPlot$Fold.Enrichment <- as.numeric(dataForBarPlot$Fold.Enrichment)
      
      if (nrow(dataForBarPlot)>30) {
        dataForBarPlot <- dataForBarPlot[1:30,]
      }
      
      p <- EnrichmentBarPlotFun(dataForBarPlot)
      p
      
    }, width = 800, height = plotHeight.Enrich.Bar())
    
    
    # output$enrich.bar.downbttn.csv <- downloadHandler(
    #   filename = function(){paste('enrich.bar.csv', sep = '')},
    #   
    #   content = function(file){
    #     write.csv(transcriptome.enrich$enrich.bar.data, file, row.names = FALSE, quote = F)
    #   })
    # 
    # output$enrich.bar.downbttn.png <- downloadHandler(
    #   filename = function(){paste('enrich.bar.png', sep = '')},
    #   
    #   content = function(file){
    #     png(file, width = 1000, height = 700)
    #     print(transcriptome.enrich$enrich.bar.plot)
    #     dev.off()
    #   })
    
    # output$enrich.bar.downbttn.pdf <- downloadHandler(
    #   filename = function(){paste('enrich.bar.pdf', sep = '')},
    #   
    #   content = function(file){
    #     pdf(file, width = 10, height = 7)
    #     print(transcriptome.enrich$enrich.bar.plot)
    #     dev.off()
    #   })
    
    
    output$signature_enrichment_bubble_plot <- renderPlot({
      
      req(nrow(signature.enrich())>0)
      
      dataForBubblePlot <- signature.enrich()
      dataForBubblePlot$BH.Adj.P <- as.numeric(dataForBubblePlot$BH.Adj.P)
      dataForBubblePlot$Count <- as.numeric(dataForBubblePlot$Count)
      dataForBubblePlot$Fold.Enrichment <- as.numeric(dataForBubblePlot$Fold.Enrichment)
      
      if (nrow(dataForBubblePlot)>30) {
        dataForBubblePlot <- dataForBubblePlot[1:30,]
      }
      
      p <- EnrichmentBubblePlotFun(dataForBubblePlot)
      p
      
    }, width = 800, height = plotHeight.Enrich.Bubble())
    
  })
  
  
  
  observe({
    
    req(input$signature_comp_signature_input, input$signature.comp.training.input,input$signature.comp.model.input)
    
    signature.model.comparison <- reactive({
      
      signature <- input$signature_comp_signature_input
      training <- input$signature.comp.training.input
      model <- input$signature.comp.model.input
      
      if (signature=='All Signatures') {
        idx <- which(model.comparison$Training==training & 
                       model.comparison$Model==model)
      } else {
        idx <- which(model.comparison$Signature==signature & 
                       model.comparison$Training==training & 
                       model.comparison$Model==model)
      }
      
      signature.model.comparison <- model.comparison[idx,]
      signature.model.comparison
    })
    
    
    
    output$signature_summary_table <- DT::renderDataTable({
      
      signature.summary.table <- signature.model.comparison()
      signature.summary.table <- signature.summary.table[,-c(7:10,12:13)]
      
      signature.summary.table[,c(5:8)] <- apply(signature.summary.table[,5:8], 2, function(v) ifelse(v<=0.001, format(as.numeric(v), digits=3), round(v,3)))
      
      signature.summary.table
      
    }, 
    callback=table.download.button,
    options = list(pageLength = 10, dom = 'lfrtBp', buttons = c('copy', 'csv', 'excel')), #i
    extensions = "Buttons", rownames=NULL,
    selection = list(mode='none', selected=1), ### === not selectable
    server = FALSE
    )
    
    
    output$signature_c_index_plot <- renderPlot({
      
      col <- brewer.pal(9, "YlOrBr")[4]
      
      dataForForestPlot <- signature.model.comparison()
      
      o <- order(dataForForestPlot$C, decreasing = F)
      dataForForestPlot <- dataForForestPlot[o,]
      
      # dataForForestPlot$Signature <- factor(dataForForestPlot$Signature, 
      #                                       levels=dataForForestPlot$Signature[o])
      
      p <- ggplot(dataForForestPlot, aes(x=Test, y=C)) +
        #geom_segment(aes(y=dataset, x=lower95.coxph, xend=upper95.coxph, yend=dataset), color='black', size=1) +
        #geom_segment(aes(y=6:1-0.1, x=lower95.coxph, xend=lower95.coxph, yend=6:!+0.1), color='black', size=1) +
        #geom_errorbar(aes(ymin=log(KM.Lower95), ymax=log(KM.Upper95)),width=0.4, size=0.8, color='black')+ 
        #geom_point(color=google.red, size=3, shape=15) + #facet_grid(.~type) +
        scale_x_discrete(limits=unique(dataForForestPlot$Test)) +
        ylim(0,1) +
        geom_point(color=google.red, size=3, shape=15) + #facet_grid(.~type) +
        #scale_colour_gradientn(limits=c(0,0.05),
        #                       colors= c(google.red, 'white', google.blue)) + #, na.value='grey'
        # scale_colour_gradientn(limits=c(0,0.05),
        #                        colors= c(cols[10], cols[1])) + #, na.value='grey'
        
        #geom_text(data =dataForForestPlot, aes(x=dataset, y=c(-2.9,-3.12,-1.16,1.2,2.58,2.5), label=P, group=NULL),
        #          size=4.4) +
        geom_hline(yintercept = 0.5, linetype='dashed') +
        #geom_text(data =dataForForestPlot, aes(x=dataset, y=c(0.35,0.5,0.2,0.45,0.95,0.55), label=p.coxph, group=NULL),
        #          size=4.4) +
        #scale_y_continuous(trans = 'log10',
        #                   breaks = c(0, 1, 2.5,50,250,7500),
        #                   labels = c(0, 1, 2.5,50,250,7500)) +
        coord_flip()+
        #ylim(0,10) +
        xlab('')+ylab('C Index') +
        #facet_wrap(~Test, nrow=2) +
        theme_bw()+theme(#axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank()) +
        theme(legend.position="none") +
        theme(axis.text=element_text(size=12, face = 'bold', color = 'black'),
              axis.title=element_text(size=14, face = 'bold'),
              axis.line = element_line(colour = "black"),
              axis.line.y = element_blank(),
              strip.text = element_text(size=14, face='bold')) +
        theme(strip.background = element_rect(fill=col))
      
      p
      
    })
    
    
    
    
    output$signature_km_plot <- renderPlot({
      
      col <- brewer.pal(9, "YlOrBr")[4]
      
      dataForForestPlot <- signature.model.comparison()
      
      o <- order(dataForForestPlot$KM.HR, decreasing = F)
      dataForForestPlot <- dataForForestPlot[o,]
      
      # dataForForestPlot$Signature <- factor(dataForForestPlot$Signature, 
      #                                       levels=dataForForestPlot$Signature[o])
      
      p <- ggplot(dataForForestPlot, aes(x=Test, y=log(KM.HR))) +
        #geom_segment(aes(y=dataset, x=lower95.coxph, xend=upper95.coxph, yend=dataset), color='black', size=1) +
        #geom_segment(aes(y=6:1-0.1, x=lower95.coxph, xend=lower95.coxph, yend=6:!+0.1), color='black', size=1) +
        geom_errorbar(aes(ymin=log(KM.Lower95), ymax=log(KM.Upper95)),width=0.3, size=0.8, color='black')+ 
        #geom_point(color=google.red, size=3, shape=15) + #facet_grid(.~type) +
        geom_point(color=google.red, size=3, shape=15) + #facet_grid(.~type) +
        #scale_colour_gradientn(limits=c(0,0.05),
        #                       colors= c(google.red, 'white', google.blue)) + #, na.value='grey'
        # scale_colour_gradientn(limits=c(0,0.05),
        #                        colors= c(cols[10], cols[1])) + #, na.value='grey'
        
        #geom_text(data =dataForForestPlot, aes(x=dataset, y=c(-2.9,-3.12,-1.16,1.2,2.58,2.5), label=P, group=NULL),
        #          size=4.4) +
        scale_x_discrete(limits=unique(dataForForestPlot$Test)) +
        geom_hline(yintercept = 0, linetype='dashed') +
        #geom_text(data =dataForForestPlot, aes(x=dataset, y=c(0.35,0.5,0.2,0.45,0.95,0.55), label=p.coxph, group=NULL),
        #          size=4.4) +
        #scale_y_continuous(trans = 'log10',
        #                   breaks = c(0, 1, 2.5,50,250,7500),
        #                   labels = c(0, 1, 2.5,50,250,7500)) +
        coord_flip()+
        #ylim(0,10) +
        xlab('')+ylab('Log(Hazard Ratio)') +
        #facet_wrap(~Test, nrow=2) +
        theme_bw()+theme(#axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank()) +
        theme(legend.position="none") +
        theme(axis.text=element_text(size=12, face = 'bold', color = 'black'),
              axis.title=element_text(size=14, face = 'bold'),
              axis.line = element_line(colour = "black"),
              axis.line.y = element_blank(),
              strip.text = element_text(size=14, face='bold')) +
        theme(strip.background = element_rect(fill=col))
      
      p
      
    })
    
    
    output$signature_auc_plot <- renderPlot({
      
      col <- brewer.pal(9, "YlOrBr")[4]
      
      dataForForestPlot <- signature.model.comparison()
      
      o <- order(dataForForestPlot$TD.AUC, decreasing = F)
      dataForForestPlot <- dataForForestPlot[o,]
      
      
      # dataForForestPlot$Signature <- factor(dataForForestPlot$Signature, 
      #                                       levels=dataForForestPlot$Signature[o])
      
      p <- ggplot(dataForForestPlot, aes(x=Test, y=TD.AUC)) +
        #geom_segment(aes(y=dataset, x=lower95.coxph, xend=upper95.coxph, yend=dataset), color='black', size=1) +
        #geom_segment(aes(y=6:1-0.1, x=lower95.coxph, xend=lower95.coxph, yend=6:!+0.1), color='black', size=1) +
        #geom_errorbar(aes(ymin=log(KM.Lower95), ymax=log(KM.Upper95)),width=0.4, size=0.8, color='black')+ 
        #geom_point(color=google.red, size=3, shape=15) + #facet_grid(.~type) +
        geom_point(color=google.red, size=3, shape=15) + #facet_grid(.~type) +
        scale_x_discrete(limits=unique(dataForForestPlot$Test)) +
        ylim(0,1) +
        #scale_colour_gradientn(limits=c(0,0.05),
        #                       colors= c(google.red, 'white', google.blue)) + #, na.value='grey'
        # scale_colour_gradientn(limits=c(0,0.05),
        #                        colors= c(cols[10], cols[1])) + #, na.value='grey'
        
        #geom_text(data =dataForForestPlot, aes(x=dataset, y=c(-2.9,-3.12,-1.16,1.2,2.58,2.5), label=P, group=NULL),
        #          size=4.4) +
        geom_hline(yintercept = 0.5, linetype='dashed') +
        #geom_text(data =dataForForestPlot, aes(x=dataset, y=c(0.35,0.5,0.2,0.45,0.95,0.55), label=p.coxph, group=NULL),
        #          size=4.4) +
        #scale_y_continuous(trans = 'log10',
        #                   breaks = c(0, 1, 2.5,50,250,7500),
        #                   labels = c(0, 1, 2.5,50,250,7500)) +
        coord_flip()+
        #ylim(0,10) +
        xlab('')+ylab('AUC') +
        #facet_wrap(~Test, nrow=2) +
        theme_bw()+theme(#axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank()) +
        theme(legend.position="none") +
        theme(axis.text=element_text(size=12, face = 'bold', color = 'black'),
              axis.title=element_text(size=14, face = 'bold'),
              axis.line = element_line(colour = "black"),
              axis.line.y = element_blank(),
              strip.text = element_text(size=14, face='bold')) +
        theme(strip.background = element_rect(fill=col))
      
      
      p
      
    })
    
    
    output$signature_all_c_index_plot <- renderPlot({
      
      col <- brewer.pal(9, "YlOrBr")[4]
      
      dataForForestPlot <- signature.model.comparison()
      
      o <- order(dataForForestPlot$Test, dataForForestPlot$C, decreasing = F)
      dataForForestPlot <- dataForForestPlot[o,]
      
      
      p <- dataForForestPlot %>% 
        mutate(Signature = reorder(Signature, C)) %>%
        group_by(Test, Signature) %>% 
        arrange(desc(C)) %>% 
        ungroup() %>% 
        mutate(Signature = factor(paste(Signature, Test, sep = "__"), 
                                  levels = rev(paste(Signature, Test, sep = "__")))) %>%
        ggplot(aes(Signature, C)) +
        
        
        # dataForForestPlot$Signature <- factor(dataForForestPlot$Signature, 
        #                                       levels=dataForForestPlot$Signature[o])
        
        #p <- ggplot(dataForForestPlot, aes(x=Signature, y=TD.AUC)) +
        #geom_segment(aes(y=dataset, x=lower95.coxph, xend=upper95.coxph, yend=dataset), color='black', size=1) +
        #geom_segment(aes(y=6:1-0.1, x=lower95.coxph, xend=lower95.coxph, yend=6:!+0.1), color='black', size=1) +
        #geom_errorbar(aes(ymin=log(KM.Lower95), ymax=log(KM.Upper95)),width=0.4, size=0.8, color='black')+ 
        #geom_point(color=google.red, size=3, shape=15) + #facet_grid(.~type) +
        scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
        geom_point(color=google.red, size=1.5, shape=15) + #facet_grid(.~type) +
        scale_y_continuous(limits = c(0,1), breaks = c(0,0.5,1)) +
        #scale_colour_gradientn(limits=c(0,0.05),
        #                       colors= c(google.red, 'white', google.blue)) + #, na.value='grey'
        # scale_colour_gradientn(limits=c(0,0.05),
        #                        colors= c(cols[10], cols[1])) + #, na.value='grey'
        
        #geom_text(data =dataForForestPlot, aes(x=dataset, y=c(-2.9,-3.12,-1.16,1.2,2.58,2.5), label=P, group=NULL),
        #          size=4.4) +
        geom_hline(yintercept = 0.5, linetype='dashed') +
        #geom_text(data =dataForForestPlot, aes(x=dataset, y=c(0.35,0.5,0.2,0.45,0.95,0.55), label=p.coxph, group=NULL),
        #          size=4.4) +
        #scale_y_continuous(trans = 'log10',
        #                   breaks = c(0, 1, 2.5,50,250,7500),
        #                   labels = c(0, 1, 2.5,50,250,7500)) +
        coord_flip()+
        #ylim(0,10) +
        xlab('')+ylab('C Index') +
        facet_wrap(~Test, nrow=2, scales = 'free_y') +
        theme_bw()+theme(#axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank()) +
        theme(legend.position="none") +
        theme(axis.text=element_text(size=10, face = 'bold', color = 'black'),
              axis.title=element_text(size=12, face = 'bold'),
              axis.line = element_line(colour = "black"),
              axis.line.y = element_blank(),
              strip.text = element_text(size=11, face='bold')) +
        theme(strip.background = element_rect(fill=col))
      
      p
      
    })
    
    
    output$signature_all_auc_plot <- renderPlot({
      
      col <- brewer.pal(9, "YlOrBr")[4]
      
      dataForForestPlot <- signature.model.comparison()
      
      o <- order(dataForForestPlot$Test, dataForForestPlot$TD.AUC, decreasing = F)
      dataForForestPlot <- dataForForestPlot[o,]
      
      
      p <- dataForForestPlot %>% 
        mutate(Signature = reorder(Signature, TD.AUC)) %>%
        group_by(Test, Signature) %>% 
        arrange(desc(TD.AUC)) %>% 
        ungroup() %>% 
        mutate(Signature = factor(paste(Signature, Test, sep = "__"), 
                                  levels = rev(paste(Signature, Test, sep = "__")))) %>%
        ggplot(aes(Signature, TD.AUC)) +
        
        
        # dataForForestPlot$Signature <- factor(dataForForestPlot$Signature, 
        #                                       levels=dataForForestPlot$Signature[o])
        
        #p <- ggplot(dataForForestPlot, aes(x=Signature, y=TD.AUC)) +
        #geom_segment(aes(y=dataset, x=lower95.coxph, xend=upper95.coxph, yend=dataset), color='black', size=1) +
        #geom_segment(aes(y=6:1-0.1, x=lower95.coxph, xend=lower95.coxph, yend=6:!+0.1), color='black', size=1) +
        #geom_errorbar(aes(ymin=log(KM.Lower95), ymax=log(KM.Upper95)),width=0.4, size=0.8, color='black')+ 
        #geom_point(color=google.red, size=3, shape=15) + #facet_grid(.~type) +
        scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
        geom_point(color=google.red, size=1.5, shape=15) + #facet_grid(.~type) +
        scale_y_continuous(limits = c(0,1), breaks = c(0,0.5,1)) +
        #scale_colour_gradientn(limits=c(0,0.05),
        #                       colors= c(google.red, 'white', google.blue)) + #, na.value='grey'
        # scale_colour_gradientn(limits=c(0,0.05),
        #                        colors= c(cols[10], cols[1])) + #, na.value='grey'
        
        #geom_text(data =dataForForestPlot, aes(x=dataset, y=c(-2.9,-3.12,-1.16,1.2,2.58,2.5), label=P, group=NULL),
        #          size=4.4) +
        geom_hline(yintercept = 0.5, linetype='dashed') +
        #geom_text(data =dataForForestPlot, aes(x=dataset, y=c(0.35,0.5,0.2,0.45,0.95,0.55), label=p.coxph, group=NULL),
        #          size=4.4) +
        #scale_y_continuous(trans = 'log10',
        #                   breaks = c(0, 1, 2.5,50,250,7500),
        #                   labels = c(0, 1, 2.5,50,250,7500)) +
        coord_flip()+
        #ylim(0,10) +
        xlab('')+ylab('AUC') +
        facet_wrap(~Test, nrow=2, scales = 'free_y') +
        theme_bw()+theme(#axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank()) +
        theme(legend.position="none") +
        theme(axis.text=element_text(size=10, face = 'bold', color = 'black'),
              axis.title=element_text(size=12, face = 'bold'),
              axis.line = element_line(colour = "black"),
              axis.line.y = element_blank(),
              strip.text = element_text(size=11, face='bold')) +
        theme(strip.background = element_rect(fill=col))
      
      p
      
    })
    
    
    output$signature_all_km_plot <- renderPlot({
      
      col <- brewer.pal(9, "YlOrBr")[4]
      
      dataForForestPlot <- signature.model.comparison()
      
      o <- order(dataForForestPlot$Test, dataForForestPlot$KM.HR, decreasing = F)
      dataForForestPlot <- dataForForestPlot[o,]
      
      p <- dataForForestPlot %>% 
        mutate(Signature = reorder(Signature, KM.HR)) %>%
        group_by(Test, Signature) %>% 
        arrange(desc(KM.HR)) %>% 
        ungroup() %>% 
        mutate(Signature = factor(paste(Signature, Test, sep = "__"), 
                                  levels = rev(paste(Signature, Test, sep = "__")))) %>%
        ggplot(aes(Signature, log(KM.HR))) +
        
        #geom_segment(aes(y=dataset, x=lower95.coxph, xend=upper95.coxph, yend=dataset), color='black', size=1) +
        #geom_segment(aes(y=6:1-0.1, x=lower95.coxph, xend=lower95.coxph, yend=6:!+0.1), color='black', size=1) +
        geom_errorbar(aes(ymin=log(KM.Lower95), ymax=log(KM.Upper95)),width=0.3, size=0.8, color='black')+ 
        #geom_point(color=google.red, size=3, shape=15) + #facet_grid(.~type) +
        geom_point(color=google.red, size=1.5, shape=15) + #facet_grid(.~type) +
        scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
        #scale_colour_gradientn(limits=c(0,0.05),
        #                       colors= c(google.red, 'white', google.blue)) + #, na.value='grey'
        # scale_colour_gradientn(limits=c(0,0.05),
        #                        colors= c(cols[10], cols[1])) + #, na.value='grey'
        
        #geom_text(data =dataForForestPlot, aes(x=dataset, y=c(-2.9,-3.12,-1.16,1.2,2.58,2.5), label=P, group=NULL),
        #          size=4.4) +
        geom_hline(yintercept = 0, linetype='dashed') +
        #geom_text(data =dataForForestPlot, aes(x=dataset, y=c(0.35,0.5,0.2,0.45,0.95,0.55), label=p.coxph, group=NULL),
        #          size=4.4) +
        #scale_y_continuous(trans = 'log10',
        #                   breaks = c(0, 1, 2.5,50,250,7500),
        #                   labels = c(0, 1, 2.5,50,250,7500)) +
        coord_flip()+
        #ylim(0,10) +
        xlab('')+ylab('Log(Hazard Ratio)') +
        facet_wrap(~Test, nrow=2, scales = 'free_y') +
        theme_bw()+theme(#axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank()) +
        theme(legend.position="none") +
        theme(axis.text=element_text(size=10, face = 'bold', color = 'black'),
              axis.title=element_text(size=12, face = 'bold'),
              axis.line = element_line(colour = "black"),
              axis.line.y = element_blank(),
              strip.text = element_text(size=11, face='bold')) +
        theme(strip.background = element_rect(fill=col))
      
      p
      
    })
    
    
    
  })
  
  
  ############################################################
  ###                Transcriptome Analysis                ###
  ############################################################
  
  output$transcriptome_dataset <- DT::renderDataTable({pcadb.dataset},
                                                      options = list(pageLength = 5,
                                                                     scrollX=TRUE),
                                                      selection = list(mode='single', selected=1)
  )
  
  observe({
    
    req(input$transcriptome_dataset_rows_selected)
    
    # ###
    # readRDSAs('data/PCaDB_Signature_Model_Comparison.RDS', 'model.comparison')
    # ###
    # readRDSAs(file='data/PCaDB_PCA_Analysis.RDS', 'pcadb.pca')
    # ###
    # readRDSAs(file='data/PCaDB_Survival_CoxPH_P0.05.RDS', 'pcadb.coxph')
    # readRDSAs(file='data/PCaDB_Survival_KM_P0.05.RDS', 'pcadb.km')
    
    transcriptome_dataset_idx <- input$transcriptome_dataset_rows_selected
    project <- as.character(datasets[transcriptome_dataset_idx,'Dataset'])
    
    meta <- meta.data[[project]]
    expr <- expr.data[[project]]
    
    output$transcriptome_text_summary_platform <- renderText({
      
      datasets[transcriptome_dataset_idx,'Platform']
      
    })
    
    output$transcriptome_text_summary_accession <- renderText({
      
      datasets[transcriptome_dataset_idx,'GEO/ArrayExpress/EGA']
      
    })
    
    output$transcriptome_text_summary_cbioportal <- renderText({
      
      datasets[transcriptome_dataset_idx,'cBioPortal']
      
    })
    
    output$transcriptome_text_summary_pipeline <- renderText({
      
      datasets[transcriptome_dataset_idx,'Data.Preprocessing']
      
    })
    
    
    
    output$transcriptome_dataset_summary <- renderText({ 
      transcriptome_dataset_summary <- as.character(paste0(datasets[transcriptome_dataset_idx,'Dataset'], ': ', 
                                                           datasets[transcriptome_dataset_idx,'Title']))
      transcriptome_dataset_summary
    })
    
    output$transcriptome_pie_sample_type <- renderPlotly({
      sample.freq <- table(meta$pcadb_group)
      dataForPiePlot <- data.frame(num=as.numeric(sample.freq), sam=names(sample.freq))
      
      p <- piePlotlyFun(dataForPiePlot)
      p
    })
    
    output$transcriptome_histogram_psa <- renderPlotly({
      keep <- which(meta$sample_type=='Tumor' | meta$sample_type=='Primary')
      dataForHistogram <- meta[keep,]
      dataForHistogram$x <- as.integer(dataForHistogram$preop_psa)
      
      p <- histPlotlyFun(dataForHistogram)
      p
      #p <- histogramFun(dataForHistogram)
      #ggplotly(p, height = 400, width = 350)
      
    })
    
    
    output$transcriptome_barplot_gleason <- renderPlotly({
      keep <- which(meta$sample_type=='Tumor' | meta$sample_type=='Primary' | meta$sample_type=='T')
      
      if (project =='GSE59745') {
        keep <- 1:nrow(meta)
      }
      
      dataForBarPlot <- meta[keep,]
      dataForBarPlot <- data.frame(table(dataForBarPlot$gleason_group),
                                   stringsAsFactors = F)
      
      colnames(dataForBarPlot) <- c('Gleason','Count')
      
      p <- barPlotlyFun(dataForBarPlot)
      p
      
    })
    
    output$transcriptome_pie_bcr_status <- renderPlotly({
      
      keep <- which(meta$sample_type=='Tumor' | meta$sample_type=='Primary' | meta$sample_type=='T' )
      
      if (project =='GSE59745') {
        keep <- 1:nrow(meta)
      }
      
      sample.freq <- table(as.numeric(meta$bcr_status[keep]))
      dataForPiePlot <- data.frame(num=as.numeric(sample.freq), sam=names(sample.freq))
      dataForPiePlot$sam <- ifelse(dataForPiePlot$sam==0, 'BCR - No', 'BCR - Yes')
      
      p <- piePlotlyFun(dataForPiePlot)
      p
    })
    
    
    output$transcriptome_km_bcr_time <- renderPlotly({
      
      keep <- which(meta$sample_type=='Tumor' | meta$sample_type=='Primary')
      
      time_to_bcr <- as.numeric(meta$time_to_bcr[keep])
      bcr_status <- as.numeric(meta$bcr_status[keep])
      
      dataForKMPlot <- data.frame(time_to_bcr, bcr_status)
      
      fit <- survfit(Surv(time_to_bcr, bcr_status) ~ 1, data=dataForKMPlot)
      
      p <- ggsurvplot(fit, data=dataForKMPlot, #pval = paste(label1, '\n', label2), pval.coord = c(xpos, ypos1),
                      #pval.size=4,
                      font.main = c(12, 'bold', 'black'), conf.int = FALSE,
                      #title = project,
                      legend = 'none',
                      #color = c('blue', 'green'),
                      palette= c(google.blue, google.red),
                      #legend.labs = c(paste('Low Expr (N=',nL,')',sep=''),
                      #                paste('High Expr  (N=',nH,')',sep='')),
                      #legend.title='group',
                      xlab = 'Time to BCR (months)', ylab = 'Survival Probability',
                      #xlab = paste(type,'(months)'), ylab = 'Survival Probability',
                      font.x = c(11), font.y = c(11), ylim=c(0,1), #16
                      censor.size=1.5, size = 0.5,
                      ggtheme = theme_bw()+ theme(axis.line = element_line(colour = "black"),
                                                  #panel.grid.major = element_blank(),
                                                  #panel.grid.minor = element_blank(),
                                                  #panel.border = element_rect(colour='black'),
                                                  panel.border = element_blank(),
                                                  panel.background = element_blank(),
                                                  legend.text = element_text(size=12),#14
                                                  legend.title = element_blank(),
                                                  legend.position = 'none',
                                                  axis.text = element_text(size=9, color='black'))) #+
      
      ggplotly(p[[1]], height = 300, width = 275)
      
    })
    
    output$dataset_link <- renderUI({
      
      # if (datasets[transcriptome_dataset_idx,'Data.Preprocessing']=='cBioPortal') {
      #   
      #   if (project=='DKFZ') {
      #     print (project)
      #     link <- 'https://www.cbioportal.org/study/summary?id=prostate_dkfz_2018'
      #   } else if (project=='SU2C-PCF-2019-Capture' | project=='SU2C-PCF-2019-PolyA') {
      #     link <- 'https://www.cbioportal.org/study/summary?id=prad_su2c_2019'
      #   } else if (project=='SMMU') {
      #     link <- 'https://www.cbioportal.org/study/summary?id=prad_eururol_2017'
      #   } else if (project=='Neuroendocrine') {
      #     link <- 'https://www.cbioportal.org/study/summary?id=nepc_wcm_2016'
      #   } else if (project=='Broad-Cornell') {
      #     link <- 'https://www.cbioportal.org/study/summary?id=prad_broad'
      #   }
      # } else if (grepl('E-MTAB', datasets[transcriptome_dataset_idx,'GEO/ArrayExpress/EGA'])) {
      #   link <- paste0('https://www.ebi.ac.uk/arrayexpress/experiments/',
      #                  datasets[transcriptome_dataset_idx,'GEO/ArrayExpress/EGA'])
      # } else if (project=='TCGA-PRAD') {
      #   
      #   link <- 'https://portal.gdc.cancer.gov/projects?filters=%7B%22op%22%3A%22and%22%2C%22content%22%3A%5B%7B%22op%22%3A%22in%22%2C%22content%22%3A%7B%22field%22%3A%22projects.project_id%22%2C%22value%22%3A%5B%22TCGA-PRAD%22%5D%7D%7D%2C%7B%22op%22%3A%22in%22%2C%22content%22%3A%7B%22field%22%3A%22projects.summary.experimental_strategies.experimental_strategy%22%2C%22value%22%3A%5B%22RNA-Seq%22%5D%7D%7D%5D%7D'
      #   
      # } else {
      #   accession <- as.character(datasets[transcriptome_dataset_idx,'GEO/ArrayExpress/EGA'])
      #   link <- paste0('https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=', accession)
      # }
      
      accession <- as.character(datasets[transcriptome_dataset_idx,'GEO/ArrayExpress/EGA'])
      
      if (grepl('E-MTAB', accession)) {
        link <- paste0('https://www.ebi.ac.uk/arrayexpress/experiments/',
                       datasets[transcriptome_dataset_idx,'GEO/ArrayExpress/EGA'])
        tags$iframe(src=link, seamless="seamless", width='100%', height='600')
      } else if (grepl('GSE', accession)) {
        link <- paste0('https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=', accession)
        tags$iframe(src=link, seamless="seamless", width='100%', height='600')
      }
      
    })
    
    
    output$transcriptome.pca.2d <- renderPlotly({
      
      dataForPCAPlot <- pcadb.pca[[project]]
      
      pc1 <- dataForPCAPlot$pc1[1]
      pc2 <- dataForPCAPlot$pc2[1]
      pc3 <- dataForPCAPlot$pc3[1]
      
      colors <- pie.colors[1:length(unique(dataForPCAPlot$Sample.Type))]
      
      p <- plot_ly(dataForPCAPlot, x = ~PC1, y = ~PC2,
                   color = ~Sample.Type,
                   colors = colors, 
                   alpha = 1,
                   marker = list(size=8),
                   text = ~Sample.Type,
                   hoverinfo = 'text',
                   showlegend=FALSE, height=400, width=500
                   
      )
      
      p <- p %>% layout(xaxis = list(title = paste0('PC1 (', pc1, '%)')),
                        yaxis = list(title = paste0('PC2 (', pc2, '%)')))
      
      p
      
      
    }) # }, height = 700, width = 700)
    
    
    output$transcriptome.pca.3d <- renderPlotly({
      
      dataForPCAPlot <- pcadb.pca[[project]]
      
      pc1 <- dataForPCAPlot$pc1[1]
      pc2 <- dataForPCAPlot$pc2[1]
      pc3 <- dataForPCAPlot$pc3[1]
      
      colors <- pie.colors[1:length(unique(dataForPCAPlot$Sample.Type))]
      
      
      p <- plot_ly(dataForPCAPlot, x = ~PC1, y = ~PC2, z = ~PC3,
                   color = ~Sample.Type,
                   colors = colors, 
                   alpha = 1,
                   marker = list(size=5),
                   text = ~Sample.Type,
                   hoverinfo = 'text',
                   showlegend=FALSE, height=400, width=500
                   
      )
      
      p <- p %>% layout(scene = list(xaxis = list(title = paste0('PC1 (', pc1, '%)')),
                                     yaxis = list(title = paste0('PC2 (', pc2, '%)')),
                                     zaxis = list(title = paste0('PC3 (', pc3, '%)'))))
      
      
      p
      
      
    }) # }, height = 700, width = 700)
    
    
    groups <- meta$pcadb_group
    
    # group.levels <- as.character(unlist(sapply(ccma.datasets[idx,'Group'], 
    #                                            function(x) strsplit(x, '; ')[[1]])))
    # 
    # groups <- factor(groups, levels = group.levels)
    
    groups <- as.data.frame(table(groups), stringsAsFactors=F)
    
    ### deg
    deg.group.names <- sapply(groups[,1], function(x) digest(x,algo='murmur32',seed=sample(1e9,1)))
    deg.groups <- cbind(rep(NA, nrow(groups)), rep(NA, nrow(groups)), groups)
    
    deg.groups[,1] <- sprintf(
      '<input type="radio" name="%s" value="%s"/>',
      deg.group.names, 1)
    
    deg.groups[,2] <- sprintf(
      '<input type="radio" name="%s" value="%s"/>',
      deg.group.names, 2)
    
    colnames(deg.groups) <- c('Control','Case','Groups','N')
    
    output$transcriptome.groups <- DT::renderDataTable(deg.groups, rownames = FALSE, escape = FALSE, selection = 'none', server = FALSE,
                                                       options=list(dom = 'tp', paging = TRUE, pageLength = 100, #ordering = FALSE,
                                                                    initComplete = JS("
                                                                                      function(setting, json) {
                                                                                      $(this.api().table().container())
                                                                                      .find('div.dataTables_paginate')
                                                                                      .css('display', this.api().page.info().pages <= 1 ? 'none' : 'block');
                                                                                      $(this.api().table().header()).css({'background-color': '#d2d6dc'});
                                                                                      }"),

                                                                    drawCallback = JS("
                                                                                      function(settings) {
                                                                                      Shiny.unbindAll(this.api().table().node());
                                                                                      Shiny.bindAll(this.api().table().node());
                                                                                      }")
        ),
        callback = JS("
                      table.rows().every(function(i, tab, row) {
                      var $this = $(this.node());
                      //$(\"input[name='\" + this.data()[2] + \"']\").prop('checked', false);
                      //console.log($this.children()[0]);
                      //console.log($.parseHTML(this.data()[0])[0].name);
                      $this.attr('id', $.parseHTML(this.data()[0])[0].name); //one time hash value
                      //$this.attr('id', this.data()[2]); //Group Name
                      $this.addClass('shiny-input-radiogroup');
                      //console.log($this.prop('checked'));
                      });
                      Shiny.unbindAll(table.table().node());
                      Shiny.bindAll(table.table().node());
                      "
                      )
        
                      )
    
    
    shinyjs::hide('transcriptome_deg_table')
    shinyjs::hide('transcriptome_volcano_plot')
    # shinyjs::hide('volcano.ccma.downbttn.csv')
    # shinyjs::hide('volcano.ccma.downbttn.pdf')
    
    observeEvent(input$deg_submit, {
      
      idx <- unlist(sapply(deg.group.names, function(i) input[[i]]))
      
      req(length(unique(idx))==2)
      
      groups <- names(idx)
      
      idx1 <- which(idx=='1')
      idx2 <- which(idx=='2')
      
      control.groups <- groups[idx1]
      case.groups <- groups[idx2]
      
      deg.group <- meta$pcadb_group
      
      idx <- which(deg.group %in% groups)
      deg.group <- deg.group[idx]
      
      
      deg.group.pcadb <- ifelse(deg.group %in% control.groups, 'Control', 'Case')
      deg.group.pcadb <- factor(deg.group.pcadb)
      
      
      ###
      #req(length(levels(deg.group.ccma)==2))
      
      design <- model.matrix(~0+deg.group.pcadb)
      colnames(design) <- levels(deg.group.pcadb)
      
      contrast.matrix <- makeContrasts(contrasts='Case - Control',
                                       levels=design)
      contrast.matrix
      
      ### Differential gene expression analysis (limma)
      
      if (project %in% c('TCGA-PRAD','GSE54460')) {
        keep <- rowSums(expr > log2(1)) >= 0.5*ncol(expr)
      } else if (project %in% c('DKFZ')) {
        keep <- rowSums(expr > log2(0.5)) >= 0.5*ncol(expr)
      } else {
        keep <- 1:nrow(expr)
      }
      
      genes <- rownames(expr)[keep]
      
      fit <- lmFit(expr[genes,idx], design)
      fit2 <- contrasts.fit(fit, contrast.matrix)
      fit2 <- eBayes(fit2)
      
      dgeTable <- topTable(fit2, coef=1, n=Inf, adjust.method='BH', sort.by='p')
      
      dataForVolcanoPlot <- dgeTable
      
      logFcThreshold <- log2(input$foldchange)
      adjPvalThreshold <- input$fdr
      
      dataForVolcanoPlot$Significance[with(dataForVolcanoPlot,
                                           logFC < logFcThreshold | adj.P.Val > adjPvalThreshold)] <- 'NS'
      dataForVolcanoPlot$Significance[with(dataForVolcanoPlot,
                                           logFC >= logFcThreshold & adj.P.Val <= adjPvalThreshold)] <- 'UP'
      dataForVolcanoPlot$Significance[with(dataForVolcanoPlot,
                                           logFC <= -logFcThreshold & adj.P.Val <= adjPvalThreshold)] <- 'DOWN'
      
      dataForVolcanoPlot <- data.frame(dataForVolcanoPlot,
                                       Symbol = gene.annotation$gene_name[match(rownames(dataForVolcanoPlot), gene.annotation$ensembl_id)],
                                       stringsAsFactors = F)
      
      shinyjs::show('transcriptome_deg_table')
      shinyjs::show('transcriptome_volcano_plot')
      # shinyjs::show('volcano.ccma.downbttn.csv')
      # shinyjs::show('volcano.ccma.downbttn.pdf')
      # 
      #volcano <- reactiveValues()
      
      output$transcriptome_volcano_plot <- renderPlot({
        
        p <- volcanoPlotFun(dataForVolcanoPlot, logFcThreshold, adjPvalThreshold)
        
        #volcano$volcano.data <- dataForVolcanoPlot
        #volcano$volcano.plot <- p
        
        p
        
      })
      
      # output$volcano.ccma.downbttn.csv <- downloadHandler(
      #   filename = function(){paste('volcano.csv', sep = '')},
      #   
      #   content = function(file){
      #     write.csv(ccma.volcano$volcano.data, row.names = FALSE, quote = F)
      #   })
      # 
      # output$volcano.ccma.downbttn.png <- downloadHandler(
      #   filename = function(){paste('volcano.png', sep = '')},
      #   
      #   content = function(file){
      #     png(file, width = 600, height = 600)
      #     print(ccma.volcano$volcano.plot)
      #     dev.off()
      #   })
      # 
      # output$volcano.ccma.downbttn.pdf <- downloadHandler(
      #   filename = function(){paste('volcano.pdf', sep = '')},
      #   
      #   content = function(file){
      #     pdf(file, width = 6, height = 6)
      #     print(ccma.volcano$volcano.plot)
      #     dev.off()
      #   })
      
      
      output$transcriptome_deg_table <- DT::renderDataTable({
        
        #deg.table <- dataForVolcanoPlot[dataForVolcanoPlot$Significance != 'NS',]
        deg.table <- dataForVolcanoPlot
        deg.table[,1:6] <- apply(dataForVolcanoPlot[,1:6], 2,
                                 function(v) ifelse(abs(v)<=0.001, format(as.numeric(v), digits=3), round(v,3)))
        deg.table
        
      }, 
      callback=table.download.button,
      options = list(pageLength = 10, dom = 'lfrtBp', buttons = c('copy', 'csv', 'excel')), #i
      extensions = "Buttons",
      selection = list(mode='none', selected=1), ### === not selectable
      rownames = TRUE,
      server = FALSE
      )
      
    })
    
    
    ### Survival Analysis
    
    if(input$transcriptome=='SurvivalAnalysis' & ! project %in% bcr.dataset) {
      shinyalert(text=paste0('Relapse-free survival (RFS) data is not available for the selected dataset: ', project));
    }
    
    output$table_coxph <- DT::renderDataTable({
      
      if(! project %in% bcr.dataset) {
        return()
      }
      
      dt <- pcadb.coxph[[project]]#[1:500,]
      
      dt[,3:6] <- apply(dt[,3:6], 2, 
                        function(v) ifelse(v>=0.01, format(round(v,3), nsmall = 3),
                                           format(v, scientific=T, digits = 3)))
      
      dt
      
    },
    callback=table.download.button,
    options = list(pageLength = 10, dom = 'lfrtBp', buttons = c('copy', 'csv', 'excel')), #i
    extensions = "Buttons",
    selection = list(mode='none', selected=1), ### === not selectable
    rownames = FALSE,
    server = FALSE
    )
    
    output$table_km <- DT::renderDataTable({
      
      if(! project %in% bcr.dataset) {
        return()
      }
      
      dt <- pcadb.km[[project]]#[1:500,]
      
      dt[,3:6] <- apply(dt[,3:6], 2, 
                        function(v) ifelse(v>=0.01, format(round(v,3), nsmall = 3),
                                           format(v, scientific=T, digits = 3)))
      
      dt
      
    },
    callback=table.download.button,
    options = list(pageLength = 10, dom = 'lfrtBp', buttons = c('copy', 'csv', 'excel')), #i
    extensions = "Buttons",
    selection = list(mode='none', selected=1), ### === not selectable
    rownames = FALSE,
    server = FALSE
    )
    
    # observeEvent(input$table__selected, {
    #   
    #   output$plot_km <- renderPlot({
    #     
    #     
    #   })
    #   
    # })
    
    ### Model
    
    if(input$transcriptome=='PrognosticModel' & ! project %in% bcr.dataset) {
      shinyalert(text=paste0('Relapse-free survival (RFS) data is not available for the selected dataset: ', project));
    }
    
    observeEvent(input$surv_submit, {
      
      # if(! project %in% bcr.dataset) {
      #   shinyalert(text=paste0('RFS data is not available for the selected dataset: ', project));
      #   return()
      # }
      
      genes <- gsub('^\\s+|\\s+$|,$|;$', '', input$surv_gene_input)
      genes <- gsub('\\s*;\\s*', ';', genes)
      genes <- gsub('\\s*,\\s*', ',', genes)
      #genes <- gsub('^\\s+$', '', genes)
      
      genes <- strsplit(x = genes, split=',|;|\\s+')[[1]]
      
      idx <- which(!genes %in% rownames(expr))
      
      output$genes <- renderText({ 
        
        if (length(idx)==0 & length(genes)!=0) {
          txt <- 'All the input genes are detected in the training dataset !'
        } else if (length(idx)!=0 & length(genes)!=0) {
          txt <- paste0('Warning: ', length(idx), ' genes are not detected in the training dataset\n',
                        paste(genes[idx], collapse = ','))
        } else if (length(genes)==0) {
        txt <- 'Warning: No gene is detected in the training dataset\n'
        }
        
        txt
        
      })
      
      keep <- intersect(genes, rownames(expr))
      
      if(length(keep) == 0) {
        #shinyalert(text='No gene is identified in the training dataset!');
        return()
      }
      
      samples <- which(meta[,'sample_type'] %in% c('Primary','Tumor'))
      
      training.geno <- expr[keep, samples]
      training.pheno <- meta[samples,]
      
      filter <- which(is.na(training.pheno$bcr_status) | is.na(training.pheno$time_to_bcr))
      
      if (length(filter)>0) {
        training.geno <- training.geno[,-filter]
        training.pheno <- training.pheno[-filter,]
      }
      
      coeffs <- survModelFun(model = input$surv_model_method, 
                             training.geno = training.geno, 
                             training.pheno = training.pheno)
      
      coeffs$Symbol <- gene.annotation$gene_name[match(coeffs$Gene, gene.annotation$ensembl_id)]
      
      output$table_coeffs <- DT::renderDataTable({
        if(length(keep) == 0) {
          return()
        }
        coeffs
        
      },
      callback=table.download.button,
      options = list(pageLength = 10, dom = 'rtBp', buttons = c('copy', 'csv', 'excel')), #i
      extensions = "Buttons",
      selection = list(mode='none', selected=1), ### === not selectable
      rownames = FALSE,
      server = FALSE
      )
      
      
      output$training_km_plot <- renderPlot({
        
        if(length(keep) == 0) {
          return()
        }
        
        score <- as.numeric(apply(training.geno, 2, function(v) sum(v*coeffs$Coefficients)))
        
        dataForKMPlot <- data.frame(expr=score, 
                                    time.to.bcr=as.numeric(training.pheno$time_to_bcr),
                                    bcr.status=as.numeric(training.pheno$bcr_status),
                                    stringsAsFactors = F)
        
        
        p <- KMPlotFun(dataForKMPlot, score.type='risk', x.adjust=-0.05, dt=NULL)
        
        p
        
      })
      
      
      kmTableVal <- reactive({
        
        datasets <- bcr.dataset[which(!bcr.dataset %in% project)]
        
        kmTable <- c()
        for (dt in datasets) {
          
          validation.geno <- expr.data[[dt]]
          validation.pheno <- meta.data[[dt]]
          
          idx <- which(validation.pheno$sample_type=='Primary' | validation.pheno$sample_type=='Tumor')
          validation.pheno <- validation.pheno[idx,]
          
          validation.genes <-  intersect(rownames(training.geno), rownames(validation.geno))
          validation.geno <- validation.geno[validation.genes,idx]
          
          validation.coeffs <- coeffs
          rownames(validation.coeffs) <- validation.coeffs$Gene
          
          validation.coeffs <- validation.coeffs[validation.genes,]
          
          score <- as.numeric(apply(validation.geno, 2, function(v) sum(v*validation.coeffs$Coefficients)))
          time.to.bcr <- as.numeric(validation.pheno$time_to_bcr)
          bcr.status <- as.numeric(validation.pheno$bcr_status)
          
          ###
          risk.group <- score > median(score, na.rm = T)
          
          if (length(unique(risk.group))==1) {
            next
          }
          
          n <- length(risk.group)
          
          n.high <- sum(risk.group, na.rm=T)
          n.low <- sum(!risk.group, na.rm=T)
          
          sdf <- survdiff(Surv(time.to.bcr, bcr.status) ~ risk.group)
          p.val <- pchisq(sdf$chisq, length(sdf$n)-1, lower.tail = FALSE)
          #p.val = 1 - pchisq(data.survdiff$chisq, length(data.survdiff$n) - 1)
          
          hr = (sdf$obs[2]/sdf$exp[2])/(sdf$obs[1]/sdf$exp[1])
          upper95 = exp(log(hr) + qnorm(0.975)*sqrt(1/sdf$exp[2]+1/sdf$exp[1]))
          lower95 = exp(log(hr) - qnorm(0.975)*sqrt(1/sdf$exp[2]+1/sdf$exp[1]))
          
          coeffs.tmp <- c(n, hr, lower95, upper95, p.val, dt)
          
          kmTable <- rbind(kmTable, coeffs.tmp)
          
          
        }
        
        kmTable <- data.frame(kmTable, stringsAsFactors = F) #row.names = NULL, 
        colnames(kmTable) <- c('N','HR','Lower95','Upper95','P.Value','Dataset')
        rownames(kmTable) <- datasets
        kmTable$HR <- as.numeric(kmTable$HR)
        kmTable$N <- as.numeric(kmTable$N)
        kmTable$Lower95 <- as.numeric(kmTable$Lower95)
        kmTable$Upper95 <- as.numeric(kmTable$Upper95)
        kmTable$P.Value <- as.numeric(kmTable$P.Value)
        
        # o <- order(kmTable$P.Value, decreasing = F)
        # kmTable <- kmTable[o,]
        # 
        kmTable
        
        #print (kmTable$HR)
        
        
      })
      
      output$validation.bcr.forest <- renderPlot({
        
        if(length(keep) == 0) {
          return()
        }
        
        dataForForestPlot <- kmTableVal()
        p <- transcriptomeKMForestplotFunT(dataForForestPlot, sort.var='P')
        p
        
      })
      
      
    })
    
    
  })
  
  }
