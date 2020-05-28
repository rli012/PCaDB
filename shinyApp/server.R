
######## Server

server <- function(input, output, session) { 
  
  updateSelectizeInput(session, 'gene.expression', choices = gene.annotation, selected = gene.default, server = TRUE)
  updateSelectizeInput(session, 'gene.survival', choices = gene.annotation, selected = gene.default, server = TRUE)
  
  gene.expression <- reactive({
    input$gene.expression
  })
  
  group.expression <- reactive({
    input$group
  })
  
  gene.survival <- reactive({
    input$gene.survival
  })
  
  survival.quantile <- reactive({
    input$quantile
  })
  
  
  group.survival <- reactive({
    input$survival_radio
  })
  
  
  output$tcgaboxplot <- renderPlot({
    
    gene.id <- gene.expression()
    gene.symbol <- gene.annotation$external_gene_name[which(gene.annotation$ensembl_id==gene.id)]
    
    grp <- group.expression()
    group <- meta.data[['TCGA-PRAD']][,grp]
    expr <- expr.data[['TCGA-PRAD']][gene.id,]
    
    dataForBoxPlot <- data.frame(expr, group)
    
    if (grp == 'preop_psa') {
      p <- corrplotFun(dataForBoxPlot)
    } else {
      p <- boxplotFun(dataForBoxPlot)
    }
    
    p
  })
  
  
  output$gse21034boxplot <- renderPlot({
    
    gene.id <- gene.expression()
    gene.symbol <- gene.annotation$external_gene_name[which(gene.annotation$ensembl_id==gene.id)]
    
    grp <- group.expression()
    
    expr <- expr.data[['GSE21034']][gene.id,]
    group <- meta.data[['GSE21034']][,grp]
    
    dataForBoxPlot <- data.frame(expr, group)
    
    if (grp == 'preop_psa') {
      p <- corrplotFun(dataForBoxPlot)
    } else {
      p <- boxplotFun(dataForBoxPlot)
    }
    
    p
  })
  
  
  output$gse70768boxplot <- renderPlot({
    
    gene.id <- gene.expression()
    gene.symbol <- gene.annotation$external_gene_name[which(gene.annotation$ensembl_id==gene.id)]
    
    grp <- group.expression()
    
    expr <- expr.data[['GSE70768']][gene.id,]
    group <- meta.data[['GSE70768']][,grp]
    
    dataForBoxPlot <- data.frame(expr, group)
    
    if (grp == 'preop_psa') {
      p <- corrplotFun(dataForBoxPlot)
    } else {
      p <- boxplotFun(dataForBoxPlot)
    }
    
    p
  })
  
  
  output$gse70769boxplot <- renderPlot({
    
    gene.id <- gene.expression()
    gene.symbol <- gene.annotation$external_gene_name[which(gene.annotation$ensembl_id==gene.id)]
    
    grp <- group.expression()
    
    expr <- expr.data[['GSE70769']][gene.id,]
    group <- meta.data[['GSE70769']][,grp]
    
    dataForBoxPlot <- data.frame(expr, group)
    
    if (grp == 'preop_psa') {
      p <- corrplotFun(dataForBoxPlot)
    } else {
      p <- boxplotFun(dataForBoxPlot)
    }
    
    p
  })
  
  
  output$gse94767boxplot <- renderPlot({
    
    gene.id <- gene.expression()
    gene.symbol <- gene.annotation$external_gene_name[which(gene.annotation$ensembl_id==gene.id)]
    
    grp <- group.expression()
    
    expr <- expr.data[['GSE94767']][gene.id,]
    group <- meta.data[['GSE94767']][,grp]
    
    dataForBoxPlot <- data.frame(expr, group)
    
    if (grp == 'preop_psa') {
      p <- corrplotFun(dataForBoxPlot)
    } else {
      p <- boxplotFun(dataForBoxPlot)
    }
    
    p
  })
  
  
  
  
  
  output$gse94767boxplot <- renderPlot({
    
    gene.id <- gene.expression()
    gene.symbol <- gene.annotation$external_gene_name[which(gene.annotation$ensembl_id==gene.id)]
    
    grp <- group.expression()
    
    expr <- expr.data[['GSE94767']][gene.id,]
    group <- meta.data[['GSE94767']][,grp]
    
    dataForBoxPlot <- data.frame(expr, group)
    
    if (grp == 'preop_psa') {
      p <- corrplotFun(dataForBoxPlot)
    } else {
      p <- boxplotFun(dataForBoxPlot)
    }
    
    p
  })
  
  
  output$gse107299boxplot <- renderPlot({
    
    gene.id <- gene.expression()
    gene.symbol <- gene.annotation$external_gene_name[which(gene.annotation$ensembl_id==gene.id)]
    
    grp <- group.expression()
    
    expr <- expr.data[['GSE107299']][gene.id,]
    group <- meta.data[['GSE107299']][,grp]
    
    dataForBoxPlot <- data.frame(expr, group)
    
    if (grp == 'preop_psa') {
      p <- corrplotFun(dataForBoxPlot)
    } else {
      p <- boxplotFun(dataForBoxPlot)
    }
    
    p
  })
  
  output$dkfz2018boxplot <- renderPlot({
    
    gene.id <- gene.expression()
    gene.symbol <- gene.annotation$external_gene_name[which(gene.annotation$ensembl_id==gene.id)]
    
    grp <- group.expression()
    
    expr <- expr.data[['DKFZ2018']][gene.id,]
    group <- meta.data[['DKFZ2018']][,grp]
    
    dataForBoxPlot <- data.frame(expr, group)
    
    if (grp == 'preop_psa') {
      p <- corrplotFun(dataForBoxPlot)
    } else {
      p <- boxplotFun(dataForBoxPlot)
    }
    
    p
  })
  
  output$gse54460boxplot <- renderPlot({
    
    gene.id <- gene.expression()
    gene.symbol <- gene.annotation$external_gene_name[which(gene.annotation$ensembl_id==gene.id)]
    
    grp <- group.expression()
    
    expr <- expr.data[['GSE54460']][gene.id,]
    group <- meta.data[['GSE54460']][,grp]
    
    dataForBoxPlot <- data.frame(expr, group)
    
    if (grp == 'preop_psa') {
      p <- corrplotFun(dataForBoxPlot)
    } else {
      p <- boxplotFun(dataForBoxPlot)
    }
    
    p
  })
  
  
  output$emtab6128boxplot <- renderPlot({
    
    gene.id <- gene.expression()
    gene.symbol <- gene.annotation$external_gene_name[which(gene.annotation$ensembl_id==gene.id)]
    
    grp <- group.expression()
    
    expr <- expr.data[['E-MTAB-6128']][gene.id,]
    group <- meta.data[['E-MTAB-6128']][,grp]
    
    dataForBoxPlot <- data.frame(expr, group)
    
    if (grp == 'preop_psa') {
      p <- corrplotFun(dataForBoxPlot)
    } else {
      p <- boxplotFun(dataForBoxPlot)
    }
    
    p
  })
  
  
  output$gse116918boxplot <- renderPlot({
    
    gene.id <- gene.expression()
    gene.symbol <- gene.annotation$external_gene_name[which(gene.annotation$ensembl_id==gene.id)]
    
    grp <- group.expression()
    
    expr <- expr.data[['GSE116918']][gene.id,]
    group <- meta.data[['GSE116918']][,grp]
    
    dataForBoxPlot <- data.frame(expr, group)
    
    if (grp == 'preop_psa') {
      p <- corrplotFun(dataForBoxPlot)
    } else {
      p <- boxplotFun(dataForBoxPlot)
    }
    
    p
  })
  
  output$gse25136boxplot <- renderPlot({
    
    gene.id <- gene.expression()
    gene.symbol <- gene.annotation$external_gene_name[which(gene.annotation$ensembl_id==gene.id)]
    
    grp <- group.expression()
    
    expr <- expr.data[['GSE25136']][gene.id,]
    group <- meta.data[['GSE25136']][,grp]
    
    dataForBoxPlot <- data.frame(expr, group)
    
    if (grp == 'preop_psa') {
      p <- corrplotFun(dataForBoxPlot)
    } else {
      p <- boxplotFun(dataForBoxPlot)
    }
    
    p
  })
  
  output$gse41408boxplot <- renderPlot({
    
    gene.id <- gene.expression()
    gene.symbol <- gene.annotation$external_gene_name[which(gene.annotation$ensembl_id==gene.id)]
    
    grp <- group.expression()
    
    expr <- expr.data[['GSE41408']][gene.id,]
    group <- meta.data[['GSE41408']][,grp]
    
    dataForBoxPlot <- data.frame(expr, group)
    
    if (grp == 'preop_psa') {
      p <- corrplotFun(dataForBoxPlot)
    } else {
      p <- boxplotFun(dataForBoxPlot)
    }
    
    p
  })
  
  output$gse46691boxplot <- renderPlot({
    
    gene.id <- gene.expression()
    gene.symbol <- gene.annotation$external_gene_name[which(gene.annotation$ensembl_id==gene.id)]
    
    grp <- group.expression()
    
    expr <- expr.data[['GSE46691']][gene.id,]
    group <- meta.data[['GSE46691']][,grp]
    
    dataForBoxPlot <- data.frame(expr, group)
    
    if (grp == 'preop_psa') {
      p <- corrplotFun(dataForBoxPlot)
    } else {
      p <- boxplotFun(dataForBoxPlot)
    }
    
    p
  })
  
  
  output$gse51066boxplot <- renderPlot({
    
    gene.id <- gene.expression()
    gene.symbol <- gene.annotation$external_gene_name[which(gene.annotation$ensembl_id==gene.id)]
    
    grp <- group.expression()
    
    expr <- expr.data[['GSE51066']][gene.id,]
    group <- meta.data[['GSE51066']][,grp]
    
    dataForBoxPlot <- data.frame(expr, group)
    
    if (grp == 'preop_psa') {
      p <- corrplotFun(dataForBoxPlot)
    } else {
      p <- boxplotFun(dataForBoxPlot)
    }
    
    p
  })
  
  output$gse37199boxplot <- renderPlot({
    
    gene.id <- gene.expression()
    gene.symbol <- gene.annotation$external_gene_name[which(gene.annotation$ensembl_id==gene.id)]
    
    grp <- group.expression()
    
    expr <- expr.data[['GSE37199']][gene.id,]
    group <- meta.data[['GSE37199']][,grp]
    
    dataForBoxPlot <- data.frame(expr, group)
    
    if (grp == 'preop_psa') {
      p <- corrplotFun(dataForBoxPlot)
    } else {
      p <- boxplotFun(dataForBoxPlot)
    }
    
    p
  })
  
  
  output$dataset <- DT::renderDataTable({dataset},
                                        options = list(pageLength = 5),
                                        selection = list(mode='single', selected=1)
  )
  
  output$dataset_summary <- renderText({ 
    idx <- input$dataset_rows_selected
    dataset_summary <- as.character(paste0(dataset[idx,'GEO Accession'], ': ', dataset[idx,'Title']))
    dataset_summary
  })
  
  
  output$pie_sample_type <- renderPlot({
    
    idx <- input$dataset_rows_selected
    accession <- as.character(dataset[idx,'GEO Accession'])
    
    sample.freq <- table(meta.data[[accession]]$sample_type)
    
    dataForPiePlot <- data.frame(num=as.numeric(sample.freq), sam=names(sample.freq))
    
    p <- pieplotFun(dataForPiePlot)
    
    p
    
  }, height = 300)
  
  #observeEvent({
  output$pie_pstage <- renderPlot({
    
    req(input$dataset_rows_selected)
    
    idx <- input$dataset_rows_selected
    accession <- as.character(dataset[idx,'GEO Accession'])
    
    sample.freq <- table(meta.data[[accession]]$pathological_t_stage)
    
    #dataForPiePlot <- data.frame(num=as.numeric(sample.freq), sam=names(sample.freq))
    
    #p <- pieplotFun(dataForPiePlot)
    #p
    
    dataForBarPlot <- data.frame(num=as.numeric(sample.freq),
                                 group=names(sample.freq))
    
    p <- barplotFun(dataForBarPlot)
    p
    
  }, height = 300)
  
  #})
  
  output$pie_cstage <- renderPlot({
    
    idx <- input$dataset_rows_selected
    accession <- as.character(dataset[idx,'GEO Accession'])
    
    sample.freq <- table(meta.data[[accession]]$clinical_t_stage)
    
    #dataForPiePlot <- data.frame(num=as.numeric(sample.freq), sam=names(sample.freq))
    
    #p <- pieplotFun(dataForPiePlot)
    #p
    
    dataForBarPlot <- data.frame(num=as.numeric(sample.freq),
                                 group=names(sample.freq))
    
    p <- barplotFun(dataForBarPlot)
    p
    
    
  }, height = 300)
  
  
  output$bar_psa <- renderPlot({
    
    idx <- input$dataset_rows_selected
    accession <- as.character(dataset[idx,'GEO Accession'])
    
    dataForHistogram <- meta.data[[accession]]
    
    p <- histogramFun(dataForHistogram)
    p
    
  }, height = 300)
  
  
  
  output$bar_gleason <- renderPlot({
    
    idx <- input$dataset_rows_selected
    accession <- as.character(dataset[idx,'GEO Accession'])
    
    gleason.freq <- table(meta.data[[accession]]$gleason_group)
    
    dataForBarPlot <- data.frame(num=as.numeric(gleason.freq),
                                 group=paste0('Gleason ', names(gleason.freq)))
    
    p <- barplotFun(dataForBarPlot)
    p
    
  }, height = 300)
  
  
  output$pie_os_status <- renderPlot({
    
    idx <- input$dataset_rows_selected
    accession <- as.character(dataset[idx,'GEO Accession'])
    
    sample.freq <- table(meta.data[[accession]]$os_status)
    
    dataForPiePlot <- data.frame(num=as.numeric(sample.freq), sam=names(sample.freq))
    
    p <- pieplotFun(dataForPiePlot)
    p
    
  }, height = 300)
  
  
  
  output$km_os_time <- renderPlot({
    
    idx <- input$dataset_rows_selected
    accession <- as.character(dataset[idx,'GEO Accession'])
    
    daysToDeath <- meta.data[[accession]]$time_to_death
    vitalStatus <- meta.data[[accession]]$os_status
    
    dataForKMPlot <- data.frame(daysToDeath, vitalStatus)
    
    fit <- survfit(Surv(daysToDeath, vitalStatus) ~ 1, data=dataForKMPlot)
    
    p <- ggsurvplot(fit, data=dataForKMPlot, #pval = paste(label1, '\n', label2), pval.coord = c(xpos, ypos1),
                    #pval.size=4,
                    font.main = c(16, 'bold', 'black'), conf.int = FALSE, 
                    #title = project,
                    legend = 'none', 
                    #color = c('blue', 'green'),
                    palette= c(google.blue, google.red),
                    #legend.labs = c(paste('Low Expr (N=',nL,')',sep=''), 
                    #                paste('High Expr  (N=',nH,')',sep='')),  
                    #legend.title='group',
                    xlab = 'Overall Survival (months)', ylab = 'Survival Probability',
                    #xlab = paste(type,'(months)'), ylab = 'Survival Probability',
                    font.x = c(16), font.y = c(16), ylim=c(0,1), #16
                    ggtheme = theme_bw()+ theme(axis.line = element_line(colour = "black"),
                                                panel.grid.major = element_blank(),
                                                panel.grid.minor = element_blank(),
                                                #panel.border = element_rect(colour='black'),
                                                panel.border = element_blank(),
                                                panel.background = element_blank(),
                                                legend.text = element_text(size=12),#14
                                                legend.title = element_blank(),
                                                legend.position = 'none',
                                                axis.text = element_text(size=14, color='black'))) #+
    
    p
    
  }, height = 300)
  
  
  
  
  output$pie_bcr_status <- renderPlot({
    
    idx <- input$dataset_rows_selected
    accession <- as.character(dataset[idx,'GEO Accession'])
    
    sample.freq <- table(meta.data[[accession]]$bcr_status)
    
    dataForPiePlot <- data.frame(num=as.numeric(sample.freq), sam=names(sample.freq))
    
    p <- pieplotFun(dataForPiePlot)
    p
    
  }, height = 300)
  
  
  output$km_bcr_time <- renderPlot({
    
    idx <- input$dataset_rows_selected
    accession <- as.character(dataset[idx,'GEO Accession'])
    
    daysToDeath <- meta.data[[accession]]$time_to_bcr
    vitalStatus <- meta.data[[accession]]$bcr_status
    
    dataForKMPlot <- data.frame(daysToDeath, vitalStatus)
    
    fit <- survfit(Surv(daysToDeath, vitalStatus) ~ 1, data=dataForKMPlot)
    
    p <- ggsurvplot(fit, data=dataForKMPlot, #pval = paste(label1, '\n', label2), pval.coord = c(xpos, ypos1),
                    #pval.size=4,
                    font.main = c(16, 'bold', 'black'), conf.int = FALSE, 
                    #title = project,
                    legend = 'none', 
                    #color = c('blue', 'green'),
                    palette= c(google.blue, google.red),
                    #legend.labs = c(paste('Low Expr (N=',nL,')',sep=''), 
                    #                paste('High Expr  (N=',nH,')',sep='')),  
                    #legend.title='group',
                    xlab = 'Relapse-free Survival (months)', ylab = 'Survival Probability',
                    #xlab = paste(type,'(months)'), ylab = 'Survival Probability',
                    font.x = c(16), font.y = c(16), ylim=c(0,1), #16
                    ggtheme = theme_bw()+ theme(axis.line = element_line(colour = "black"),
                                                panel.grid.major = element_blank(),
                                                panel.grid.minor = element_blank(),
                                                #panel.border = element_rect(colour='black'),
                                                panel.border = element_blank(),
                                                panel.background = element_blank(),
                                                legend.text = element_text(size=12),#14
                                                legend.title = element_blank(),
                                                legend.position = 'none',
                                                axis.text = element_text(size=14, color='black'))) #+
    
    p
    
  }, height = 300)
  
  
  output$pie_metastasis_status <- renderPlot({
    
    idx <- input$dataset_rows_selected
    accession <- as.character(dataset[idx,'GEO Accession'])
    
    sample.freq <- table(meta.data[[accession]]$metastasis_status)
    
    dataForPiePlot <- data.frame(num=as.numeric(sample.freq), sam=names(sample.freq))
    
    p <- pieplotFun(dataForPiePlot)
    p
    
  }, height = 300)
  
  
  output$km_metastasis_time <- renderPlot({
    
    idx <- input$dataset_rows_selected
    accession <- as.character(dataset[idx,'GEO Accession'])
    
    daysToDeath <- meta.data[[accession]]$time_to_metastasis
    vitalStatus <- meta.data[[accession]]$metastasis_status
    
    dataForKMPlot <- data.frame(daysToDeath, vitalStatus)
    
    fit <- survfit(Surv(daysToDeath, vitalStatus) ~ 1, data=dataForKMPlot)
    
    p <- ggsurvplot(fit, data=dataForKMPlot, #pval = paste(label1, '\n', label2), pval.coord = c(xpos, ypos1),
                    #pval.size=4,
                    font.main = c(16, 'bold', 'black'), conf.int = FALSE, 
                    #title = project,
                    legend = 'none', 
                    #color = c('blue', 'green'),
                    palette= c(google.blue, google.red),
                    #legend.labs = c(paste('Low Expr (N=',nL,')',sep=''), 
                    #                paste('High Expr  (N=',nH,')',sep='')),  
                    #legend.title='group',
                    xlab = 'Metastasis-free Survival (months)', ylab = 'Survival Probability',
                    #xlab = paste(type,'(months)'), ylab = 'Survival Probability',
                    font.x = c(16), font.y = c(16), ylim=c(0,1), #16
                    ggtheme = theme_bw()+ theme(axis.line = element_line(colour = "black"),
                                                panel.grid.major = element_blank(),
                                                panel.grid.minor = element_blank(),
                                                #panel.border = element_rect(colour='black'),
                                                panel.border = element_blank(),
                                                panel.background = element_blank(),
                                                legend.text = element_text(size=12),#14
                                                legend.title = element_blank(),
                                                legend.position = 'none',
                                                axis.text = element_text(size=14, color='black'))) #+
    
    p
    
  }, height = 300)
  
  
  
  
  output$kmtcga <- renderPlot({
    
    gene.id <- gene.survival()
    group <- group.survival()
    quan <- survival.quantile()
    
    p <- kmplotFun(meta.data[['TCGA-PRAD']], expr.data[['TCGA-PRAD']], group, gene.id, quan)
    p
    
  })
  
  output$kmgse107299 <- renderPlot({
    
    gene.id <- gene.survival()
    group <- group.survival()
    quan <- survival.quantile()
    
    p <- kmplotFun(meta.data[['GSE107299']], expr.data[['GSE107299']], group, gene.id, quan)
    p
    
  })
  
  
  output$kmgse21034 <- renderPlot({
    
    gene.id <- gene.survival()
    group <- group.survival()
    quan <- survival.quantile()
    
    p <- kmplotFun(meta.data[['GSE21034']], expr.data[['GSE21034']], group, gene.id, quan)
    p
    
  })
  
  
  output$kmdkfz2018 <- renderPlot({
    
    gene.id <- gene.survival()
    group <- group.survival()
    quan <- survival.quantile()
    
    p <- kmplotFun(meta.data[['DKFZ2018']], expr.data[['DKFZ2018']], group, gene.id, quan)
    p
    
  })
  
  output$kmgse54460 <- renderPlot({
    
    gene.id <- gene.survival()
    group <- group.survival()
    quan <- survival.quantile()
    
    p <- kmplotFun(meta.data[['GSE54460']], expr.data[['GSE54460']], group, gene.id, quan)
    p
    
  })
  
  output$kmgse70768 <- renderPlot({
    
    gene.id <- gene.survival()
    group <- group.survival()
    quan <- survival.quantile()
    
    p <- kmplotFun(meta.data[['GSE70768']], expr.data[['GSE70768']], group, gene.id, quan)
    p
    
  })
  
  output$kmgse70769 <- renderPlot({
    
    gene.id <- gene.survival()
    group <- group.survival()
    quan <- survival.quantile()
    
    p <- kmplotFun(meta.data[['GSE70769']], expr.data[['GSE70769']], group, gene.id, quan)
    p
    
  })
  
  output$kmgse94767 <- renderPlot({
    
    gene.id <- gene.survival()
    group <- group.survival()
    quan <- survival.quantile()
    
    p <- kmplotFun(meta.data[['GSE94767']], expr.data[['GSE94767']], group, gene.id, quan)
    p
    
  })
  
  output$kmemtab6128 <- renderPlot({
    
    gene.id <- gene.survival()
    group <- group.survival()
    quan <- survival.quantile()
    
    p <- kmplotFun(meta.data[['E-MTAB-6128']], expr.data[['E-MTAB-6128']], group, gene.id, quan)
    p
    
  })
  
  output$kmgse116918 <- renderPlot({
    
    gene.id <- gene.survival()
    group <- group.survival()
    quan <- survival.quantile()
    
    p <- kmplotFun(meta.data[['GSE116918']], expr.data[['GSE116918']], group, gene.id, quan)
    p
    
  })
  
  output$gse <- renderUI({
    
    idx <- input$dataset_rows_selected
    accession <- as.character(dataset[idx,'GEO Accession'])
    
    #if (accession=='TCGA-PRAD') {
    #  link <- 'https://portal.gdc.cancer.gov/projects/TCGA-PRAD'
    if (accession=='DKFZ') {
      link <- 'https://www.cbioportal.org/study/summary?id=prostate_dkfz_2018'
    } else if (accession=='E-MTAB-6128') {
      link <- 'https://www.ebi.ac.uk/arrayexpress/experiments/E-MTAB-6128/'
    } else {
      base.url <- 'https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc='
      link <- paste0(base.url, accession)
    }
    
    tags$iframe(src=link, seamless="seamless", width='100%', height='600')
  })
  
  
  
  
  ########################################################################
  
  
  output$volcano_sample_type <- renderPlot({
    
    idx <- input$dataset_rows_selected
    accession <- as.character(dataset[idx,'GEO Accession'])
    
    expr <- expr.data[[accession]]
    
    group <- meta.data[[accession]][,input$degbox]
    
    group[group %in% input$control_sample_type] <- 'Control'
    group[group %in% input$case_sample_type] <- 'Case'
    
    group <- factor(group)
    
    design <- model.matrix(~0+group)
    colnames(design) <- levels(group)
    
    contrast.matrix <- makeContrasts(contrasts='Case - Control',
                                     levels=design)
    contrast.matrix
    
    ### Differential gene expression analysis (limma)
    
    fit <- lmFit(expr, design)
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
    
    p <- ggplot(dataForVolcanoPlot, aes(x = logFC, y = -log10(adj.P.Val))) +
      #xlim(-2,2) +
      labs(x=expression('log'[2]*'(Fold Change)'), 
           y=(expression('-log'[10]*'(FDR)')), 
           title=NULL) +
      geom_point(aes(color=Significance), alpha=1, size=2) +
      geom_vline(xintercept = c(-logFcThreshold, logFcThreshold),
                 color='darkgreen', linetype='dashed') +
      geom_hline(yintercept = -log10(adjPvalThreshold), 
                 color='darkgreen',linetype='dashed')+
      #scale_x_continuous(breaks=c(-4,-2,0,2,4,6,8,10)) +
      #scale_y_continuous(expand = c(0.3, 0)) +
      #scale_color_manual(values = c('#4285F4',"gray", '#FBBC05')) +
      scale_color_manual(values = c(google.green,"gray", google.red)) +
      #facet_wrap(~Comparison, ncol = 2) +
      #geom_text_repel(data = subset(dataForVolcanoPlot, 
      #                              adj.P.Val < adjPvalThreshold & logFC > logFcThreshold), 
      #                segment.alpha = 0.4, aes(label = Symbol), 
      #                size = 3.5, color='red', segment.color = 'black') +
      #geom_text_repel(data = subset(dataForVolcanoPlot, 
      #                              adj.P.Val < adjPvalThreshold & logFC < logFcThreshold*-1), 
      #                segment.alpha = 0.4, aes(label = Symbol), 
      #                size = 3.5, color='green3', segment.color = 'black') +
      
      theme_bw() +
      theme(axis.line = element_blank(),
            #panel.grid.major = element_blank(),
            #panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
      theme(legend.position="none") +
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=16),
            strip.text = element_text(size=14, face='bold')) +
      theme(plot.margin =  margin(t = 0.25, r = 0.25, b = 0.25, l = 0.25, unit = "cm"))
    
    
    p
    
    
  })
  
  
  
  output$table_sample_type <- DT::renderDataTable({
    
    idx <- input$dataset_rows_selected
    accession <- as.character(dataset[idx,'GEO Accession'])
    
    expr <- expr.data[[accession]]
    
    group <- meta.data[[accession]][,input$degbox]
    
    group[group %in% input$control_sample_type] <- 'Control'
    group[group %in% input$case_sample_type] <- 'Case'
    
    group <- factor(group)
    
    design <- model.matrix(~0+group)
    colnames(design) <- levels(group)
    
    contrast.matrix <- makeContrasts(contrasts='Case - Control',
                                     levels=design)
    contrast.matrix
    
    ### Differential gene expression analysis (limma)
    
    fit <- lmFit(expr, design)
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
    
    dataForVolcanoPlot[,-ncol(dataForVolcanoPlot)] <- apply(dataForVolcanoPlot[,-ncol(dataForVolcanoPlot)], 2, 
                                                            function(v) format(as.numeric(v), digits=3))
    dataForVolcanoPlot
    
    
  })
  
  
}
