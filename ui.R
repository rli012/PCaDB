
.libPaths(c(.libPaths(), '/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.6/'))

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyalert)
#library(readxl)
#library(ggplot2)
#library(gridExtra)
#library(Matrix)
#library(stringr)
#library(Biobase)
#library(survival)
#library(survminer)
#library(limma)
#library(edgeR)
#library(reshape2)
#library(htmlwidgets) # JS
library(shinyWidgets)
library(DT) # formatStyle
library(plotly)
#library(ggrepel)
#library(dplyr)
#library(digest)
#library(glmnet)

library(shinydashboardPlus)
library(shinythemes)
library(dashboardthemes)
library(shinycssloaders)
library(shinydisconnect)
library(slickR)
#library(shinybusy)
#library(waiter)

source('helper_functions.R')
#source('load_data.R') -> global.R


############################################################
######   Theme

title <- tagList(
  tags$img(src='img/logo.png', height=40) #width=150, 
)

logo_blue_gradient <- shinyDashboardLogoDIY(
  
  #boldText = h2(strong("PCaDB"), style='font-family: Georgia; color:white'), # Georgia, fantacy, cursive
  boldText = title,
  mainText = "",
  textSize = 36,
  badgeText = "",
  badgeTextColor = "black",
  badgeTextSize = 4, #max 4
  badgeBackColor = "rgb(23,103,124)", #"#40E0D0", # 
  badgeBorderRadius = 10
  
)


### creating custom theme object
theme_blue_gradient <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(248,248,248)"
  
  ### header
  ,logoBackColor = "rgb(23,103,124)"
  
  ,headerButtonBackColor = "rgb(238,238,238)"
  ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(238,238,238)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(20,97,117)"
    ,colorMiddle = "rgb(56,161,187)"
    ,colorEnd = "rgb(3,22,56)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 16
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(24,188,156)"
  #,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxPrimaryColor = "rgb(44,62,80)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)


############################################################
######   UI

gene.default <- 'ENSG00000142515' # KLK3

#h4(strong('Search a gene')
search.gene.label <- h4(strong(icon("search"), "Search a gene"), align='center', style='font-family:Georgia;color:#2C3E50')
#tags$div(HTML('<h4><b><i class="fa fa-search" style = "color:black;"></i> Search a gene</b></h4>'))
gene.id <- selectizeInput(inputId = "gene.id", label=search.gene.label, #list(h4('Search a miRNA:'), icon('search', 'fa-1.5x')),# h4(strong('miRNA'))
                          choices = NULL, selected = gene.default,
                          multiple = FALSE, width = 300,
                          options = list(placeholder = 'e.g. KLK3',
                                         server = TRUE, selectOnTab=TRUE,
                                         searchField = c('gene_name', 'alias_symbol', 'description', 'ensembl_id', 'entrez_id'),
                                         labelField = "gene_name",
                                         valueField = "ensembl_id",
                                         maxOptions = 30,
                                         render = I("{option: function(item, escape) {
                                                    var gene = '<div>' + '<strong>' + escape(item.gene_name) + '</strong>:' + '<ul>';
                                                    gene = gene + '<li>' + item.alias_symbol + '</li>';
                                                    gene = gene + '<li>' + item.description + '</li>';
                                                    gene = gene + '<li>' + 'Entrez: ' + item.entrez_id + '</li>';
                                                    gene = gene + '<li>' + 'Ensembl: ' + item.ensembl_id + '</li>' + '</ul>' + '</div>';
                                                    return gene
                                                    }
                                                    }")
                          ))


left_menu <- tagList(
  tags$img(src='img/ucr1.jpg', width=50),
  h4(strong("Welcome to PCaDB, a database of transcriptomes from prostate cancer population cohorts"), align='left', style='font-family:Georgia;color:#2C3E50')#,
  #tags$img(src='ucr2.jpg', width=60)
)

header=dashboardHeaderPlus(title = logo_blue_gradient, disable = FALSE,
                           left_menu = left_menu)

sidebar=dashboardSidebar(
  
  sidebarMenu(
    #style = 'position:fixed; overflow: visible', 
    
    hr(style="border-top: 1px solid white"),
    menuItem("Home", tabName = "tab_home", icon = icon("home"), selected = T),
    menuItem("Query", tabName = "tab_query", icon = icon("search")),
    menuItem("Prognostic Signatures", tabName = "tab_signature", icon = icon("dna")),
    menuItem("Transcriptome Analysis", tabName = "tab_transcriptome", icon = icon("bar-chart")),
    menuItem("PCaDB Pipeline", tabName = "tab_pipeline", icon = icon("pencil")),
    menuItem("Download", tabName = "tab_download", icon = icon("download")),
    menuItem("Tutorial", tabName = "tab_tutorial", icon = icon("file-alt")),
    menuItem("Contact", tabName = "tab_contact", icon = icon("envelope"))
  )
)


###### Tabs

### Home
tab_home <- fluidRow(
  
  # box(
  #   title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
  #   width = 12,
  # 
  #   #h4(strong("Welcome to PCaDB, a comprehensive database of transcriptomics data from prostate cancer population cohorts"), align='center')
  #   h3(strong("Welcome to PCaDB, a database of transcriptomes from prostate cancer cohorts"), align='center', style='font-family:Georgia')
  #   
  # ),
  # 
  box(
    title = NULL, solidHeader = TRUE, collapsible = FALSE,
    width = 12, # solidHeader=TRUE can remove the top boarder
    
    #column(1),
    column(12,
           
           valueBox(value = tags$p(strong("77"), style = "font-size: 80%;"), color = 'teal', width = 3, #color = 'aqua'
                    subtitle = tags$p(strong("Datasets"), style = "font-size: 150%;"), tags$i(class = "fa fa-database", style="font-size: 60px")), #icon = icon("database fa-0.5x")
           valueBox(value = tags$p(strong("9,068"), style = "font-size: 80%;"), color = 'teal', width = 3,
                    subtitle = tags$p(strong("Samples"), style = "font-size: 150%;"), tags$i(class = "fa fa-user-circle", style="font-size: 65px")), #icon = icon("user-circle fa-0.5x")
           valueBox(value = tags$p(strong("30"), style = "font-size: 80%;"), color = 'teal', width = 3,
                    subtitle = tags$p(strong("Signatures"), style = "font-size: 150%;"), tags$i(class = "fa fa-dna", style="font-size: 60px")) #icon = icon("dna fa-0.5x")
    )
  ),
  
  box(
    title = NULL, solidHeader = TRUE, collapsible = FALSE,
    width = 12, # solidHeader=TRUE can remove the top boarder
    
    column(12,
           
    h3(strong("Introduction")),
    
    h4(strong("Prostate cancer")),
    tags$p("Prostate cancer (PCa) is the second most frequently diagnosed cancer in men worldwide, 
           which accounts for 14.1% of the newly diagnosed cancer cases in 2020 [",
           a("GLOBOCAN", 
             href = "https://doi.org/10.3322/caac.21660",target='_blank'), ']. 
           Localized PCa is a heterogeneous disease with highly variable clinical 
           outcomes, which presents enormous challenges in the clinical management. 
           The majority of patients with indolent PCa only require 
           active surveillance, whereas patients with aggressive PCa require immediate local treatment. 
           Comprehensive collection of transcriptomics data from large population cohorts 
           is very critical for understanding the molecular biology of cancer, 
           drug target identification and evaluation, and 
           biomarker discovery for cancer diagnosis and prognosis, etc.', 
           style = "font-size: 120%;"),
    
    #tags$img(src='The-scheme-of-different-stages-and-progression-of-prostate-cancer.png', width=600),
    
    #br(),
    tags$hr(style="border-top: 1px dashed #A9A9A9"),
    h4(strong("About PCaDB")),
    tags$p('PCaDB is a comprehensive and interactive database for transcriptomes from prostate cancer population cohorts. 
           We collected', strong('77 transcriptomics datasets'), 'with', strong('9,068 patient samples'), 'and', strong('a single-cell RNA-sequencing (scRNAseq) 
           dataset'), 'with', strong('~ 30,000 cells'), 'for normal human prostates from public data repositories. We also included a comprehensive collection 
           of', strong('30 published gene expression-based prognostic signatures'), 'in PCaDB.
           A standard bioinformatics pipeline is developed to download and process the expression data and sample metadata of the transcriptomics datasets.', 
           style = "font-size: 120%;"),
    
    tags$p('PCaDB provides a user-friendly interface and a suite of data analysis and visualization modules for the comprehensive analysis of', strong('individual 
           genes, prognostic signatures,'), 'and the', strong('whole transcriptomes'), 'to elucidate the molecular 
           heterogeneity in PCa, understand the mechanisms of tumor initiation and progression, 
           as well as develop and validate prognostic signatures in large independent cohorts.', 
           style = "font-size: 120%;"),
    
    br(),
    
    # column(12, 
    #        tags$hr(style="border-top: 1px dashed #A9A9A9")
    #        ),
    
    # actionLink("link_to_tab_query", tags$img(src='analysis.png', height=500)),
    
    #tags$img(src='img/analysis.png', height=580),
    
    #column(1),
    withSpinner(slickROutput("slick_output", width = '100%'), type=8),
    
    tags$hr(style="border-top: 1px dashed #A9A9A9"),
    
    h5(strong("Citation")),
    tags$p('Please cite the following publication:
           Li, R., Zhu, J., Zhong, W.-D., and Jia, Z. (2021) PCaDB - a comprehensive and interactive database for transcriptomes from prostate cancer population cohorts. bioRxiv, 10.1101/2021.06.29.449134.', 
           style = "font-size: 120%;"),
    tags$p(HTML("<a href='https://doi.org/10.1101/2021.06.29.449134' target='_blank'><h5>https://doi.org/10.1101/2021.06.29.449134</h5></a>")),
    br(),
    shinyjs::hidden(tags$p(HTML('<script type="text/javascript" src="//rf.revolvermaps.com/0/0/3.js?i=5dfs4o8b9k7&amp;b=5&amp;s=0&amp;m=2&amp;cl=ffffff&amp;co=010020&amp;cd=aa0000&amp;v0=60&amp;v1=60&amp;r=1" async="async"></script>'), align = 'left')),
    
    tags$hr(style="border-top: 2px solid #A9A9A9"),
    
    tags$p(HTML("<script type='text/javascript' id='clustrmaps' src='//cdn.clustrmaps.com/map_v2.js?cl=ffffff&w=251&t=tt&d=m1OK5wpD2xEM4YTLneQr-HXIvLS_D8Vs34uIkQuX-8w'></script>"), align = 'left')
    )
  )
)


### Query

tab_query <- div(id='query', fluidRow(
  
  # box(
  #     title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
  #     width = 12,
  
  box(
    title = NULL, solidHeader = FALSE, collapsible = FALSE, status = 'primary',
    width = 12, # solidHeader=TRUE can remove the top boarder
    
    column(2),
    column(4, #br(),
           tags$div(id='div_mir',
                    class='mir',
                    gene.id)
    ),
    column(6,
           #strong(uiOutput("gene.symbol")),
           h4(strong(textOutput("gene.symbol")), style='color:orange; font-family:Georgia'), # #595959
           h5(strong(textOutput("gene.alias")), style='color:#737373'),
           h5(strong(textOutput("gene.ensembl.id")), style='color:#737373'),
           h5(strong(textOutput("gene.entrez.id")), style='color:#737373'),
           h5(strong(textOutput("gene.description")), style='color:#737373'),
           h5(strong(uiOutput("gene.external")), style='color:#737373')
    )
    
  ),
  
  box(
    title = NULL, solidHeader = FALSE, collapsible = FALSE, #status = "success", 
    width = 12,
    
    #navlistPanel
    tabsetPanel(id = 'tabset_query', type='pills', #widths = c(2,10),
                
                
                tabPanel(strong("Gene Expression"),
                         br(),
                         hr(),
                         
                         column(12,
                                column(12,
                                       h5("Gene Expression in Different Sample Types", align = 'center'),
                                       #h6("(Wilcoxon rank-sum test, ***: P < 0.001; **: P < 0.01; *: P < 0.05; ns: P > 0.05)", align = 'center'),
                                       tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                       expr.dataset.id,
                                       br(),
                                       withSpinner(uiOutput('query_boxplot_ui'), type = 4, proxy.height=300)
                                       
                                ),
                                
                                column(1),
                                column(11,
                                       # downloadBttn(
                                       #   outputId = "query.expression.downbttn.csv",
                                       #   label = 'CSV',
                                       #   style = "jelly",
                                       #   color = "success", 
                                       #   size = 'xs'
                                       # ),
                                       # downloadBttn(
                                       #   outputId = "query.expression.downbttn.pdf",
                                       #   label = 'PDF',
                                       #   style = "jelly",
                                       #   color = "success", 
                                       #   size = 'xs'
                                       # )
                                       uiOutput("query_expression_downbttn_csv", inline = TRUE),
                                       uiOutput("query_expression_downbttn_pdf", inline = TRUE)
                                )
                                
                         )
                ),
                
                tabPanel(strong("Survival Analysis"),
                         br(),
                         hr(),
                         column(12,
                                h5("Kaplan Meier Survival Analysis (Forest Plot)", align = 'center'),
                                h6("(Low- and high-expression groups were separated by median values)", align = 'center'),
                                #tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                column(1),
                                column(11,
                                       #expr.dataset.id,
                                       withSpinner(plotOutput('survival_forest',width = 800, height = '100%'), type=4, proxy.height=300)
                                ),
                                
                                column(2),
                                column(10,
                                # downloadBttn(
                                #   outputId = "query.survival.forest.downbttn.csv",
                                #   label = 'CSV',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                #   ),
                                # downloadBttn(
                                #   outputId = "query.survival.forest.downbttn.pdf",
                                #   label = 'PDF',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                #   )
                                uiOutput("query_survival_forest_downbttn_csv", inline = TRUE),
                                uiOutput("query_survival_forest_downbttn_pdf", inline = TRUE)
                                )
                         ),
                         column(12,
                                br(),
                                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                h5("Kaplan Meier Survival Curve", align = 'center'),
                                h6("(Low- and high-expression groups were separated by median values)", align = 'center'),
                                column(1),
                                column(11, 
                                       withSpinner(plotOutput('survival_km',height = '100%'), type=4, proxy.height=300) # width = 1100, height = 1360)
                                ),
                                
                                column(2),
                                column(10,
                                       # downloadBttn(
                                       #   outputId = "query.survival.km.downbttn.csv",
                                       #   label = 'CSV',
                                       #   style = "jelly",
                                       #   color = "success", 
                                       #   size = 'xs'
                                       # ),
                                       # downloadBttn(
                                       #   outputId = "query.survival.km.downbttn.pdf",
                                       #   label = 'PDF',
                                       #   style = "jelly",
                                       #   color = "success", 
                                       #   size = 'xs'
                                       # )
                                       uiOutput("query_survival_km_downbttn_csv", inline = TRUE),
                                       uiOutput("query_survival_km_downbttn_pdf", inline = TRUE)
                                )
                                
                         )
                         
                ),
                
                tabPanel(strong("Single-Cell RNAseq"), value = 'scRNAseq',
                         br(),
                         hr(),
                         
                         #h5("Expression of the gene of interest in normal prostate cells", align = 'center'),
                         
                         column(12,
                                column(6,
                                       h5(strong('tSNE Plot'), align='center')
                                ),
                                column(6,
                                       h5(strong('UMAP Plot'), align='center')
                                )
                         ),
                         
                         
                         column(12,
                                br(),
                                column(5,
                                       
                                       withSpinner(plotOutput('henry_sc_tsne_cell',width = 450, height = 400),
                                                   type = 4, proxy.height=300)
                                       # br(),
                                       # column(7),
                                       # column(5,
                                       #        downloadButton(outputId='transcriptome.box.downbttn.csv', label = "CSV"),
                                       #        #downloadButton(outputId='transcriptome.box.downbttn.png', label = "PNG"),
                                       #        downloadButton(outputId='transcriptome.box.downbttn.pdf', label = "PDF")
                                       # )
                                       
                                ),
                                
                                column(1),
                                column(5,
                                       withSpinner(plotOutput('henry_sc_umap_cell',width = 450, height = 400),
                                                   type = 4, proxy.height=300)
                                       # br(),
                                       # column(7),
                                       # column(5,
                                       #        downloadButton(outputId='transcriptome.box.downbttn.csv', label = "CSV"),
                                       #        #downloadButton(outputId='transcriptome.box.downbttn.png', label = "PNG"),
                                       #        downloadButton(outputId='transcriptome.box.downbttn.pdf', label = "PDF")
                                       # )
                                       
                                )
                         ),
                         
                         column(1),
                         column(5,
                                # downloadBttn(
                                #   outputId = "query.sc.tsne.downbttn.csv",
                                #   label = 'CSV',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                # ),
                                # downloadBttn(
                                #   outputId = "query.sc.tsne.downbttn.pdf",
                                #   label = 'PDF',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                # )
                                
                                uiOutput("query_sc_tsne_downbttn_csv", inline = TRUE),
                                uiOutput("query_sc_tsne_downbttn_pdf", inline = TRUE)
                         ),
                         
                         column(1),
                         column(5,
                                # downloadBttn(
                                #   outputId = "query.sc.umap.downbttn.csv",
                                #   label = 'CSV',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                # ),
                                # downloadBttn(
                                #   outputId = "query.sc.umap.downbttn.pdf",
                                #   label = 'PDF',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                # )
                                
                                uiOutput("query_sc_umap_downbttn_csv", inline = TRUE),
                                uiOutput("query_sc_umap_downbttn_pdf", inline = TRUE)
                         ),
                         
                         br(),
                         br(),
                         
                         column(12,
                                br(),
                                br(),
                                column(5,
                                       plotOutput('henry_sc_tsne_expr',width = 450, height = 400)#,
                                       # br(),
                                       # column(7),
                                       # column(5,
                                       #        downloadButton(outputId='transcriptome.box.downbttn.csv', label = "CSV"),
                                       #        #downloadButton(outputId='transcriptome.box.downbttn.png', label = "PNG"),
                                       #        downloadButton(outputId='transcriptome.box.downbttn.pdf', label = "PDF")
                                       # )
                                       
                                       
                                ),
                                
                                column(1),
                                
                                column(5,
                                       plotOutput('henry_sc_umap_expr',width = 450, height = 400)#,
                                       # br(),
                                       # column(7),
                                       # column(5,
                                       #        downloadButton(outputId='transcriptome.box.downbttn.csv', label = "CSV"),
                                       #        #downloadButton(outputId='transcriptome.box.downbttn.png', label = "PNG"),
                                       #        downloadButton(outputId='transcriptome.box.downbttn.pdf', label = "PDF")
                                       # )
                                       
                                )
                                
                         ),
                         
                         column(1),
                         column(5,
                                # downloadBttn(
                                #   outputId = "query.sc.tsne.expr.downbttn.csv",
                                #   label = 'CSV',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                # ),
                                # downloadBttn(
                                #   outputId = "query.sc.tsne.expr.downbttn.pdf",
                                #   label = 'PDF',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                # )
                                
                                uiOutput("query_sc_tsne_expr_downbttn_csv", inline = TRUE),
                                uiOutput("query_sc_tsne_expr_downbttn_pdf", inline = TRUE)
                              
                         ),
                         
                         column(1),
                         column(5,
                                # downloadBttn(
                                #   outputId = "query.sc.umap.expr.downbttn.csv",
                                #   label = 'CSV',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                # ),
                                # downloadBttn(
                                #   outputId = "query.sc.umap.expr.downbttn.pdf",
                                #   label = 'PDF',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                # )
                                
                                uiOutput("query_sc_umap_expr_downbttn_csv", inline = TRUE),
                                uiOutput("query_sc_umap_expr_downbttn_pdf", inline = TRUE)
                         ),
                         
                         column(12,
                                br(),
                                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                plotOutput('henry_sc_bubble',width = 900, height = 350)#,
                                # br(),
                                # column(7),
                                # column(5,
                                #        downloadButton(outputId='transcriptome.box.downbttn.csv', label = "CSV"),
                                #        #downloadButton(outputId='transcriptome.box.downbttn.png', label = "PNG"),
                                #        downloadButton(outputId='transcriptome.box.downbttn.pdf', label = "PDF")
                                # )
                                
                         ),
                         
                         column(1),
                         column(11,
                                # downloadBttn(
                                #   outputId = "query.sc.bubble.downbttn.csv",
                                #   label = 'CSV',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                # ),
                                # downloadBttn(
                                #   outputId = "query.sc.bubble.downbttn.pdf",
                                #   label = 'PDF',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                # )
                                uiOutput("query_sc_bubble_downbttn_csv", inline = TRUE),
                                uiOutput("query_sc_bubble_downbttn_pdf", inline = TRUE)
                         ),
                         
                         column(12,
                                br(),
                                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                plotOutput('henry_sc_violin',width = 900, height = 400)#,
                                # br(),
                                # column(7),
                                # column(5,
                                #        downloadButton(outputId='transcriptome.box.downbttn.csv', label = "CSV"),
                                #        #downloadButton(outputId='transcriptome.box.downbttn.png', label = "PNG"),
                                #        downloadButton(outputId='transcriptome.box.downbttn.pdf', label = "PDF")
                                # )
                                
                         ),
                         
                         column(1),
                         column(11,
                                # downloadBttn(
                                #   outputId = "query.sc.violin.downbttn.csv",
                                #   label = 'CSV',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                # ),
                                # downloadBttn(
                                #   outputId = "query.sc.violin.downbttn.pdf",
                                #   label = 'PDF',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                # ),
                                uiOutput("query_sc_violin_downbttn_csv", inline = TRUE),
                                uiOutput("query_sc_violin_downbttn_pdf", inline = TRUE)
                         ),
                         
                         
                         
                )
                
                
    )
  )
  
)
)


##### Signature

tab_signature <- fluidRow(
  
  box(
    title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
    width = 12, height = 55,
    
    #h4(strong("Prognostic Signatures"), align='center', style='font-family:Georgia;color:#2C3E50')
    h4(strong(icon("dna"), HTML('&nbsp;'), "Prognostic Signatures"), align='center', style='font-family:Georgia;color:#2C3E50')
  ),
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h5(strong("Gene Expression Signatures for Prostate Cancer Prognosis")),
    tags$p("An inclusive collection of 30 published gene expression-based prognostic signatures for prostate cancer is 
           included in PCaDB. The performances of the signatures have been investigated in the study [",
           a("Comprehensive evaluation of machine learning models and gene expression signatures for prostate cancer prognosis using large population cohorts", 
             href = "https://doi.org/10.1101/2021.07.02.450975",target='_blank'), "]. Comprehensive characterization of the signatures can be further performed in PCaDB.", 
           style = "font-size: 120%;"),
    #tags$p("Comprehensive characterization of the signatures can be further performed in PCaDB.", style = "font-size: 120%;"),
    
    
    br(),
    column(6,
           tags$img(src='img/signature_table.png',width=450)),
    column(6,
           tags$img(src='img/signature_circos.png',width=450))
    
  ),
  
  box(
    title = NULL, solidHeader = FALSE, collapsible = FALSE, #status = "success",
    width = 12,
    
    #navlistPanel
    tabsetPanel(id = 'signature', type='pills', #widths = c(2,10),
                
                tabPanel(strong("Signature Genes"),
                         br(),
                         hr(),
                         
                         column(12,
                                column(4,
                                       #h5("List of genes", align = 'left'),
                                       #tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                       overview.signature
                                       #withSpinner(uiOutput('query_boxplot_ui'), type = 1)
                                )
                         ),
                         column(12,
                                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                #br(),
                                h5('List of genes in the selected prognostic signature', align='center'),
                                DT::dataTableOutput("signature.gene.list")
                         )
                         
                         
                ),
                
                tabPanel(strong("Differential Expression"), # all genes, but label the common genes
                         br(),
                         hr(),
                         
                         column(12,
                                column(4,
                                       #h5("List of genes", align = 'left'),
                                       #tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                       de.dataset
                                       #withSpinner(uiOutput('query_boxplot_ui'), type = 1)
                                )
                         ),
                         
                         
                         column(12,
                                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                h5('Volcano Plot of Differentially Expressed Genes in the Signatures', align='center'),
                                h6('(|Fold Change| > 2 & FDR < 0.01)', align='center'),
                                
                                column(3),
                                column(6, withSpinner(plotOutput("signature.de.volcano", height = 400), type=4, proxy.height=300)),
                                column(3)
                         ),
                         
                         column(4),
                         column(8,
                                # downloadBttn(
                                #   outputId = "signature.volcano.downbttn.csv",
                                #   label = 'CSV',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                # ),
                                # downloadBttn(
                                #   outputId = "signature.volcano.downbttn.pdf",
                                #   label = 'PDF',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                # )
                                
                                uiOutput("signature_volcano_downbttn_csv", inline = TRUE),
                                uiOutput("signature_volcano_downbttn_pdf", inline = TRUE)
                         ),
                         
                         
                         column(12,
                                br(),
                                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                
                                h5('List of Differentially Expressed Genes in the Signatures', align='center'),
                                h6('(|Fold Change| > 2 & FDR < 0.01)', align='center'),
                                DT::dataTableOutput("signature.de.list")
                         )
                         
                ),
                tabPanel(strong("Survival Analysis"), # all genes, but label the common genes
                         br(),
                         hr(),
                         
                         column(12,
                                column(4,
                                       #h5("List of genes", align = 'left'),
                                       #tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                       bcr.dataset.input
                                       #withSpinner(uiOutput('query_boxplot_ui'), type = 1)
                                )
                         ),
                         
                         
                         # column(12,
                         #        tags$hr(style="border-top: 1px dashed #A9A9A9"),
                         #        h5('Volcano Plot of Genes Associated with BCR in the Signatures', align='center'),
                         #        h6('(|Hazard Ratio| > 1 & FDR < 0.01)', align='center'),
                         #        
                         #        column(3),
                         #        column(6, withSpinner(plotOutput("signature.bcr.volcano", height = 400), type=4)),
                         #        column(3)
                         # ),
                         
                         column(12,
                                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                h5('KM Survival Analysis of RFS for Common Genes in 3 or more Signatures', align='center'),
                                h6("(Low- and high-expression groups were separated by median values)", align = 'center'),
                                
                                column(1),
                                column(10, withSpinner(plotOutput("signature.bcr.forest", height = 1350), type=4, proxy.height=300)),
                                column(1)
                         ),
                         
                         column(2),
                         column(10,
                                # downloadBttn(
                                #   outputId = "signature.survival.forest.downbttn.csv",
                                #   label = 'CSV',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                # ),
                                # downloadBttn(
                                #   outputId = "signature.survival.forest.downbttn.pdf",
                                #   label = 'PDF',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                # )
                                
                                uiOutput("signature_survival_forest_downbttn_csv", inline = TRUE),
                                uiOutput("signature_survival_forest_downbttn_pdf", inline = TRUE)
                         ),
                         
                         
                         column(12,
                                br(),
                                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                
                                h5('KM Survival Analysis of RFS for All the Signature Genes', align='center'),
                                h6("(Low- and high-expression groups were separated by median values)", align = 'center'),
                                DT::dataTableOutput("signature.bcr.list")
                         )
                         
                         
                ),
                tabPanel(strong("Pathway Analysis"), # Signature & common genes
                         br(),
                         hr(),
                         
                         column(12,
                                column(1),
                                column(4,
                                       signature.pathway.input
                                ),
                                
                                column(6,
                                       signature.ontology.input
                                )
                         ),
                         
                         column(12,
                                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                h5('Functional Enrichment Analysis of Signature Genes', align='center'),
                                br(),
                                DT::dataTableOutput("signature.enrich.table")
                         ),
                         
                         column(12, 
                                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                column(1),
                                column(10,
                                       h5('Bar Plot of the Top 30 Enriched Pathways', align='center'),
                                       withSpinner(plotOutput('signature_enrichment_bar_plot', height = '100%'), type = 4, proxy.height = 300)
                                )#,
                                # column(8),
                                # column(4,
                                #        downloadButton(outputId='enrich.bar.downbttn.csv', label = "CSV"),
                                #        #downloadButton(outputId='enrich.bar.downbttn.png', label = "PNG"),
                                #        downloadButton(outputId='enrich.bar.downbttn.pdf', label = "PDF")
                                # )
                         ),
                         
                         column(2),
                         column(10,
                                # downloadBttn(
                                #   outputId = "signature.enrichment.barplot.downbttn.csv",
                                #   label = 'CSV',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                # ),
                                # downloadBttn(
                                #   outputId = "signature.enrichment.barplot.downbttn.pdf",
                                #   label = 'PDF',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                # )
                                
                                uiOutput("signature_enrichment_barplot_downbttn_csv", inline = TRUE),
                                uiOutput("signature_enrichment_barplot_downbttn_pdf", inline = TRUE)
                         ),
                         
                         column(12,
                                br(),
                                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                column(1),
                                column(10,
                                       h5('Bubble Plot of the Top 30 Enriched Pathways', align='center'),
                                       withSpinner(plotOutput('signature_enrichment_bubble_plot', height = '100%'), type = 4, proxy.height = 300)
                                )#,
                                # column(8),
                                # column(4,
                                #        downloadButton(outputId='enrich.bubble.downbttn.csv', label = "CSV"),
                                #        #downloadButton(outputId='enrich.bubble.downbttn.png', label = "PNG"),
                                #        downloadButton(outputId='enrich.bubble.downbttn.pdf', label = "PDF")
                                # )
                         ),
                         
                         column(2),
                         column(10,
                                # downloadBttn(
                                #   outputId = "signature.enrichment.bubble.downbttn.csv",
                                #   label = 'CSV',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                # ),
                                # downloadBttn(
                                #   outputId = "signature.enrichment.bubble.downbttn.pdf",
                                #   label = 'PDF',
                                #   style = "jelly",
                                #   color = "success", 
                                #   size = 'xs'
                                # )
                                
                                uiOutput("signature_enrichment_bubble_downbttn_csv", inline = TRUE),
                                uiOutput("signature_enrichment_bubble_downbttn_pdf", inline = TRUE)
                         ),
                         
                ),
                
                tabPanel(strong("Model Comparison"), # CoxPH, Ridge, plsRcox; intra, inter
                         br(),
                         hr(),
                         
                         column(12,
                                column(4,
                                       #h5("List of genes", align = 'left'),
                                       #tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                       signature_comp_signature_input
                                       #withSpinner(uiOutput('query_boxplot_ui'), type = 1)
                                ),
                                
                                column(4,
                                       #h5("List of genes", align = 'left'),
                                       #tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                       signature.comp.training.input
                                       #withSpinner(uiOutput('query_boxplot_ui'), type = 1)
                                ),
                                column(4,
                                       #h5("List of genes", align = 'left'),
                                       #tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                       signature.comp.model.input
                                       #withSpinner(uiOutput('query_boxplot_ui'), type = 1)
                                ),
                                column(12,
                                       tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                       
                                       h5('Comparison of the performances of the prognostic signatures', align='center'),
                                       #DT::dataTableOutput('signature_summary_table'),
                                       div(DT::dataTableOutput("signature_summary_table"),style = "font-size:90%"),
                                       tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                       
                                       column(4,
                                              conditionalPanel(condition = 'input.signature_comp_signature_input!="All Signatures"',
                                                               
                                                               h5('C Index', align='center'),
                                                               withSpinner(plotOutput('signature_c_index_plot',width = 300, height = 400), type = 4, proxy.height = 300)
                                              )),
                                       
                                       column(4,
                                              conditionalPanel(condition = 'input.signature_comp_signature_input!="All Signatures"',
                                                               
                                                               h5('Hazard Ratio (KM)', align='center'),
                                                               withSpinner(plotOutput('signature_km_plot',width = 300, height = 400), type = 4, proxy.height = 300)
                                              )),
                                       
                                       column(4,
                                              conditionalPanel(condition = 'input.signature_comp_signature_input!="All Signatures"',
                                                               
                                                               h5('Area Under the ROC Curve', align='center'),
                                                               withSpinner(plotOutput('signature_auc_plot',width = 300, height = 400), type = 4, proxy.height = 300)
                                                               
                                              ))
                                       
                                ),
                                
                                column(12,
                                       conditionalPanel(condition = 'input.signature_comp_signature_input=="All Signatures"',
                                                        
                                                        h5('C Index', align='center'),
                                                        withSpinner(plotOutput('signature_all_c_index_plot',width = 925, height = 750), type = 4, proxy.height = 300)
                                       )),
                                column(12,
                                       conditionalPanel(condition = 'input.signature_comp_signature_input=="All Signatures"',
                                                        tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                                        h5('Hazard Ratio of KM analysis', align='center'),
                                                        withSpinner(plotOutput('signature_all_km_plot',width = 925, height = 750), type = 4, proxy.height = 300)
                                       )),
                                column(12,
                                       conditionalPanel(condition = 'input.signature_comp_signature_input=="All Signatures"',
                                                        tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                                        h5('Time-dependent AUC', align='center'),
                                                        withSpinner(plotOutput('signature_all_auc_plot',width = 925, height = 750), type = 4, proxy.height = 300)
                                       ))
                                
                                
                         )
                )
                
    )
  )
  
                                         )



##### Transcriptome analysis

tab_transcriptome <- fluidRow(
  
  box(
    title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
    width = 12, height = 55,
    
    #h4(strong("Transcriptome Analysis"), align='center', style='font-family:Georgia;color:#2C3E50')
    h4(strong(icon("bar-chart", lib = "font-awesome"), HTML('&nbsp;'), "Transcriptome Analysis"), align='center', style='font-family:Georgia;color:#2C3E50')
  ),
  
  
  box(
    #title = strong('======================================= Select a Transcriptome Dataset ======================================='), status = "info", solidHeader = TRUE, collapsible = FALSE,
    title = tags$div(HTML('<b><i class="fa fa-chevron-circle-down" style = "color:black;"></i> Select a Transcriptome Dataset</b>')), status = "info", solidHeader = TRUE, collapsible = FALSE,
    #strong('Select a Transcriptome Dataset')       
    width = 12,
    div(DT::dataTableOutput("transcriptome_dataset"),style = "font-size:90%")
  ),
  
  box(
    title = NULL, solidHeader = FALSE, collapsible = FALSE, #status = "success", 
    width = 12,
    
    tabsetPanel(id = 'transcriptome', type='pills', #widths = c(2,10),
                #tabBox(width = 12, id = 'transcriptome',
                
                tabPanel(strong("Summary"), value = 'Summary',
                         br(),
                         hr(),
                         
                         div(h4(strong(textOutput("transcriptome_dataset_summary"))), style = "color:black", align='center'),
                         
                         br(),
                         
                         box(title = 'Summary',
                             status = "info", solidHeader = TRUE, collapsible = FALSE,
                             width = 4,
                             height = 375,
                             
                             strong('Platform:'),
                             textOutput('transcriptome_text_summary_platform'),
                             #hr(style="border-top: 1px dashed #A9A9A9"),
                             br(),
                             strong('GEO/ArrayExpress Accession:'),
                             textOutput('transcriptome_text_summary_accession'),
                             #hr(style="border-top: 1px dashed #A9A9A9"),
                             strong('SRA Accession:'),
                             textOutput('transcriptome_text_summary_sra'),
                             #hr(style="border-top: 1px dashed #A9A9A9"),
                             strong('cBioPortal:'),
                             textOutput('transcriptome_text_summary_cbioportal'),
                             br(),
                             strong('Data Preprocessing:'),
                             textOutput('transcriptome_text_summary_pipeline')#,
                             #br(),
                             #strong('Available Metadata:')
                         ),
                         
                         box(title = 'Sample Type',
                             status = "info", solidHeader = TRUE, collapsible = FALSE,
                             width = 4,
                             height = 375,
                             plotlyOutput('transcriptome_pie_sample_type', width='100%', height='300px')
                         ),
                         
                         conditionalPanel(condition = 'input.transcriptome_dataset_rows_selected == 1 || 
                                          input.transcriptome_dataset_rows_selected == 2 || 
                                          (input.transcriptome_dataset_rows_selected >= 4 && input.transcriptome_dataset_rows_selected <= 10) || 
                                          input.transcriptome_dataset_rows_selected == 12 || 
                                          (input.transcriptome_dataset_rows_selected >= 25 && input.transcriptome_dataset_rows_selected <= 29) || 
                                          input.transcriptome_dataset_rows_selected == 41 ||
                                          input.transcriptome_dataset_rows_selected == 47 || 
                                          input.transcriptome_dataset_rows_selected == 50 ||
                                          input.transcriptome_dataset_rows_selected == 51 ||
                                          input.transcriptome_dataset_rows_selected == 64 || 
                                          input.transcriptome_dataset_rows_selected == 65',
                                          box(title = 'PSA',
                                              status = "info", solidHeader = TRUE, collapsible = FALSE,
                                              width = 4,
                                              height = 375,
                                              plotlyOutput('transcriptome_histogram_psa', width='100%', height='300px')
                                          )
                         ),
                         
                         conditionalPanel(condition = 'input.transcriptome_dataset_rows_selected == 1 || 
                                          (input.transcriptome_dataset_rows_selected >= 3 && input.transcriptome_dataset_rows_selected <= 9) || 
                                          input.transcriptome_dataset_rows_selected == 12 || 
                                          input.transcriptome_dataset_rows_selected == 17 || 
                                          input.transcriptome_dataset_rows_selected == 43 || 
                                          input.transcriptome_dataset_rows_selected == 47 || 
                                          input.transcriptome_dataset_rows_selected == 50 ||
                                          input.transcriptome_dataset_rows_selected == 51 ||
                                          input.transcriptome_dataset_rows_selected == 72 || 
                                          input.transcriptome_dataset_rows_selected == 74',
                                          # 1, 3, 4, 5, 6, 7, 8, 9, 12, 17, 43, 47, 50, 51, 72, 74
                                          box(title = 'Gleason Pattern',
                                              status = "info", solidHeader = TRUE, collapsible = FALSE,
                                              width = 4,
                                              height = 375,
                                              plotlyOutput('transcriptome_barplot_gleason', width='100%', height='300px')
                                          )
                         ),
                         
                         conditionalPanel(condition = 'input.transcriptome_dataset_rows_selected <= 12 || 
                                          input.transcriptome_dataset_rows_selected == 16 || 
                                          input.transcriptome_dataset_rows_selected == 17 || 
                                          input.transcriptome_dataset_rows_selected == 51 || 
                                          input.transcriptome_dataset_rows_selected == 52',
                                          box(title = 'BCR Status',
                                              status = "info", solidHeader = TRUE, collapsible = FALSE,
                                              width = 4,
                                              height = 375,
                                              plotlyOutput('transcriptome_pie_bcr_status', width='100%', height='300px')
                                          )
                         ),
                         
                         conditionalPanel(condition = 'input.transcriptome_dataset_rows_selected <= 10',
                                          box(title = 'Time to BCR',
                                              status = "info", solidHeader = TRUE, collapsible = FALSE,
                                              width = 4,
                                              height = 375,
                                              plotlyOutput('transcriptome_km_bcr_time', width='100%', height='300px')
                                          )
                         ),
                         
                         conditionalPanel(condition = 'input.transcriptome_dataset_rows_selected != 1 && 
                                          input.transcriptome_dataset_rows_selected != 4 && 
                                          input.transcriptome_dataset_rows_selected != 22 && 
                                          input.transcriptome_dataset_rows_selected != 23 && 
                                          input.transcriptome_dataset_rows_selected != 47 && 
                                          input.transcriptome_dataset_rows_selected != 49 && 
                                          input.transcriptome_dataset_rows_selected != 50',
                                          box(title = NULL,
                                              status = "info", solidHeader = FALSE, collapsible = TRUE,
                                              width = 12,
                                              height = 600,
                                              htmlOutput("dataset_link")
                                          )
                         )
                         
                         
),

tabPanel(strong("Dimensionality Reduction"), value='PCA',
         br(),
         hr(),
         
         column(6, 
                br(),
                h5('2D Principal Component Analysis', align='center'),
                withSpinner(plotlyOutput('transcriptome.pca.2d'),
                            type = 1)
                
         ),
         
         column(6, 
                br(),
                h5('3D Principal Component Analysis', align='center'),
                withSpinner(plotlyOutput('transcriptome.pca.3d'),
                            type = 1)
         )
         
         
         
         
         
         
),
tabPanel(strong("Differential Expression"), value='DEA',
         br(),
         hr(),
         
         column(12,
                column(7,
                       h5(strong("Select the Case/Control Groups for Comparison:")),
                       br(),
                       DT::dataTableOutput("transcriptome.groups")
                ),
                
                column(5,
                       
                       column(12, radioButtons(inputId = "deg.test", label = h5(strong("Method:")),
                                               c('Limma'),#, 'Wilcoxon Rank Sum Test'
                                               inline = FALSE)),
                       
                       
                       column(6, numericInput(inputId = "foldchange", label = strong('Fold Change:'),
                                              value = 2, min = 0, max = 10, step = 0.1, width = 150)),
                       column(6, numericInput(inputId = "fdr", label = strong('BH Adjusted P Value:'),
                                              value = 0.01, min = 0, max = 1, step = 0.01, width = 150)),
                       
                       #column(1),
                       column(6, actionButton(inputId = 'deg_submit', label = strong('Submit'), icon=icon("check"), 
                                              style="color: #fff; background-color: #4095c9; border-color: #368dc2; border-width: 2px; font-size: 12px;",
                                              class = 'btn-sm', width = 200))
                       
                )
         ),
         
         column(12, 
                br(),
                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                
                column(3),
                column(6,
                       conditionalPanel(condition = 'input.deg_submit',
                                        h5('Volcano Plot of Differentially Expressed Genes', align='center'),
                                        withSpinner(id = 'volcano_spinner', plotOutput('transcriptome_volcano_plot', height = 500), type = 4, proxy.height = 300)
                       )
                )
                ),
         column(12, 
                column(4),
                column(8,
                       conditionalPanel(condition = 'input.deg_submit',
                       # downloadBttn(
                       #   outputId = "transcriptome.volcano.downbttn.csv",
                       #   label = 'CSV',
                       #   style = "jelly",
                       #   color = "success", 
                       #   size = 'xs'
                       # ),
                       # downloadBttn(
                       #   outputId = "transcriptome.volcano.downbttn.pdf",
                       #   label = 'PDF',
                       #   style = "jelly",
                       #   color = "success", 
                       #   size = 'xs'
                       # )
                       uiOutput("transcriptome_volcano_downbttn_csv", inline = TRUE),
                       uiOutput("transcriptome_volcano_downbttn_pdf", inline = TRUE)
                       
                       )
                )
                ),
         
         
         column(12, 
                conditionalPanel(condition = 'input.deg_submit',
                                 br(),
                                 tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                 h5('List of Differentially Expressed Genes', align='center'),
                                 div(DT::dataTableOutput("transcriptome_deg_table"),style = "font-size:90%")
                )
         )
         
         
),

tabPanel(strong("Survival Analysis"), value='SurvivalAnalysis',
         conditionalPanel(condition = 'input.transcriptome_dataset_rows_selected <= 10',
                          column(12,
                                 br(),
                                 hr(),
                                 h5('Univariate Kaplan Meier (KM) Survival Analysis', align='center'),
                                 h6("(Low- and high-expression groups were separated by median values)", align = 'center'),
                                 div(DT::dataTableOutput("table_km"),style = "font-size:90%")
                          ),   
                          
                          column(12,
                                 br(),
                                 tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                 h5('Univariate Cox Proportional-Hazards (CoxPH) Survival Analysis', align='center'),
                                 div(DT::dataTableOutput("table_coxph"),style = "font-size:90%")
                          ),
                          
                          column(12,
                                 br(),
                                 tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                 h5('Multivariate Cox Proportional-Hazards (CoxPH) Survival Analysis', align='center'),
                                 div(DT::dataTableOutput("table_coxph_multi"),style = "font-size:90%"),
                                 
                                 br(),
                                 tags$hr(style="border-top: 1px dashed #A9A9A9"),

                                 tags$img(src='img/Avail_Clinical_Multivariate.jpg', height=250, #width=150,
                                          style="display: block; margin-left: auto; margin-right: auto;"),
                                 h5('* Clinical factors that are used for multivariate CoxPH survival analysis in each dataset', align='center'),
                                 h6('(Y: included; N: not included)', align='center')
                          )
                          
         )
         
         # column(2),
         # 
         # column(6,
         #        br(),
         #        plotOutput('plot_km')
         # )
),

# tabPanel(strong("Pathway Analysis"),
#          br(),
#          hr()
# ),

tabPanel(strong("Prognostic Model"), value='PrognosticModel',
         conditionalPanel(condition = 'input.transcriptome_dataset_rows_selected <= 10',
         br(),
         hr(),
         column(4,
                textAreaInput(inputId = "surv_gene_input", label = h5(strong("Paste A Gene List\n(Ensembl ID):")), 
                              value = "ENSG00000143322\nENSG00000187764\nENSG00000005844\nENSG00000173372\nENSG00000102265\nENSG00000124762", width = "300px", height = '200px')
         ),
         column(8,
                column(12, #radioButtons(inputId = "surv_model_method", label = h5(strong("Method:")),
                          #              choices = c('CoxPH', 'CoxLasso', 'CoxRidge'), # ,'plsRcox'
                          #              inline = TRUE)
                       prettyRadioButtons(inputId = "surv_model_method", label = h5(strong("Method:")),
                                          choices = c('CoxPH', 'CoxLasso', 'CoxRidge'), # ,'plsRcox'
                                          inline = TRUE) # , icon = icon("check")
                       
                       ),
                
                #column(1),
                column(6, actionButton(inputId = 'surv_submit', label = strong('Submit'), icon=icon("check"), 
                                       style="color: #fff; background-color: #4095c9; border-color: #368dc2; border-width: 2px; font-size: 12px;",
                                       class = 'btn-sm', width = 200)),
                
                column(10, 
                       br(),
                       br(),
                       br(),
                       verbatimTextOutput("genes"),
                       tags$head(tags$style(HTML("
                            #genes {font-size: 14px; color: darkred}")))
                )
         ),
         
         
         
         column(12,
                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                
                column(6,
                       conditionalPanel(condition = 'input.surv_submit',
                                        h5(strong('Prognostic Model'), align='center'),
                                        div(DT::dataTableOutput("table_coeffs"),style = "font-size:90%")
                       )
                ),
                
                column(6,
                       column(12,
                              conditionalPanel(condition = 'input.surv_submit',
                                               h5('Kaplan Meier Survival Analysis in the Training Dataset', align='center'),
                                               h6("(Low- and high-risk groups were separated by median values)", align = 'center')
                              )
                       ),
                       column(1),
                       column(11,
                              conditionalPanel(condition = 'input.surv_submit',
                                               withSpinner(plotOutput('training_km_plot',width = 360, height = 360), type = 4, proxy.height = 300),
                                               column(2),
                                               column(10,
                                                      # downloadBttn(
                                                      #   outputId = "transcriptome.survival.km.downbttn.csv",
                                                      #   label = 'CSV',
                                                      #   style = "jelly",
                                                      #   color = "success", 
                                                      #   size = 'xs'
                                                      # ),
                                                      # downloadBttn(
                                                      #   outputId = "transcriptome.survival.km.downbttn.pdf",
                                                      #   label = 'PDF',
                                                      #   style = "jelly",
                                                      #   color = "success", 
                                                      #   size = 'xs'
                                                      # )
                                                      uiOutput("transcriptome_survival_km_downbttn_csv", inline = TRUE),
                                                      uiOutput("transcriptome_survival_km_downbttn_pdf", inline = TRUE)
                                               )
                              )
                       )
                )
                
         ),
         
         column(12,
                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                
                column(1),
                column(11, 
                       conditionalPanel(condition = 'input.surv_submit',
                                        h5('Kaplan Meier Survival Analysis in the Validation Datasets', align='center'),
                                        withSpinner(plotOutput("validation.bcr.forest", width = 800, height = 600), type=4, proxy.height=300),
                                        column(1),
                                        column(11,
                                               # downloadBttn(
                                               #   outputId = "transcriptome.survival.forest.downbttn.csv",
                                               #   label = 'CSV',
                                               #   style = "jelly",
                                               #   color = "success", 
                                               #   size = 'xs'
                                               # ),
                                               # downloadBttn(
                                               #   outputId = "transcriptome.survival.forest.downbttn.pdf",
                                               #   label = 'PDF',
                                               #   style = "jelly",
                                               #   color = "success", 
                                               #   size = 'xs'
                                               # )
                                               uiOutput("transcriptome_survival_forest_downbttn_csv", inline = TRUE),
                                               uiOutput("transcriptome_survival_forest_downbttn_pdf", inline = TRUE)
                                        )
                                        
                                        )
                       
                )
                
         )
         )
)


)
  )

)



### Pipeline

tab_pipeline <- fluidRow(
  
  box(
    title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
    width = 12, height = 55, 
    
    #h4(strong("PCaDB Pipelines"), align='center', style='font-family:Georgia;color:#2C3E50')
    h4(strong(icon("pencil"), HTML('&nbsp;'), "PCaDB Pipeline"), align='center', style='font-family:Georgia;color:#2C3E50')
    ),
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('Details about PCaDB pipeline'), align='left'),
    
    tags$ul(tags$li('Overview', style="font-size:120%;")),
    # tags$li('Data collection', style="font-size:120%;"),
    tags$ul(tags$li('SRA RNAseq data', style="font-size:120%;")),
    tags$ul(tags$li('TCGA RNAseq data', style="font-size:120%;")),
    tags$ul(tags$li('cBioPortal RNAseq data', style="font-size:120%;")),
    tags$ul(tags$li('GEO/ArrayExpress microarray data', style="font-size:120%;")),
    tags$ul(tags$li('Sample metadata model', style="font-size:120%;")),
    tags$ul(tags$li('Single-cell RNAseq data', style="font-size:120%;")),
    tags$ul(tags$li('Gene annotation', style="font-size:120%;"))#,
    #tags$ul(tags$li('Prognostic signature evaluation', style="font-size:120%;")),
    #tags$ul(tags$li('Transcriptomics data analysis', style="font-size:120%;"))
    # tags$ul(tags$li('Dimensionality reduction (PCA)', style="font-size:110%;")),
    # tags$ul(tags$li('Differential expression analysis', style="font-size:110%;")),
    # tags$ul(tags$li('Survival analysis', style="font-size:110%;")),
    # tags$ul(tags$li('Prognostic models', style="font-size:110%;"))
  ),
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('Overview'), align='left'),

    tags$p("A suite of bioinformatics pipelines has been developed to download, process, and harmonize the transcriptomics data 
           from multiple public data repositories, including 
           Sequence Read Archive (SRA), Genomic Data Commons (GDC)/The Cancer Genome Atlas (TCGA), 
           cBioPortal, Gene Expression Omnibus (GEO), and ArrayExpress.",
           style = "font-size: 110%;"),
           
    tags$p("In general, among the 77 datasets in PCaDB, 
           (i) 17 datasets from SRA and 37 datasets from GEO/ArrayExpress with the raw FASTQ or CEL files, respectively, were reprocessed using the PCaDB pipeline; 
           (ii) read count data for the TCGA-PRAD project was downloaded from GDC; 
           (iii) normalized data such as RPKM/FPKM values for the 6 RNAseq datasets in cBioPortal were downloaded directly; and 
           (iv) normalized intensities for the other 16 microarray datasets without raw data were downloaded from GEO. 
           For RNAseq data from TCGA and SRA, 
           the raw count data is normalized using the Trimmed Mean of Mvalues (TMM) method 
           implemented in the R package ", style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("edgeR"), style = "font-size: 100%;display:inline;"),
    
    tags$p(" [1], and lowly-expressed genes with logCPM < 0 in more than 50% of samples 
           are filtered out prior to any downstream analysis. For the Affymetrix microarray data 
           with raw CEL files in GEO/ArrayExpress, the Robust Multichip Average (RMA) method 
           implemented in the R package ", 
           style = "font-size: 110%; display:inline;"),
    
    tags$i(strong("oligo"), style = "font-size: 100%; display:inline"),
    tags$p(" [2] was used for data normalization. For a small proportion of the datasets that 
           only have normalized data provided in the public data repository, 
           the methods for data processing described in the original papers 
           were carefully inspected to make sure that appropriate processing methods had been used.
           All the gene expression values are in log2 scale. 
           In PCaDB, the Ensembl gene identifiers are used for all the datasets. 
           If multiple probes/genes matched to the same Ensembl ID, only the most informative one with 
           the maximum interquartile range (IQR) for the gene expression is used for this Ensembl ID.",
           style = "font-size: 110%;display:inline;"),
    br(),
    br(),
           
    tags$p("A comprehensive metadata model has been developed to harmonize the sample metadata (phenotype data) 
           from the public repositories. The ExpressionSet object has been created for each dataset, allowing users 
           to easily download both the gene expression data and harmonized metadata for advanced downstream analyses.",
           style = "font-size: 110%;"),
    
    tags$p("A single-cell RNAseq dataset from normal human prostae tissue [3] was downloaded from GUDMAP (https://www.gudmap.org/), 
           allowing the investigation of a gene of interest at the single-cell level.", 
           style = "font-size: 110%;"),
    
    tags$p("30 prognostic signatures for prostate cancer were obtained from a comprehensive study evaluating the machine learning 
           models and gene expression signatures for prostate cancer prognosis using large population cohorts [4]. 
           The 10 datasets used in that study were all included in the PCaDB database.", 
           style = "font-size: 110%;"),
    
    tags$p("We also systematically organized the gene annotation information from HGNC, Ensembl, NCBI, and Gencode to facilitate the 
           query of individual genes and the cross-dataset anlaysis.", 
           style = "font-size: 110%;"),
    
    tags$p("Overview of the pipeline is shown in the figure below and details about the pipeline are described in the sub-sections.", 
           style = "font-size: 110%;"),
    
    tags$hr(style="border-top: 1px dashed #A9A9A9"),
    
    tags$img(src='img/pipeline.jpg', height=450) #width=150,
    
    
  ),
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('SRA RNAseq data'), align='left'),
    
    tags$p("For the 17 RNAseq datasets in SRA, the ", style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("fasterq-dump"), style = "font-size: 100%;display:inline;"),
    
    tags$p("tool in ", style = "font-size: 110%;display:inline;"), 
    
    tags$i(strong("SRA Toolkit (2.10.8)"), style = "font-size: 100%;display:inline;"),
                  
    tags$p(" [5] was used to download and convert SRA data into FASTQ format. The ", style = "font-size: 110%;display:inline;"),
    tags$i(strong("FastQC (v0.11.5)"), style = "font-size: 100%;display:inline;"),
    
    tags$p(" [6] tool was used for a comprehensive quality control of 
    the sequencing data, including base sequence quality, GC content, overrepresented sequences, and adapter content, etc. 
    Read trimming was not performed since a few benchmark studies have demonstrated that it's not necessary to 
    remove adapter sequences or low-quality bases before alignment because the adapter sequences can be efficiently 
    removed by read aligner via 'soft-cipping' and the low-quality bases can be rescued by the aligner [ref]. 
    The most widely used aligner for RNAseq data - ", style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("STAR (2.7.9a)"), style = "font-size: 100%;display:inline;"),
           
    tags$p(" [7] was adopted to align the sequencing reads to 
    the reference genome", style = "font-size: 110%;display:inline;"),
    
    tags$p(strong("GRCh38 (primary assembly)"), style = "font-size: 100%;display:inline;"),
    
    tags$p(" based on the", style = "font-size: 110%;display:inline;"),
    tags$p(strong("GENCODE (release 38)"), style = "font-size: 100%;display:inline;"),
    
    tags$p("gene annotation [8]. 
    The 2-pass mapping was performed for each sample separately. ", 
    style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("STAR"), style = "font-size: 100%;display:inline;"),
           
    tags$p(" will perform the 1st pass mapping and 
    then automatically extract junctions and insert them into the genome index, and, finally, re-align 
    all reads in the 2nd mapping pass [ref]. The BAM files generated from the alignment step were sorted using ",
           style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("samtools (v1.9)"),style = "font-size: 100%;display:inline;"),
           
    tags$p(" [9]. The ", style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("featureCounts"), style = "font-size: 100%;display:inline;"),
           
    tags$p(" program [10] in the ", style = "font-size: 110%;display:inline;"),
           
    tags$i(strong("Subread (2.0.3)"), style = "font-size: 100%;display:inline;"),
                  
    tags$p(" package was used for gene expression quantification to generate the 
    count matrix. Only read pairs that were uniquely mapped to the exonic regions were quantified. 
    The count data was finally normalized using the Trimmed Mean of Mvalues (TMM) method 
           implemented in the R package ", style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("edgeR"), style = "font-size: 100%;display:inline;"),
    
    tags$p(", and lowly-expressed genes with logCPM < 0 in more than 50% of samples 
           were filtered out prior to any downstream analysis. 
           To generate a more comprehensive QC report, we used the ", style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("rnaseqc (v2.4.2)"),style = "font-size: 100%;display:inline;"),
    
    tags$p("tool [11] to characterize the quality of the RNA, sequencing data, alignment, and expression 
           profile of each sample. Over 70 QC metrics can be computed by ",style = "font-size: 110%;display:inline;"),
           
    tags$i(strong("rnaseqc"), style = "font-size: 100%;display:inline;"),
           
    tags$p(", including rRNA rate and exonic/intronic/intergenic rate, etc. 
           Finally, an aggregated QC report can be generated using ",style = "font-size: 110%;display:inline;"),

    tags$i(strong("MultiQC"), style = "font-size: 100%;display:inline;"),
    
    tags$p(" [12] to display the QC metrics from multiple 
           bioinformatics analyses, including ", style = "font-size: 110%;display:inline;"),
           
    tags$i(strong("FastQC"),style = "font-size: 100%;display:inline;"),
    tags$p(", ", style = "font-size: 110%;display:inline;"),
    tags$i(strong("STAR"),style = "font-size: 100%;display:inline;"),
    tags$p(", ", style = "font-size: 110%;display:inline;"),
    tags$i(strong("featureCounts"),style = "font-size: 100%;display:inline;"),
    tags$p(", and ", style = "font-size: 110%;display:inline;"),
    tags$i(strong("rnaseqc"),style = "font-size: 100%;display:inline;"),
    tags$p(".", style = "font-size: 110%;display:inline;"),
    
    br(),
    br(),
    
    tags$p("All the scipts for SRA RNAseq data processing are publicly available in Github: ",
           style = "font-size: 110%;display:inline;"),
    tags$a(href="https://github.com/rli012/PCaDB", "https://github.com/rli012/PCaDB", 
           target='_blank', style = "font-size: 110%;display:inline;"),
    tags$p(".",
           style = "font-size: 110%;display:inline;"),
    
    tags$hr(style="border-top: 1px dashed #A9A9A9"),
    
    tags$img(src='img/SRA_RNAseq_pipeline.jpg', height=400) #width=150,
           
  ),
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('TCGA RNAseq data'), align='left'),
    
    tags$p("The raw sequencing data in TCGA was processed 
    using a similar pipeline (", style = "font-size: 110%;display:inline;"),
    
    tags$a(href="https://docs.gdc.cancer.gov/Data/Bioinformatics_Pipelines/Expression_mRNA_Pipeline/", 
           "TCGA mRNA Analysis Pipeline", 
           target='_blank', style = "font-size: 110%;display:inline;"),
           
    tags$p(") as what is being used by PCaDB for SRA RNAseq data processing (described above). 
    The sequencing reads were aligned to the human reference genome ", 
           style = "font-size: 110%;display:inline;"),
           
           
    tags$p(strong("GRCh38"), style = "font-size: 100%;display:inline;"),
    
    tags$p(" using the 2-pass mapping implemented in ", style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("STAR (2.4.2a)"), style = "font-size: 100%;display:inline;"),
    
    tags$p(" based on ", style = "font-size: 110%;display:inline;"),
           
    tags$p(strong("GENCODE (release22)"), style = "font-size: 100%;display:inline;"),

    tags$p(" for gene annotation. The major difference between TCGA workflow and PCaDB workflow is that ",
           style = "font-size: 110%;display:inline;"),
           
    tags$i(strong("HTSeq (0.6.1p1)"),style = "font-size: 100%;display:inline;"),
           
           
    tags$p(" [13] was used by TCGA for gene expression quantification, whereas ", 
           style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("featureCounts/Subread (2.0.3)"),style = "font-size: 100%;display:inline;"),
    
    tags$p(" is adopted by PCaDB. The HTSeq-Counts data for The Cancer Genome Atlas Prostate Adenocarcinoma (TCGA-PRAD) project 
    is publicly available in Genomic Data Commons (GDC), which can be downloaded and processed directly 
    by a series of functions in the R package ",
           style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("GDCRNATools"),
           style = "font-size: 100%; display:inline;"),
    
    
    tags$p(" [14] to generate the count matrix. The count data was also normalized using the Trimmed 
           Mean of M values (TMM) method implemented in the R package ",
           style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("edgeR"),
           style = "font-size: 100%; display:inline;"),
    
    tags$p(". Silimarly, lowly-expressed genes with logCPM < 0 in more than 50% of samples 
           were filtered out prior to any downstream analysis.", style = "font-size: 110%; display:inline;"),
    
    br(),
    br(),
    
    tags$p("Below is the script for downloading, preprocessing, and organizing data from TCGA-PRAD.", style = "font-size: 110%;"),
    
    
    tags$hr(style="border-top: 1px dashed #A9A9A9"),
    
    #tags$code("library(GDCRNATools)"))
    
    tags$iframe(src = 'rmd/tcga.html', 
                width = '100%', height = '400px', 
                frameborder = 0, scrolling = 'auto')

  ),
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('cBioPortal RNAseq data'), align='left'),
    
    tags$p("Although transcriptomics data for multiple PCa cohorts are included in cBioPortal, the 
    cBioPortal data are usually not used in PCaDB, unless the raw data are not available in any 
           of the other data repositories, because only the normalized RNAseq data are provided in cBioPortal, 
           which may not be suitable for some of the downstream analyses. e.g., the FPKM/RPKM values 
           are not recommended for differential expression analysis.", style = "font-size: 110%;"),
    
    tags$p("For the 6 cBioPortal RNAseq datasets included in PCaDB, normalized gene expression data 
           (FPKM/RPKM values) were downloaded directly from cBioPortal. Log2 transformation may be 
           performed if it hadn't been done and the gene symbols/entrez ids were all converted to Ensembl IDs. 
           If multiple genes matched to the same Ensembl ID, only the most informative one with 
           the maximum interquartile range (IQR) for the gene expression is used for this Ensembl ID.", 
           style = "font-size: 110%;"),
    
    tags$p("A custom script below is used to curate cBioPortal data.", style = "font-size: 110%;"),
    
    tags$hr(style="border-top: 1px dashed #A9A9A9"),
    
    tags$iframe(src = 'rmd/cbioportal.html', 
                width = '100%', height = '400px', 
                frameborder = 0, scrolling = 'auto')
    
  ),
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('GEO/ArrayExpress microarray data'), align='left'),
    
    tags$p("Among the 38 datasets generated 
           using the Affymetrix arrays (e.g., Affymetrix Human Exon 1.0 ST Array, Affymetrix Human Gene 2.0 ST Array, 
           Affymetrix Human Genome U133A Array, and Affymetrix Human Genome U133 Plus 2.0 Array, etc.), 37 of them with 
           the raw CEL files were reprocessed by the PCaDB pipeline. The R package ", style = "font-size: 110%; display:inline;"),
    
    tags$i(strong("GEOquery"), style = "font-size: 100%; display:inline;"),
    
    tags$p(" [15] was used to download the CEL files and the Robust Multichip Average (RMA) method  
           implemented in the R package ", 
           style = "font-size: 110%; display:inline;"),
    
    tags$i(strong("oligo"),
           style = "font-size: 100%; display:inline"),
    
    tags$p(" was used for background correction, quantile normalization, and log2 transformation. The annotation 
           package downloaded from the ", style = "font-size: 110%; display:inline"),
    
    tags$a(href="http://brainarray.mbni.med.umich.edu/Brainarray/Database/CustomCDF/24.0.0/gencodeg.asp", 
           "Brainarray database", 
           target='_blank', style = "font-size: 110%;display:inline;"),
           
    tags$p("(", style = "font-size: 110%; display:inline"),
    tags$p(strong("Version 24.0.0; GENCODE release 32"),
           style = "font-size: 100%; display:inline"),
    
    tags$p(") was used for probe/gene annotation [16].", style = "font-size: 110%; display:inline"),

    tags$hr(style="border-top: 1px dashed #A9A9A9"),
    
    tags$iframe(src = 'rmd/geo_affymetrix.html', 
                width = '100%', height = '400px', 
                frameborder = 0, scrolling = 'auto'),
    
    tags$hr(style="border-top: 1px solid #A9A9A9"),
    
    tags$p("The only Affymetrix microarray dataset that was not reprocessed by our PCaDB pipeline is the CPC-Gene dataset (GSE107299). 
    The reason was because that this dataset was generated based on two different Affymetrix arrays: 
    Affymetrix Human Gene 2.0 ST Array (Batch 1,3,4,5) and Affymetrix Human Transcriptome Array 2.0 (Batch 2) with 
    a batch effect. Therefore, the batch-corrected normalized data was downloaded directly from GEO. 
    Likewise, the normalized intensities for the 15 microarray datasets generated 
           using other technologies (e.g., Illumina HumanHT-12 V3.0 Expression Beadchip and 
           Agilent-012391 Whole Human Genome Oligo Microarray G4112A, etc.) without raw data in GEO 
           were downloaded directly from the repository using ", style = "font-size: 110%; display:inline;"),
    
    tags$i(strong("GEOquery"), style = "font-size: 100%; display:inline;"),
   
    tags$p(". The methods for data processing described in the original papers 
           were carefully inspected to make sure that appropriate processing methods had been used. 
           Log2 transformation may be  
           performed on the normalized data if it hadn't been done and the probe/gene ids were all converted to Ensembl IDs. 
           If multiple probes/genes matched to the same Ensembl ID, only the most informative one with 
           the maximum interquartile range (IQR) for the gene expression is used for this Ensembl ID.", style = "font-size: 110%; display:inline;"),

    tags$hr(style="border-top: 1px dashed #A9A9A9"),
    
    tags$iframe(src = 'rmd/geo_normalized.html', 
                width = '100%', height = '400px', 
                frameborder = 0, scrolling = 'auto'),
    

    
  ),
  
  
  
  
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('Sample metadata model'), align='left'),
    
    tags$p("Harmonization of sample metadata is very critical for the management and downstream analysis of omics data. 
    In PCaDB, a comprehensive metadata model is built to harmonize the most important sample and clinical 
    information across the datasets. A total of 33 key fields/traits are included: ", style = "font-size: 110%;display:inline"),
    tags$code("sample_id", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("patient_id", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("tissue", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("batch", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("platform", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("sample_type", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("age_at_diagnosis", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("ethnicity", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("race", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("clinical_stage", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("clinical_t_stage", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("clinical_n_stage", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("clinical_m_stage", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("pathological_stage", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("pathological_t_stage", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("pathological_n_stage", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("pathological_m_stage", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("preop_psa", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("gleason_primary_pattern", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("gleason_secondary_pattern", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("gleason_tertiary_pattern", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("gleason_group", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("gleason_score", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("time_to_death", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("os_status", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("time_to_bcr", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("bcr_status", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("time_to_metastasis", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("metastasis_status", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("risk_group", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("treatment", style = "font-size: 100%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("additional_treatment", style = "font-size: 100%;display:inline"), tags$p(", and ", style = "font-size: 110%;display:inline"),
    tags$code("pcadb_group", style = "font-size: 100%;display:inline"), tags$p(". ", style = "font-size: 110%;display:inline"),
    
    tags$p("The ", style = "font-size: 110%;display:inline"),
    tags$code("pcadb_group", style = "font-size: 100%;display:inline"), 
    
    tags$p(" column in each dataset was manually created with the group information that are of the most interests to end users depending on 
    the availability, such as 
    normal, primary tumor, and metastatic castration-resistant prostate cancer (CRPC); 
    pre-androgen-deprivation therapy (ADT) and post-ADT samples; 
    samples from the peripheral zone and the transcription zone of the prostate; 
    samples from the European Americans and African Americans, etc.", style = "font-size: 110%;display:inline"),
    
    tags$p("The phenotypic values were also standardized, especially for the sample type field. For example, ",
           style = "font-size: 110%;display:inline"),
    tags$i("tumor, tumour, primary, localized, ", style = "font-size: 110%;display:inline"),
    tags$p("etc. were all labeled as ", style = "font-size: 110%;display:inline"),
    tags$i(strong("Primary"), style = "font-size: 100%;display:inline"),
    tags$p(", whereas ", style = "font-size: 110%;display:inline"),
    tags$i("normal, adjacent, benign, ", style = "font-size: 110%;display:inline"),
    tags$p("etc. were all labeled as ", style = "font-size: 110%;display:inline"),
    tags$i(strong("Normal"), style = "font-size: 100%;display:inline"),
    tags$p(".", style = "font-size: 110%;display:inline"),
    br(),
    br(),
    
    tags$p("The pipeline for metadata harmonization is very similar for data from different repositories. The differences 
           are just the ways to download and retrieve the data.", 
           style = "font-size: 110%;"),
    
    tags$p("For sample metadata from the TCGA-PRAD project, the XML files with the clinical information of the patients 
           were downloaded and organized using the R package ", 
           style = "font-size: 110%; display:inline;"),
    
    tags$i(strong("GDCRNATools"),
           style = "font-size: 100%; display:inline;"),
    
    tags$p(". Some clinical features, such as preoperative 
    prostate-specific antigen (PSA) level, which were not available in 
    the Genomic Data Commons (GDC) data portal were retrieved from Broad 
           GDAC Firehose (https://gdac. broadinstitute.org/).",
           style = "font-size: 110%; display:inline;"),
    
    br(),
    br(),
    
    tags$p("The phenotype data of the datasets in GEO was retrieved using our in-house web tool - ",
           style = "font-size: 110%; display:inline;"),
    
    tags$a(href="http://bioinfo.jialab-ucr.org/WebGEO/", "WebGEO", 
           target='_blank', style = "font-size: 110%;display:inline;"),
    
    tags$p("(under development) based on the R package ", style = "font-size: 110%; display:inline;"),
    
    tags$i(strong("GEOquery"),
           style = "font-size: 100%; display:inline;"),
    
    tags$p(", which allows querying and downloading the sample metadata of a dataset in seconds. 
    In case users are interested in downloading phenotype data programmatically using ", 
           style = "font-size: 110%; display:inline;"),
  
    tags$i(strong("GEOquery"),
         style = "font-size: 100%; display:inline;"),
    
    tags$p(", the script is also provided below", style = "font-size: 110%; display:inline;"),
    
    br(),
    br(),
    
    tags$p("Most of the SRA datasets (14 out of 17) also have corresponding GEO accessions, 
           thus the sample metadata for these datasets were also retrieved from GEO using ", style = "font-size: 110%; display:inline;"),
    
    tags$a(href="http://bioinfo.jialab-ucr.org/WebGEO/", "WebGEO", 
           target='_blank', style = "font-size: 110%;display:inline;"),
    
    tags$p(". For the 3 remaining SRA datasets that do not have corresponding GEO accessions, 
           the metadata (SraRunTable) were downloaded directly from SRA.",
           style = "font-size: 110%; display:inline;"),
    
    tags$p("Most of the SRA datasets (14 out of 17) also have corresponding GEO accessions, 
           thus the sample metadata for these datasets were also retreived from GEO using ", style = "font-size: 110%; display:inline;"),
    
    tags$a(href="http://bioinfo.jialab-ucr.org/WebGEO/", "WebGEO", 
           target='_blank', style = "font-size: 110%;display:inline;"),
    
    tags$p(". For the 3 remaining SRA datasets that do not have corresponding GEO accessions, 
           the metadata (SraRunTable) were downloaded directly from SRA.",
           style = "font-size: 110%; display:inline;"),
    
    br(),
    br(),
    
    tags$p("Clinical data for the 6 cBioPortal RNAseq datasets were manually downloaded from the portal, and sample 
           metadata (.sdrf.txt file) for the 4 ArrayExpress datasets were downloaded using ",
           style = "font-size: 110%;display:inline;"),
    tags$i("wget.", style = "font-size: 110%;display:inline;"),
    br(),
    br(),
    
    tags$p("In addition, the original publications were carefully reviewed to 
    further ensure the accuracy and comprehensiveness of the sample metadata for each dataset in PCaDB.",
           style = "font-size: 110%;"),
    
    tags$hr(style="border-top: 1px dashed #A9A9A9"),
    
    tags$iframe(src = 'rmd/metadata.html', 
                width = '100%', height = '400px', 
                frameborder = 0, scrolling = 'auto')
    
  ),
  
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('Single-cell RNAseq data'), align='left'),
    
    tags$p("The Seurat object of the single-cell RNAseq data can be directly downloaded from ",
           style = "font-size: 110%; display:inline;"),
    
    tags$a(href="https://www.gudmap.org/chaise/record/#2/RNASeq:Study/RID=W-RAHW", "GUDMAP", 
           target='_blank', style = "font-size: 110%;display:inline;"),
    
    tags$p(". The normalized gene 
           expression matrix,  cell type annotation, as well as the TSNE and UMAP coordiantes 
           were included in an R list object in the PCaDB database.",
           style = "font-size: 110%;display:inline;"),
    
    tags$p("For details about the scRNAseq data analysis, such as data processing, 
           data normalization, tSNE and UMAP analyses, etc., please refer to the original study [3].",
           style = "font-size: 110%;display:inline;"),
    
    br(),
    
    tags$hr(style="border-top: 1px dashed #A9A9A9"),
    
    tags$iframe(src = 'rmd/scrnaseq.html', 
                width = '100%', height = '400px', 
                frameborder = 0, scrolling = 'auto')
    
    
  ),
  
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('Gene annotation'), align='left'),
    
    tags$p("Gene annotation information were downloaded from the official websites/FTPs of HGNC [17], 
           Ensembl [18], NCBI [19], and GENCODE [8], and a comprehensive pipeline was created to map the gene IDs 
           from different resources to facilitate the query of individual genes and the cross-dataset anlaysis. 
           The harmonized gene annotation data has been deposited in PCaDB and can be easily downloaded on the ",
           style = "font-size: 110%;display:inline;"),
           
    tags$p(strong("Download"), style = "font-size: 100%;display:inline;"),
           
    tags$p(" page (PCaDB_Gene_Annotation.RDS).", 
           style = "font-size: 110%;display:inline;"),
    
    tags$hr(style="border-top: 1px dashed #A9A9A9"),
    
    tags$iframe(src = 'rmd/gene_annotation.html', 
                width = '100%', height = '400px', 
                frameborder = 0, scrolling = 'auto')
    
  ),
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    #tags$hr(style="border-top: 1px dashed #A9A9A9"),
    
    h5(strong('References'), align='left'),
    
    tags$p("[1] Robinson, M.D., McCarthy, D.J. and Smyth, G.K. (2010). edgeR: a Bioconductor package for differential 
           expression analysis of digital gene expression data. Bioinformatics, 26(1), 139-140.",
           style = "font-size: 100%;"),
    tags$p("[2] Carvalho, B.S. and Irizarry, R.A. (2010). A framework for oligonucleotide microarray preprocessing. 
           Bioinformatics, 23(14), 1846-1847.",
           style = "font-size: 100%;"),
    tags$p("[3] Henry, G.H., Malewska, A., Joseph, D.B., Malladi, V.S., Lee, J., Torrealba, J., Mauck, R.J., 
           Gahan, J.C., Raj, G.V., Roehrborn, C.G., Hon, G.C., MacConmara, M.P., Reese, J.C., Hutchinson, R.C., 
           Vezina, C.M. and Strand, D.W. (2018). 
           A Cellular Anatomy of the Normal Adult Human Prostate and Prostatic Urethra. Cell reports, 25(12), 3530-3542.",
           style = "font-size: 100%;"),
    tags$p("[4] Li, R., Zhu, J., Zhong, W.-D., and Jia, Z. (2021) 
           Comprehensive evaluation of machine learning models and gene expression signatures for 
           prostate cancer prognosis using large population cohorts. bioRxiv. https://doi.org/10.1101/2021.07.02.450975",
           style = "font-size: 100%;"),
    tags$p("[5] SRA Toolkit Development Team. Available online at: https://trace.ncbi.nlm.nih.gov/Traces/sra/sra.cgi?view=software",
           style = "font-size: 100%;"),
    tags$p("[6] Andrews, S. (2010). FastQC: a quality control tool for high throughput sequence data. 
           Available online at: http://www.bioinformatics.babraham.ac.uk/projects/fastqc",
           style = "font-size: 100%;"),
    tags$p("[7] Dobin, A., Davis, C.A., Schlesinger, F., Drenkow, J., Zaleski, C., Jha, S., Batut, P., 
           Chaisson, M. and Gingeras, T.R., (2013). STAR: ultrafast universal RNA-seq aligner. 
           Bioinformatics, 29(1), pp.15-21.",
           style = "font-size: 100%;"),
    tags$p("[8] Frankish, A., Diekhans, M., Jungreis, I., Lagarde, J., Loveland, J.E., Mudge, J.M., 
           Sisu, C., Wright, J.C., Armstrong, J., Barnes, I. and Berry, A., (2021). GENCODE 2021. Nucleic acids research, 49(D1), pp.D916-D923.",
           style = "font-size: 100%;"),
    tags$p("[9] Li, H., Handsaker, B., Wysoker, A., Fennell, T., Ruan, J., Homer, N., Marth, G., Abecasis, G. and Durbin, R., (2009). 
           The sequence alignment/map format and SAMtools. Bioinformatics, 25(16), pp.2078-2079.",
           style = "font-size: 100%;"),
    tags$p("[10] Liao, Y., Smyth, G.K. and Shi, W., (2014). featureCounts: an efficient general purpose program 
           for assigning sequence reads to genomic features. Bioinformatics, 30(7), pp.923-930.",
           style = "font-size: 100%;"),
    tags$p("[11] Graubert, A., Aguet, F., Ravi, A., Ardlie, K.G. and Getz, G., (2021). RNA-SeQC 2: 
           efficient RNA-seq quality control and quantification for large cohorts. Bioinformatics.",
           style = "font-size: 100%;"),
    tags$p("[12] Ewels, P., Magnusson, M., Lundin, S. and Käller, M., (2016). MultiQC: summarize analysis results for multiple tools 
           and samples in a single report. Bioinformatics, 32(19), pp.3047-3048.",
           style = "font-size: 100%;"),
    tags$p("[13] Anders, S., Pyl, P.T. and Huber, W., (2015). HTSeq—a Python framework to work with high-throughput sequencing data. bioinformatics, 31(2), pp.166-169.",
           style = "font-size: 100%;"),
    tags$p("[14] Li, R., Qu, H., Wang, S., Wei, J., Zhang, L., Ma, R., Lu, J., Zhu, J., Zhong, W.-D. and Jia, Z. (2018). 
           GDCRNATools: an R/Bioconductor package for integrative analysis of lncRNA, miRNA and mRNA data in GDC. 
           Bioinformatics, 34(14), 2515-2517.",
           style = "font-size: 100%;"),
    tags$p("[15] Davis, S. and Meltzer, P.S. (2007). GEOquery: a bridge between the Gene Expression Omnibus (GEO) and 
           BioConductor. Bioinformatics, 23(14), 1846-1847.",
           style = "font-size: 100%;"),
    tags$p("[16] Dai, M., Wang, P., Boyd, A.D., Kostov, G., Athey, B., Jones, E.G., Bunney, W.E., 
           Myers, R.M., Speed, T.P., Akil, H. and Watson, S.J., (2005). Evolving gene/transcript 
           definitions significantly alter the interpretation of GeneChip data. 
           Nucleic acids research, 33(20), pp.e175-e175.",
           style = "font-size: 100%;"),
    tags$p("[17] Tweedie, S., Braschi, B., Gray, K., Jones, T.E., Seal, R.L., Yates, B. and Bruford, E.A., 
           (2021). Genenames. org: the HGNC and VGNC resources in 2021. Nucleic Acids Research, 49(D1), pp.D939-D946.",
           style = "font-size: 100%;"),
    tags$p("[18] Howe, K.L., Achuthan, P., Allen, J., Allen, J., Alvarez-Jarreta, J., Amode, M.R., 
           Armean, I.M., Azov, A.G., Bennett, R., Bhai, J. and Billis, K., (2021). Ensembl 2021. 
           Nucleic acids research, 49(D1), pp.D884-D891.",
           style = "font-size: 100%;"),
    tags$p("[19] Sayers, E.W., Beck, J., Bolton, E.E., Bourexis, D., Brister, J.R., Canese, K., 
           Comeau, D.C., Funk, K., Kim, S., Klimke, W. and Marchler-Bauer, A., (2021). 
           Database resources of the national center for biotechnology information. 
           Nucleic acids research, 49(D1), p.D10.",
           style = "font-size: 100%;"),
  )#,
  
  # box(
  #   title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
  #   width = 12,
  #   
  #   h4(strong('Prognostic signature evaluation'), align='left')
  #   
  #   
  #   
  # ),
  # 
  # box(
  #   title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
  #   width = 12,
  #   
  #   h4(strong('Transcriptomics data analysis'), align='left'),
  #   
  #   tags$hr(style="border-top: 1px dashed #A9A9A9"),
  #   
  #   h5(strong('Dimensionality reduction'), align='left'),
  #   
  #   tags$hr(style="border-top: 1px dashed #A9A9A9"),
  #   
  #   h5(strong('Differential expression analysis'), align='left'),
  #   
  #   
  #   tags$hr(style="border-top: 1px dashed #A9A9A9"),
  #   
  #   h5(strong('Survival analysis'), align='left'),
  #   
  #   
  #   tags$hr(style="border-top: 1px dashed #A9A9A9"),
  #   
  #   h5(strong('Prognostic model'), align='left')
  #   
  #   
  # )
  
  
)




### Download

tab_download <- fluidRow(
  
  box(
    title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
    width = 12, height = 55,
    
    #h4(strong("Download"), align='center', style='font-family:Georgia;color:#2C3E50')
    h4(strong(icon("download"), HTML('&nbsp;'), "Download"), align='center', style='font-family:Georgia;color:#2C3E50')
  ),
  
  box(
    title = strong('Transcriptome Datasets'), status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    column(12, 
           h6(strong("Summary"), align='left', style='color:black'),
           tags$li(a(href='downloads/PCaDB_Transcriptome_Datasets_Summary.xlsx', target='_blank', 
                     'PCaDB_Transcriptome_Datasets_Summary.xlsx')),
           br(),
           h6(strong("ExpressionSet"), align='left', style='color:black')
    ),
    
    column(3,
           tags$li(a(href='downloads/TCGA-PRAD_eSet.RDS', target='_blank', 'TCGA-PRAD')),
           tags$li(a(href='downloads/CPC-Gene_eSet.RDS', target='_blank', 'CPC-Gene/GSE107299')),
           tags$li(a(href='downloads/Taylor_eSet.RDS', target='_blank', 'Taylor/GSE21034')),
           tags$li(a(href='downloads/DKFZ_eSet.RDS', target='_blank', 'DKFZ/EGAS00001002923')),
           tags$li(a(href='downloads/GSE54460_eSet.RDS', target='_blank', 'GSE54460/SRP036848')),
           tags$li(a(href='downloads/Cambridge_eSet.RDS', target='_blank', 'Cambridge/GSE70768')),
           tags$li(a(href='downloads/Stockholm_eSet.RDS', target='_blank', 'Stockholm/GSE70769')),
           tags$li(a(href='downloads/CancerMap_eSet.RDS', target='_blank', 'CancerMap/GSE94767')),
           tags$li(a(href='downloads/CIT_eSet.RDS', target='_blank', 'CIT/E-MTAB-6128')),
           tags$li(a(href='downloads/Belfast_eSet.RDS', target='_blank', 'Belfast/GSE116918')),
           tags$li(a(href='downloads/GSE25136_eSet.RDS', target='_blank', 'GSE25136')),
           tags$li(a(href='downloads/GSE41408_eSet.RDS', target='_blank', 'GSE41408')),
           tags$li(a(href='downloads/GSE46691_eSet.RDS', target='_blank', 'GSE46691')),
           tags$li(a(href='downloads/GSE51066_eSet.RDS', target='_blank', 'GSE51066')),
           tags$li(a(href='downloads/GSE37199_eSet.RDS', target='_blank', 'GSE37199')),
           tags$li(a(href='downloads/GSE44353_eSet.RDS', target='_blank', 'GSE44353')),
           tags$li(a(href='downloads/GSE59745_eSet.RDS', target='_blank', 'GSE59745')),
           tags$li(a(href='downloads/GSE79021_eSet.RDS', target='_blank', 'GSE79021')),
           tags$li(a(href='downloads/GSE3933-GPL2695_eSet.RDS', target='_blank', 'GSE3933-GPL2695')),
           tags$li(a(href='downloads/GSE35988-GPL6480_eSet.RDS', target='_blank', 'GSE35988-GPL6480'))
    ),
    column(3, 
           tags$li(a(href='downloads/GSE35988-GPL6848_eSet.RDS', target='_blank', 'GSE35988-GPL6848')),
           tags$li(a(href='downloads/SU2C-PCF-2019-Capture_eSet.RDS', target='_blank', 'SU2C-PCF-2019-Capture')),
           tags$li(a(href='downloads/SU2C-PCF-2019-PolyA_eSet.RDS', target='_blank', 'SU2C-PCF-2019-PolyA')),
           tags$li(a(href='downloads/GSE62116_eSet.RDS', target='_blank', 'GSE62116')),
           tags$li(a(href='downloads/GSE62667_eSet.RDS', target='_blank', 'GSE62667')),
           tags$li(a(href='downloads/GSE79957_eSet.RDS', target='_blank', 'GSE79957')),
           tags$li(a(href='downloads/GSE79956_eSet.RDS', target='_blank', 'GSE79956')),
           tags$li(a(href='downloads/GSE79958_eSet.RDS', target='_blank', 'GSE79958')),
           tags$li(a(href='downloads/GSE72291_eSet.RDS', target='_blank', 'GSE72291')),
           tags$li(a(href='downloads/GSE79915_eSet.RDS', target='_blank', 'GSE79915')),
           tags$li(a(href='downloads/GSE3325_eSet.RDS', target='_blank', 'GSE3325')),
           tags$li(a(href='downloads/GSE32269_eSet.RDS', target='_blank', 'GSE32269')),
           tags$li(a(href='downloads/GSE6919-GPL8300_eSet.RDS', target='_blank', 'GSE6919-GPL8300')),
           tags$li(a(href='downloads/GSE6919-GPL92_eSet.RDS', target='_blank', 'GSE6919-GPL92')),
           tags$li(a(href='downloads/GSE6919-GPL93_eSet.RDS', target='_blank', 'GSE6919-GPL93')),
           tags$li(a(href='downloads/GSE6752_eSet.RDS', target='_blank', 'GSE6752')),
           tags$li(a(href='downloads/GSE17951_eSet.RDS', target='_blank', 'GSE17951')),
           tags$li(a(href='downloads/GSE8218_eSet.RDS', target='_blank', 'GSE8218')),
           tags$li(a(href='downloads/E-TABM-26-U133A_eSet.RDS', target='_blank', 'E-TABM-26-U133A'))
    ),
    column(3, 
           tags$li(a(href='downloads/E-TABM-26-U133B_eSet.RDS', target='_blank', 'E-TABM-26-U133B')),
           tags$li(a(href='downloads/GSE5132-GPL3834_eSet.RDS', target='_blank', 'GSE5132-GPL3834')),
           tags$li(a(href='downloads/GSE29079_eSet.RDS', target='_blank', 'GSE29079')),
           tags$li(a(href='downloads/GSE97284_eSet.RDS', target='_blank', 'GSE97284')),
           tags$li(a(href='downloads/GSE85698_eSet.RDS', target='_blank', 'GSE85698')),
           tags$li(a(href='downloads/GSE2109_eSet.RDS', target='_blank', 'GSE2109')),
           tags$li(a(href='downloads/GSE62872_eSet.RDS', target='_blank', 'GSE62872')),
           tags$li(a(href='downloads/SMMU_eSet.RDS', target='_blank', 'SMMU')),
           tags$li(a(href='downloads/GSE77930_eSet.RDS', target='_blank', 'GSE77930')),
           tags$li(a(href='downloads/Neuroendocrine_eSet.RDS', target='_blank', 'Neuroendocrine')),
           tags$li(a(href='downloads/Broad-Cornell_eSet.RDS', target='_blank', 'Broad-Cornell')),
           
           tags$li(a(href='downloads/GSE111177_eSet.RDS', target='_blank', 'GSE111177/SRP133573')),
           tags$li(a(href='downloads/GSE120741_eSet.RDS', target='_blank', 'GSE120741/SRP163173')),
           tags$li(a(href='downloads/ERP006077_eSet.RDS', target='_blank', 'ERP006077')),
           tags$li(a(href='downloads/SRP119917_eSet.RDS', target='_blank', 'SRP119917')),
           tags$li(a(href='downloads/GSE104131_eSet.RDS', target='_blank', 'GSE104131/SRP118614')),
           tags$li(a(href='downloads/GSE133626_eSet.RDS', target='_blank', 'GSE133626/SRP212704')),
           tags$li(a(href='downloads/GSE80609_eSet.RDS', target='_blank', 'GSE80609/SRP073789')),
           tags$li(a(href='downloads/GSE118435_eSet.RDS', target='_blank', 'GSE118435/SRP157215'))

    ),
    
    column(3,
           tags$li(a(href='downloads/SRP151104_eSet.RDS', target='_blank', 'SRP151104')),
           tags$li(a(href='downloads/GSE22260_eSet.RDS', target='_blank', 'GSE22260/SRP002628')),
           tags$li(a(href='downloads/GSE114740_eSet.RDS', target='_blank', 'GSE114740/SRP148500')),
           tags$li(a(href='downloads/E-MTAB-5021_eSet.RDS', target='_blank', 'E-MTAB-5021/ERP017433')),
           tags$li(a(href='downloads/GSE95369_eSet.RDS', target='_blank', 'GSE95369/SRP100706')),
           tags$li(a(href='downloads/GSE48403_eSet.RDS', target='_blank', 'GSE48403/SRP026387')),
           tags$li(a(href='downloads/GSE51005_eSet.RDS', target='_blank', 'GSE51005/SRP030027')),
           tags$li(a(href='downloads/GSE126078_eSet.RDS', target='_blank', 'GSE126078/SRP183532')),
           tags$li(a(href='downloads/GSE12378_eSet.RDS', target='_blank', 'GSE12378')),
           tags$li(a(href='downloads/GSE2443_eSet.RDS', target='_blank', 'GSE2443')),
           tags$li(a(href='downloads/GSE26910_eSet.RDS', target='_blank', 'GSE26910')),
           
           tags$li(a(href='downloads/GSE28680_eSet.RDS', target='_blank', 'GSE28680')),
           tags$li(a(href='downloads/GSE29650_eSet.RDS', target='_blank', 'GSE29650')),
           tags$li(a(href='downloads/GSE30521_eSet.RDS', target='_blank', 'GSE30521')),
           tags$li(a(href='downloads/GSE32448_eSet.RDS', target='_blank', 'GSE32448')),
           tags$li(a(href='downloads/GSE32571_eSet.RDS', target='_blank', 'GSE32571')),
           tags$li(a(href='downloads/GSE6956_eSet.RDS', target='_blank', 'GSE6956')),
           tags$li(a(href='downloads/GSE7055_eSet.RDS', target='_blank', 'GSE7055')),
           tags$li(a(href='downloads/GSE38241_eSet.RDS', target='_blank', 'GSE38241'))
    )
  ),
  
  box(
    title = strong('Prognostic Signatures'), status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    column(12, 
           h6(strong("Summary"), align='left', style='color:black'),
           tags$li(a(href='downloads/PCaDB_Prognostic_Signatures_Summary.xlsx', target='_blank', 
                     'PCaDB_Prognostic_Signatures_Summary.xlsx')),
           br()),
    
    column(12, 
           h6(strong("Gene List"), align='left', style='color:black'),
           tags$li(a(href='downloads/PCaDB_Prognostic_Signatures_Gene_List.xlsx', target='_blank', 
                     'PCaDB_Prognostic_Signatures_Gene_List.xlsx')))
  ),
  
  box(
    title = strong('Gene Annotation'), status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    column(12, 
           tags$li(a(href='downloads/PCaDB_Gene_Annotation.RDS', target='_blank', 
                     'PCaDB_Gene_Annotation.RDS')))
    
  )
  
)








### Tutorial

tab_tutorial <- fluidRow(
  
  box(
    title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
    width = 12, height = 55,
    
    #h4(strong("Tutorial"), align='center', style='font-family:Georgia;color:#2C3E50')
    h4(strong(icon("file-alt"), HTML('&nbsp;'), "Tutorial"), align='center', style='font-family:Georgia;color:#2C3E50')
  ),
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('Details about the tutorial'), align='left'),
    
    #tags$ul(tags$li('Overview', style="font-size:120%;")),
    # tags$li('Data collection', style="font-size:120%;"),
    tags$ul(tags$li('Query a gene of interest', style="font-size:120%;")),
    tags$ul(tags$li('Prognostic signature evaluation', style="font-size:120%;")),
    tags$ul(tags$li('Transcriptomics data analysis', style="font-size:120%;")),
    tags$ul(tags$li('Data download', style="font-size:120%;"))
  ),
  
  # box(
  #   title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
  #   width = 12,
  #   
  #   h4(strong('Overview'), align='left'),
  #   
  #   tags$p("PCaDB provides a user-friendly interface for the comprehensive analysis 
  #          of individual genes, prognostic signatures, and the whole transcriptomes.",
  #          style = "font-size: 110%;"),
  #   
  #   tags$p("When querying a gene of interest in PCaDB", 
  #          style = "font-size: 110%;"),
  #   ),
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,

    h4(strong('Query a gene of interest'), align='left'),
    
    h5(strong('1.1. Overview'), align='left'),

    tags$p("Users can query a gene of interest by", strong("typing"), "the Ensembl ID, 
           Entrez ID, or HGNC approved symbol and alias symbol in the 
           'Search a gene' field and", strong("selecting"), "the gene from the dropdown list. 
           The general information about the gene and some useful external 
           links to the databases such as ENSEMBL, HGNC, and NCBI for more 
           detailed description of the gene, Genotype-Tissue Expression (GTEx) 
           and Human Protein Atlas (HPA) for the gene expression pattern 
           in different human tissues, and Kyoto Encyclopedia of Genes and 
           Genomes (KEGG) for the pathways that the gene involves in are 
           provided (Figure 1-1).",
           style = "font-size: 110%;"),
    
    tags$p("A suite of advanced analyses and visualizations can be interactively 
           performed for the selected gene, including:",
           style = "font-size: 110%;"),

    tags$p("(1) Gene expression in different sample types",
           style = "font-size: 110%;"),
    
    tags$p("(2) Survival analysis of relapse-free survival (RFS)",
           style = "font-size: 110%;"),
    
    tags$p("(3) Gene expression analysis at the single-cell level",
           style = "font-size: 110%;"),
    
    br(),
    
    tags$img(src='img/tutorial_query_overview.png', width=900),
    
    br(),
    h5(strong('Figure 1-1. Query a gene of interest'), align='center'),
    
    br(),
    
    ###
    h5(strong('1.2. Gene expression in different sample types'), align='left'),
    tags$p("Users can select a dataset from the dropdown list and a boxplot 
           will be generated to visualize the gene expression levels in 
           different samples, such as primary tumor, tumor-adjacent normal, 
           or metastatic tumor samples, depending on the availability of the 
           data in the selected dataset (Figure 1-2)",
           style = "font-size: 110%;"),
    tags$p("The pipelines used for gene expression data normalization have been described 
           in the 'PCaDB Pipeline' tab. In general, (i) for RNAseq data from TCGA 
           and SRA, the counts per million mapped reads (CPM) vaules are used; 
           (ii) for microarray data from GEO and ArrayExpress, the normalized intensity values are used; 
           and (iii) for the normalized RNAseq data downloaded directly from cBioPortal, 
           the fragments/reads per kilobase of transcript per million mapped reads (FPKM/RPKM) 
           values are used. All the gene expression values are in log2 scale. 
           In PCaDB, the Ensembl gene identifiers are used for all the datasets. 
           If multiple probes/genes matched to the same Ensembl ID, only the most informative one with 
           the maximum interquartile range (IQR) for the gene expression is used for this Ensembl ID.", 
           style = "font-size: 110%;"),
    br(),
    tags$img(src='img/tutorial_query_expression.jpg', width=900),
    br(),
    h5(strong('Figure 1-2. Gene expression in different sample types'), align='center'),
    br(),
    
    ###
    h5(strong('1.3. Survival analysis of relapse-free survival (RFS)'), align='left'),
    tags$p("Kaplan Meier (KM) survival analysis of relapse-free survival (RFS) for the selected gene can be 
           performed in the 10 datasets with 1,558 primary tumor samples from 
           PCa patients with the data of biochemical recurrence (BCR) status and follow 
           up time after definitive treatment.",
           style = "font-size: 110%;"),
    
    tags$p("The patients in each dataset are dichotomized into 
           low- and high-expression groups based on the median expression 
           value of the selected gene. KM analysis is performed using the 
           R package ", style = "font-size: 110%;display:inline;"),
           
    tags$i(strong("survival"), style = "font-size: 110%;display:inline;"),
          
    tags$p("to estimate the hazard ratio (HR) and 95% confidence intervals (CIs). 
           The log-rank test is used to compare the two survival curves and calculate the 
           p value.",
           style = "font-size: 110%;display:inline;"),
    br(),
    br(),
    
    tags$p("A forest plot with the result from the survival analysis, including 
           the numbers of samples, hazard ratios (HRs), 95% confidence intervals 
           (CIs), and p values across all the datasets, will be generated. 
           The KM survival curve for each dataset will also be plotted (Figure 1-3).",
           style = "font-size: 110%;"),
    br(),
    tags$img(src='img/tutorial_query_survival.jpg', width=900),
    br(),
    h5(strong('Figure 1-3. Forest plot and KM survival curves'), align='center'),
    br(),
    
    ###
    h5(strong('1.4. Gene expression analysis at the single-cell level'), align='left'),
    tags$p("The expression pattern of the selected gene in different cell types 
           from normal human prostate, including basal, luminal, 
           neuroendocrine (NE), club, and hillock epithelia, endothelia, 
           leukocyte, fibroblast, and smooth muscle, can be visualized using 
           the uniform manifold approximation and projection (UMAP) plot 
           and t-distributed stochastic neighbor embedding (t-SNE) plot, 
           violin plot, and bubble plot with the average gene expression 
           and percent of cells expressed for each cell type (Figure 1-4). The 
           data used in this module was downloaded directly from 
           GUDMAP (https://www.gudmap.org/chaise/record/#2/RNASeq:Study/RID=W-RAHW).",
           style = "font-size: 110%;"),
    
    tags$p("For details about the scRNAseq data analysis, such as data processing, 
           data normalization, tSNE and UMAP analyses, etc., please refer to the orignial study: ",
           style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("Henry, Gervaise H., et al. A cellular anatomy of the normal adult 
                  human prostate and prostatic urethra. Cell reports 25.12 (2018): 3530-3542"), style = "font-size: 100%;display:inline;"),
    
    br(),
    br(),
    tags$img(src='img/tutorial_query_scRNAseq.jpg', width=900),
    br(),
    h5(strong('Figure 1-4. tSNE plot, UMAP plot, bubble plot, and violin plot 
              to visualize the gene expression pattern at the single-cell level'), 
       align='left'),
    br()
    
    
    
    ),
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('Prognostic signature evaluation'), align='left'),
    
    h5(strong('2.1. Overview'), align='left'),
    
    tags$p("A comprehensive evaluation of the prognostic performances 
           of 30 published signatures was performed in a previous study 
           and we included all those signatures in the PCaDB database, 
           allowing for a more detailed characterization of the signatures 
           (Figure 2-1), including:",
           style = "font-size: 110%;"),
    
    tags$p("(1) List of signature genes",
           style = "font-size: 110%;"),
    
    tags$p("(2) Differential expression analysis of the signature genes",
           style = "font-size: 110%;"),
    
    tags$p("(3) KM Survival analysis of RFS for the signature genes",
           style = "font-size: 110%;"),
    
    tags$p("(4) Pathway analysis of the signature genes",
           style = "font-size: 110%;"),
    
    tags$p("(5) Evaluation of the performances of the prognostic signatures",
           style = "font-size: 110%;"),
    
    br(),
    
    tags$img(src='img/tutorial_signature_overview.png', width=900),
    
    br(),
    h5(strong('Figure 2-1. Overview of modules for prognostic signature evaluation'), 
       align='center'),
    br(),
    
    ###
    h5(strong('2.2. List of signature genes'), align='left'),
    tags$p("The list of genes in a prognositc signature can be viewed by 
           selecting the signature from the dropdown list. A comprehensive 
           pipeline was created to convert the gene IDs/symbols reported in 
           the orignial publication to the Ensembl IDs and HGNC gene symbols. 
           We also did a manual inspection very carefully to double check the 
           ambiguous signature genes.",
           style = "font-size: 110%;"),
    br(),
    tags$img(src='img/tutorial_signature_gene_list.jpg', width=900),
    br(),
    h5(strong('Figure 2-2. Th list of genes in the selected signature'), 
       align='center'),
    br(),
    
    ###
    h5(strong('2.3. Differential expression analysis of the signature genes'), align='left'),
    tags$p("The DE analysis of all signature genes between primary tumor and 
           tumor-adjacent normal samples can be performed using the R package ",
           style = "font-size: 110%;display:inline;"),
    tags$i(strong("limma"), style = "font-size: 110%;display:inline;"),
    tags$p(" in multiple datasets with certain sample types. The normalized gene 
           expression data in log2 scale is used as input. i.e., for RNAseq data from TCGA and SRA, 
           the raw count data is normalized using the Trimmed Mean of Mvalues (TMM) method 
           implemented in the R package ", style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("edgeR"), style = "font-size: 110%;display:inline;"),
    tags$p(", followed by ", style = "font-size: 110%;display:inline;"),
    tags$i("voom", style = "font-size: 110%;display:inline;"),
    tags$p(" transformation using ", style = "font-size: 110%;display:inline;"),
    tags$i(strong("limma"), style = "font-size: 110%;display:inline;"),
    tags$p(", which is more powerful especially when the library sizes are quite 
           variable between samples. 
           Lowly-expressed genes (logCPM < 0 in more than 50% of samples) 
           are filtered out before any downstream analysis. 
           A linear model will then be fitted to estimate the fold changes and 
           standard errors for each gene, followed by empirical Bayes smoothing. 
           The differential expression analysis for Affymetrix microarray data is very similar to 
           that for RNAseq data, except that the Robust Multichip Average (RMA) normalized intensity in log2 scale 
           is fitted to the linear model. For some other datasets, the normalized data provided by the 
           original studies are used as input for differential expression analysis.", 
           style = "font-size: 110%;display:inline;"),
    
    br(),
    br(),
    tags$p("A volcano plot will be generated to visualize the DE analysis result. 
           Only differentially expressed genes (DEGs) in >= 3 signatures are 
           labeled in the volcano plot. 
           A data table will also be created to list the DEGs. The log2(fold change), 
           BH-adjusted p value (FDR), the number of signatures, and the names of 
           the signatures are reported in the table (Figure 2-3).",
           style = "font-size: 110%;display:inline;"),
    br(),
    br(),
    tags$img(src='img/tutorial_signature_differential_expression.jpg', width=900),
    br(),
    h5(strong('Figure 2-3. Differential expression analysis of the signature genes'), 
       align='center'),
    br(),
    
    ###
    h5(strong('2.4. KM Survival analysis of RFS for the signature genes'), align='left'),

    tags$p("Kaplan Meier (KM) survival analysis of relapse-free survival (RFS) for the signature genes can be 
           performed in the 10 datasets with 1,558 primary tumor samples from 
           PCa patients with the data of biochemical recurrence (BCR) status and follow 
           up time after definitive treatment.",
           style = "font-size: 110%;"),
    
    tags$p("The patients in each dataset are dichotomized into 
           low- and high-expression groups based on the median expression 
           value of the signature gene. KM analysis is performed using the 
           R package ", style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("survival"), style = "font-size: 110%;display:inline;"),
    
    tags$p("to estimate the hazard ratio (HR) and 95% confidence intervals (CIs). 
           The log-rank test is used to compare the two survival curves and calculate the 
           p value.",
           style = "font-size: 110%;display:inline;"),
    br(),
    br(),
    
    tags$p("A forest plot for the common genes in 3 or more signatures 
           and a data table with the survival analysis result for 
           all the signature genes are generated. In the data table, the 
           hazard ratios (HRs), 95% confidence intervals (CIs), nominal p values, and 
           BH-adjusted p values (FDRs), as well as the number of signatures and names of the 
           signatures are reported (Figure 2-4).",
           style = "font-size: 110%;"),
    
    br(),
    tags$img(src='img/tutorial_signature_survival.jpg', width=900),
    br(),
    h5(strong('Figure 2-4. KM Survival analysis of RFS for the signature genes'), 
       align='center'),
    br(),
    
    ###
    h5(strong('2.5. Pathway analysis of the signature genes'), align='left'),
    tags$p("Functional enrichment analysis of the signature genes can be 
           performed using the R package ", 
           style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("clusterProfiler"), 
           style = "font-size: 110%;display:inline;"),
    
    tags$p(" in PCaDB based on hypergeometric test. Users can select a gene 
           list from the dropdown list for the enrichment analysis. 
           A gene list could consist of genes in 
           an individual signature, common genes in at least 2, 3, or 
           4 signatures, and all the 1,042 signature genes. 
           Pathways or Gene sets with < 
           10 genes or > 500 genes are excluded. Benjamini-Hochberg (BH) is used 
           for multiple testing correction.", 
           style = "font-size: 110%;display:inline;"),
    
    br(),
    br(),
    
    tags$p("PCaDB supports functional enrichment analysis with many 
           pathway/ontology knowledgebases, including: ", 
           style = "font-size: 110%;"),
    
    tags$p("(1) KEGG: Kyoto Encyclopedia of Genes and Genomes", style = "font-size: 110%;"),
    tags$p("(2) REACTOME", style = "font-size: 110%;"),
    tags$p("(3) DO: Disease Ontology", style = "font-size: 110%;"),
    tags$p("(4) NCG: Network of Cancer Gene", style = "font-size: 110%;"),
    tags$p("(5) DisGeNET", style = "font-size: 110%;"),
    tags$p("(6) GO-BP: Gene Ontology (Biological Process)", style = "font-size: 110%;"),
    tags$p("(7) GO-CC: Gene Ontology (Cellular Component)", style = "font-size: 110%;"),
    tags$p("(8) GO-MF: Gene Ontology (Molecular Function)", style = "font-size: 110%;"),
    tags$p("(9) MSigDB-H: Molecular Signatures Database (Hallmark)", style = "font-size: 110%;"),
    tags$p("(10) MSigDB-C4: Molecular Signatures Database (CGN: Cancer Gene Neighborhoods)", style = "font-size: 110%;"),
    tags$p("(11) MSigDB-C4: Molecular Signatures Database (CM: Cancer Modules)", style = "font-size: 110%;"),
    tags$p("(12) MSigDB-C6: Molecular Signatures Database (C6: Oncogenic Signature Gene Sets)", style = "font-size: 110%;"),
    #tags$p("(13) MSigDB-C7: Molecular Signatures Database (C7: Immunologic Signature Gene Sets)", style = "font-size: 110%;"),
    
    tags$p("A data table is produced to summarize the significantly enriched 
    pathways/ontologies in descending order based on their significance 
    levels, as well as the number and proportion of enriched genes and the 
    gene symbols in each pathway/ontology term.", style = "font-size: 110%;;display:inline;"),
    
    tags$i("'Count'", style = "font-size: 110%;display:inline;"),
    tags$p("is the number of genes shared between the input gene list and pathway/gene set; ", style = "font-size: 110%;;display:inline;"),
    
    tags$i("'List Total'", style = "font-size: 110%;display:inline;"),
    tags$p("is the total number of genes in the input gene list; ", style = "font-size: 110%;;display:inline;"),
    
    tags$i("'Pop Hits'", style = "font-size: 110%;display:inline;"),
    tags$p("is the number of genes in the pathway/gene set; and ", style = "font-size: 110%;;display:inline;"),
    
    tags$i("'Pop Total'", style = "font-size: 110%;display:inline;"),
    tags$p("is the total number of genes in the pathway/gene set database, e.g., KEGG, GO, and REACTOME, etc.", 
           style = "font-size: 110%;display:inline;"),
    
    br(),
    br(),
    
    tags$p("The top enriched pathways/ontologies (based on the BH-adjusted p value) 
           are visualized using both bar plot and bubble plot.", style = "font-size: 110%;"),
    
    br(),
    tags$img(src='img/tutorial_signature_pathway.jpg', width=900),
    br(),
    h5(strong('Figure 2-5. Pathway analysis of the signature genes'), 
       align='center'),
    br(),
    
    
    ###
    h5(strong('2.6. Evaluation of the performances of the prognostic signatures'), align='left'),
    tags$p("A comprehensive evaluation of the performances of the published prognostic signatures 
           can be performed in PCaDB using different survival analysis algorithms 
           and different training and test datasets.",
           style = "font-size: 110%;"),
    tags$p("If a given signature is selected, a prognostic model can be developed 
           using the expression data of the signature genes in the selected 
           training dataset and the selected survival analysis method. 
           The risk score of each patient in the test datasets will be 
           computed based on the model.",
           style = "font-size: 110%;"),
    tags$p("For the intra-dataset comparison, 10-fold CV was used for model 
           evaluation, whereas for the inter-dataset comparison, 
           one dataset was treated as the training set and the other datasets 
           were used as the test datasets at a time.",
           style = "font-size: 110%;"),
    tags$p("Three metrics including the concordance index (C-index), 
           time-dependent receiver operating characteristics (ROC) curve, 
           and hazard ratio (HR) estimated by the KM survival analysis are 
           used to assess the prognostic power for the signature based on the 
           independent test cohorts. 
           Forest plots are used to visualize the results, while data tables 
           with more detailed results are also provided.",
           style = "font-size: 110%;"),
    
    tags$p("For details about the machine learning algorithms, prognostic model training and testing, 
           as well as the calculation of C index, AUC, and HR, etc., please refer to the orignial study: ",
           style = "font-size: 110%;display:inline;"),

    tags$i(strong("Li R, Zhu J, Zhong W-D, Jia Z. Comprehensive evaluation of machine 
                  learning models and gene expression signatures for prostate cancer 
                  prognosis using large population cohorts. bioRxiv. 2021 (https://doi.org/10.1101/2021.07.02.450975)"), 
           style = "font-size: 100%;display:inline;"),
    
    br(),
    br(),
    tags$img(src='img/tutorial_signature_model_individual.jpg', width=900),
    br(),
    h5(strong('Figure 2-6. Evaluation of the prognostic performances of an individual signature'), 
       align='center'),
    br(),
    
    tags$p("Users can also select ‘All Signatures’ from the dropdown list to perform 
           a comprehensive analysis to compare and rank the prognostic abiligies of 
           all the signatures in each individual test set based on the three metrics.",
           style = "font-size: 110%;"),
    br(),
    tags$img(src='img/tutorial_signature_model_all.jpg', width=900),
    br(),
    h5(strong('Figure 2-7. Evaluation of the prognostic performances of all the signatures'), 
       align='center'),
    br()
    

  ),
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('Transcriptomics data analysis'), align='left'),
    h5(strong('3.1. Overview'), align='left'),
    
    tags$p("More advanced and comprehensive analyses can be performed 
           at the whole-transcriptome level in PCaDB, 
           allowing users to identify DEGs associated with 
           tumor initiation and progression, identify biomarkers 
           associated with clinical outcomes (i.e., BCR), 
           as well as develop and validate gene expression-based 
           signatures and models for PCa prognosis (Figure 3-1). 
           The whole-transcriptome level analysis includes: ",
           style = "font-size: 110%;"),
    
    tags$p("(1) Summary of the dataset",
           style = "font-size: 110%;"),
    
    tags$p("(2) Dimensionality reduction",
           style = "font-size: 110%;"),
    
    tags$p("(3) Differential gene expression analysis",
           style = "font-size: 110%;"),
    
    tags$p("(4) KM and CoxPH survival analyses of RFS (only for the first 10 datasets with the information of time to BCR)",
           style = "font-size: 110%;"),
    
    tags$p("(5) Development and validation of a new prognostic model (only for the first 10 datasets with the information of time to BCR)",
           style = "font-size: 110%;"),
    
    br(),
    
    tags$img(src='img/tutorial_transcriptome_overview.png', width=900),
    
    br(),
    h5(strong('Figure 3-1. Overview of whole-transcriptome data analysis'), align='center'),
    br(),
    
    ###
    h5(strong('3.2. Summary of the dataset'), align='left'),
    tags$p("When a transcriptomics dataset is selected, the summary of the dataset 
           including platform, data processing pipeline, and the available metadata, 
           such as sample type, preoperative PSA, Gleason score, 
           BCR status, and time to BCR, will be displayed automatically (Figure 3-2).",
           style = "font-size: 110%;"),
    br(),
    tags$img(src='img/tutorial_transcriptome_summary.jpg', width=900),
    br(),
    h5(strong('Figure 3-2. Summary of the dataset'), 
       align='center'),
    br(),
    
    ###
    h5(strong('3.3. Dimensionality reduction'), align='left'),
    tags$p("Principal component analysis (PCA) can be performed using the 
           whole transcriptome profiling (lowly-expressed genes with logCPM < 0 
           in 50% of samples are filtered out 
           for RNAseq data) in the selected dataset. 
           The normalized gene expression data is rescaled by z-score standardization 
           and the ", style = "font-size: 110%;display:inline;"),
    tags$i(strong("prcomp"), style = "font-size: 100%;display:inline;"),
    
    tags$p("function in R is used for PCA analysis. The 
           2D or 3D interactive plot based on the first two 
           or three principal components, respectively, 
           will be generated for visualization (Figure 3-3).",
           style = "font-size: 110%;display:inline;"),
    
    br(),
    br(),
    tags$img(src='img/tutorial_transcriptome_pca.jpg', width=900),
    br(),
    h5(strong('Figure 3-3. Principal component analysis'), 
       align='center'),
    br(),
    
    ###
    h5(strong('3.4. Differential gene expression analysis'), align='left'),
    tags$p("The DE analysis using the whole-transcriptome data allows 
           users to identify DEGs associated with tumor 
           initiation or progression by comparing the case and 
           control groups, i.e., primary tumor vs. tumor-adjacent normal, 
           or metastatic tumor vs. primary tumor, etc. 
           The R package", style = "font-size: 110%;display:inline;"),
    tags$i(strong("limma"), style = "font-size: 110%;display:inline;"),
    tags$p(" is used to identify DEGs in PCaDB. The normalized gene 
           expression data in log2 scale is used as input. i.e., for RNAseq data from TCGA and SRA, 
           the raw count data is normalized using the Trimmed Mean of Mvalues (TMM) method 
           implemented in the R package ", style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("edgeR"), style = "font-size: 110%;display:inline;"),
    tags$p(", followed by ", style = "font-size: 110%;display:inline;"),
    tags$i("voom", style = "font-size: 110%;display:inline;"),
    tags$p(" transformation using ", style = "font-size: 110%;display:inline;"),
    tags$i(strong("limma"), style = "font-size: 110%;display:inline;"),
    tags$p(", which is more powerful especially when the library sizes are quite 
           variable between samples. 
           Lowly-expressed genes (logCPM < 0 in more than 50% of samples) 
           are filtered out before any downstream analysis. 
           A linear model will then be fitted to estimate the fold changes and 
           standard errors for each gene, followed by empirical Bayes smoothing. 
           The differential expression analysis for Affymetrix microarray data is very similar to 
           that for RNAseq data, except that the Robust Multichip Average (RMA) normalized intensity in log2 scale 
           is fitted to the linear model. For some other datasets, the normalized data provided by the 
           originial studies are used as input for differential expression analysis. 
           Benjamini-Hochberg (BH) is used for multiple testing correction.", 
           style = "font-size: 110%;display:inline;"),
    
    br(),
    br(),
    
    tags$p("A volcano plot will be generated to visualize the DE analysis result. 
           By default, BH-adjusted p value (FDR) < 0.01 and absolute fold change > 2 
           are used as the thresholds for determining DEGs.
           A data table will also be created to list the DE analysis result. The ensembl id,
           log2(fold change), average expression across all the samples, t statistics, 
           nornimal p value, BH-adjusted p value (FDR), B statistics, gene expression regulation 
           (Up: significantly up-regulated; Down: significantly down-regulated; NS: not significant),
           and gene symbol are reported in the table (Figure 3-4).",
           style = "font-size: 110%;"),
    
    br(),
    tags$img(src='img/tutorial_transcriptome_differential_expression.jpg', width=900),
    br(),
    h5(strong('Figure 3-4. Identification of differentially expressed genes'), 
       align='center'),
    br(),
    
    ###
    h5(strong('3.5. CoxPH and KM survival analyses of RFS'), align='left'),
    tags$p("Both the univariate Cox Proportional-Hazards (CoxPH) and Kaplan Meier (KM) 
    survival analyses of relapse-free survival (RFS) can be 
           performed at the whole-transcriptome level to 
           identify biomarkers associated with clinical outcome of 
           PCa in a selected dataset of interest (Figure 3-5). Note that the 
           survival analysis can only be done in the first 10 datasets that 
           have the BCR data.",
           style = "font-size: 110%;"),
    
    tags$p("For CoxPH regression analysis, the continuous gene expression data, i.e., 
    normalized expression value (in log2 scale) and the censored time to BCR data 
    are modeled to identify 
    genes that are significantly asspciated with RFS, which may be potentially 
    used as prognostic markers. CoxPH analysis is performed using the 
           R package ", style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("survival"), style = "font-size: 110%;display:inline;"),
    
    tags$p(". The hazard ratio (HR), 95% confidence intervals (CIs), and p value from 
           log-rank test for each gene are all reported in a data table (Figure 3-5).",
           style = "font-size: 110%;display:inline;"),
    br(),
    br(),
    
    tags$p("For KM survival analysis, the patients in each dataset are dichotomized into 
           low- and high-expression groups based on the median expression 
           value of the signature gene. KM analysis is also performed using the 
           R package ", style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("survival"), style = "font-size: 110%;display:inline;"),
    
    tags$p("to estimate the hazard ratio (HR) and 95% confidence intervals (CIs). 
           The log-rank test is used to compare the two survival curves and calculate the 
           p value. The KM survival analysis result is also reported in a data table (Figure 3-5).",
           style = "font-size: 110%;display:inline;"),
    
    br(),
    br(),
    
    tags$img(src='img/tutorial_transcriptome_survival.jpg', width=900),
    br(),
    h5(strong('Figure 3-5. Univariate CoxPH and KM survival analyses'), 
       align='center'),
    br(),
    
    tags$p("Multivariate Cox Proportional-Hazards (CoxPH) 
    survival analysis of relapse-free survival (RFS) can also be 
           performed to identify biomarkers associated with BCR by 
           adjusting for some of the most important clinical factors, 
           including age at diagnosis, preoperative PSA, Gleason score, and 
           clinical T stage. The clinical factors that are included in 
           the multivariate CoxPH model for each dataset merely depend on the availablity of 
           the data, which has been summarized in a table below (Figure 3-6). Please be 
           noted that not all the samples in the selected dataset are used in the 
           multivariate CoxPH survival analysis because of the existing of missing data for the 
           four cliical factors.",
           style = "font-size: 110%;"),
    
    tags$img(src='img/tutorial_transcriptome_survival_coxph_multi.jpg', width=900),
    br(),
    h5(strong('Figure 3-6. Multivariate CoxPH survival analysis'), 
       align='center'),
    
    br(),
    
    
    ###
    h5(strong('3.6. Development and validation of a new prognostic model'), align='left'),
    tags$p("In PCaDB, an advanced functional module is provided, allowing 
    users to provide a list of genes (e.g., genes that are significantly associated 
    with the endpoint based on univiarate and/or multivariate CoxPH survival analysis), 
    and select a survival 
           analysis method, including multivariate CoxPH, Cox model regularized 
           with lasso penalty (Cox-Lasso), or ridge penalty (Cox-Ridge), to develop 
           and independently validate a prognostic model using the 10 datasets with the time to 
           biochemical recurrence (BCR) data.", style = "font-size: 110%;"),
    
    tags$p("The prognostic model is trained using the selected dataset based on the selected 
           survival analysis method. The R package ", style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("survival"), style = "font-size: 100%;display:inline;"),
    
    tags$p(" is used to build the CoxPH model, and the R package ", style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("glmnet"), style = "font-size: 100%;display:inline;"),
    
    tags$p(" is adapted to build the Cox-Lasso and Cox-Ridge models with the penalties 
           alpha (α) = 1 and alpha (α) = 0, respectively. Cox-Lasso uses L1 regularization technique, which 
           adds “absolute value of magnitude” of coefficient as penalty term to the loss function,
           whereas Cox-Ridge uses L2 regularization technique that adds “quared magnitude” of 
           coefficient as penalty term to the loss function.
           The major difference between Cox-Ridge and Cox-Lasso regression is that 
           Cox-Lasso tends to shrink coefficients of the insignificant features to absolute zero, 
           while Cox-Ridge never sets the coefficients to absolute zero.
           When training a Cox-Lasso or Cox-Ridge model, 
           the tuning parameter lambda (λ), which controls the overall strength of the L1 or L2 penalty, 
           is determined based on a built-in 10-fold cross-validation. A maximum of 1000 iterations is 
           implemented over the data for all lambda (λ) values.", style = "font-size: 110%;display:inline;"),
    
    br(),
    br(),
    
    tags$p("The risk score for a patient in the training set is calcualted as a linear combination of 
    the gene expression valuesand coefficients of the signature genes, 
    i.e., risk score = β1*E1 + β2*E2 + ... + βn*En, where β1 to βn are the coefficients of the signature genes 
           and E1 to En are the normalized gene expression values in log2 scale. The median value of the risk scores for all patients 
           is used as the threshold to dichotomize 
           these patients into low- and high-risk groups. A KM plot 
           is generated to show the prognostic performance of the signature 
           in the training set. The R package ", style = "font-size: 110%;display:inline;"),
    
    tags$i(strong("survival"), style = "font-size: 110%;display:inline;"),
    
    tags$p(" is used to estimate the hazard ratio (HR) and 95% confidence intervals (CIs). 
           The log-rank test is performed to compare the two survival curves and calculate the p value. 
           Theoretically, any prognostic models should be significantly 
           associated with the endpoint in the training dataset. Therefore, it's very criticial 
           to perform external validations of the prognostic models in multiple independent cohorts. 
           In PCaDB, the risk scores are calculated for 
           patients in each of the nine remaining datasets with RFS data, to validate the prognostic model, 
           and a forest plot is generated to show the HRs, 95% CIs, and log rank test p values from 
           KM analyses in the nine validation datasets (Figure 3-7).", 
           style = "font-size: 110%;display:inline;"),
    
    br(),
    br(),
    
    tags$p("* Please be noted that only genes that are present in the training dataset 
    are used for prognostic model development. If a gene in the final model has not 
    been quantified in the validation dataset, the expression value would be set to zero.",
           style = "font-size: 110%;"),
    
    tags$img(src='img/tutorial_transcriptome_model.jpg', width=900),
    br(),
    h5(strong('Figure 3-7. Development and validation of the user-provided prognostic signature'), 
       align='center'),
    br(),
    
    
    
  ),
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('Data Download'), align='left'),
    
    #br(),
    
    h5(strong('4.1. Download processed data'), align='left'),
    
    tags$p("All the processed data deposited in PCaDB, including the ", 
           style = "font-size: 110%;display:inline;"),
    tags$i(strong("ExpresionSet"), style = "font-size: 110%;display:inline;"),
    tags$p(" of a transcriptomics data in the .RDS format, 
           prognostic signatures in the .xlsx format, and 
           gene annotation in the .RDS format can be donwloaded directly 
           by clicking the link to the data on the", strong("Download"), " page (Figure 4-1).", 
           style = "font-size: 110%;display:inline;"),
    
    br(),
    br(),
    
    tags$img(src='img/tutorial_download.jpg', width=900),
    
    h5(strong('Figure 4-1. Download processed gene expression data, sample metadata, and gene annotation data'), align='center'),
    
    br(),
    
    tags$p("For example, suppose the .RDS file for the TCGA-PRAD dataset has been downloaded, 
           the gene expression data and sample metadata can be easily retrieved by running the 
           following command in R (Figure 4-2).", 
           style = "font-size: 110%;"),
    
    tags$img(src='img/tutorial_read_RDS.jpg', width=900),
    
    h5(strong('Figure 4-2. Retrieve gene expression data and sample metadata from the eSet object'), align='center'),
    
    br(),
    
    
    h5(strong('4.2. Export data analysis & visualization results'), align='left'),
    
    tags$p("We provide two download buttons (",
           style = "font-size: 110%;display:inline"),
    tags$i(strong("PDF"), style = "font-size: 110%;display:inline"),
    tags$p(" and ",
           style = "font-size: 110%;display:inline"),
    tags$i(strong("CSV"), style = "font-size: 110%;display:inline"),
    
    tags$p(") under each figure generated by PCaDB, allowing the users to 
           download the high-resolution publication-quality vector image in PDF 
           format and the data that is used to generate the figure in CSV format.", 
           style = "font-size: 110%;display:inline;"),
    
    br(),
    br(),
    
    tags$img(src='img/tutorial_download_pdf.jpg', width=900),
    
    h5(strong('Figure 4-3. Download the publication-quality vector image in PDF format'), align='center'),
    
    br(),
    
    tags$p("In this example, the expression data of the selected gene ", 
           style = "font-size: 110%;display:inline"),
    
    tags$i(strong("KLK3"), style = "font-size: 110%;display:inline"),
    
    tags$p(" for the samples in the selected dataset GSE6919-GPL8300 will be saved to the 
                                     CSV file ", 
           style = "font-size: 110%;display:inline"),
    
    tags$i(strong("ENSG00000142515_KLK3_GSE6919-GPL8300_Expression.csv"), 
           style = "font-size: 110%;display:inline"),
    
    tags$p(" by clicking the download button (Figure4-4).", style = "font-size: 110%;display:inline"),
    
    br(),
    br(),
    
    tags$img(src='img/tutorial_download_csv.jpg', width=900),
    
    h5(strong('Figure 4-4. Download the data that is used to generate the figure to a CSV file'), align='center'),
    
    br(),
    
    tags$p("All the data tables generated for the data analysis outputs are also exportable in ", 
           style = "font-size: 110%;display:inline"),
    
    tags$i(strong("CSV"), style = "font-size: 110%;display:inline"),
    tags$p(" or ",
           style = "font-size: 110%;display:inline"),
    tags$i(strong("EXCEL"), style = "font-size: 110%;display:inline"),
    
    tags$p(" formats. The output can also be copied to the clipboard.",
           style = "font-size: 110%;display:inline"),
    
    br(),
    br(),
    
    tags$img(src='img/tutorial_download_table.jpg', width=900),
    
    h5(strong('Figure 4-5. Download the data table for the data analysis output'), align='center')
    
  )
  
  
  
  # box(
  #   title = 'Dataset', status = "primary", solidHeader = TRUE, collapsible = FALSE,
  #   width = 12
  # ),
  # 
  # box(
  #   title = 'Dataset', status = "info", solidHeader = TRUE, collapsible = FALSE,
  #   width = 12
  # ),
  # 
  # box(
  #   title = 'Dataset', status = "success", solidHeader = TRUE, collapsible = FALSE,
  #   width = 12
  # ),
  # box(
  #   title = 'Dataset', status = "warning", solidHeader = TRUE, collapsible = FALSE,
  #   width = 12
  # ),
  # box(
  #   title = 'Dataset', status = "danger", solidHeader = TRUE, collapsible = FALSE,
  #   width = 12
  # )
  
)

### Contact

tab_contact <- fluidRow(
  
  box(
    title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
    width = 12, height = 55,
    
    #h4(strong("Contact Us"), align='center', style='font-family:Georgia;color:#2C3E50')
    h4(strong(icon("envelope"), HTML('&nbsp;'), "Contact Us"), align='center', style='font-family:Georgia;color:#2C3E50')
  ),
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    column(12,
           h5(strong("Please feel free to contact us if you have any questions on the database or need any help to 
                     curate data for cancer studies."), align='left', style='color:black;'),
           br(),
           
           h5(strong("Ruidong Li, Ph.D., Research Bioinformatics Scientist"), align='left', style='color:black'),
           h5("Email: rli012@ucr.edu", align='left', style='color:black'),
           
           #webpage <- a('Webpage', href = 'https://rli012.github.io/', target="_blank", style = "font-size: 110%;"),
           github <- a('Github', href = 'https://github.com/rli012', target="_blank", style = "font-size: 110%;"),
           google.scholar <- a('Google Scholar', href = 'https://scholar.google.com/citations?hl=en&user=dsoteJwAAAAJ&view_op=list_works&sortby=pubdate', target="_blank", style = "font-size: 110%;"),
           #tagList(webpage, google.scholar, github),
           
           br(),
           br(),
           h5(strong("Zhenyu Jia, Ph.D., Associate Professor and Geneticist"), align='left', style='color:black'),
           h5("Botany and Plant Sciences, University of California, Riverside", align='left', style='color:black'),
           h5("Email: arthur.jia@ucr.edu", align='left', style='color:black'),
           tags$p(HTML("<a href='https://sites.google.com/ucr.edu/jia-lab-ucr/' target='_blank'><h5>Jia Lab @ University of California, Riverside</h5></a>"))
           )
  )
  
)



body=dashboardBody(
  useShinyjs(),
  useShinyalert(),
  
  includeCSS("www/css/style.css"),
  
  #selectizeInput(inputId = "gene", label='Gene', choices = gene.annotation, selected = gene.default, multiple=FALSE, 
  #               options = list(
  #                 placeholder = 'Select a gene', maxOptions = 10,
  #                 selectOnTab=TRUE)),
  
  #tags$head(tags$style(HTML('.box {margin: 1px;}'))), # distance between box
  
  theme_blue_gradient,
  
  disconnectMessage(
    text = "Your session has timed out, please reload the page.",
    refresh = "Reload now",
    width = 'full', top = 'center', css = 'font-family:Georgia',
    size = 30,
    colour = google.blue,
    background = "rgba(64, 64, 64, 0.9)",
    overlayColour = "#999",
    overlayOpacity = 0.7,
    refreshColour = google.green
  ),
  
  tags$head(tags$meta(name = "viewport", content = "width=1200")),
  #tags$head(tags$style("#genes{color:black; font-size:12px; font-style:bold;}")),
  
  #https://stackoverflow.com/questions/45706670/shiny-dashboadpage-lock-dashboardheader-on-top
  tags$script(HTML("$('body').addClass('fixed');")), # fix header & sidebar
  
  # add_busy_bar(color = "red", height = "8px"),
  # add_busy_gif(
  #   src = "https://jeroen.github.io/images/banana.gif",
  #   height = 70, width = 70
  # ),
  
  #use_waiter(), # include dependencies
  
  # waiting_screen <- tagList(
  #   spin_ball(),
  #   h3(strong("Loading data for PCaDB ..."))
  # ),
  # 
  # waiter_show_on_load(html = waiting_screen, color = 'white'),
  
  # busy_start_up(
  #   loader = spin_epic("semipolar", color = google.red),
  #   text = h4(strong('Loading data, please wait ...')),
  #   timeout = 1500,
  #   color = google.red,
  #   background = "grey"
  # ),
  

  tabItems(
    tabItem(tabName="tab_home", tab_home),
    tabItem(tabName="tab_query", tab_query),
    tabItem(tabName="tab_signature",tab_signature),
    tabItem(tabName="tab_transcriptome",tab_transcriptome),
    tabItem(tabName="tab_pipeline", tab_pipeline),
    tabItem(tabName="tab_download", tab_download),
    tabItem(tabName="tab_tutorial", tab_tutorial),
    tabItem(tabName="tab_contact", tab_contact)
  ),

  tags$head(tags$script(HTML("
          $(document).on('click', 'a[href*=\"#tab\"]', function(){
          	href = $(this).attr('href');
          	$(this).attr('href', href.slice(href.indexOf('#tab'), href.length));
          });

					$(document).on('click', 'input', function(){
          	if ($(this).prop('type') == 'radio'
								&& !($(this).prop('name') == 'surv_model_method' || $(this).prop('name') == 'deg.test')) { //client side is just eye candy
              if ($(this).data('waschecked') == true) {
                $(this).prop('checked', false);
                $(this).data('waschecked', false);
	            }
              else {
                $(this).data('waschecked', true);
							}
							Shiny.onInputChange('selGroup', {name:$(this).prop('name'), value:$(this).prop('value'), checked:$(this).prop('checked')});
            }
          });

          $(document).keyup(function(event) {
            if ($('#search_experiment').is(':focus') && (event.keyCode == 13)) {
              $('#do_search_experiment').click();
            }
          });

					$(document).on('click', '#do_search_experiment', function() {
						//alert($('#search_experiment').val());
						_paq.push(['trackSiteSearch', $('#search_experiment').val()]);
					});

					$(document).on('click', '#selectedGroups button', function() {
						$('#selectedGroups :button').hide();
						var table = ($(this).parents('table')).dataTable().api();
						Shiny.onInputChange('remove_comparison', {idx:table.row($(this).parents('tr')).index(), rand: Math.random()}); //force event trigger without changing idx
					});

          var dimension = [0, 0];
          $(document).on('shiny:connected', function(e) {
              dimension[0] = window.innerWidth;
              dimension[1] = window.innerHeight;
              Shiny.onInputChange('dimension', dimension);
          });
          $(window).resize(function(e) {
              dimension[0] = window.innerWidth;
              dimension[1] = window.innerHeight;
              Shiny.onInputChange('dimension', dimension);
          });")))
  
)


footer=dashboardFooter(right_text = HTML("<script type='text/javascript' id='clustrmaps' src='//cdn.clustrmaps.com/map_v2.js?cl=ffffff&w=300&t=tt&d=m1OK5wpD2xEM4YTLneQr-HXIvLS_D8Vs34uIkQuX-8w'></script>"),
                       left_text = '')

#https://www.revolvermaps.com/
#                       left_text = HTML("<footer><h6>Contact: <a href='https://github.com/rli012' target='_blank'>Ruidong Li</a><br>Email: rli012@ucr.edu</h6><strong><h5><a href='http://jialab.ucr.acsitefactory.com/' target='_blank'>Jia Lab @ University of California, Riverside</a></h5></strong></footer>"))
#left_text = HTML("<footer><h6>\t\tCopyright &#169 2020 <a href='http://jialab.ucr.acsitefactory.com/' target='_blank'>Jia Lab</a>. <br><a href='https://plantbiology.ucr.edu/' target='_blank'>Department of Botany & Plant Sciences</a>, <br><a href='https://plantbiology.ucr.edu/' target='_blank'>University of California, Riverside</a></h6></footer>"))

ui <- dashboardPagePlus(title='PCaDB', header, sidebar, body) # skin = 'blue', footer = footer
