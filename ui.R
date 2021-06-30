
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
#library(shinybusy)
#library(waiter)

source('helper_functions.R')
#source('load_data.R') -> global.R


############################################################
######   Theme

title <- tagList(
  tags$img(src='logo.png', height=40) #width=150, 
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
                                         #maxOptions = 5,
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
  tags$img(src='ucr1.jpg', width=50),
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
           
           valueBox(value = tags$p(strong("50"), style = "font-size: 80%;"), color = 'teal', width = 3, #color = 'aqua'
                    subtitle = tags$p(strong("Datasets"), style = "font-size: 150%;"), tags$i(class = "fa fa-database", style="font-size: 60px")), #icon = icon("database fa-0.5x")
           valueBox(value = tags$p(strong("7,231"), style = "font-size: 80%;"), color = 'teal', width = 3,
                    subtitle = tags$p(strong("Samples"), style = "font-size: 150%;"), tags$i(class = "fa fa-user-circle", style="font-size: 65px")), #icon = icon("user-circle fa-0.5x")
           valueBox(value = tags$p(strong("30"), style = "font-size: 80%;"), color = 'teal', width = 3,
                    subtitle = tags$p(strong("Signatures"), style = "font-size: 150%;"), tags$i(class = "fa fa-dna", style="font-size: 60px")) #icon = icon("dna fa-0.5x")
    )
  ),
  
  box(
    title = NULL, solidHeader = TRUE, collapsible = FALSE,
    width = 12, # solidHeader=TRUE can remove the top boarder
    
    h4(strong("Introduction")),
    
    h5(strong("Prostate cancer")),
    tags$p("Prostate cancer (PCa) is the second most frequently diagnosed cancer in men worldwide, 
           which accounts for 14.1% of the newly diagnosed cancer cases in 2020 [",
           a("GLOBOCAN", 
             href = "https://doi.org/10.3322/caac.21660",target='_blank'), ']. 
           Localized PCa is a heterogeneous disease with highly variable clinical behaviors. 
           Low-risk PCa usually grows slowly and men with indolent PCa may only require active surveillance, 
           whereas many others with aggressive PCa require immediate local treatment such as radiotherapy or surgery. 
           Comprehensive collection of transcriptomics data from large population cohorts 
           is very critical for understanding the molecular biology of cancer, 
           drug target identification and evaluation, 
           biomarker discovery for cancer diagnosis and prognosis, etc.', 
           style = "font-size: 120%;"),
    
    #tags$img(src='The-scheme-of-different-stages-and-progression-of-prostate-cancer.png', width=600),
    
    #br(),
    tags$hr(style="border-top: 1px dashed #A9A9A9"),
    h5(strong("About PCaDB")),
    tags$p('PCaDB is a comprehensive and interactive database for transcriptomes from prostate cancer population cohorts. 
           We collected 50 transcriptomics datasets with 7,231 samples from public data repositories, 
           including TCGA, cBioPortal, GEO, and ArrayExpress. A standard bioinformatics pipeline is used to download and 
           process the expression data and metadata.
           PCaDB provides a user-friendly interface for the comprehensive analysis of individual 
           genes, prognostic signatures, and the whole transcriptomes to elucidate the molecular 
           heterogeneity in PCa, understand the mechanisms of tumor initiation and progression, 
           as well as develop and validate prognostic signatures in large independent cohorts', style = "font-size: 120%;"),
    
    # column(12, 
    #        tags$hr(style="border-top: 1px dashed #A9A9A9")
    #        ),
    
    # actionLink("link_to_tab_query", tags$img(src='analysis.png', height=500)),
    
    tags$img(src='analysis.png', height=500),
    
    
    # column(12,
    #        column(1),
    #        column(4#,
    #               #plotlyOutput('pie_sample_type_introduction', width='100%', height='300px')
    #        ),
    #        column(7)
    # ),
    
    column(12,
           #br(),
           tags$hr(style="border-top: 1px dashed #A9A9A9")
           ),
    # br(),
    # tags$hr(style="border-top: 1px dashed #A9A9A9"),
    
    h5(strong("Citation")),
    tags$p('Please cite the following publication:
           Li, R. and Jia, Z., PCaDB - a comprehensive and interactive database for transcriptomes from prostate cancer population cohorts. bioRxiv (2021)', style = "font-size: 120%;"),
    tags$p(HTML("<a href='https://doi.org/10.1093/bib/bbaa197' target='_blank'><h5>hhttps://doi.org/10.1093/bib/bbaa197</h5></a>")),
    br(),
    tags$p(HTML('<script type="text/javascript" src="//rf.revolvermaps.com/0/0/3.js?i=5dfs4o8b9k7&amp;b=5&amp;s=0&amp;m=2&amp;cl=ffffff&amp;co=010020&amp;cd=aa0000&amp;v0=60&amp;v1=60&amp;r=1" async="async"></script>'), align = 'left')
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
                                column(11,
                                       h5("Gene Expression in Different Sample Types", align = 'center'),
                                       #h6("(Wilcoxon rank-sum test, ***: P < 0.001; **: P < 0.01; *: P < 0.05; ns: P > 0.05)", align = 'center'),
                                       tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                       expr.dataset.id,
                                       br(),
                                       withSpinner(uiOutput('query_boxplot_ui'), type = 4, proxy.height=300)
                                       
                                ),
                                column(10)#,
                                #column(2,
                                       #downloadButton(outputId='query.box.summ.downbttn.csv', label = "CSV"),
                                       #downloadButton(outputId='transcriptome.box.summ.downbttn.png', label = "PNG"),
                                       #downloadButton(outputId='query.box.summ.downbttn.pdf', label = "PDF")
                                #)
                                
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
                                )
                                
                         )
                         
                         
                         
                         # column(10),
                         # column(2,
                         #        downloadButton(outputId='query.box.summ.downbttn.csv', label = "CSV"),
                         #        #downloadButton(outputId='transcriptome.box.summ.downbttn.png', label = "PNG"),
                         #        downloadButton(outputId='query.box.summ.downbttn.pdf', label = "PDF")
                         # )
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
                         column(12,
                                br(),
                                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                plotOutput('henry_sc_violin',width = 900, height = 350)#,
                                # br(),
                                # column(7),
                                # column(5,
                                #        downloadButton(outputId='transcriptome.box.downbttn.csv', label = "CSV"),
                                #        #downloadButton(outputId='transcriptome.box.downbttn.png', label = "PNG"),
                                #        downloadButton(outputId='transcriptome.box.downbttn.pdf', label = "PDF")
                                # )
                                
                         )
                         
                         
                         
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
    tags$p("A comprehensive collection of 30 published gene expression-based prognostic signatures for prostate cancer is 
           included in PCaDB. The performances of the signatures have been investigated in the study [",
           a("Comprehensive evaluation of machine learning models and gene expression signatures for prostate cancer prognosis using large population cohorts", 
             href = "https://doi.org/10.1093/bib/bbaa197",target='_blank'), "]. Comprehensive characterization of the signatures can be further performed in PCaDB.", 
           style = "font-size: 120%;"),
    #tags$p("Comprehensive characterization of the signatures can be further performed in PCaDB.", style = "font-size: 120%;"),
    
    
    br(),
    column(6,
           tags$img(src='signature_table.png',width=450)),
    column(6,
           tags$img(src='signature_circos.png',width=450))
    
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
                         )
                         
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
                             strong('GEO/ArrayExpress/EGA Accession:'),
                             textOutput('transcriptome_text_summary_accession'),
                             #hr(style="border-top: 1px dashed #A9A9A9"),
                             br(),
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
                                          input.transcriptome_dataset_rows_selected == 50',
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
                                          input.transcriptome_dataset_rows_selected == 50',
                                          # 1, 3, 4, 5, 6, 7, 8, 9, 12, 17, 43, 47, 50
                                          box(title = 'Gleason Pattern',
                                              status = "info", solidHeader = TRUE, collapsible = FALSE,
                                              width = 4,
                                              height = 375,
                                              plotlyOutput('transcriptome_barplot_gleason', width='100%', height='300px')
                                          )
                         ),
                         
                         conditionalPanel(condition = 'input.transcriptome_dataset_rows_selected <= 12 || 
                                          input.transcriptome_dataset_rows_selected == 16 || 
                                          input.transcriptome_dataset_rows_selected == 17',
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
                h5('2D Principal Component Analysis using Highly Expressed Genes', align='center'),
                withSpinner(plotlyOutput('transcriptome.pca.2d'),
                            type = 1)
                
         ),
         
         column(6, 
                br(),
                h5('3D Principal Component Analysis using Highly Expressed Genes', align='center'),
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
                                        withSpinner(plotOutput('transcriptome_volcano_plot', height = 500), type = 4, proxy.height = 300)
                       )
                )
                
                # column(7),
                # column(5,
                #        downloadButton(outputId='volcano.ccma.downbttn.csv', label = "CSV"),
                #        #downloadButton(outputId='circ.expr.downbttn.png', label = "PNG"),
                #        downloadButton(outputId='volcano.ccma.downbttn.pdf', label = "PDF")
                # )
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
         
         column(12,
                br(),
                hr(),
                h5('Cox Proportional-Hazards (CoxPH) Survival Analysis', align='center'),
                div(DT::dataTableOutput("table_coxph"),style = "font-size:90%")
         ),
         
         column(12,
                br(),
                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                h5('Kaplan Meier (KM) Survival Analysis', align='center'),
                h6("(Low- and high-expression groups were separated by median values)", align = 'center'),
                div(DT::dataTableOutput("table_km"),style = "font-size:90%")
         )#,
         
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
         br(),
         hr(),
         column(4,
                textAreaInput(inputId = "surv_gene_input", label = h5(strong("Paste A Gene List\n(Ensembl ID):")), 
                              value = "ENSG00000143322\nENSG00000187764\nENSG00000005844\nENSG00000173372\nENSG00000102265\nENSG00000124762", width = "300px", height = '200px')
         ),
         column(8,
                column(12, radioButtons(inputId = "surv_model_method", label = h5(strong("Method:")),
                                        choices = c('CoxPH', 'CoxLasso', 'CoxRidge'), # ,'plsRcox'
                                        inline = TRUE)),
                
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
                                               withSpinner(plotOutput('training_km_plot',width = 360, height = 360), type = 4, proxy.height = 300)
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
                                        withSpinner(plotOutput("validation.bcr.forest", width = 800, height = 600), type=4, proxy.height=300))
                       
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
    tags$ul(tags$li('TCGA data', style="font-size:120%;")),
    tags$ul(tags$li('cBioPortal data', style="font-size:120%;")),
    tags$ul(tags$li('GEO data', style="font-size:120%;")),
    tags$ul(tags$li('ArrayExpress data', style="font-size:120%;")),
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

    tags$p("A bioinformatics pipeline was developed to download, preprocess, and curate the public transcriptomics data 
           from TCGA, cBioPortal, GEO, and ArrayExpress. RNAseq and microarray data (Affymetrix) were reprocessed if 
           raw data (i.e., .fastq/.CEL files) were available. 
           Otherwise, the normalized data such as FPKM values for RNAseq and normalized intensities 
           for microarray data were downloaded directly from the public repositories. Phenotype data (metadata) were obtained 
           from the repositories and harmonized using the pipeline. The ExpressionSet object was created for each dataset.",
           style = "font-size: 110%;"),
    
    tags$p("A single-cell RNAseq dataset from normal human prostae tissue (Henry et al., 2018) was downloaded from GUDMAP (https://www.gudmap.org/), 
           allowing the investigation of a gene of interest at the single-cell level.", 
           style = "font-size: 110%;"),
    
    tags$p("30 prognostic signatures for prostate cancer were obtained from a comprehensive study evaluating the machine learning 
           models and gene expression signatures for prostate cancer prognosis using large population cohorts (Li et al., 2021). 
           The 10 datasets used in that study were all included in the PCaDB database.", 
           style = "font-size: 110%;"),
    
    tags$p("We also systematically organized the gene annotation information from HGNC, Ensembl, NCBI, and Gencode to facilitate the 
           query of individual genes and the cross-dataset anlaysis.", 
           style = "font-size: 110%;"),
    
    tags$p("Overview of the pipeline is shown in the figure below and details about the pipeline are described in the sub-sections.", 
           style = "font-size: 110%;"),
    
    tags$hr(style="border-top: 1px dashed #A9A9A9"),
    
    tags$img(src='pipeline.png', height=350) #width=150,
    
    
  ),
  
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('TCGA data'), align='left'),
    
    tags$p("The Cancer Genome Atlas Prostate Adenocarcinoma (TCGA-PRAD) data 
           was downloaded and processed as described in a previous study (Li et al., 2020). 
           The HTSeq-Counts of RNA-seq and clinical data were downloaded and 
           processed by a series of functions in the R package ",
           style = "font-size: 110%;display:inline"),
    
    tags$i("GDCRNATools",
           style = "font-size: 110%; display:inline"),
           
    tags$p(" (Li et al., 2018). The raw count data was normalized using the Trimmed 
           Mean of Mvalues (TMM) method implemented in the R package ",
           style = "font-size: 110%;display:inline"),
    
    tags$i("edgeR",
           style = "font-size: 110%; display:inline"),
    
    tags$p(" (Robinson et al., 2010). Clinical characteristics, such as preoperative 
           prostate-specific antigen (PSA) level, which were not available in 
           the Genomic Data Commons (GDC) data portal were retrieved from Broad 
           GDAC Firehose (https://gdac. broadinstitute.org/).",
           style = "font-size: 110%;display:inline"),
    br(),
    br(),
    
    tags$p("Generally, the metadata harmonization is very similar for data from different repositories. The differences 
           are just the way to download and retrieve the data. Although the TCGA data is more organized, we'd like to 
           describe the metadata harmonization pipeline here in the begining.", 
           style = "font-size: 110%;"),
    
    tags$p("To facilitate the analysis across datasets, a comprehensive list was created for the key traits, including: ", style = "font-size: 110%;display:inline"),
    tags$code("sample_id", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("patient_id", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("tissue", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("batch", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("platform", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("sample_type", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("age_at_diagnosis", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("ethnicity", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("race", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("clinical_stage", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("clinical_t_stage", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("clinical_n_stage", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("clinical_m_stage", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("pathological_stage", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("pathological_t_stage", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("pathological_n_stage", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("pathological_m_stage", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("preop_psa", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("gleason_primary_pattern", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("gleason_secondary_pattern", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("gleason_tertiary_pattern", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("gleason_group", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("gleason_score", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("time_to_death", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("os_status", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("time_to_bcr", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("bcr_status", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("time_to_metastasis", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("metastasis_status", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("risk_group", style = "font-size: 110%;display:inline"), tags$p(",", style = "font-size: 110%;display:inline"),
    tags$code("treatment", style = "font-size: 110%;display:inline"), tags$p(", and ", style = "font-size: 110%;display:inline"),
    tags$code("additional_treatment", style = "font-size: 110%;display:inline"), tags$p(". ", style = "font-size: 110%;display:inline"),
    
    tags$p("The phenotypic values were also standardized, especially for the sample type field. For example, ",
           style = "font-size: 110%;display:inline"),
    tags$i("tumor, tumour, primary, localized, ", style = "font-size: 110%;display:inline"),
    tags$p("etc. were all labeled as ", style = "font-size: 110%;display:inline"),
    tags$i(strong("Primary"), style = "font-size: 100%;display:inline"),
    tags$p(", whereas ", style = "font-size: 110%;display:inline"),
    tags$i("normal, adjacent, benign, ", style = "font-size: 110%;display:inline"),
    tags$p("etc. were all labeled as ", style = "font-size: 110%;display:inline"),
    tags$i(strong("Normal"), style = "font-size: 100%;display:inline"),
    tags$p(". The standardized sample type information was added to a new column ", style = "font-size: 110%;display:inline"),
    tags$code("pcadb_group", style = "font-size: 110%;display:inline"),
    tags$p(".", style = "font-size: 110%;display:inline"),
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
    
    h4(strong('cBioPortal data'), align='left'),
    
    tags$p("The expression data and clinical data can be downloaded directly from cBioPortal. 
           cBioPortal data were usually not used in PCaDB, unless the raw data were not available in any 
           of the other data repositories, because only the normalized data are provided in cBioPortal, 
           which are not suitable for some of the downstream analyses. e.g., the FPKM/RPKM values 
           are not recommended for differential expression analysis. The preprocessing of the expression data 
           in cBioPortal is relatively simple. Log2 transformation may be 
           performed if it hadn't been done and the gene symbols should be converted to Ensembl IDs. A custom 
           script is used to curate cBioPortal data.", 
           style = "font-size: 110%;"),
    
    tags$hr(style="border-top: 1px dashed #A9A9A9"),
    
    tags$iframe(src = 'rmd/cbioportal.html', 
                width = '100%', height = '400px', 
                frameborder = 0, scrolling = 'auto')
    
  ),
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('GEO data'), align='left'),
    
    
    
    tags$p("Most of the microarray data in PCaDB were from GEO. All the datasets generated 
           using the Affymetrix arrays were reprocessed from the raw .CEL files. The annotation 
           data of the probes were downloaded from the Brainarray database (GENCODEG, Version 24). 
           The expression data were preprocessed using the Robust Multichip Average (RMA) algorithm 
           implemented in the R package ", 
           style = "font-size: 110%; display:inline"),
    
    tags$i("oligo",
           style = "font-size: 110%; display:inline"),
    
    tags$p(" (Carvalho et al., 2010). ", 
           style = "font-size: 110%; display:inline"),
    br(),
    br(),
    
    tags$p("The normalized intensities for microarray data generated 
           using other technologies were downloaded directly from GEO and log2 transformation may be  
           performed if it hadn't been done. Metadata in the series matrix files were downloaded 
           using the R package ", 
           style = "font-size: 110%; display:inline"),
    
    tags$i("GEOquery",
           style = "font-size: 110%; display:inline"),
    
    tags$p(" (Davis et al., 2007).", 
           style = "font-size: 110%; display:inline"),
    br(),
    br(),
    
    tags$p("Below is the script for downloading, preprocessing, and organizing data from GEO.", 
           style = "font-size: 110%; display:inline"),
    
    
    tags$hr(style="border-top: 1px dashed #A9A9A9"),
    
    tags$iframe(src = 'rmd/geo.html', 
                width = '100%', height = '400px', 
                frameborder = 0, scrolling = 'auto')
    
  ),
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('ArrayExpress data'), align='left'),
    
    tags$p("Microarray gene expression data downloaded from ArrayExpress can be 
           processed using the same pipeline as that from GEO. The format of metadata 
           in ArrayExpress is slightly different than that in GEO, and a simple script 
           can be created for metadata harmonization.", 
           style = "font-size: 110%;"),
    
    tags$hr(style="border-top: 1px dashed #A9A9A9"),
    
    tags$iframe(src = 'rmd/arrayexpress.html', 
                width = '100%', height = '400px', 
                frameborder = 0, scrolling = 'auto')
    
  ),
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('Single-cell RNAseq data'), align='left'),
    
    tags$p("The Seurat object of the single-cell RNAseq data can be directly downloaded from GUDMAP 
           (https://www.gudmap.org/chaise/record/#2/RNASeq:Study/RID=W-RAHW). The normalized gene 
           expression matrix,  cell type annotation, as well as the TSNE and UMAP coordiantes 
           were included in an R list object in the PCaDB database.",
           style = "font-size: 110%;"),
    
    tags$hr(style="border-top: 1px dashed #A9A9A9"),
    
    tags$iframe(src = 'rmd/scrnaseq.html', 
                width = '100%', height = '400px', 
                frameborder = 0, scrolling = 'auto')
    
    
  ),
  
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('Gene annotation'), align='left'),
    
    tags$p("Gene annotation information were downloaded from the official websites/FTPs of HGNC, 
           Ensembl, NCBI, and Gencode, and a comprehensive pipeline was created to map the gene IDs 
           from different resources to facilitate the query of individual genes and the cross-dataset anlaysis.", 
           style = "font-size: 110%;"),
    
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
    
    tags$p("[1] Henry G.H., Malewska A., Joseph D.B., Malladi V.S., Lee J., Torrealba J., Mauck R.J., 
           Gahan J.C., Raj G.V., Roehrborn C.G., Hon G.C., MacConmara M.P., Reese J.C., Hutchinson R.C., 
           Vezina C.M. and Strand D.W. (2018). 
           A Cellular Anatomy of the Normal Adult Human Prostate and Prostatic Urethra. Cell reports, 25(12), 3530-3542.",
           style = "font-size: 100%;"),
    tags$p("[2] Li R. and Jia Z.(2021) 
           Comprehensive evaluation of machine learning models and gene expression signatures for 
           prostate cancer prognosis using large population cohorts. (2021). bioRxiv.",
           style = "font-size: 100%;"),
    tags$p("[3] Li R., Wang S., Cui Y., Qu H., Chater J.M., Zhang L., Wei J., Wang M., 
           Xu Y., Yu L., Lu J., Feng Y., Zhou R., Huang Y., Ma R., Zhu J., Zhong W. and Jia Z. (2020). 
           Extended application of genomic selection to screen multiomics data for prognostic 
           signatures of prostate cancer. Briefings in Bioinformormatics, bbaa197.",
           style = "font-size: 100%;"),
    tags$p("[4] Li, R., Qu, H., Wang, S., Wei, J., Zhang, L., Ma, R., Lu, J., Zhu, J., Zhong, W.D. and Jia, Z. (2018). 
           GDCRNATools: an R/Bioconductor package for integrative analysis of lncRNA, miRNA and mRNA data in GDC. 
           Bioinformatics, 34(14), 2515-2517.",
           style = "font-size: 100%;"),
    tags$p("[5] Robinson, M.D., McCarthy, D.J. and Smyth, G.K. (2010). edgeR: a Bioconductor package for differential 
           expression analysis of digital gene expression data. Bioinformatics, 26(1), 139-140.",
           style = "font-size: 100%;"),
    tags$p("[6] Carvalho, B.S. and Irizarry, R.A. (2010). A framework for oligonucleotide microarray preprocessing. 
           Bioinformatics, 23(14), 1846-1847.",
           style = "font-size: 100%;"),
    tags$p("[7] Davis, S. and Meltzer, P.S. (2007). GEOquery: a bridge between the Gene Expression Omnibus (GEO) and 
           BioConductor. Bioinformatics, 23(14), 1846-1847.",
           style = "font-size: 100%;")
    
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
           tags$li(a(href='downloads/CPC-Gene_eSet.RDS', target='_blank', 'CPC-Gene (GSE107299)')),
           tags$li(a(href='downloads/Taylor_eSet.RDS', target='_blank', 'Taylor (GSE21034)')),
           tags$li(a(href='downloads/DKFZ_eSet.RDS', target='_blank', 'DKFZ (EGAS00001002923)')),
           tags$li(a(href='downloads/GSE54460_eSet.RDS', target='_blank', 'GSE54460')),
           tags$li(a(href='downloads/Cambridge_eSet.RDS', target='_blank', 'Cambridge (GSE70768)')),
           tags$li(a(href='downloads/Stockholm_eSet.RDS', target='_blank', 'Stockholm (GSE70769)')),
           tags$li(a(href='downloads/CancerMap_eSet.RDS', target='_blank', 'CancerMap (GSE94767)')),
           tags$li(a(href='downloads/CIT_eSet.RDS', target='_blank', 'CIT (E-MTAB-6128)')),
           tags$li(a(href='downloads/Belfast_eSet.RDS', target='_blank', 'Belfast (GSE116918)')),
           tags$li(a(href='downloads/GSE25136_eSet.RDS', target='_blank', 'GSE25136')),
           tags$li(a(href='downloads/GSE41408_eSet.RDS', target='_blank', 'GSE41408')),
           tags$li(a(href='downloads/GSE46691_eSet.RDS', target='_blank', 'GSE46691'))
    ),
    column(3, 
           tags$li(a(href='downloads/GSE51066_eSet.RDS', target='_blank', 'GSE51066')),
           tags$li(a(href='downloads/GSE37199_eSet.RDS', target='_blank', 'GSE37199')),
           tags$li(a(href='downloads/GSE44353_eSet.RDS', target='_blank', 'GSE44353')),
           tags$li(a(href='downloads/GSE59745_eSet.RDS', target='_blank', 'GSE59745')),
           tags$li(a(href='downloads/GSE79021_eSet.RDS', target='_blank', 'GSE79021')),
           tags$li(a(href='downloads/GSE3933-GPL2695_eSet.RDS', target='_blank', 'GSE3933-GPL2695')),
           tags$li(a(href='downloads/GSE35988-GPL6480_eSet.RDS', target='_blank', 'GSE35988-GPL6480')),
           tags$li(a(href='downloads/GSE35988-GPL6848_eSet.RDS', target='_blank', 'GSE35988-GPL6848')),
           tags$li(a(href='downloads/SU2C-PCF-2019-Capture_eSet.RDS', target='_blank', 'SU2C-PCF-2019-Capture')),
           tags$li(a(href='downloads/SU2C-PCF-2019-PolyA_eSet.RDS', target='_blank', 'SU2C-PCF-2019-PolyA')),
           tags$li(a(href='downloads/GSE62116_eSet.RDS', target='_blank', 'GSE62116')),
           tags$li(a(href='downloads/GSE62667_eSet.RDS', target='_blank', 'GSE62667')),
           tags$li(a(href='downloads/GSE79957_eSet.RDS', target='_blank', 'GSE79957'))
    ),
    column(3, 
           
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
           tags$li(a(href='downloads/GSE8218_eSet.RDS', target='_blank', 'GSE8218'))
           
    ),
    column(3,
           tags$li(a(href='downloads/E-TABM-26-U133A_eSet.RDS', target='_blank', 'E-TABM-26-U133A')),
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
           tags$li(a(href='downloads/Broad-Cornell_eSet.RDS', target='_blank', 'Broad-Cornell'))
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

    tags$p("Users can query a gene of interest by typing the Ensembl ID, 
           Entrez ID, or HGNC approved symbol and alias symbol in the 
           'Search a gene' field and selecting the gene from the dropdown list. 
           The general information about the gene and some useful external 
           links to the databases such as ENSEMBL, HGNC, and NCBI for more 
           detailed description of the gene, Genotype-Tissue Expression (GTEx) 
           and Human Protein Atlas (HPA) for the gene expression pattern 
           in different human tissues, and Kyoto Encyclopedia of Genes and 
           Genomes (KEGG) for the pathways that the gene involves in are 
           provided.",
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
    
    tags$img(src='img/tutorial_query_overview.png', width=800),
    
    br(),
    h5(strong('Figure 1. Query a gene of interest'), align='center')#,
    
    # br(),
    # 
    # tags$img(src='img/tutorial_query_gene_expression_boxplot.png', width=800),
    # 
    # br(),
    # h5(strong('Figure 1-1. Gene expression in different sample types'), align='center'),
    # 
    # br(),
    # 
    # tags$img(src='img/tutorial_query_gene_expression_boxplot.png', width=800),
    # 
    # br(),
    # h5(strong('Figure 1-1. Gene expression in different sample types'), align='center'),
    # 
    # br(),
    
    
    
    ),
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('Prognostic signature evaluation'), align='left'),
    
    tags$p("A comprehensive evaluation of the prognostic performances 
           of 30 published signatures was performed in a previous study 
           and we included all those signatures in the PCaDB database, 
           allowing a more detailed characterization of the signatures, 
           including:",
           style = "font-size: 110%;"),
    
    tags$p("(1) List of signature genes",
           style = "font-size: 110%;"),
    
    tags$p("(2) Differential expression of the signature genes",
           style = "font-size: 110%;"),
    
    tags$p("(3) KM Survival analysis of RFS for the signature genes",
           style = "font-size: 110%;"),
    
    tags$p("(4) Pathway analysis of the signature genes",
           style = "font-size: 110%;"),
    
    tags$p("(5) Evaluation of the performances of the prognostic signatures",
           style = "font-size: 110%;"),
    
    br(),
    
    tags$img(src='img/tutorial_signature_overview.png', width=800),
    
    br(),
    h5(strong('Figure 2. Characterization of prognostic signatures'), align='center')#,
    
  ),
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('Transcriptomics data analysis'), align='left'),
    
    tags$p("More advanced and comprehensive analyses can be performed 
           at the whole-transcriptome level in PCaDB, 
           allowing users to identify DEGs associated with 
           tumor initiation and progression, identify biomarkers 
           associated with clinical outcomes (i.e., BCR), 
           as well as develop and validate gene expression-based 
           signatures and models for PCa prognosis. 
           The whole-transcriptome level analysis includes: ",
           style = "font-size: 110%;"),
    
    tags$p("(1) Summary of the dataset",
           style = "font-size: 110%;"),
    
    tags$p("(2) Dimensionality reduction",
           style = "font-size: 110%;"),
    
    tags$p("(3) Differential gene expression analysis",
           style = "font-size: 110%;"),
    
    tags$p("(4) Kaplan Meier and CoxPH survival analysis of RFS",
           style = "font-size: 110%;"),
    
    tags$p("(5) Development and validation of a new prognostic model",
           style = "font-size: 110%;"),
    
    br(),
    
    tags$img(src='img/tutorial_transcriptome_overview.png', width=800),
    
    br(),
    h5(strong('Figure 3. Whole-transcriptome data analysis'), align='center')#,
    
    
  ),
  
  box(
    title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    h4(strong('Data download'), align='left'),
    
    tags$p("All the processed data deposited in PCaDB, including 
           the ExpresionSet of a transcriptomics data in the .RDS format, 
           prognostic signatures in the .xlsx format, and 
           gene annotation in the .RDS format can be donwloaded directly 
           by clicking the link to the data on the ", strong('Download'), ' page',
           style = "font-size: 110%;")
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
                     curate transcriptomics datasets for prostate cancer."), align='left', style='color:black'),
           br(),
           
           h5(strong("Ruidong Li, Ph.D. Research Data Scientist"), align='left', style='color:black'),
           h5("Email: rli012@ucr.edu", align='left', style='color:black'),
           
           webpage <- a('Webpage', href = 'https://rli012.github.io/', target="_blank", style = "font-size: 110%;"),
           google.scholar <- a('Google Scholar', href = 'https://scholar.google.com/citations?user=dsoteJwAAAAJ&hl', target="_blank", style = "font-size: 110%;"),
           github <- a('Github', href = 'https://github.com/rli012', target="_blank", style = "font-size: 110%;"),
           #tagList(webpage, google.scholar, github),
           
           br(),
           br(),
           h5(strong("Zhenyu Jia, Ph.D. Associate Professor and Geneticist"), align='left', style='color:black'),
           h5("Botany and Plant Sciences, University of California, Riverside", align='left', style='color:black'),
           h5("Email: arthur.jia@ucr.edu", align='left', style='color:black'),
           tags$p(HTML("<a href='http://jialab.ucr.acsitefactory.com/' target='_blank'><h5>Jia Lab @ University of California, Riverside</h5></a>"))
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
  )
  
)


# footer=dashboardFooter(right_text = HTML('<footer><script type="text/javascript" src="//rf.revolvermaps.com/0/0/3.js?i=5ec4xu8g4x1&amp;b=10&amp;s=0&amp;m=2&amp;cl=ffffff&amp;co=010020&amp;cd=aa0000&amp;v0=60&amp;v1=60&amp;r=1" async="async"></script></footer>'),
#                        left_text = '')

#https://www.revolvermaps.com/
#                       left_text = HTML("<footer><h6>Contact: <a href='https://github.com/rli012' target='_blank'>Ruidong Li</a><br>Email: rli012@ucr.edu</h6><strong><h5><a href='http://jialab.ucr.acsitefactory.com/' target='_blank'>Jia Lab @ University of California, Riverside</a></h5></strong></footer>"))
#left_text = HTML("<footer><h6>\t\tCopyright &#169 2020 <a href='http://jialab.ucr.acsitefactory.com/' target='_blank'>Jia Lab</a>. <br><a href='https://plantbiology.ucr.edu/' target='_blank'>Department of Botany & Plant Sciences</a>, <br><a href='https://plantbiology.ucr.edu/' target='_blank'>University of California, Riverside</a></h6></footer>"))

ui <- dashboardPagePlus(title='PCaDB', header, sidebar, body) # skin = 'blue', footer = footer
