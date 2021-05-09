
library(shiny)
library(shinydashboard)
library(shinyjs)
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

library(shinydashboardPlus)
library(shinythemes)
library(dashboardthemes)
library(shinycssloaders)

source('shiny_functions.R')
source('load_data.R')


############################################################################################

################### Theme

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


###########################################

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


###########################################

##### ui

table.download.button <- JS('$("button.buttons-copy").css("font-size",12);
                            $("button.buttons-csv").css("font-size",12);
                            $("button.buttons-excel").css("font-size",12);
                            return table;')

left_menu <- tagList(
  tags$img(src='ucr1.jpg', width=50),
  h4(strong("Welcome to PCaDB, a database of transcriptomes from prostate cancer population cohorts"), align='left', style='font-family:Georgia;color:#2C3E50'),
  tags$img(src='ucr2.jpg', width=60)
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
    menuItem("PCaDB Pipelines", tabName = "tab_pipelines", icon = icon("pencil")),
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
           valueBox(value = tags$p(strong("7,082"), style = "font-size: 80%;"), color = 'teal', width = 3,
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
    tags$p("Prostate cancer is the second most common cancer in men worldwide. 
           An estimated 1,276,106 new cases and 358,989 deaths were reported in 2018 [",
           a("GLOBOCAN", 
             href = "https://acsjournals.onlinelibrary.wiley.com/doi/full/10.3322/caac.21492"), '].', 
           style = "font-size: 120%;"),
    
    #column(width = 600) {
    
    #}
    #tags$img(src='prostate_cancer.png', width=1000), # in www
    tags$img(src='The-scheme-of-different-stages-and-progression-of-prostate-cancer.png', width=600),
    
    br(),
    tags$hr(style="border-top: 1px dashed #A9A9A9"),
    h5(strong("About PCaDB")),
    tags$p('We collected 50 public transcriptomics datasets from prostate cancer population cohorts.', style = "font-size: 120%;"),
    
    column(12,
    column(1),
    column(4,
    plotlyOutput('pie_sample_type_introduction', width='100%', height='300px')
    ),
    column(7)
    ),
    
    column(12,
           br(),
           tags$hr(style="border-top: 1px dashed #A9A9A9")
           ),
    # br(),
    # tags$hr(style="border-top: 1px dashed #A9A9A9"),
    
    h5(strong("Citation")),
    tags$p('Please cite the following publication:
           Li, R. and Jia, Z., PCaDB - a database and web server for integrating transcriptomics data in prostate cancer population cohorts. bioRxiv (2021)', style = "font-size: 120%;"),
    tags$p(HTML("<a href='https://www.biorxiv.org/content/10.1101/2020.10.04.325670v1' target='_blank'><h5>https://www.biorxiv.org/content/10.1101/2020.10.04.325670v1</h5></a>"))
    
    
  )
)


### Query

tab_query <- fluidRow(
    
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
    tabsetPanel(id = 'query', type='pills', #widths = c(2,10),
                
                tabPanel(strong("Gene Expression"),
                         br(),
                         hr(),
                         
                         column(12,
                                column(11,
                                       h4("Gene Expression in Different Sample Types", align = 'center'),
                                       h5("(Wilcoxon rank-sum test, ***: P < 0.001; **: P < 0.01; *: P < 0.05; ns: P > 0.05)", align = 'center'),
                                       tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                       expr.dataset.id,
                                       br(),
                                       withSpinner(uiOutput('query_boxplot_ui'), type = 4, proxy.height=300)
                                       
                                ),
                                column(10),
                                column(2,
                                       downloadButton(outputId='query.box.summ.downbttn.csv', label = "CSV"),
                                       #downloadButton(outputId='tcga.box.summ.downbttn.png', label = "PNG"),
                                       downloadButton(outputId='query.box.summ.downbttn.pdf', label = "PDF")
                                )
                                
                         )
                ),
                
                tabPanel(strong("Survival Analysis"),
                         br(),
                         hr(),
                         column(12,
                                h4("Kaplan Meier Survival Analysis (Forest Plot)", align = 'center'),
                                h5("(Kaplan Meier, ***: P < 0.001; **: P < 0.01; *: P < 0.05; ns: P > 0.05)", align = 'center'),
                                #tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                column(1),
                                column(11,
                                       #expr.dataset.id,
                                       withSpinner(plotOutput('survival_forest',width = 800, height = 600), type=4, proxy.height=300)
                                       
                                )
                         ),
                         column(12,
                                br(),
                                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                h4("Kaplan Meier Survival Curve", align = 'center'),
                                h5("(Kaplan Meier, ***: P < 0.001; **: P < 0.01; *: P < 0.05; ns: P > 0.05)", align = 'center'),
                                column(1),
                                column(11,
                                       withSpinner(plotOutput('survival_km',width = 680, height = 1750), type=4, proxy.height=300) # width = 1100, height = 1360)
                                       )
                                
                                )
                         
                         
                         
                         # column(10),
                         # column(2,
                         #        downloadButton(outputId='query.box.summ.downbttn.csv', label = "CSV"),
                         #        #downloadButton(outputId='tcga.box.summ.downbttn.png', label = "PNG"),
                         #        downloadButton(outputId='query.box.summ.downbttn.pdf', label = "PDF")
                         # )
                         ),
                
                tabPanel(strong("Single-Cell RNAseq"),
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
                                       
                                       withSpinner(plotOutput('henry_sc_tsne_cell',width = 410, height = 400),
                                                   type = 4, proxy.height=300)
                                       # br(),
                                       # column(7),
                                       # column(5,
                                       #        downloadButton(outputId='tcga.box.downbttn.csv', label = "CSV"),
                                       #        #downloadButton(outputId='tcga.box.downbttn.png', label = "PNG"),
                                       #        downloadButton(outputId='tcga.box.downbttn.pdf', label = "PDF")
                                       # )
                                       
                                ),
                                
                                column(1),
                                column(5,
                                       withSpinner(plotOutput('henry_sc_umap_cell',width = 410, height = 400),
                                                   type = 4, proxy.height=300)
                                       # br(),
                                       # column(7),
                                       # column(5,
                                       #        downloadButton(outputId='tcga.box.downbttn.csv', label = "CSV"),
                                       #        #downloadButton(outputId='tcga.box.downbttn.png', label = "PNG"),
                                       #        downloadButton(outputId='tcga.box.downbttn.pdf', label = "PDF")
                                       # )
                                       
                                )
                                ),
                         
                         br(),
                         br(),
                         
                         column(12,
                                br(),
                                br(),
                                column(5,
                                       plotOutput('henry_sc_tsne_expr',width = 500, height = 400)#,
                                       # br(),
                                       # column(7),
                                       # column(5,
                                       #        downloadButton(outputId='tcga.box.downbttn.csv', label = "CSV"),
                                       #        #downloadButton(outputId='tcga.box.downbttn.png', label = "PNG"),
                                       #        downloadButton(outputId='tcga.box.downbttn.pdf', label = "PDF")
                                       # )
                                       
                                ),
                                
                                column(1),
                                
                                column(5,
                                       plotOutput('henry_sc_umap_expr',width = 500, height = 400)#,
                                       # br(),
                                       # column(7),
                                       # column(5,
                                       #        downloadButton(outputId='tcga.box.downbttn.csv', label = "CSV"),
                                       #        #downloadButton(outputId='tcga.box.downbttn.png', label = "PNG"),
                                       #        downloadButton(outputId='tcga.box.downbttn.pdf', label = "PDF")
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
                                #        downloadButton(outputId='tcga.box.downbttn.csv', label = "CSV"),
                                #        #downloadButton(outputId='tcga.box.downbttn.png', label = "PNG"),
                                #        downloadButton(outputId='tcga.box.downbttn.pdf', label = "PDF")
                                # )
                                
                         ),
                         column(12,
                                br(),
                                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                plotOutput('henry_sc_violin',width = 900, height = 350)#,
                                # br(),
                                # column(7),
                                # column(5,
                                #        downloadButton(outputId='tcga.box.downbttn.csv', label = "CSV"),
                                #        #downloadButton(outputId='tcga.box.downbttn.png', label = "PNG"),
                                #        downloadButton(outputId='tcga.box.downbttn.pdf', label = "PDF")
                                # )
                                
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
    tags$p("A comprehensive collection of 30 gene expression-based prognostic signatures for prostate cancer is 
           included in PCaDB. The performances of the signatures has been investigated in the study [",
           a("Comprehensive evaluation of machine learning models and gene expression signatures for prostate cancer prognosis using large population cohorts", 
             href = "https://acsjournals.onlinelibrary.wiley.com/doi/full/10.3322/caac.21492"), "].", 
           style = "font-size: 120%;"),
    br(),
    column(6,
           tags$img(src='signature_table.png',width=450)),
    column(6,
           tags$img(src='signature_circos.png',width=450)),
    tags$p("Comprehensive characterization of the signatures", style = "font-size: 120%;")
    
  ),
  
  box(
    title = NULL, solidHeader = FALSE, collapsible = FALSE, #status = "success",
    width = 12,
    
    #navlistPanel
    tabsetPanel(id = 'signature', type='pills', #widths = c(2,10),
                
                tabPanel(strong("Overview"),
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
                                br(),
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
                                h4('Volcano Plot of Differentially Expressed Genes in the Signatures', align='center'),
                                h5('(|Fold Change| > 2 & FDR < 0.01)', align='center'),
                                
                                column(3),
                                column(6, withSpinner(plotOutput("signature.de.volcano", height = 400), type=4, proxy.height=300)),
                                column(3)
                                ),
                         
                         column(12,
                                br(),
                                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                
                                h4('List of Differentially Expressed Genes in the Signatures', align='center'),
                                h5('(|Fold Change| > 2 & FDR < 0.01)', align='center'),
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
                                h4('Association of Genes in More Than 2 Signatures with BCR', align='center'),
                                h5('(Kaplan Meier Survival Analysis, |Hazard Ratio| > 1 & FDR < 0.01)', align='center'),

                                column(1),
                                column(10, withSpinner(plotOutput("signature.bcr.forest", height = 1200), type=4, proxy.height=300)),
                                column(1)
                         ),
                         
                         
                         
                         column(12,
                                br(),
                                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                
                                h4('List of Genes Associated with BCR in the Signatures', align='center'),
                                h5('(Kaplan Meier Sruvival Analysis, |Hazard Ratio| > 2 & FDR < 0.01)', align='center'),
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
                                h4('Functional Enrichment Analysis of Signature', align='center'),
                                br(),
                                DT::dataTableOutput("signature.enrich.table")
                         ),
                         
                         column(12, 
                                tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                column(1),
                                column(10,
                                       h5('Bar Plot of the Top 30 Enriched Pathways', align='center'),
                                       withSpinner(plotOutput('signature_enrichment_bar_plot',width = 800, height = 500), type = 4, proxy.height = 300)
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
                                       withSpinner(plotOutput('signature_enrichment_bubble_plot',width = 800, height = 500), type = 4, proxy.height = 300)
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
                                       
                                       h5('Comparison', align='center'),
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
                                                        withSpinner(plotOutput('signature_all_c_index_plot',width = 900, height = 750), type = 4, proxy.height = 300)
                                                 )),
                                column(12,
                                conditionalPanel(condition = 'input.signature_comp_signature_input=="All Signatures"',
                                                 tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                                        h5('KM', align='center'),
                                                        withSpinner(plotOutput('signature_all_km_plot',width = 900, height = 750), type = 4, proxy.height = 300)
                                                 )),
                                column(12,
                                conditionalPanel(condition = 'input.signature_comp_signature_input=="All Signatures"',
                                                 tags$hr(style="border-top: 1px dashed #A9A9A9"),
                                                        h5('AUC', align='center'),
                                                        withSpinner(plotOutput('signature_all_auc_plot',width = 900, height = 750), type = 4, proxy.height = 300)
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
    div(DT::dataTableOutput("dataset"),style = "font-size:90%")
  ),
  
  box(
    title = NULL, solidHeader = FALSE, collapsible = FALSE, #status = "success", 
    width = 12,
    
    tabsetPanel(id = 'transcriptome', type='pills', #widths = c(2,10),
    #tabBox(width = 12, id = 'transcriptome',
   
           tabPanel(strong("Summary"),
                    br(),
                    hr()
           ),
           tabPanel(strong("PCA"),
                    br(),
                    hr()
           ),
           tabPanel(strong("Differential Expression"),
                    br(),
                    hr()
           ),
           tabPanel(strong("Survival Analysis"),
                    br(),
                    hr()
           ),
           tabPanel(strong("Pathway Analysis"),
                    br(),
                    hr()
           ),
           
           tabPanel(strong("Prognostic Model"),
                    br(),
                    hr()
           )
           
           )
    )
)



### Pipeline

tab_pipelines <- fluidRow(
  
  box(
    title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
    width = 12, height = 55, 
    
    #h4(strong("PCaDB Pipelines"), align='center', style='font-family:Georgia;color:#2C3E50')
    h4(strong(icon("pencil"), HTML('&nbsp;'), "PCaDB Pipeline"), align='center', style='font-family:Georgia;color:#2C3E50')
  )
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
           tags$li(a(href='PCaDB_Transcriptome_Datasets_Summary.xlsx', target='_blank', 
                     'PCaDB_Transcriptome_Datasets_Summary.xlsx')),
           br(),
           h6(strong("ExpressionSet"), align='left', style='color:black')
    ),
    
    column(3,
           tags$li(a(href='TCGA-PRAD_eSet.RDS', target='_blank', 'TCGA-PRAD')),
           tags$li(a(href='CPC-Gene_eSet.RDS', target='_blank', 'CPC-Gene (GSE107299)')),
           tags$li(a(href='Taylor_eSet.RDS', target='_blank', 'Taylor (GSE21034)')),
           tags$li(a(href='DKFZ_eSet.RDS', target='_blank', 'DKFZ (EGAS00001002923)')),
           tags$li(a(href='GSE54460_eSet.RDS', target='_blank', 'GSE54460')),
           tags$li(a(href='Cambridge_eSet.RDS', target='_blank', 'Cambridge (GSE70768)')),
           tags$li(a(href='Stockholm_eSet.RDS', target='_blank', 'Stockholm (GSE70769)')),
           tags$li(a(href='CancerMap_eSet.RDS', target='_blank', 'CancerMap (GSE94767)')),
           tags$li(a(href='CIT_eSet.RDS', target='_blank', 'CIT (E-MTAB-6128)')),
           tags$li(a(href='Belfast_eSet.RDS', target='_blank', 'Belfast (GSE116918)')),
           tags$li(a(href='GSE25136_eSet.RDS', target='_blank', 'GSE25136')),
           tags$li(a(href='GSE41408_eSet.RDS', target='_blank', 'GSE41408')),
           tags$li(a(href='GSE46691_eSet.RDS', target='_blank', 'GSE46691'))
    ),
    column(3, 
           tags$li(a(href='GSE51066_eSet.RDS', target='_blank', 'GSE51066')),
           tags$li(a(href='GSE37199_eSet.RDS', target='_blank', 'GSE37199')),
           tags$li(a(href='GSE44353_eSet.RDS', target='_blank', 'GSE44353')),
           tags$li(a(href='GSE59745_eSet.RDS', target='_blank', 'GSE59745')),
           tags$li(a(href='GSE79021_eSet.RDS', target='_blank', 'GSE79021')),
           tags$li(a(href='GSE3933-GPL2695_eSet.RDS', target='_blank', 'GSE3933-GPL2695')),
           tags$li(a(href='GSE35988-GPL6480_eSet.RDS', target='_blank', 'GSE35988-GPL6480')),
           tags$li(a(href='GSE35988-GPL6848_eSet.RDS', target='_blank', 'GSE35988-GPL6848')),
           tags$li(a(href='SU2C-PCF-2019-Capture_eSet.RDS', target='_blank', 'SU2C-PCF-2019-Capture')),
           tags$li(a(href='SU2C-PCF-2019-PolyA_eSet.RDS', target='_blank', 'SU2C-PCF-2019-PolyA')),
           tags$li(a(href='GSE62116_eSet.RDS', target='_blank', 'GSE62116')),
           tags$li(a(href='GSE62667_eSet.RDS', target='_blank', 'GSE62667')),
           tags$li(a(href='GSE79957_eSet.RDS', target='_blank', 'GSE79957'))
    ),
    column(3, 
           
           tags$li(a(href='GSE79956_eSet.RDS', target='_blank', 'GSE79956')),
           tags$li(a(href='GSE79958_eSet.RDS', target='_blank', 'GSE79958')),
           tags$li(a(href='GSE72291_eSet.RDS', target='_blank', 'GSE72291')),
           tags$li(a(href='GSE79915_eSet.RDS', target='_blank', 'GSE79915')),
           tags$li(a(href='GSE3325_eSet.RDS', target='_blank', 'GSE3325')),
           tags$li(a(href='GSE32269_eSet.RDS', target='_blank', 'GSE32269')),
           tags$li(a(href='GSE6919-GPL8300_eSet.RDS', target='_blank', 'GSE6919-GPL8300')),
           tags$li(a(href='GSE6919-GPL92_eSet.RDS', target='_blank', 'GSE6919-GPL92')),
           tags$li(a(href='GSE6919-GPL93_eSet.RDS', target='_blank', 'GSE6919-GPL93')),
           tags$li(a(href='GSE6752_eSet.RDS', target='_blank', 'GSE6752')),
           tags$li(a(href='GSE17951_eSet.RDS', target='_blank', 'GSE17951')),
           tags$li(a(href='GSE8218_eSet.RDS', target='_blank', 'GSE8218'))
           
    ),
    column(3,
           tags$li(a(href='E-TABM-26-U133A_eSet.RDS', target='_blank', 'E-TABM-26-U133A')),
           tags$li(a(href='E-TABM-26-U133B_eSet.RDS', target='_blank', 'E-TABM-26-U133B')),
           tags$li(a(href='GSE5132-GPL3834_eSet.RDS', target='_blank', 'GSE5132-GPL3834')),
           tags$li(a(href='GSE29079_eSet.RDS', target='_blank', 'GSE29079')),
           tags$li(a(href='GSE97284_eSet.RDS', target='_blank', 'GSE97284')),
           tags$li(a(href='GSE85698_eSet.RDS', target='_blank', 'GSE85698')),
           tags$li(a(href='GSE2109_eSet.RDS', target='_blank', 'GSE2109')),
           tags$li(a(href='GSE62872_eSet.RDS', target='_blank', 'GSE62872')),
           tags$li(a(href='SMMU_eSet.RDS', target='_blank', 'SMMU')),
           tags$li(a(href='GSE77930_eSet.RDS', target='_blank', 'GSE77930')),
           tags$li(a(href='Neuroendocrine_eSet.RDS', target='_blank', 'Neuroendocrine')),
           tags$li(a(href='Broad-Cornell_eSet.RDS', target='_blank', 'Broad-Cornell'))
    )
    ),
    
    box(
      title = strong('Prognostic Signatures'), status = "info", solidHeader = TRUE, collapsible = FALSE,
      width = 12,
    
    column(12, 
           h6(strong("Summary"), align='left', style='color:black'),
           tags$li(a(href='PCaDB_Prognostic_Signatures_Summary.xlsx', target='_blank', 
                     'PCaDB_Prognostic_Signatures.xlsx')),
           br()),
    
    column(12, 
           h6(strong("Gene List"), align='left', style='color:black'),
           tags$li(a(href='PCaDB_Prognostic_Signatures_Gene_List.xlsx', target='_blank', 
                     'PCaDB_Prognostic_Signatures_Gene_List.xlsx')))
    ),
    
    box(
      title = strong('Gene Annotation'), status = "info", solidHeader = TRUE, collapsible = FALSE,
      width = 12,
    column(12, 
           tags$li(a(href='PCaDB_Gene_Annotation.RDS', target='_blank', 
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
    title = 'Dataset', status = "primary", solidHeader = TRUE, collapsible = FALSE,
    width = 12
  ),
  
  box(
    title = 'Dataset', status = "info", solidHeader = TRUE, collapsible = FALSE,
    width = 12
  ),
  
  box(
    title = 'Dataset', status = "success", solidHeader = TRUE, collapsible = FALSE,
    width = 12
  ),
  box(
    title = 'Dataset', status = "warning", solidHeader = TRUE, collapsible = FALSE,
    width = 12
  ),
  box(
    title = 'Dataset', status = "danger", solidHeader = TRUE, collapsible = FALSE,
    width = 12
  )
  
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
  
  includeCSS("www/css/style.css"),
  #selectizeInput(inputId = "gene", label='Gene', choices = gene.annotation, selected = gene.default, multiple=FALSE, 
  #               options = list(
  #                 placeholder = 'Select a gene', maxOptions = 10,
  #                 selectOnTab=TRUE)),
  
  #tags$head(tags$style(HTML('.box {margin: 1px;}'))), # distance between box
  
  theme_blue_gradient,
  tags$head(tags$meta(name = "viewport", content = "width=1260")),
  
  
  #https://stackoverflow.com/questions/45706670/shiny-dashboadpage-lock-dashboardheader-on-top
  tags$script(HTML("$('body').addClass('fixed');")), # fix header & sidebar
  
  tabItems(
    tabItem(tabName="tab_home", tab_home),
    tabItem(tabName="tab_query", tab_query),
    tabItem(tabName="tab_signature",tab_signature),
    tabItem(tabName="tab_transcriptome",tab_transcriptome),
    tabItem(tabName="tab_pipelines", tab_pipelines),
    tabItem(tabName="tab_download", tab_download),
    tabItem(tabName="tab_tutorial", tab_tutorial),
    tabItem(tabName="tab_contact", tab_contact)
  )
  
)

#footer=dashboardFooter(right_text = HTML('<footer><script type="text/javascript" src="//rf.revolvermaps.com/0/0/2.js?i=59d9778kul4&amp;m=0&amp;s=70&amp;c=ff0000&amp;t=1" async="async"></script></footer>'),
#                       #https://www.revolvermaps.com/
#                       left_text = HTML("<footer><h6>Contact: <a href='https://github.com/rli012' target='_blank'>Ruidong Li</a><br>Email: rli012@ucr.edu</h6><strong><h5><a href='http://jialab.ucr.acsitefactory.com/' target='_blank'>Jia Lab @ University of California, Riverside</a></h5></strong></footer>"))
#left_text = HTML("<footer><h6>\t\tCopyright &#169 2020 <a href='http://jialab.ucr.acsitefactory.com/' target='_blank'>Jia Lab</a>. <br><a href='https://plantbiology.ucr.edu/' target='_blank'>Department of Botany & Plant Sciences</a>, <br><a href='https://plantbiology.ucr.edu/' target='_blank'>University of California, Riverside</a></h6></footer>"))

ui <- dashboardPagePlus(title='PCaDB', header, sidebar, body) # skin = 'blue', footer = footer


######## Server

server <- function(input, output, session) { 
  
  shinyjs::hide(selector = ".navbar > .sidebar-toggle")
  
  updateSelectizeInput(session, 'gene.id', choices = gene.annotation, selected = gene.default, server = TRUE)
  updateSelectizeInput(session, 'expr.dataset.id', choices = expr.dataset, selected = expr.dataset.default, server = TRUE)
  updateSelectizeInput(session, 'overview.signature', choices = signature.name, selected = signature.default, server = TRUE)
  
  updateSelectizeInput(session, 'de.dataset', choices = signature.de.dataset, selected = de.dataset.default, server = TRUE)
  updateSelectizeInput(session, 'bcr.dataset.input', choices = bcr.dataset, selected = bcr.dataset.default, server = TRUE)
  updateSelectizeInput(session, 'signature.pathway.input', choices = signature.geneset, selected = signature.geneset.default, server = TRUE)
  updateSelectizeInput(session, 'signature.ontology.input', choices = signature.ontology, selected = signature.ontology.default, server = TRUE)
  
  updateSelectizeInput(session, 'signature_comp_signature_input', choices = c('All Signatures', signature.name), selected = signature.comp.signature.default, server = TRUE)
  updateSelectizeInput(session, 'signature.comp.training.input', choices = bcr.dataset, selected = signature.comp.training.default, server = TRUE)
  updateSelectizeInput(session, 'signature.comp.model.input', choices = signature.models, selected = signature.comp.model.default, server = TRUE)
  
  
  output$pie_sample_type_introduction <- renderPlotly({
    
    dataForPiePlot <- readRDS(file='data/PCaDB_Pie_Sample_Type.RDS')
    
    p <- piePlotlyFun(dataForPiePlot)
    p
  })
  
  
  gene.id <- reactive({
    input$gene.id
  })
  
  
  
  ############################################################
  ###                         Query                        ###
  ############################################################
  
  #observeEvent(input$gene.id, {
  observe({
    req(input$gene.id)
    
    
    ### Gene Information
    
    #output$gene.symbol <- renderUI({ 
      #gene.id <- gene.id()
      #gene.symbol <- gene.annotation[gene.id, 'gene_name']
      #mir.url <- paste0('http://www.mirbase.org/cgi-bin/mature.pl?mature_acc=', mir.id)
      #mir.url <- a(mir.name, href = mir.url, target="_blank", style = "font-size: 150%;")
      #tagList(mir.url)
    #})
    
    output$gene.symbol <- renderText({ 
      gene.id <- gene.id()
      gene.symbol <- gene.annotation[gene.id, 'gene_name']
      gene.symbol
    })
    
    output$gene.alias <- renderText({ 
      gene.id <- gene.id()
      gene.alias <- gene.annotation[gene.id, 'alias_symbol']
      gene.alias <- paste0('Alias: ', gene.alias)
      gene.alias
    })
    
    
    output$gene.ensembl.id <- renderText({ 
      gene.id <- gene.id()
      gene.ensembl.id <- gene.annotation[gene.id, 'ensembl_id']
      gene.ensembl.id <- paste0('Ensembl ID: ', gene.ensembl.id)
      gene.ensembl.id
    })
    
    output$gene.entrez.id <- renderText({ 
      gene.id <- gene.id()
      gene.entrez.id <- gene.annotation[gene.id, 'entrez_id']
      gene.entrez.id <- paste0('Entrez ID: ', gene.entrez.id)
      gene.entrez.id
    })
    
    output$gene.description <- renderText({ 
      gene.id <- gene.id()
      gene.description <- gene.annotation[gene.id, 'description']
      gene.description <- paste0('Description: ', gene.description)
      gene.description
    })
    
    
    output$gene.external <- renderUI({ 
      gene.id <- gene.id()
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
      
      
      tagList("External links:", ensembl.link, hgnc.link, ncbi.link, gtex.link, hpa.link, kegg.link)
    })
    
  })
  
  ### Expression
  
  expr.dataset.id <- reactive({
    input$expr.dataset.id
    
  })
  
  output$query_boxplot <- renderPlot({
    
    gene.id <- gene.id()
    expr.dataset.id <- expr.dataset.id()
    
    req(length(expr.data[[expr.dataset.id]][gene.id,])>0)
    
    dataForBoxPlot <- data.frame(expr=expr.data[[expr.dataset.id]][gene.id,],
                                 group=meta.data[[expr.dataset.id]][,'sample_type'],
                                 stringsAsFactors = F)
    
    if (expr.dataset.id=='Taylor') {
      filter <- which(dataForBoxPlot$group=='Cell line')
      dataForBoxPlot <- dataForBoxPlot[-filter,]
    }
    
    p <- ExprBoxPlotFun(dataForBoxPlot, colors)
    p
    
  })#, height = 400, width = 600)
  
  
  plotWidth <- reactive({
    expr.dataset.id <- expr.dataset.id()
    
    n.sample.type <- length(unique(meta.data[[expr.dataset.id]][,'sample_type']))
    
    if (n.sample.type<=3) {
      plotWidth <- 400
    } else if (n.sample.type > 3 & n.sample.type <= 6) {
      plotWidth <- 600
    } else if (n.sample.type > 6) {
      plotWidth <- 900
    }
    
    if (expr.dataset.id=='Taylor') {
      plotWidth <- 400
    }
    
    plotWidth
    
  })
  
  
  plotHeight <- reactive({
    expr.dataset.id <- expr.dataset.id()
    
    if (expr.dataset.id %in% c('GSE32269','SU2C-PCF-2019-PolyA','GSE6752','GSE77930','Neuroendocrine')) {
      plotHeight <- 450
    } else if (expr.dataset.id %in% c('GSE6919-GPL8300','GSE6919-GPL92','GSE6919-GPL93'
                                      )) {
      plotHeight <- 500
    } else {
      plotHeight <- 350
    }
    
    plotHeight
    
  })
  
  output$query_boxplot_ui <- renderUI({
    plotOutput("query_boxplot", height = plotHeight(), width = plotWidth())
  })
  
  ### Survival
  
  # bcr.dataset <- reactive({
  #   bcr.dataset
  #   
  # })
  
  
  dataForSurvivalAnalysis <- reactive({
    gene.id <- gene.id()
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
    
    kmTable <- data.frame(kmTable, stringsAsFactors = F) #row.names = NULL, 
    colnames(kmTable) <- c('N','HR','Lower95','Upper95','P.Value','Dataset')
    rownames(kmTable) <- datasets
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
  
  output$survival_forest <- renderPlot({
    
    dataForForestPlot <- kmTable()
    p <- tcgaKMForestplotFunT(dataForForestPlot)
    p
    
  })
  
  output$survival_km <- renderPlot({
    
    dataForSurvivalAnalysis <- dataForSurvivalAnalysis()
    datasets <- unique(dataForSurvivalAnalysis$dataset)
    
    KMPlotList <- list()
    
    for (dt in datasets) {
      
      idx <- which(dataForSurvivalAnalysis$dataset==dt)
      c('expr','time.to.bcr','bcr.status','dataset')
      
      dataForKMPlot <- dataForSurvivalAnalysis[idx,]
      
      KMPlotList[[dt]] <- KMPlotFun(dataForKMPlot, dt=dt)
      
    }
    
    p <- grid.arrange(grobs=KMPlotList, ncol = 2)
    
    p
    

  })
  
  ### Single Cell
  
  dataForTSNEPlot <- reactive({
    gene.id <- gene.id()
    
    dataForTSNEPlot <- data.frame(tSNE.1=scDataHenry$tsne[,1],
                                  tSNE.2=scDataHenry$tsne[,2],
                                  cell.type=as.character(scDataHenry$annotation$Population),
                                  expr=as.numeric(scDataHenry$expr[gene.id,]),
                                  gene=gene.annotation[gene.id, 'gene_name'],
                                  stringsAsFactors = F)
    
    dataForTSNEPlot
    
  })
  
  
  dataForUMAPPlot <- reactive({
    gene.id <- gene.id()
    
    dataForUMAPPlot <- data.frame(UMAP.1=scDataHenry$umap[,1],
                                  UMAP.2=scDataHenry$umap[,2],
                                  cell.type=as.character(scDataHenry$annotation$Population),
                                  expr=as.numeric(scDataHenry$expr[gene.id,]),
                                  gene=gene.annotation[gene.id, 'gene_name'],
                                  stringsAsFactors = F)
    
    dataForUMAPPlot
    
  })

  output$henry_sc_tsne_cell <- renderPlot({
    
    dataForTSNEPlot <- dataForTSNEPlot()
    p <- scTSNEPlotFun(dataForTSNEPlot, expr=FALSE)
    p
    
  })
  
  output$henry_sc_tsne_expr <- renderPlot({
    
    dataForTSNEPlot <- dataForTSNEPlot()
    p <- scTSNEPlotFun(dataForTSNEPlot, expr=TRUE)
    p
    
  })
  
  
  output$henry_sc_umap_cell <- renderPlot({
    
    dataForUMAPPlot <- dataForUMAPPlot()
    p <- scUMAPPlotFun(dataForUMAPPlot, expr=FALSE)
    p
    
  })
  
  output$henry_sc_umap_expr <- renderPlot({
    
    dataForUMAPPlot <- dataForUMAPPlot()
    p <- scUMAPPlotFun(dataForUMAPPlot, expr=TRUE)
    p
    
  })
  
  
  output$henry_sc_bubble <- renderPlot({
    
    dataForBubblePlot <- dataForTSNEPlot() %>% group_by(gene, cell.type) %>% 
      summarise(mean.all=mean(expr), 
                mean.expressed=ifelse(sum(expr>0)>0, sum(expr)/sum(expr>0), 0),
                percent.expressed=sum(expr>0)/length(expr)*100)
    
    p <- scBubblePlotFun(dataForBubblePlot)
    p
    
  })
  
  
  output$henry_sc_violin <- renderPlot({
    
    dataForViolinPlot <- dataForTSNEPlot()
    p <- scViolinPlotFun(dataForViolinPlot)
    p
    
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
      
    })
    
    
    # output$enrich.bar.downbttn.csv <- downloadHandler(
    #   filename = function(){paste('enrich.bar.csv', sep = '')},
    #   
    #   content = function(file){
    #     write.csv(tcga.enrich$enrich.bar.data, file, row.names = FALSE, quote = F)
    #   })
    # 
    # output$enrich.bar.downbttn.png <- downloadHandler(
    #   filename = function(){paste('enrich.bar.png', sep = '')},
    #   
    #   content = function(file){
    #     png(file, width = 1000, height = 700)
    #     print(tcga.enrich$enrich.bar.plot)
    #     dev.off()
    #   })
    
    # output$enrich.bar.downbttn.pdf <- downloadHandler(
    #   filename = function(){paste('enrich.bar.pdf', sep = '')},
    #   
    #   content = function(file){
    #     pdf(file, width = 10, height = 7)
    #     print(tcga.enrich$enrich.bar.plot)
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
      
    })
    
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
      geom_point(color=google.red, size=2, shape=15) + #facet_grid(.~type) +
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
      theme(axis.text=element_text(size=11, face = 'bold', color = 'black'),
            axis.title=element_text(size=12, face = 'bold'),
            axis.line = element_line(colour = "black"),
            axis.line.y = element_blank(),
            strip.text = element_text(size=12, face='bold')) +
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
      geom_point(color=google.red, size=2, shape=15) + #facet_grid(.~type) +
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
      theme(axis.text=element_text(size=11, face = 'bold', color = 'black'),
            axis.title=element_text(size=12, face = 'bold'),
            axis.line = element_line(colour = "black"),
            axis.line.y = element_blank(),
            strip.text = element_text(size=12, face='bold')) +
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
      geom_point(color=google.red, size=2, shape=15) + #facet_grid(.~type) +
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
      theme(axis.text=element_text(size=11, face = 'bold', color = 'black'),
            axis.title=element_text(size=12, face = 'bold'),
            axis.line = element_line(colour = "black"),
            axis.line.y = element_blank(),
            strip.text = element_text(size=12, face='bold')) +
      theme(strip.background = element_rect(fill=col))
    
    p
    
  })
  
    
    
  })
                   
                   
  ############################################################
  ###                Transcriptome Analysis                ###
  ############################################################
  
  output$dataset <- DT::renderDataTable({pcadb.dataset},
                                        options = list(pageLength = 5,
                                                       scrollX=TRUE),
                                        selection = list(mode='single', selected=1)
  )
  
}



shinyApp(
  ui = ui,
  server = server
)

