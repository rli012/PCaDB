
setwd('C:\\Users/rli3/Documents/Publications/PCaDB/')

library(shiny)
library(shinydashboard)
library(shinyjs)
library(readxl)
library(ggplot2)
library(Matrix)
library(stringr)
library(Biobase)
library(survival)
library(survminer)
library(limma)
library(edgeR)

library(shinydashboardPlus)
library(shinythemes)
library(dashboardthemes)

source('shiny_functions.R')
source('load_data.R')


############################################################################################

################### Theme

logo_blue_gradient <- shinyDashboardLogoDIY(
  
  boldText = "PCaDB",
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
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
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




##### ui

header=dashboardHeader(title = logo_blue_gradient, disable = FALSE)
#header=dashboardHeader(title = 'PCaDB')
#header=dashboardHeaderPlus(title = logo_blue_gradient, fixed = TRUE)


sidebar=dashboardSidebar(
  
  ### header color
  # https://stackoverflow.com/questions/50467136/how-to-create-custom-shinydashboard-skin
  # tags$head(tags$style(HTML('.logo {
  #                             background-color: #4285F4 !important;
  #                             }
  #                             .navbar {
  #                             background-color: #4285F4 !important;
  #                             }
  #                             '))),
  
  sidebarMenu(
    style = 'position:fixed; overflow: visible', # 
    
    # tags$div(h5("Introduction"),
    #          style = "margin-top: 12px; font-size: 1em; padding: 0 0.5em; #text-align: center; 
    #          color: white;"),
    hr(style="border-top: 1px solid white"),
    menuItem("Home", tabName = "tab_home", icon = icon("home"), selected = T),
    
    # hr(),
    # tags$div(p(strong("Gene-Level Analysis")),
    #          style = "margin-top: 0px; font-family: 'Georgia'; font-size: 1em; padding: 0 0.5em; #text-align: center; 
    #          color: black;"),
    # menuItem("Gene Expression", tabName = 'tab_boxplot', icon = icon("dna")#,
    #          #menuSubItem("Boxplot" , tabName = "tab_boxplot", icon = icon("bar-chart")),
    #          #menuSubItem("Heatmap" , tabName = "heatmap", icon = icon("scatter-chart"))
    # ),
    # menuItem("Survival Analysis" , tabName = "tab_kmplot", icon = icon("pencil")),
    # #br(),
    # tags$div(p(strong("Dataset-Level Analysis")),
    #          style = "margin-top: 12px; font-family: 'Georgia'; font-size: 1em; padding: 0 0.5em; #text-align: center; 
    #          color: black;"),
    # menuItem("Differential Expression", tabName = 'tab_dataset', icon = icon("database")),
    # menuItem("Pathway Analysis", tabName = 'pathway', icon = icon("database")),
    # #menuItem("Co-expression Analysis", tabName = 'coexpression', icon = icon("burn")),
    # menuItem("Signature Analysis", tabName = 'signature', icon = icon("dna")),
    # 
    # hr(),
    menuItem("Query", tabName = "tab_query", icon = icon("search")),
    menuItem("Transcriptome Analysis", tabName = "tab_analysis", icon = icon("bar-chart")),
    menuItem("PCaDB Pipelines", tabName = "tab_pipelines", icon = icon("pencil")),
    menuItem("Download", tabName = "tab_download", icon = icon("download")),
    menuItem("Tutorial", tabName = "tab_tutorial", icon = icon("file-alt")),
    menuItem("Contact", tabName = "tab_contact", icon = icon("envelope"))
  )
)




gene.expression <- selectizeInput(inputId = "gene.expression", label=h4(strong('Gene')), choices = NULL, selected = gene.default, 
                                  multiple = FALSE, width = 300,
                                  options = list(placeholder = 'Select a gene',
                                                 server = TRUE, selectOnTab=TRUE,
                                                 searchField = c('external_gene_name', 'alias_symbol', 'description', 'ensembl_id', 'entrez_id'),
                                                 labelField = "external_gene_name",
                                                 valueField = "ensembl_id",
                                                 maxOptions = 5,
                                                 render = I("{option: function(item, escape) {
                                                            var gene = '<div>' + '<strong>' + escape(item.external_gene_name) + '</strong>:' + '<ul>';
                                                            gene = gene + '<li>' + item.alias_symbol + '</li>';
                                                            gene = gene + '<li>' + item.description + '</li>';
                                                            gene = gene + '<li>' + 'Entrez: ' + item.entrez_id + '</li>';
                                                            gene = gene + '<li>' + 'Ensembl: ' + item.ensembl_id + '</li>' + '</ul>' + '</div>';
                                                            return gene
                                                            }
                                                            }")
                                  ))

gene.survival <- selectizeInput(inputId = "gene.survival", label=h4(strong('Gene')), choices = NULL, selected = gene.default, 
                                multiple = FALSE, width = 300,
                                options = list(placeholder = 'Select a gene',
                                               server = TRUE, selectOnTab=TRUE,
                                               searchField = c('external_gene_name', 'alias_symbol', 'description', 'ensembl_id', 'entrez_id'),
                                               labelField = "external_gene_name",
                                               valueField = "ensembl_id",
                                               maxOptions = 5,
                                               render = I("{option: function(item, escape) {
                                                          var gene = '<div>' + '<strong>' + escape(item.external_gene_name) + '</strong>:' + '<ul>';
                                                          gene = gene + '<li>' + item.alias_symbol + '</li>';
                                                          gene = gene + '<li>' + item.description + '</li>';
                                                          gene = gene + '<li>' + 'Entrez: ' + item.entrez_id + '</li>';
                                                          gene = gene + '<li>' + 'Ensembl: ' + item.ensembl_id + '</li>' + '</ul>' + '</div>';
                                                          return gene
                                                          }
                                                          }")
                                ))


### Gene Expression
tab_boxplot <- fluidRow(
  
  #style='margin-left:0em',
  
  box(
    title = NULL, status = "primary", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    gene.expression,
    
    radioButtons(inputId = "group", label = h4(strong('Group')), #width = 12,
                 choices = c('Sample Type' = 'sample_type', 
                             'Pathological T Stage' = 'pathological_t_stage',
                             'Gleason Score' = 'gleason_group',
                             'Preoperative PSA' = 'preop_psa'),
                 inline = TRUE)
    
  ),
  
  
  ### TCGA-PRAD
  conditionalPanel(condition = "input.group == 'sample_type' || input.group == 'gleason_group' || 
                   input.group == 'pathological_t_stage' || input.group == 'preop_psa'",
                   box(#id = 'tcgatest',
                     title = 'TCGA-PRAD', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                     width = 4, plotOutput("tcgaboxplot", height = 250)
                   )
  ),
  
  conditionalPanel(condition = "input.group == 'preop_psa'",
                   box(
                     title = 'GSE107299', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                     width = 4, plotOutput("gse107299boxplot", height = 250)
                   )
  ),
  
  
  ### GSE21034
  conditionalPanel(condition = "input.group == 'sample_type' || input.group == 'gleason_group'",
                   box(
                     title = 'GSE21034', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                     width = 4, plotOutput("gse21034boxplot", height = 250)
                   )
  ),
  
  
  conditionalPanel(condition = "input.group == 'gleason_group' || input.group == 'preop_psa'",
                   box(
                     title = 'DKFZ2018', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                     width = 4, plotOutput("dkfz2018boxplot", height = 250)
                   )
  ),
  
  conditionalPanel(condition = "input.group == 'gleason_group' || input.group == 'preop_psa' || 
                   input.group == 'pathological_t_stage'",
                   box(
                     title = 'GSE54460', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                     width = 4, plotOutput("gse54460boxplot", height = 250)
                   )
  ),
  
  
  ### GSE70768
  conditionalPanel(condition = "input.group == 'sample_type' || input.group == 'gleason_group' || 
                   input.group == 'preop_psa'",
                   box(title = 'GSE70768', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                       width = 4, plotOutput("gse70768boxplot", height = 250)
                   )
  ),
  
  conditionalPanel(condition = "input.group == 'gleason_group' || input.group == 'preop_psa'",
                   box(
                     title = 'GSE70769', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                     width = 4, plotOutput("gse70769boxplot", height = 250)
                   )
  ),
  
  
  conditionalPanel(condition = "input.group == 'sample_type' || input.group == 'gleason_group' || 
                   input.group == 'preop_psa'",
                   box(
                     title = 'GSE94767', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                     width = 4, plotOutput("gse94767boxplot", height = 250)
                   )
  ),
  
  conditionalPanel(condition = "input.group == 'sample_type' || input.group == 'gleason_group' || 
                   input.group == 'preop_psa'",
                   box(
                     title = 'E-MTAB-6128', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                     width = 4, plotOutput("emtab6128boxplot", height = 250)
                   )
  ),
  
  conditionalPanel(condition = "input.group == 'preop_psa'",
                   box(
                     title = 'GSE116918', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                     width = 4, plotOutput("gse116918boxplot", height = 250)
                   )
  ),
  
  conditionalPanel(condition = "input.group == 'pathological_t_stage' || input.group == 'gleason_group'",
                   box(
                     title = 'GSE41408', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                     width = 4, plotOutput("gse41408boxplot", height = 250)
                   )
  )
  
  # GSE25136, GSE46691, GSE51066, GSE37199
  
)


tab_kmplot <- fluidRow(
  
  #style='margin-left:0em',
  
  box(
    title = NULL, status = "primary", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    gene.survival,
    
    radioButtons(inputId = "survival_radio", label = h4(strong('Survival')), #width = 6,
                 c('Overall Survival' = 'time_to_death',
                   'Relapse-free Survival' = 'time_to_bcr',
                   'Metastasis-free Survival' = 'time_to_metastasis'),
                 inline = TRUE),
    
    sliderInput(inputId = "quantile", label = h4(strong('Quantile')), 
                min = 0, max = 100,  value = 50, width = 300)
  ),
  
  
  conditionalPanel(condition = "input.survival_radio == 'time_to_death' || input.survival_radio == 'time_to_bcr'",
                   box(title = 'TCGA-PRAD', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                       width = 4,
                       plotOutput("kmtcga", height = 250)
                   )
  ),
  
  conditionalPanel(condition = "input.survival_radio == 'time_to_bcr'",
                   box(title = 'GSE107299', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                       width = 4,
                       plotOutput("kmgse107299", height = 250)
                   )
  ),
  
  
  conditionalPanel(condition = "input.survival_radio == 'time_to_bcr'",
                   box(title = 'GSE21034', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                       width = 4,
                       plotOutput("kmgse21034", height = 250)
                   )
  ),
  
  
  conditionalPanel(condition = "input.survival_radio == 'time_to_bcr'",
                   box(title = 'DKFZ2018', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                       width = 4,
                       plotOutput("kmdkfz2018", height = 250)
                   )
  ),
  
  conditionalPanel(condition = "input.survival_radio == 'time_to_bcr'",
                   box(title = 'GSE54460', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                       width = 4,
                       plotOutput("kmgse54460", height = 250)
                   )
  ),
  
  conditionalPanel(condition = "input.survival_radio == 'time_to_bcr'",
                   box(title = 'GSE70768', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                       width = 4,
                       plotOutput("kmgse70768", height = 250)
                   )
  ),
  
  conditionalPanel(condition = "input.survival_radio == 'time_to_bcr'",
                   box(title = 'GSE70769', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                       width = 4,
                       plotOutput("kmgse70769", height = 250)
                   )
  ),
  
  
  conditionalPanel(condition = "input.survival_radio == 'time_to_bcr'",
                   box(title = 'GSE94767', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                       width = 4,
                       plotOutput("kmgse94767", height = 250)
                   )
  ),
  
  conditionalPanel(condition = "input.survival_radio == 'time_to_bcr'",
                   box(title = 'E-MTAB-6128', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                       width = 4,
                       plotOutput("kmemtab6128", height = 250)
                   )
  ),
  
  conditionalPanel(condition = "input.survival_radio == 'time_to_bcr' || input.survival_radio == 'time_to_metastasis'",
                   box(title = 'GSE116918', status = "primary", solidHeader = TRUE, collapsible = TRUE,
                       width = 4,
                       plotOutput("kmgse116918", height = 250)
                   )
  )
  
  #GSE25136, GSE41408, GSE46691, GSE51066, GSE37199, GSE44353
  
)



tab_analysis <- fluidRow(
  box(
    title = 'Dataset', status = "primary", solidHeader = TRUE, collapsible = TRUE,
    width = 12,
    DT::dataTableOutput("dataset")
  ),
  
  
  box(
    title = 'Differential Expression', status = "primary", solidHeader = TRUE, collapsible = TRUE,
    width = 12,
    
    tabBox(width = 12, id = 'degbox',
           
           tabPanel("Summary", 
                    #column(12, 
                    #),
                    
                    #box(title = 'Summary',  
                    #     status = "info", solidHeader = TRUE, collapsible = TRUE,
                    #     width = 12,
                    #     #height = 400,
                    #     textOutput("dataset_summary"),
                    #     
                    #     div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
                    #     br(),
                    #     p("span does the same thing as div, but it works with",
                    #       span("groups of words", style = "color:blue"),
                    #       "that appear inside a paragraph.")
                    # ),
                    
                    div(h4(strong(textOutput("dataset_summary"))), style = "color:black", align='center'),
                    
                    #div(h4(strong(dataset_summary)), style = "color:black", align='center'),
                    br(),
                    #p(span("Experimental design:", style = 'color:blue'), overall_design),
                    
                    
                    #conditionalPanel(condition = "input.dataset_rows_selected==1 || input.dataset_rows_selected==3 || input.dataset_rows_selected==6 || 
                    #                 input.dataset_rows_selected==8 || input.dataset_rows_selected==9",
                    #                 box(title = 'Sample Type',  
                    #                     status = "info", solidHeader = TRUE, collapsible = TRUE,
                    #                     width = 4,
                    #                     height = 400,
                    #                     plotOutput('pie_sample_type')
                    #                 )
                    #),
                    
                    box(title = 'Sample Type',
                        status = "info", solidHeader = TRUE, collapsible = TRUE,
                        width = 4,
                        height = 400,
                        plotOutput('pie_sample_type')
                    ),
                    
                    conditionalPanel(condition = "input.dataset_rows_selected==1 || input.dataset_rows_selected==5 || input.dataset_rows_selected==12",
                                     box(title = 'Pathological T Stage', 
                                         status = "info", solidHeader = TRUE, collapsible = TRUE,
                                         width = 4,
                                         height = 400,
                                         plotOutput('pie_pstage')
                                     )
                    ),
                    
                    conditionalPanel(condition = "input.dataset_rows_selected==1 || input.dataset_rows_selected==2 || input.dataset_rows_selected==3",
                                     box(title = 'Clinical T Stage', 
                                         status = "info", solidHeader = TRUE, collapsible = TRUE,
                                         width = 4,
                                         height = 400,
                                         plotOutput('pie_cstage')
                                     )
                    ),
                    
                    conditionalPanel(condition = "input.dataset_rows_selected!=3 && input.dataset_rows_selected<10",
                                     box(title = 'Preoperative PSA', 
                                         status = "info", solidHeader = TRUE, collapsible = TRUE,
                                         width = 4,
                                         height = 400,
                                         plotOutput('bar_psa')
                                     )
                    ),
                    
                    conditionalPanel(condition = "input.dataset_rows_selected<12 && input.dataset_rows_selected!=2 && input.dataset_rows_selected!=10 && 
                                     input.dataset_rows_selected!=11",
                                     box(title = 'Gleason Score', 
                                         status = "info", solidHeader = TRUE, collapsible = TRUE,
                                         width = 4,
                                         height = 400,
                                         plotOutput('bar_gleason')
                                     )
                    ),
                    
                    conditionalPanel(condition = "input.dataset_rows_selected==1",
                                     box(title = 'Overall Survival Status', 
                                         status = "info", solidHeader = TRUE, collapsible = TRUE,
                                         width = 4,
                                         height = 400,
                                         plotOutput('pie_os_status')
                                     )
                    ),
                    
                    conditionalPanel(condition = "input.dataset_rows_selected==1",
                                     box(title = 'Overall Survival', 
                                         status = "info", solidHeader = TRUE, collapsible = TRUE,
                                         width = 4,
                                         height = 400,
                                         plotOutput('km_os_time')
                                     )
                    ),
                    
                    conditionalPanel(condition = "input.dataset_rows_selected<=12",
                                     box(title = 'Relapse-free Survival Status', 
                                         status = "info", solidHeader = TRUE, collapsible = TRUE,
                                         width = 4,
                                         height = 400,
                                         plotOutput('pie_bcr_status')
                                     )
                    ),
                    
                    
                    conditionalPanel(condition = "input.dataset_rows_selected<=10",
                                     box(title = 'Relapse-free Survival', 
                                         status = "info", solidHeader = TRUE, collapsible = TRUE,
                                         width = 4,
                                         height = 400,
                                         plotOutput('km_bcr_time')
                                     )
                    ),
                    
                    
                    conditionalPanel(condition = "input.dataset_rows_selected==10 || input.dataset_rows_selected==12 || input.dataset_rows_selected==13 || 
                                     input.dataset_rows_selected==14",
                                     box(title = 'Metastasis-free Survival Status', 
                                         status = "info", solidHeader = TRUE, collapsible = TRUE,
                                         width = 4,
                                         height = 400,
                                         plotOutput('pie_metastasis_status')
                                     )
                    ),
                    
                    conditionalPanel(condition = "input.dataset_rows_selected==10",
                                     box(title = 'Metastasis-free Survival', 
                                         status = "info", solidHeader = TRUE, collapsible = TRUE,
                                         width = 4,
                                         height = 400,
                                         plotOutput('km_metastasis_time')
                                     )
                    ),
                    
                    
                    #box(title = 'Preop PSA', 
                    #     status = "info", solidHeader = TRUE, collapsible = TRUE,
                    #     width = 4,
                    #     #height = 400,
                    #     plotOutput('pie_psa')
                    # ),
                    
                    
                    htmlOutput("gse")
                    
                    #splitLayout(cellWidths = c("50%", "50%"), 
                    #             plotOutput('pie_sample_type'), 
                    #             plotOutput('pie_gleason')),
  ),
  
  tabPanel(title = "Sample Type", value = 'sample_type',
           checkboxGroupInput(inputId = "control_sample_type", label = "Control group",
                              choices = c('Normal', 
                                          'Primary Tumor' = 'Primary',
                                          'Metastasis'),
                              selected = 'Normal',
                              inline = TRUE),
           checkboxGroupInput(inputId = "case_sample_type", label = "Case group",
                              choices = c('Normal',
                                          'Primary Tumor' = 'Primary',
                                          'Metastasis'),
                              selected = 'Primary',
                              inline = TRUE),
           
           
           sliderInput(inputId = "foldchange", label = h5(strong('Fold Change')), 
                       min = 0, max = 3,  step = 0.1, value = 2, width = 300),
           
           sliderInput(inputId = "fdr", label = h5(strong('BH Adjusted P Value')), 
                       min = 0, max = 0.1,  step = 0.01, value = 0.01, width = 300),
           
           
           column(4, br(), plotOutput('volcano_sample_type', height = 500)),
           column(6, br(), DT::dataTableOutput("table_sample_type"))
           
           
           
  ),
  
  tabPanel("Gleason Score", 
           radioButtons(inputId = "gleason_control", label = "Control group",
                        c('6=3+3', '7=3+4','7=4+3','8=4+4','9=4+5','9=5+4','10=5+5'),
                        inline = TRUE),
           radioButtons(inputId = "gleason_case", label = "Case group",
                        c('6=3+3', '7=3+4','7=4+3','8=4+4','9=4+5','9=5+4','10=5+5'),
                        inline = TRUE)
  ),
  
  tabPanel("Pathological T Stage", 
           radioButtons(inputId = "stage_control", label = "Control group",
                        c('Stage I', 'Stage II','Stage III','Stage IV'),
                        inline = TRUE),
           radioButtons(inputId = "stage_case", label = "Case group",
                        c('Stage I', 'Stage II','Stage III','Stage IV'),
                        inline = TRUE)
  ),
  
  tabPanel("Preoperative PSA", 
           
           sliderInput("psa", 
                       "Preoperative PSA Value", 
                       min = 0,
                       max = 50, 
                       value = 4,
                       width = 500)
  )
  
    )#,
  
  #DT::dataTableOutput("dataset")
  
  )
  
)


tab_home <- fluidRow(
  
  box(
    title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
    width = 12, 
    
    h4(strong("Welcome to PCaDB, a comprehensive database of transcriptomes in prostate cancer population cohorts"), align='center')
    
  ),
  
  box(
    title = NULL, solidHeader = TRUE, collapsible = FALSE,
    width = 12, # solidHeader=TRUE can remove the top boarder
    
    column(12,
           
           valueBox(value = tags$p(strong("50"), style = "font-size: 80%;"), color = 'teal', width = 3, #color = 'aqua'
                    subtitle = tags$p(strong("Datasets"), style = "font-size: 150%;"),  icon = icon("database fa-0.5x")),
           valueBox(value = tags$p(strong("7,082"), style = "font-size: 80%;"), color = 'teal', width = 3,
                    subtitle = tags$p(strong("Samples"), style = "font-size: 150%;"),  icon = icon("user-circle fa-0.5x")),
           valueBox(value = tags$p(strong("30"), style = "font-size: 80%;"), color = 'teal', width = 3,
                    subtitle = tags$p(strong("Signatures"), style = "font-size: 150%;"),  icon = icon("dna fa-0.5x"))
    )#,
    
    # br(),
    # 
    # column(12,
    #        
    #        valueBox(value = tags$p(strong("xx"), style = "font-size: 80%;"), color = 'teal', width = 3, 
    #                 subtitle = tags$p(strong("Normal"), style = "font-size: 120%;"),  icon = icon("dna  fa-0.5x")),
    #        valueBox(value = tags$p(strong("yy"), style = "font-size: 80%;"), color = 'teal', width = 3,
    #                 subtitle = tags$p(strong("Primary"), style = "font-size: 120%;"), icon = icon("database fa-0.5x")),
    #        valueBox(value = tags$p(strong("zz"), style = "font-size: 80%;"), color = 'teal', width = 3,
    #                 subtitle = tags$p(strong("Metastatic"), style = "font-size: 120%;"),  icon = icon("user-circle fa-0.5x"))
    #        
    # )
    # 
    ),
  
  box(
    title = NULL, solidHeader = TRUE, collapsible = FALSE,
    width = 12, # solidHeader=TRUE can remove the top boarder
    
    h3(strong("Introduction")),
    
    h4(strong("Prostate cancer")),
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
    h4(strong("About PCaDB")),
    tags$p('We collected 50 public transcriptomic datasets in prostate cancer', style = "font-size: 120%;"),
    
    
    br(),
    tags$hr(style="border-top: 1px dashed #A9A9A9"),
    
    h4(strong("Citation")),
    tags$p('Please cite the following publication:
           Li, R. and Jia, Z., PCaDB - a database and web server for integrating transcriptomics data in prostate cancer population cohorts. bioRxiv (2021)', style = "font-size: 120%;"),
    tags$p(HTML("<a href='https://www.biorxiv.org/content/10.1101/2020.10.04.325670v1' target='_blank'><h5>https://www.biorxiv.org/content/10.1101/2020.10.04.325670v1</h5></a>"))
    
    
  )
)




body=dashboardBody(
  
  useShinyjs(),
  #selectizeInput(inputId = "gene", label='Gene', choices = gene.annotation, selected = gene.default, multiple=FALSE, 
  #               options = list(
  #                 placeholder = 'Select a gene', maxOptions = 10,
  #                 selectOnTab=TRUE)),
  
  #tags$head(tags$style(HTML('.box {margin: 1px;}'))), # distance between box
  
  theme_blue_gradient,
  
  
  #https://stackoverflow.com/questions/45706670/shiny-dashboadpage-lock-dashboardheader-on-top
  tags$script(HTML("$('body').addClass('fixed');")), # fix header & sidebar
  
  tabItems(
    tabItem(tabName="tab_home", tab_home)#,
    # tabItem(tabName="tab_query", tab_query),
    # tabItem(tabName="tab_analysis",tab_analysis),
    # tabItem(tabName="tab_pipeline", tab_pipeline),
    # tabItem(tabName="tab_download", tab_download),
    # tabItem(tabName="tab_tutorial", tab_tutorial),
    # tabItem(tabName="tab_contact", tab_contact)
  )
  
)


ui <- dashboardPage(title='PCaDB', header, sidebar, body) # skin = 'blue',

  
  




######## Server

server <- function(input, output, session) { 
  
  shinyjs::hide(selector = ".navbar > .sidebar-toggle")
  
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
  
  
  output$dataset <- DT::renderDataTable({pcadb.dataset},
                                        options = list(pageLength = 5),
                                        selection = list(mode='single', selected=1)
  )
  
  output$dataset_summary <- renderText({ 
    idx <- input$dataset_rows_selected
    dataset_summary <- as.character(paste0(pcadb.dataset[idx,'Dataset'], ': ', pcadb.dataset[idx,'Title']))
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





shinyApp(
  ui = ui,
  server = server
)

