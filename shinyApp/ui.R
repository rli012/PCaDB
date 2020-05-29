header=dashboardHeader(title = 'PCa Transcriptomes')

sidebar=dashboardSidebar(
  sidebarMenu(
    style = 'position:fixed; overflow: visible',
    
    tags$div(tags$h4("Introduction"),
             style = 'color: white'),
    menuItem("Introduction", tabName = "tab_introduction", icon = icon("pencil"), selected = T),
    
    
    tags$div(tags$h4("Gene-Level"),
             style = 'color: white'),
    menuItem("Gene Expression", tabName = 'tab_boxplot', icon = icon("dna")#,
             #menuSubItem("Boxplot" , tabName = "tab_boxplot", icon = icon("bar-chart")),
             #menuSubItem("Heatmap" , tabName = "heatmap", icon = icon("scatter-chart"))
    ),
    menuItem("Survival Analysis" , tabName = "tab_kmplot", icon = icon("pencil")),
    
    tags$div(tags$h4("Dataset-Level")),
    menuItem("Differential Expression", tabName = 'tab_dataset', icon = icon("database")),
    menuItem("Pathway Analysis", tabName = 'pathway', icon = icon("database")),
    menuItem("Co-expression Analysis", tabName = 'coexpression', icon = icon("burn")),
    menuItem("Signature Analysis", tabName = 'signature', icon = icon("dna"))
    
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



tab_dataset <- fluidRow(
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


tab_introduction <- fluidRow(
  box(
    title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
    width = 12,
    valueBox(value = '58', color = 'orange', width = 3,
             subtitle = tags$p(strong("Datasets"), style = "font-size: 200%;"), icon = icon("database")),
    valueBox(value = '10,072', color = 'orange', width = 3,
             subtitle = tags$p(strong("Samples"), style = "font-size: 200%;"),  icon = icon("user-circle")),
    valueBox(value = '30', color = 'orange', width = 3,
             subtitle = tags$p(strong("Signatures"), style = "font-size: 200%;"),  icon = icon("dna"))
    
  ),
  
  box(
    title = NULL, solidHeader = TRUE, collapsible = FALSE,
    width = 12, # solidHeader=TRUE can remove the top boarder
    
    h2(strong("Introduction")),
    h3(strong("Prostate cancer")),
    tags$p("Prostate cancer is the second most common cancer in men worldwide. 
           An estimated 1,276,106 new cases and 358,989 deaths were reported in 2018 [",
           a("GLOBOCAN", 
             href = "https://acsjournals.onlinelibrary.wiley.com/doi/full/10.3322/caac.21492"), '].', 
           style = "font-size: 150%;"),
    
    #column(width = 600) {
      
    #}
    #tags$img(src='prostate_cancer.png', width=1000), # in www
    tags$img(src='The-scheme-of-different-stages-and-progression-of-prostate-cancer.png', width=600),
    
    
    
    br(),
    h3(strong("Transcriptomic data")),
    tags$p('We collected 40 public transcriptomic datasets in prostate cancer', style = "font-size: 150%;")
    
  )
  
  
  
)




body=dashboardBody(
  
  useShinyjs(),
  #selectizeInput(inputId = "gene", label='Gene', choices = gene.annotation, selected = gene.default, multiple=FALSE, 
  #               options = list(
  #                 placeholder = 'Select a gene', maxOptions = 10,
  #                 selectOnTab=TRUE)),
  
  #tags$head(tags$style(HTML('.box {margin: 1px;}'))), # distance between box
  
  tabItems(
    tabItem(tabName="tab_introduction", tab_introduction),
    
    tabItem(tabName="tab_boxplot", tab_boxplot),
    tabItem(tabName="tab_kmplot",tab_kmplot),
    tabItem(tabName="tab_dataset",tab_dataset)
  )
  
)


ui <- dashboardPage(title='PCa Transcriptomes', skin = 'green', header, sidebar, body)
