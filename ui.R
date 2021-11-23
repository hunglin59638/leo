#!/usr/bin/env Rscript
options(encoding = "UTF-8")
suppressPackageStartupMessages({
  library(shiny, quietly=T)
  library(shinydashboard, quietly=T)
  library(shinythemes, quietly=T)
})
file_fmt <- c("text/csv",
              "text/comma-separated-values,text/plain",
              ".csv",".tsv", ".xslx")
statistic_tabpanel <- tabPanel("Statistics",
                               fluidRow(
                                 column(2, sidebarPanel(width=12,
                                                selectInput("s_type", "Load data of type:",
                                                                selected="csv", 
                                                                choices=c("csv", "xslx")),
                                                   checkboxInput(inputId="s_header", label="Header", 
                                                                 value=T),
                                                   conditionalPanel(condition="input.s_type == 'csv'",
                                                                    selectInput("s_sep", "Separator:",
                                                                                selected="comma",
                                                                                choices=c("comma","tab"))
                                                                    
                                                   ),
                                                   fileInput(inputId="s_file",
                                                             label=NULL,
                                                             multiple = F,
                                                             accept=file_fmt
                                                   ),
                                                   conditionalPanel("input.s_setpanel == 's_normality'",
                                                                    selectInput("s_nor_method", "Choose stastics methods",
                                                                                choices=c('Shapiro-Wilk test (3<n<50)', 
                                                                                          'Kolmogorov-Smirnov test'),
                                                                                selected='Shapiro-Wilk test (3<n<50)')
                                                   ),
                                                   conditionalPanel("input.s_setpanel == 's_sign'",
                                                                    selectInput(inputId='m_type',
                                                                                label='parametric or nonparametric method',
                                                                                choices=c('parametric', 'nonparametric'),
                                                                                selected='parametric'),
                                                                    selectInput(inputId='m_method',
                                                                                label='Select a stastistical method',
                                                                                choices=c('Least Squares Means'),
                                                                                selected='Least Squares Means')
                                                   ),
                                                   actionButton(inputId="s_action", label="Run")
                            )),
                            column(10, tabsetPanel(id="s_setpanel",
                                                   tabPanel("Preview", value="tab_preview",
                                                            hr(),
                                                            DT::dataTableOutput("s_data")),
                                                   tabPanel("Summary", value="s_summary",
                                                            mainPanel(hr(),
                                                                      verbatimTextOutput("descriptive_summary"),
                                                                      h2("Histogram"),
                                                                      plotOutput("descriptive_barplot"),
                                                                      hr(),
                                                                      h2("Boxplot"),
                                                                      plotOutput("descriptive_boxplot")
                                                            )
                                                   ),
                                                   tabPanel("Normality Tests", value="s_normality",
                                                            mainPanel(
                                                              # tableOutput("normalitytest_table"),
                                                              verbatimTextOutput("normality_summary"),
                                                              plotOutput("normality_plot")
                                                            )
                                                   ),
                                                   tabPanel("Heteroscedasticity test", value="s_heter",
                                                            mainPanel(
                                                              verbatimTextOutput("hetero_result"),
                                                              verbatimTextOutput("h_test")
                                                            )
                                                   ),
                                                   tabPanel("Significance test", value="s_sign",
                                                            mainPanel(
                                                              hr(),
                                                              verbatimTextOutput("sign_summary"),
                                                              plotOutput("mcompare_plot"),
                                                              hr(),
                                                              hr(),
                                                              verbatimTextOutput("hoctest")
                                                            )
                                                   )
                            ))
                          ))
id_conv_tabpanel <- tabPanel("ID conversion",
                             sidebarLayout(
                               sidebarPanel(
                                 h2("1. Provide your identifiers"),
                                 textAreaInput("id_input",NULL, value="93100\n223646"),
                                 h2("2. Select options"),
                                 selectInput("id_select_fr","From", 
                                             selected="GI number",
                                             choices=c("GI number",
                                                       "Entrez Gene (GeneID)", "Protein Accession")),
                                 selectInput("id_select_to", "To",
                                             selected="Entrez Gene (GeneID)",
                                             choices=c("Entrez Gene (GeneID)", "Protein Accession")),
                                 fluidRow(
                                   column(2, actionButton("id_action", "Submit")),
                                   column(2, actionButton("id_clear", "Clear"))
                                 )
                                 
                               ),
                               mainPanel(
                                 h2("Conversion result"),
                                 DT::dataTableOutput("con_result"),
                                 uiOutput("con_file_render"),
                                 # conditionalPanel(condition="input.id_action",
                                 #                  downloadButton(outputId="con_file", label="Download")
                                 # )
                                 # 
                               )))

go_class_tabpanel <- tabPanel("GO classification", 
                              fluidRow(column(2, 
                                        sidebarPanel(width=12,
                                           h3("Provide gene ids (Entrez Gene)"),
                                           textAreaInput("go_geneid",NULL, value="93100\n223646",
                                                         width="100%"),
                                           h5("or upload your own file"),
                                           fileInput(inputId="g_file",
                                                     label=NULL,
                                                     multiple = F,
                                                     accept=file_fmt
                                                     ),
                                           selectInput(inputId = "species_choice",
                                                       label = "Choose species",
                                                       choices = c("Chicken", "Pig"),
                                                       selected = "Chicken"),
                                           checkboxGroupInput("go_term",
                                                              label = h4("GO Term"),
                                                              choices = list("Biological Process"="bp",
                                                                             "Cellular Component"="cc",
                                                                             "Molecular Function"="mf"),
                                                              selected = list("Biological Process"="bp",
                                                                              "Cellular Component"="cc",
                                                                              "Molecular Function"="mf")
                                           ),
                                           numericInput("level_ggo", 
                                                        label = h4("Specific GO level"),
                                                        value=2,min=2),
                                           actionButton("go_action","Submit")
                                           
                                                           )),
                                       column(10,
                                            mainPanel(
                                              h2("GO classification"),
                                              DT::dataTableOutput("ggo_df"),
                                              uiOutput("ggo_f_render"),
                                              hr(),
                                              plotOutput("ggo_plot"),
                                              uiOutput("ggo_p_render"),
                                            ))
                                       ))

shinyUI(
  navbarPage("Bioinformatic toolkit",
             statistic_tabpanel,
             id_conv_tabpanel,
             go_class_tabpanel,
                      # fluidRow(
                      #   column(2, sidebarPanel(width=12,)
                      # )),
             tabPanel("Over-representation analysis")
  )
)
