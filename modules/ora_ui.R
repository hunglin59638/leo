#!/usr/bin/env Rscript
file_fmt <- c("text/csv",
              "text/comma-separated-values,text/plain",
              ".csv",".tsv", ".xslx")

ora_demo <- function() {}

ora_tabpanel <- 
  tabPanel("Over-Representation Analysis", 
           fluidRow(
             column(3, 
                    sidebarPanel(
                      width=12, 
                      h3("Provide gene ids (Entrez Gene)"),
                      textAreaInput("ora_geneid", NULL, value="",
                                    width="100%"),
                      h5("or upload your own file"),
                      fileInput(inputId="ora_file", label=NULL,
                                multiple=F, accept=file_fmt),
                      checkboxInput(inputId="ora_use_demo", 
                                    label="Use demo data",
                                    value=FALSE),
                      selectInput(inputId="species_choice",
                                  label="Choose species",
                                  choices=c("Chicken", "Pig", "Human"),
                                  selected="Chicken"),
                      selectInput(inputId="db_choice",
                                  label="Chosse database",
                                  choices=c("KEGG", "GO"),
                                  selected="GO"),
                      conditionalPanel(condition="input.db_choice == 'GO'",
                                       selectInput("go_ont", "GO ontology",
                                                   selected="ALL",
                                                   choices=c("ALL","BP","CC", "MF"))
                                       
                      ),
                      selectInput("padjust",
                                  label=h4("Adjust p-values for multiple comparisons"),
                                  choices=c("Bonferroni","Holm","Hochberg", "Hommel",
                                              "Benjamini & Hochberg","Benjamini & Yekutieli",
                                              "None"),
                                  selected="Benjamini & Hochberg"),
                      numericInput("pvalue_cutoff", 
                                   label=h4("Cutoff value of pvalue."),
                                   value=0.05),
                      numericInput("qvalue_cutoff",
                                   label=h4("Cutoff value of qvalue."),
                                   value=0.2),
                      actionButton("ora_action","Submit")
                    )
             ),
             column(9,
                    mainPanel(
                      h2("Over-Representation Analysis"),
                      DT::dataTableOutput("ora_df"),
                      uiOutput("ora_f_render"),
                      hr(),
                      plotOutput("ora_plot"),
                      uiOutput("ora_p_render"),
                    )
             )
           )
  )