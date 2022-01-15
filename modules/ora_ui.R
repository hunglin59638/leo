#!/usr/bin/env Rscript
file_fmt <- c("text/csv",
              "text/comma-separated-values,text/plain",
              ".csv",".tsv", ".xslx")

ora_tabpanel <- 
  tabPanel("Over-Representation Analysis", 
           sidebarLayout(
             sidebarPanel(
               width=3,
               h3("Provide gene ids (Entrez Gene)"),
               textAreaInput("ora_geneid", NULL, value="", width="100%"),
               # h5("or upload your own file"),
               # fileInput(
               #   inputId="ora_file", label=NULL, multiple=F, accept=file_fmt
               #   ),
               selectInput(inputId="ora_species_choice",
                           label="Choose species",
                           choices=c("Chicken", "Pig", "Human", "Acipenser ruthenus"),
                           selected="Chicken"),
               selectInput(inputId="db_choice",
                           label="Chosse database",
                           choices=c("KEGG", "GO"),
                           selected="GO"),
               conditionalPanel(condition="input.db_choice == 'GO'",
                                selectInput("ora_go_ont", "GO ontology",
                                            selected="ALL",
                                            choices=c("ALL","BP","CC", "MF"))
                                ),
               selectInput("padjust_method",
                           label=h4("p-values adjust method for multiple comparisons"),
                           choices=c("Bonferroni", "Holm","Hochberg", "Hommel",
                                     "Benjamini & Hochberg","Benjamini & Yekutieli",
                                     "None"),
                           selected="Benjamini & Hochberg"),
               numericInput("pvalue_cutoff", 
                            label=h4("Cutoff value of pvalue."),
                            value=0.05, max=1, min=0),
               numericInput("qvalue_cutoff",
                            label=h4("Cutoff value of qvalue."),
                            value=0.2, max=1, min=0),
               fluidRow(
                 column(2, actionButton("ora_action", "Submit")),
                 column(2, actionButton("ora_demo", "Example"))
                 )
               ),
             mainPanel(
               width=9,
               h2("The Result of Over-Representation Analysis"),
               DT::dataTableOutput("ora_df"),
               uiOutput("ora_df_render"),
               # hr(),
               # plotOutput("ora_plot"),
               # uiOutput("ora_p_render"),
               )
             )
           )