#!/usr/bin/env Rscript
file_fmt <- c("text/csv",
              "text/comma-separated-values,text/plain",
              ".csv",".tsv", ".xslx")

go_class_tabpanel <- 
  tabPanel("GO classification", 
           fluidRow(
             column(4, 
                    sidebarPanel(
                      width=12, 
                      h3("Provide gene ids (Entrez Gene)"),
                      textAreaInput("go_geneid", NULL, value="",
                                    width="100%"),
                      # h5("or upload your own file"),
                      # fileInput(inputId="g_file", label=NULL,
                      #           multiple = F, accept=file_fmt),
                      selectInput(inputId = "species_choice",
                                  label = "Choose species",
                                  choices = c("Chicken", "Pig", "Swan goose",
                                              "Human", "Acipenser ruthenus"),
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
                                   label = h4("GO level"),
                                   value=2, min=2),
                      fluidRow(
                        column(2, actionButton("go_action", "Submit")),
                        column(2, actionButton("go_id_clear", "Clear")),
                        column(2, actionButton("go_demo", "Example")),
                      )
                      )

                    ),
             column(8,
                    mainPanel(
                      h2("GO classification"),
                      DT::dataTableOutput("ggo_df"),
                      uiOutput("ggo_f_render"),
                      hr(),
                      plotOutput("ggo_plot"),
                      uiOutput("ggo_p_render"),
                      )
                    )
             )
           )
