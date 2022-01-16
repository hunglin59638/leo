#/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(shiny, quietly=T)
})
file_fmt <- c("text/csv",
              "text/comma-separated-values,text/plain",
              ".csv",".tsv", ".xslx")

statistic_tabpanel <- 
  tabPanel("Statistics",
           fluidRow(
             column(2, 
                    sidebarPanel(width=12, 
                                 selectInput("s_type", 
                                             "Load data of type:",
                                             selected="csv", 
                                             choices=c("csv", "xslx")),
                                 checkboxInput(inputId="s_header", label="Header", 
                                               value=T),
                                 # conditionalPanel(condition="input.s_type == 'csv'",
                                 #                  selectInput("s_sep", "Separator:",
                                 #                              selected="comma",
                                 #                              choices=c("comma","tab"))
                                 #                  
                                 # ),
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
                                 actionButton("s_example", "Example"),
                                 actionButton("s_action", "Run"),

                                 
                    )),
             column(10, tabsetPanel(id="s_setpanel",
                                    tabPanel("Preview", value="tab_preview",
                                             hr(),
                                             DT::DTOutput("s_data")),
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