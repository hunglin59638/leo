# /usr/bin/env Rscript

source("modules/conversion.R")

id_conv_tabpanel <-
  tabPanel(
    "ID conversion",
    sidebarLayout(
      sidebarPanel(
        h2("1. Provide your identifiers"),
        textAreaInput("id_input", NULL, value = echo_gi_demo()),
        h2("2. Select options"),
        selectInput("id_select_fr", "From",
          selected = "GI number",
          choices = c(
            "GI number",
            "Protein Accession"
          )
        ),
        selectInput("id_select_to", "To",
          selected = "Entrez Gene (GeneID)",
          choices = c("Entrez Gene (GeneID)", "Protein Accession")
        ),
        fluidRow(
          column(2, actionButton("id_action", "Submit")),
          column(2, actionButton("id_clear", "Clear"))
        )
      ),
      mainPanel(
        h2("Conversion result"),
        DT::dataTableOutput("con_result"), uiOutput("con_file_render"),
      )
    )
  )
