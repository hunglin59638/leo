#!/usr/bin/env Rscript
options(encoding="UTF-8")
suppressPackageStartupMessages({
  require(shiny)
  require(AnnotationHub)
  require(clusterProfiler)
  require(DOSE)
  require(AnnotationDbi)
  require(ggplot2)
  require(stringr)
  require(magrittr)
  require(dplyr)
  require(plyr)
  require(purrr)
  library(rstatix)
  library(FSA)
  library(lattice)
  library(car)
  library(emmeans)
  library(corrplot)
})

get_file_path <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  if (length(grep("^-f$", cmdArgs)) > 0) {
    # R console option
    normalizePath(dirname(cmdArgs[grep("^-f", cmdArgs) + 1]))[1]
  } else if (length(grep("^--file=", cmdArgs)) > 0) {
    # Rscript/R console option
    scriptPath <- normalizePath(dirname(sub("^--file=", "", cmdArgs[grep("^--file=", cmdArgs)])))[1]
  } else if (Sys.getenv("RSTUDIO") == "1") {
    if (rstudioapi::isAvailable(version_needed=NULL,child_ok=FALSE)) {
      # RStudio interactive
      dirname(rstudioapi::getSourceEditorContext()$path)
    } else if (is.null(knitr::current_input(dir = TRUE)) == FALSE) {
      # Knit
      knitr::current_input(dir = TRUE)
    } else {
      # R markdown on RStudio
      getwd()
    }
  } else if (is.null(attr(stub, "srcref")) == FALSE) {
    # 'source'd via R console
    dirname(normalizePath(attr(attr(stub, "srcref"), "srcfile")$filename))
  } else {
    stop("Cannot find file path")
  }
}
root_dir = get_file_path()
source(paste0(root_dir, "/modules/utils.R"))
source(paste0(root_dir,"/modules/statisitcs.R"))
source(paste0(root_dir,"/modules/visualize.R"))
source(paste0(root_dir,"/modules/conversion.R"))
source(paste0(root_dir,"/modules/orgdb.R"))
source(paste0(root_dir,"/modules/ora"))

#donwload gallus gallus database
#ah <- AnnotationHub()
#databaseid<- query(ah, c("Gallus gallus","OrgDb")) 
#gallus<- ah[[databaseid$ah_id]]



shinyServer(function(input, output, session) {
  # Statisitic

  s_data <- eventReactive(input$s_action, {
    infile <- input$s_file
    header <- input$s_header
    if (input$s_type == "csv") {
      exec_func(args=list(infile$datapath, header), func=read_file)
    }
  })
  
  callback <- c(
    "var colnames = table.columns().header().to$().map(function(){return this.innerHTML;}).get();",
    "Shiny.onInputChange('colnames', colnames);",
    "table.on('dblclick.dt', 'thead th', function(e) {",
    "  var $th = $(this);",
    "  var index = $th.index();",
    "  var colname = $th.text(), newcolname = colname;",
    "  var $input = $('<input type=\"text\">')",
    "  $input.val(colname);",
    "  $th.empty().append($input);",
    "  $input.on('change', function(){",
    "    newcolname = $input.val();",
    "    if(newcolname != colname){",
    "      $(table.column(index).header()).text(newcolname);",
    "      colnames[index] = newcolname;",
    "      Shiny.onInputChange('colnames', colnames);",
    "    }",
    "    $input.remove();",
    "  }).on('blur', function(){",
    "    $(table.column(index).header()).text(newcolname);",
    "    $input.remove();",
    "  });",
    "});"
  )

  output$s_data <- DT::renderDT(s_data(), server=T, editable="cell", 
                                selection = 'none', callback=DT::JS(callback))
  observeEvent(input$s_action, {run_desc(input,output,s_data())})
  observeEvent(input$s_action, {run_normality_test(input,output,s_data())})
  observeEvent(input$s_action, {run_hetero_test(input,output,s_data())})
  observe({
    m_type <- input$m_type
    m_methods <- if (m_type == "parametric") c('Least Squares Means') else 
      c('Kruskal-Wallis test for equal variances', "Welch's anova for unequal variances") 
    updateSelectInput(session, "m_method",
                      label = paste("Select a stastistical method"),
                      choices = m_methods,
                      selected = tail(m_methods, 1))
    
  })
  observeEvent(input$s_action, {run_sign_test(input, output, s_data())})
  # id conversion
  id_types <- list(`GI number`=c("Entrez Gene (GeneID)", "Protein Accession"),
                   `Entrez Gene (GeneID)`=c("GI number", "Protein Accession"),
                   `Protein Accession`=c("GI number", "Entrez Gene (GeneID)"))
  observeEvent(input$id_select_fr, {
    fr <- input$id_select_fr
    to <- input$id_select_to
    updateSelectInput(session, "id_select_to", label = paste("To"),
                      choices = id_types[[fr]],
                      selected = head(id_types[[fr]], 1))
    if (fr == "GI number") {
      value <- echo_gi_demo()
    } else if (fr == "Entrez Gene (GeneID)") {
      value <- echo_entrezid_demo()
    } else if (fr == "Protein Accession") {
      value <- echo_prot_acc_demo()
    }
    
    updateTextAreaInput(session, "id_input", value=value)

  })
  observeEvent(input$id_clear, {
    updateTextAreaInput(session, "id_input", value="")
  })

  observeEvent(input$id_action, {
    showModal(modalDialog("In progress for converting gene ids", footer=NULL))
    run_convert_id(input, output)
    removeModal()
  })
  
  observeEvent(input$go_action, {
    showModal(modalDialog("In progress for grouping GO classification", footer=NULL))
    run_go_class(input, output)
    removeModal()
  })
})