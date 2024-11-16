# /usr/bin/env Rscript
library(clusterProfiler)
source("modules/orgdb.R")

get_goclass_demo <- function() {
  file <- sub("/modules", "", paste0(get_file_path(), "/test/goclass_demo.txt"))
  df <- read.csv(file, header = F)
  return(df[, 1])
}

go_class <- function(genes, org_name, onts = c("BP", "MF", "CC"), level = 2) {
  genes <- as.character(genes[!is.na(genes) && genes != ""])
  org_db <- get_orgdb(org_name)
  result <- list()
  for (ont in onts) {
    r <- groupGO(
      gene = genes, OrgDb = org_db, keyType = "ENTREZID", ont = ont,
      level = level, readable = F
    ) %>% as.data.frame()
    result[[ont]] <- r
  }
  return(result)
}

run_go_class <- function(input, output) {
  level <- input$level_ggo
  onts <- input$go_term
  org_name <- input$species_choice
  genes <- str_split(input$go_geneid, "(\n|\\s)")[[1]]
  genes <- genes[genes != "" & !is.na(genes)]
  result <- go_class(genes = genes, org_name = org_name, onts = onts, level = level)
  df <- data.frame()
  for (ont in names(result)) {
    r_df <- as.data.frame(result[[ont]]) %>% filter(Count > 0)
    r_df$ontology <- rep(ont, nrow(r_df))
    df <- rbind(df, r_df)
  }
  rownames(df) <- NULL
  df$ontology <- toupper(df$ontology)

  output$ggo_df <- DT::renderDataTable(
    DT::datatable(df, options = list(searching = FALSE))
  )

  if (nrow(df) > 0) {
    df$Count <- as.integer(df$Count)
    output$ggo_plot <- renderPlot({
      if (is.null(df)) {
        return(NULL)
      }
      plot_go(ggo_df = df)
    })
    output$ggo_p_render <- renderUI({
      downloadButton(outputId = "ggo_plot_f", label = "Download plot")
    })
  }

  output$ggo_f_render <- renderUI({
    downloadButton(outputId = "ggo_df_f", label = "Download table")
  })



  output$ggo_df_f <- downloadHandler(
    filename = function() {
      paste("go_classification_", Sys.Date(), ".csv", sep = "")
    },
    content = function(fname) {
      write.csv(df,
        file = fname,
        row.names = F
      )
    },
    contentType = NA
  )

  output$ggo_plot_f <- downloadHandler(
    filename = function() {
      paste("go_classification_", Sys.Date(), ".png", sep = "")
    },
    content = function(fname) {
      plot_go(ggo_df = df)
      ggsave(fname, width = 20, height = 12, units = "cm")
    },
    contentType = NA
  )
}
