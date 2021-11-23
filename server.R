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
source(paste0(root_dir,"/modules/gene_analysis.R"))

#donwload gallus gallus database
#ah <- AnnotationHub()
#databaseid<- query(ah, c("Gallus gallus","OrgDb")) 
#gallus<- ah[[databaseid$ah_id]]

run_desc <- function(input,output, data) {
  #Descriptive Statistics
  output$descriptive_summary <- renderPrint(
    exec_func(args=data, func=desc_summarize, except=cat("", sep = "")))
  
  output$descriptive_barplot <- renderPlot(
    width = 450, height = 450,
    exec_func(args=data, func=plot_histogram)
  )
  
  output$descriptive_boxplot <- renderPlot(
    width = 450, height = 450,
    exec_func(args=data, func=plot_boxplot)
  )
  
}

run_normality_test <- function(input, output, data) {
  #Normality Test
  normality_method<- reactive(input$s_nor_method)
  
  output$normality_summary<- renderPrint({
    if (is.null(data)) { cat("",sep = "")
    } else {
      data_group <- base::split(data, data[, 1])
      result<- paste0("Normality test", "\n")
      for(i in seq(length(data_group))) {
        group_name <- as.character(data_group[[i]][, 1][1])
        result <- paste0(result,"Group: ", group_name)
        if (normality_method()== 'Shapiro-Wilk test (3<n<50)') {
          shapiro_result <- shapiro.test(data_group[[i]][,2])
          words <- paste0(", Shapiro-Wilk normality test W = ",
                          shapiro_result$statistic,
                          " p-value = ", shapiro_result$p.value, "\n")
          result <- paste0(result, words)
          
        } else if (normality_method()=='Kolmogorov-Smirnov test') {
          ks.result <- ks.test(data_group[[i]][, 2], pnorm,
                               mean = mean(data_group[[i]][, 2]),
                               sd = sd(data_group[[i]][, 2]))
          words <- paste0(", Kolmogorov-Smirnov normality test D = ",
                          ks.result$statistic,
                          " p-value = ", ks.result$p.value, "\n")
          result <- paste0(result, words)
        }
      }
      cat(result, sep="\n", append=T)
    }
    
  })
  qqplot_line <- function(x) {
    par(pin=c(4,4))
    qqnorm(x)
    qqline(x)
  }
  output$normality_plot<- renderPlot({
    if (is.null(data)) return(NULL)
    qqplot_line(data[ ,2])
  })
}

run_hetero_test <- function(input, output, data) {
  #Heteroscedasticity test
  output$hetero_result <- renderPrint({
    if (is.null(data)) { cat("",sep = ""); warning("None file")
    } else {
      data[, 1] <- as.factor(data[, 1])
      cols <- colnames(data)
      exp <- eval(parse(text=paste(cols[2], "~", cols[1])))
      leveneTest(exp, data=data)
    }
  })

}

run_sign_test <- function(input, output, data) {
  data[, 1] <- as.factor(data[, 1])
  group <- colnames(data[1])
  value <- colnames(data[2])
  type <- input$m_type
  #type <- "parametric"
  if (type == "parametric") {
    formula <- as.formula(paste(value, "~", group, "-1", sep=" "))
  } else if (type == "nonparametric") {
    formula <- as.formula(paste(value, "~", group, sep=" "))
  }
  
  method <- input$m_method
  #method <- "Least Squares Means"
  # mcompareresult <- lm(formula, data=data)
  if (method == "Least Squares Means") {
    model <- lm(formula, data=data)
  } else if (method == 'Kruskal-Wallis test for equal variances') {
    model <- kruskal.test(formula, data=data)
  } else if (method == "Welch's anova for unequal variances") {
    model <- oneway.test(formula, data=data, var.equal=FALSE)
  }
  
  output$sign_summary <- renderPrint(
    if (is.null(data)) {cat("",sep = "")} else if (type == "parametric") {
      summary(model) } else if (type == "nonparametric") {
        model
      }
  )
  if (type == "parametric") {
    output$mcompare_plot <- renderPlot(
      width = 450, height = 450,
      if (is.null(data)) {return(NULL)
      } else {
        
        Info <- summary(model)
        Coef <- as.data.frame(Info$coefficients[, 1:2])
        Coef <- within(Coef, {
          lower <- Estimate - qt(p=0.90, df=Info$df[2]) * `Std. Error`
          upper <- Estimate + qt(p=0.90, df=Info$df[2]) * `Std. Error`
          group <- rownames(Coef)
        })
        ggplot(Coef, aes(x=Estimate, y=group)) + geom_point() +
          geom_errorbarh(aes(xmin=lower, xmax=upper), height=.3) +
          ggtitle(paste(value, "by", group,"calculated from regression model"))
        
      }
    )
  } else if (type == "nonparametric") {
    
    output$mcompare_plot <- renderPlot(
      ggplot(data, aes(x=Month, y=Temp, fill=Month)) + geom_boxplot()
    )
  }
  

  output$hoctest <- renderPrint(
    if (is.null(data)) { cat("",sep = "")
    } else if (method == "Least Squares Means") {
      lsm <- lsmeans(model, group)
      contrast(lsm, method = "pairwise", interaction = T)
    } else if (method == "Kruskal-Wallis test for equal variances") {
      dunntest <- dunnTest(formula, data=data, method="bh")
      out <- capture.output(dunntest)
      cat("### Dunn test\n", out, sep = "\n")
      #Dunn (1964) Kruskal-Wallis multiple comparison
      #p-values adjusted with the Benjamini-Hochberg method.
    } else if (method == "Welch's anova for unequal variances") {
      #oneway(data[,1], y = data[,2], posthoc = 'games-howell')
      out <- rstatix::games_howell_test(data, formula, conf.level = 0.95, 
                                        detailed = FALSE)
      cat("Games-Howell Post-Hoc Test\n")
      out
    }
  )
}

run_convert_id <- function(input, output) {
  ids <- str_split(input$id_input,"(\n|\\s)")[[1]]
  ids <- ids[ids != ""]
  fr <- input$id_select_fr
  to <- input$id_select_to
  result <- convert_id(ids=ids, fr=fr, to=to)
  df <- data.frame()
  for (name in names(result)) {
    row <- c(name, result[[name]])
    df <- rbind(df, row)
  }
  colnames(df) <- c(fr, to)
  if (nrow(df)>0) {
    df %<>% filter(df[,1]!="")
    # updateNumericInput(session, inputId="df_exist",value=1)
    output$con_result <- DT::renderDataTable(
      DT::datatable(df, options=list(searching=FALSE))
    )
    output$con_file_render <- renderUI({
      downloadButton(outputId="con_file", label="Download")
    })
    output$con_file <- downloadHandler(
      filename = function() {
        paste("geneid_conversion_",Sys.Date(),".csv",sep = "")
      },
      content = function(fname) {
        write.csv(df, file = fname,
                  row.names = F)
      },
      contentType = NA
    )
  }
}

run_go_class <- function(input, output) {
  level <- input$level_ggo
  onts <- input$go_term
  org_name <- input$species_choice
  genes <- str_split(input$go_geneid, "(\n|\\s)")[[1]]
  genes <- genes[genes != "" & !is.na(genes)]
  result <- go_class(genes=genes, org_name=org_name, onts=onts, level=level)
  df <- data.frame()
  for (ont in names(result)) {
    r_df <- as.data.frame(result[[ont]]) %>% filter(Count > 0)
    r_df$ontology <- rep(ont, nrow(r_df))
    df <- rbind(df, r_df)
  }
  rownames(df) <- NULL
  df$ontology <- toupper(df$ontology)

  output$ggo_df <- DT::renderDataTable(
    DT::datatable(df, options=list(searching=FALSE))
  )
  output$ggo_plot <- renderPlot({
    if (is.null(df)) return(NULL)
    plot_go(ggo_df=df)
  })
  
  output$ggo_f_render <- renderUI({
    downloadButton(outputId="ggo_df_f", label="Download table")
  })
  output$ggo_p_render <- renderUI({
    downloadButton(outputId="ggo_plot_f", label="Download plot")
  })
  
  
  output$ggo_df_f <- downloadHandler(
    filename = function() {
      paste("go_classification_",Sys.Date(),".csv",sep = "")
    },
    content = function(fname) {
      write.csv(df, file = fname,
                row.names = F)
    },
    contentType = NA
  )
  
  output$ggo_plot_f <- downloadHandler(
    filename = function() {
      paste("go_classification_",Sys.Date(),".png",sep = "")
    },
    content = function(fname) {
      plot_go(ggo_df=df)
      ggsave(fname, width = 20, height = 12, units = "cm")    },
    contentType = NA
  )
  
}

shinyServer(function(input, output, session) {
  # Statisitic

  s_data <- eventReactive(input$s_action, {
    infile <- input$s_file
    header <- input$s_header
    if (input$s_type == "csv") {
      exec_func(args=list(infile$datapath, header), func=read_file)
    }
  })
  
  output$s_data <- DT::renderDataTable(
    DT::datatable(s_data(), options=list(searching=FALSE))
  )
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