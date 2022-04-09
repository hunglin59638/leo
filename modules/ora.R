#/usr/bin/env Rscript
suppressPackageStartupMessages({
library(clusterProfiler)
})

get_ora_demo <- function() {
  file <- sub("/modules","", paste0(get_file_path(),"/test/goclass_demo.txt"))
  df <- read.csv(file)
  return(df[,1])
}

ora_kegg <- function(genelist, organism="gga", pvalueCutoff=0.05, 
                     pAdjustMethod="BH", qvalueCutoff=0.2) {
  ekegg <- enrichKEGG(genelist, organism=organism,
                     keyType="ncbi-geneid",
                     pvalueCutoff=pvalueCutoff, 
                     pAdjustMethod=pAdjustMethod,
                     qvalueCutoff=qvalueCutoff,
                     use_internal_data=F)# %>% 
    # setReadable(., OrgDb=orgdb, keyType="ENTREZID")
  return(ekegg)
}

ora_go <- function(genelist, orgdb, ont, pvalueCutoff=0.05, 
                   pAdjustMethod="BH", qvalueCutoff=0.2) {
  ego<- enrichGO(gene=genelist, 
                 OrgDb=orgdb, 
                 keyType="ENTREZID",
                 ont=ont, #'ALL','BP',"MF','CC'
                 pAdjustMethod=pAdjustMethod, 
                 pvalueCutoff=pvalueCutoff,
                 qvalueCutoff=qvalueCutoff,
                 pool=F)
  return(ego)
}

plot_ora <- function(object, file="", plot_type="dotplot") {
  if(plot_type == "dotplot") {
    object %>% clusterProfiler::dotplot(x="GeneRatio",
                                        font.size=9,
                                        color="p.adjust", 
                                        showCategory=20) 
  }
  if(plot_type == "Gene-Concept-Network") {
    object %>% cnetplot(categorySize="p.adjust", showCategory=5)
  }
  if(plot_type == "emapplot") {
    object %>% emapplot(showCategory=20, color="p.adjust", layout="kk",
                        vertex.label.cex=1.2)
  }
  if (!nchar(file)) {file <- paste(plot_type, '.png', sep='')}
  ggsave(file)
}

run_ora <- function(input, output) {
  
  genelist <- str_split(input$ora_geneid, "(\n|\\s)")[[1]]
  
  padjust_method <- reactive({
    padjust_methods <- input$padjust_method
    if(padjust_methods == "Bonferroni") {return("bonferroni")
    } else if (padjust_methods == "Holm") {return("holm")
    } else if (padjust_methods == "Hochberg") {return( "hochberg")
    } else if (padjust_methods == "Hommel") {return("hommel")
    } else if (padjust_methods == "Benjamini & Hochberg") {return("BH")
    } else if (padjust_methods == "Benjamini & Yekutieli") {return("BY")
    } else if (padjust_methods == "None") {return("none")
    }
  })
  pvalue_ora <- reactive(input$pvalue_cutoff)
  qvalue_ora <- reactive(input$qvalue_cutoff)
  species <- reactive(input$ora_species_choice)
  orgdb <- get_orgdb(species())

    if (input$db_choice == "GO") {
    ont <- reactive(input$ora_go_ont)
    ora_result <- ora_go(genelist=genelist, orgdb=orgdb, 
                         ont=ont(),
                         pvalueCutoff=pvalue_ora(), 
                         pAdjustMethod=padjust_method(), 
                         qvalueCutoff=qvalue_ora())
    
  } else if (input$db_choice == "KEGG") {
    org_name <- switch (input$species_choice,
      # https://www.genome.jp/kegg/catalog/org_list.html
      "Chicken"="gga",
      "Gallus gallus"="gga",
      "Pig"="ssc",
      "Anser cygnoides domesticus"="acyg",
      "Swan googse"="acyg",
      "Sus scrofa"="ssc",
      "Human"="hsa",
      "Homo sapiens"="hsa",
      "Acipenser ruthenus"="arut"
    )
    ora_result <- ora_kegg(genelist=genelist, organism=org_name,
                           pvalueCutoff=pvalue_ora(), 
                           pAdjustMethod=padjust_method(), 
                           qvalueCutoff=qvalue_ora())
  } else {warning("Error db choice")}
  
  if (is.null(ora_result)) {
    output$ora_df <- DT::renderDataTable(
      DT::datatable(data.frame(empty=c("Not found")), options=list(searching=FALSE))
    )
    return(NULL)
  }
  
  df <- ora_result@result
  rownames(df) <- NULL
  
  output$ora_df <- DT::renderDataTable(
    DT::datatable(df, options=list(searching=FALSE))
  )
  if (nrow(ora_result@result)) {
    output$ora_df_render <- renderUI({
      downloadButton(outputId="ora_df_f", label="Download table")
    })
  }

  output$ora_df_f <- downloadHandler(
    filename = function() {
      paste("ora_", Sys.Date(),".csv", sep = "")
    },
    content = function(fname) {
      write.csv(df, file = fname,
                row.names = F)
    },
    contentType = NA
  )
  # output$ora_dotplot <- renderPlot({
  #   if (is.null(ora_result@result)) {return(NULL)}
  #   plot_ora(ora_result, plot_type="dotplot")
  # })
  # 
  # output$ora_dotplot_render <- renderUI({
  #   downloadButton(outputId="ora_dotplot", label="Download dotplot")
  # })
  # 
}
