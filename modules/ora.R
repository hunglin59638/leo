#/usr/bin/env Rscript
library(clusterProfiler)
padjust_method <- reactive({
  padjust_methods <- input$padjust
  if(padjust.methods == "Bonferroni") {return("bonferroni")
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