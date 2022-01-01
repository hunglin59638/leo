#!/usr/bin/env Rscript
if (!require("AnnotationHub")) {
  BiocManager::install("AnnotationHub", quietly = TRUE)}; library(AnnotationHub)

ah <- AnnotationHub()
org_name <- "Gallus gallus"
org_info <- subset(ah, species == org_name & rdataclass == 'OrgDb')
orgdb <- ah[[org_info$ah_id]]
