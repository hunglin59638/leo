#!/usr/bin/env Rscript
if (!require("AnnotationHub")) {
  BiocManager::install("AnnotationHub", quietly = TRUE)}; library(AnnotationHub)

get_orgdb <- function(organism="Gallus gallus") {
  ah <- AnnotationHub(ask=FALSE, cache=tools::R_user_dir("AnnotationHub", which="cache"))
  if (organism %in% c("Chicken", "Gallus gallus")) {
    organism <- "Gallus gallus"
  } else if (organism %in% c("Pig", "Sus scrofa")) {
    organism <- "Sus scrofa"
  } else if (organism %in% c("Human", "Homo sapiens")) {
    organism <- "Homo sapiens"
  } else if (organism %in% c("Sterlet", "Acipenser ruthenus")) {
    organism <- "Acipenser ruthenus"
  }
  org_info <- subset(ah, species == organism & rdataclass == 'OrgDb')
  return(ah[[org_info$ah_id]])
}

