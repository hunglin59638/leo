#!/usr/bin/env Rscript
options(encoding = "UTF-8")
suppressPackageStartupMessages({
  library(shiny, quietly=T)
  library(shinydashboard, quietly=T)
  library(shinythemes, quietly=T)
})
source("modules/stastistics_ui.R")
source("modules/idconvert_ui.R")
source("modules/go_ui.R")
source("modules/ora_ui.R")
shinyUI(
  navbarPage("Bioinformatic toolkit",
             statistic_tabpanel,
             id_conv_tabpanel,
             go_class_tabpanel,
             ora_tabpanel,
  )
)
