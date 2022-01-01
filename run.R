#!/usr/bin/env Rscript
library(shiny)
cmdArgs <- commandArgs(trailingOnly=FALSE)
root_dir <- normalizePath(dirname((sub("^--file=", "", cmdArgs[grep("^--file=", cmdArgs)]))))[1]
# print(paste("root_dir: ", root_dir))
options(shiny.port = 6523)
options(shiny.host = "0.0.0.0")
runApp(appDir = root_dir, launch.browser = F)
