#!/usr/bin/env Rscript
library(shiny)
cmdArgs <- commandArgs(trailingOnly = FALSE)
root_dir <- normalizePath(dirname((sub("^--file=", "", cmdArgs[grep("^--file=", cmdArgs)]))))[1]
runApp(root_dir, launch.browser=TRUE)
