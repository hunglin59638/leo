#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  lapply(c(
    "shiny",
    "yaml",
    "AnnotationHub",
    "clusterProfiler",
    "DOSE",
    "AnnotationDbi",
    "ggplot2",
    "stringr",
    "magrittr",
    "dplyr",
    "plyr",
    "purrr",
    "rstatix",
    "FSA",
    "lattice",
    "car",
    "emmeans",
    "corrplot",
    "rentrez",
    "DT",
    "lattice"
  ), require, character.only = TRUE)
})
cmdArgs <- commandArgs(trailingOnly = FALSE)
root_dir <- normalizePath(dirname((sub("^--file=", "", cmdArgs[grep("^--file=", cmdArgs)]))))[1]
config <- yaml::read_yaml(file.path(root_dir, "config.yml"))
port <- cmdArgs[length(cmdArgs)]
if (is.na(port) || grepl("^--file=", port, fixed = F)) {
  port <- config$port
} else {
  port <- as.numeric(port)
}
options(shiny.port = port)
options(shiny.host = "0.0.0.0")
runApp(appDir = root_dir, launch.browser = F)
