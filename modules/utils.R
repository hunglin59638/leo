#!/usr/bin/env Rscript

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

read_file <- function(file, header=T, sep=NULL) {
  if (!is.null(file)) {
    if (endsWith(file, ".csv")) {
      sep <- ","
      data <- read.table(file=file, header=header, sep=sep) } 
    else if (endsWith(file, ".tsv")) {
      sep <- "\t"
      data <- read.table(file=file, header=header, sep=sep) } 
    else if (endsWith(file, ".xslx")) {
      sep <- NULL
      data <- readxl::read_excel(path=file, col_names=T) }
    } else {
      return(NULL)
      #warning(paste(file, "is not found"))
    }
  return(data)
}

exec_func <- function(args, func, except=NULL){
  if (is.null(args)) {return(except)} else {

    if (class(args) == "list") {
      text = "func("
      count <- 0
      for (arg in args) {
        arg_str <- paste0("arg", as.character(count))
        delim <- if (count > 0) "," else ""
        text <- stringi::stri_join(c(text, arg_str), collapse = delim)
        assign(arg_str, arg)
        count <- count + 1
      }
      text = paste0(text, ")")
      eval(parse(text=text))
    } else {
      func(args)
    }


  }
}
