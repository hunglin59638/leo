#!/usr/bin/env Rscript
pkg <- c("lattice")
suppressMessages(invisible(lapply(pkg, require, character.only=T, quietly=T)))

desc_summarize <- function(data) {
  group <- colnames(data)[1]
  value <- colnames(data)[2]
  exp <- eval(parse(text = paste(value, "~", group)))
  Summarize(exp, data =data)
}

