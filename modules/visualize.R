#!/usr/bin/env Rscript
pkg <- c("FSA")
suppressMessages(invisible(lapply(pkg, require, character.only=T, quietly=T)))

plot_histogram <- function(data) {
  group <- colnames(data)[1]
  value <- colnames(data)[2]
  exp <- eval(parse(text = paste("~", value, "|", group)))
  lattice::histogram(exp,  data=data, 
            layout=c(1,length(levels(as.factor(data[group])))), 
            panel=function(x, ...) {                             
              panel.histogram(x, ...) 
              panel.mathdensity(dmath = dnorm, col = "black",
                                args = list(mean=mean(x),sd=sd(x)))          
            })
}

plot_boxplot <- function(data) {
  group <- colnames(data)[1]
  value <- colnames(data)[2]
  exp <- eval(parse(text = paste(value, "~",  group)))
  boxplot(exp, data = data,
          ylab=value,
          xlab=group)
}

plot_go <- function(ggo_df) {
  ggo_df %<>% dplyr::select(ontology,Description,Count)
  ggo_df <- ggo_df[order(ggo_df[['ontology']],-ggo_df[['Count']]), ]
  ggo_df$Description <- factor(ggo_df$Description,
                                levels= ggo_df$Description)
  sort_df_all <- data.frame()
  if("BP" %in% ggo_df$ontology) {
    sort_df_BP <- filter(ggo_df, ontology == "BP")
    if (nrow(sort_df_BP) >10) {
      sort_df_BP <- sort_df_BP[1:10, ]
    }
    sort_df_BP$ontology <- rep("Biological process", nrow(sort_df_BP))
    sort_df_all <- rbind(sort_df_all, sort_df_BP, deparse.level = T)
  }
  if("CC" %in% ggo_df$ontology) {
    sort_df_CC <- filter(ggo_df, ontology == "CC" )
    if (nrow(sort_df_CC) >10) {
      sort_df_CC <- sort_df_CC[1:10, ]
    }
    sort_df_CC$ontology <- rep("Cellular component", nrow(sort_df_CC))
    sort_df_all <- rbind(sort_df_all, sort_df_CC,deparse.level = T)
  }
  if("MF" %in% ggo_df$ontology) {
    sort_df_MF <- filter(ggo_df, ontology == "MF")
    if (nrow(sort_df_MF) >10) {
      sort_df_MF <- sort_df_MF[1:10, ]
    }
    sort_df_MF$ontology <- rep("Molecular function", nrow(sort_df_MF))
    sort_df_all <- rbind(sort_df_all, sort_df_MF,deparse.level = T)
  }
  sort_df_all$Description <- factor(sort_df_all$Description,
                                    levels= sort_df_all$Description)
  ont_n <- length(levels(as.factor(ggo_df$ontology)))
  if(length(ont_n)==3) {
    word_size <- 10
  } else if(length(ont_n == 2)){
    word_size <- 11
  } else if(length(ont_n == 1)) {
    word_size <- 13
  }
  ggplot(data=sort_df_all, mapping = aes(x=Description ,y=Count,fill=ontology))+
    geom_bar(stat="identity") +
    coord_flip()+
    scale_x_discrete(limits=rev(levels(sort_df_all$Description)))+
    labs( x='Term', fill ='Ontology') +
    theme_bw(base_size=word_size)
}