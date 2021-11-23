library(AnnotationHub)
library(clusterProfiler)

get_ah <- function() {
  AnnotationHub()
}

query_orgdb <- function(org_name) {
  # ah <- AnnotationHub()
  # orgdbs <- query(ah, c("orgDB"))
  
  if (org_name %in% c("Gallus gallus", "Chicken")) {
    orgdb_name <- "org.Gg.eg.db"
  } else if (org_name %in% c("Sus scrofa", "Pig")) {
    orgdb_name <- "org.Ss.eg.db"
  } else if (org_name %in% c("Homo sapiens")) {
    orgdb_name <- "org.Hs.eg.db"
  }
  BiocManager::install(orgdb_name, ask=F)
  library(orgdb_name, character.only=T)
  return(eval(parse(text=orgdb_name)))
}

go_class <- function(genes, org_name, onts=c("BP","MF", "CC"), level=2) {
  genes <- as.character(genes[!is.na(genes) && genes != ""])
  org_db <- query_orgdb(org_name)
  result <- list()
  for (ont in onts) {
    r <- groupGO(gene=genes, OrgDb=org_db, keyType="ENTREZID", ont=ont,
                 level=level, readable=F) %>% as.data.frame()
    result[[ont]] <- r
  }
  return(result)
}

