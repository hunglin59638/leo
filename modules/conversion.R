library(rentrez)

link_db <- function(ids, fr_db="protein", target_db="gene") {
  if (class(ids) %in% c("character", "numeric")) { ids <- c(as.character(ids))}
  ids <- ids[ids !=""]
  # all_links_sep <- entrez_link(db=target_db, dbfrom=fr_db, 
  #                              id=ids, by_id=TRUE)
  all_links_sep <- list()
  for (chunk in split(ids, rep_len(1:100, length(ids)))) {
    all_links_sep <- append(all_links_sep, entrez_link(db=target_db, dbfrom=fr_db, 
                                        id=chunk, by_id=TRUE))
  }
  r_list <- list()
  for (i in 1:length(all_links_sep)) {
    value <- tryCatch({
      all_links_sep[[i]]$links[[paste0(fr_db, "_",target_db)]]
    }, error = function(err) {"error"}
    )
    # value <- all_links_sep[[i]]$links[[paste0(fr_db, "_",target_db)]]
    if (is.null(value)) {
      value <- ""
    } else if (value == "error") {
      next
    }
    r_list[[ids[i]]] <- value
  }
  return(r_list)
}

summary_id <- function(ids, db="gene") {
  fetch <- list()
  for  (chunk in split(ids, rep_len(1:100, length(ids)))) 
  fetch <- append(fetch, entrez_summary(db=db, id=chunk))
  return(fetch)
}

convert_id <- function(ids, fr, to) {
  if (class(ids) %in% c("character", "numeric")) { ids <- c(as.character(ids))}
  db_fr <- switch(fr, 
         `GI number`="protein", 
         `Protein Accession`="protein",
         `Entrez Gene (GeneID)`="gene", "protein")
  db_to <- switch(to, 
                  `GI number`="protein", 
                  `Protein Accession`="protein",
                  `Entrez Gene (GeneID)`="gene", "protein")

  if (fr %in% c("GI number", "Protein Accession") && to == "Entrez Gene (GeneID)") {
    # for (col in cols) {
      result <- link_db(ids=ids, fr_db="protein", target_db="gene")
    # }  
  } else if ((fr == "Protein Accession" && to == "GI number") || 
             (fr == "GI number" && to == "Protein Accession") ) {
    fetch <- summary_id(db=db_fr, id=ids)
    cols <- names(fetch)
    result <- list()
    for (col in cols) {
      value <- fetch[[col]] 
      prot_acc <- value[["caption"]]
      acc_ver <- value[["accessionversion"]]
      gi_number <- value[["gi"]]
      
      if (prot_acc %in% ids) {
        id <- prot_acc
      } else if (acc_ver %in% ids) {
        id <- acc_ver
      } else if (fr == "GI number") {
        id <- gi_number
      } else {
        print(gi_number)
        next
      }
      
      if (fr == "GI number") {
        result[gi_number] <- acc_ver 
      } else if (fr == "Protein Accession") {
        result[id] <- gi_number
        } 
      
      
    }
  } else {return(NULL)}
  return(result)
}
