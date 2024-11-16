# /usr/bin/env Rscript
library(rentrez)

echo_gi_demo <- function() {
  ids <- as.character(c(211497, 2024483540, 2024385564, 2024387417, 2024484284, 454))
  return(paste(ids, collapse = "\n"))
}

echo_prot_acc_demo <- function() {
  ids <- as.character(c(
    "AAA48674.1", "XP_040547394.1", "XP_015154259.2",
    "XP_040509289.1", "XP_040547759.1"
  ))
  return(paste(ids, collapse = "\n"))
}

echo_entrezid_demo <- function() {
  ids <- as.character(c(395198, 100858920, 121107692, 475))
  return(paste(ids, collapse = "\n"))
}

prot_to_gene <- function(ids, fr_db = "protein", target_db = "gene") {
  if (class(ids) %in% c("character", "numeric")) {
    ids <- c(as.character(ids))
  }
  ids <- ids[ids != ""]
  ids_trim <- str_split(string = ids, pattern = "\\.[0-9]") %>%
    lapply(., function(x) {
      head(x, 1)
    })
  names(ids_trim) <- ids
  all_links_sep <- list()
  for (chunk in split(sapply(ids_trim, function(x) {
    x[1]
  }), rep_len(1:100, length(ids_trim)))) {
    res <- entrez_link(
      db = target_db, dbfrom = fr_db,
      id = as.character(chunk), by_id = T, cmd = "neighbor"
    )
    if (length(chunk) > 1) {
      names(res) <- chunk
    } else {
      res <- list(res)
      names(res) <- chunk
    }

    all_links_sep <- append(all_links_sep, res)
  }
  r_list <- list()
  for (id_trim in names(ids_trim)) {
    id <- ids_trim[[id_trim]]
    value <- all_links_sep[[id]]$links[[paste0(fr_db, "_", target_db)]]
    if (is.null(value)) {
      value <- ""
    }
    r_list[[id_trim]] <- value
  }


  for (res in all_links_sep) {
    value <- tryCatch(
      {
        all_links_sep[[i]]$links[[paste0(fr_db, "_", target_db)]]
      },
      error = function(err) {
        "error"
      }
    )
    if (is.null(value)) {
      value <- ""
    } else if (value == "error") {
      next
    }
    r_list[[ids[i]]] <- value
  }
  return(r_list)
}

gi_to_protein_acc <- function(ids, reverse = F) {
  fr <- if (reverse) "prot_acc" else "gi"
  fetch <- list()
  for  (chunk in split(ids, rep_len(1:100, length(ids)))) {
    tryCatch(
      fetch <- append(fetch, entrez_summary(
        db = "protein", id = chunk,
        always_return_list = T
      )),
      error = function(err) {
        if (class(chunk) == "character") {
          fetch[[chunk]] <- NA
        } else {
          for (i in chunk) {
            fetch[[i]] <- NA
          }
        }
      }
    )
  }
  dict <- list()
  for (key in names(fetch)) {
    if ("uid" %in% names(fetch[[key]])) {
      gi <- fetch[[key]][["uid"]]
    } else {
      gi <- NA
    }
    if ("accessionversion" %in% names(fetch[[key]])) {
      prot_acc <- fetch[[key]][["accessionversion"]]
    } else {
      prot_acc <- NA
    }

    if (reverse) {
      if (!is.na(prot_acc)) {
        dict[[prot_acc]] <- gi
      }
    } else {
      if (!is.na(gi)) {
        dict[[gi]] <- prot_acc
      }
    }
  }

  result <- list()
  for (id in ids) {
    value <- dict[[id]]
    result[[id]] <- if (!is.null(value)) value else NA
  }
  return(result)
}

convert_id <- function(ids, fr, to) {
  if (class(ids) %in% c("character", "numeric")) {
    ids <- c(as.character(ids))
  }
  db_fr <- switch(fr,
    `GI number` = "protein",
    `Protein Accession` = "protein",
    `Entrez Gene (GeneID)` = "gene",
    "protein"
  )
  db_to <- switch(to,
    `GI number` = "protein",
    `Protein Accession` = "protein",
    `Entrez Gene (GeneID)` = "gene",
    "protein"
  )

  if (fr %in% c("GI number", "Protein Accession") && to == "Entrez Gene (GeneID)") {
    result <- prot_to_gene(ids = ids, fr_db = "protein", target_db = "gene")
  } else if ((fr == "Protein Accession" && to == "GI number") ||
    (fr == "GI number" && to == "Protein Accession")) {
    result <- gi_to_protein_acc(id = ids, reverse = if (fr == "GI number") F else T)
  } else {
    return(NULL)
  }
  return(result)
}

run_convert_id <- function(input, output) {
  ids <- str_split(input$id_input, "(\n|\\s)")[[1]]
  ids <- ids[ids != ""]
  fr <- input$id_select_fr
  to <- input$id_select_to
  result <- convert_id(ids = ids, fr = fr, to = to)
  df <- data.frame()
  for (name in names(result)) {
    row <- c(name, result[[name]])
    df <- rbind(df, row)
  }
  colnames(df) <- c(fr, to)
  if (nrow(df) > 0) {
    df %<>% filter(df[, 1] != "")
    output$con_result <- DT::renderDataTable(
      DT::datatable(df, options = list(searching = FALSE))
    )
    output$con_file_render <- renderUI({
      downloadButton(outputId = "con_file", label = "Download")
    })
    output$con_file <- downloadHandler(
      filename = function() {
        paste("geneid_conversion_", Sys.Date(), ".csv", sep = "")
      },
      content = function(fname) {
        write.csv(df, file = fname, na = "", row.names = F)
      },
      contentType = NA
    )
  }
}
