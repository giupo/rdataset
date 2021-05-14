#' Converte in un file XLSX il dataset
#'
#' @name to_xlsx
#' @usage to_xlsx(x, path, bycol)
#' @param x nome del Dataset
#' @param periodo percorso del file
#' @param bycol esporta per colonna se `TRUE`, altrimenti per riga
#' @export

methods::setGeneric(
  "to_xlsx",
  function(x, path, bycol=T) {
    standardGeneric("to_xlsx")
  })


methods::setMethod(
  "to_xlsx",
  signature("Dataset", "character", "logical"),
  function(x, path, bycol=T) {
    .to_xlsx(x, path, bycol = bycol)
  })


do.call.cbind <- function(lst) {
  while (length(lst) > 1) {
    idxlst <- seq(from = 1, to = length(lst), by = 2)
    lst <- lapply(idxlst, function(i) {
      if (i == length(lst)) { return(lst[[i]]) }
      return(cbind(lst[[i]], lst[[i + 1]]))
    })
  }

  lst[[1]]
}

#' Exporta in xlsx un Dataset
#'
#' @name .to_xlsx
#' @usage .to_xlsx(x, path, bycol)
#' @param x Istanza di Dataset
#' @param path percorso dove esportare i dati
#' @param bycol esporta per colonna se `TRUE`, altrimenti per riga
#' @include as_data_frame.r utils.r


.to_xlsx <- function(x, path, bycol = TRUE) {
  df <- as.data.frame(x)
  by_freq <- split(df, df$freq)

  tutte <- as.list(x)
  tutte <- lapply(tutte, xts::as.xts)

  for(name in names(tutte)) {
    data <- tutte[[name]]
    colnames(data) <- name
    tutte[[name]] <- data
  }

  container <- list()
  for(freq in names(by_freq)) {
    nomi <- unique(by_freq[[freq]]$name)
    lista <- tutte[nomi]
 
    singola <- do.call.cbind(lista)
    idx <- date_index(zoo::index(singola), as.integer(freq))
    singola <- as.data.frame(singola)
    singola <- if(bycol) singola else as.data.frame(t(singola))
    # aggiunge il row.names al data.frame non supportato da write_xlsx
    singola <- if(bycol) {
      cbind(" " = idx, singola)
    } else {
      cbind(" " = row.names(singola), singola)
    }
    container[[freq]] <- singola
  }

  writexl::write_xlsx(container, path = path)
  container
}
