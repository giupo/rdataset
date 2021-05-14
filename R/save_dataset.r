#' Salva un Dataset.
#'
#' @name saveDataset
#' @title Salva un Dataset
#' @export
#' @seealso \code{saveGraph}
#' @param x un Dataset
#' @param path percorso dove salvare il Dataset
#' @docType methods
#' @rdname saveDataset-methods

methods::setGeneric (
  "saveDataset",
  function(x, path){
    standardGeneric("saveDataset")
  })

#' @rdname saveDataset-methods
#' @aliases saveDataset,Dataset,character-method

methods::setMethod(
  "saveDataset",
  signature("Dataset", "character"),
  function(x, path) {
    .saveDataset(x, path)
  })

.saveDataset <- function(x, path) {
  data <- x@data
  data <- lapply(as.list(data), to_list)
  json_data <- RJSONIO::toJSON(data, digits=20)
  write(json_data, path)
}
