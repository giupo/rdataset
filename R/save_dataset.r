methods::setGeneric (
           "saveDataset",
           function(x, path){
             standardGeneric("saveDataset")
           })


#' Salva un Dataset.
#'
#' @name saveDataset
#' @title Salva un Dataset
#' @export
#' @seealso \code{saveGraph}
#' @param x un Dataset
#' @param path percorso dove salvare il Dataset

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
