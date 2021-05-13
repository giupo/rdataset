
#' Questa funzione esegue una deep copy dell'oggetto
#'
#' @name copy
#' @usage copy(x)
#' @param x oggetto da copiare
#' @return una copia di `x`
#' @export
#' @exportMethod copy

methods::setGeneric(
  "copy",
  function(x) {
    standardGeneric("copy")
  })


methods::setMethod(
  "copy",
  signature("Dataset"),
  function(x) {
    ret <- Dataset()
    for (name in names(x)) {
      ret[[name]] <- x[[name]]
    }
    ret@url <- x@url
    ret
  })
