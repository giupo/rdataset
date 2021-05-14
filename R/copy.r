
#' Makes a deep copy of the object
#'
#' @name copy
#' @param x oggetto da copiare
#' @return una copia di `x`
#' @export
#' @docType methods
#' @rdname copy-methods

methods::setGeneric(
  "copy",
  function(x) {
    standardGeneric("copy")
  })


#' @rdname copy-methods
#' @aliases copy,Dataset-method

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
