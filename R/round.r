#' Yields a rounding of the object
#'
#' @rdname round-methods
#' @docType methods
#' @name round
#' @param x object to round
#' @param digits numeber of digits for roundings
#' @return the same object, with roundings
NULL

#' @rdname round-methods
#' @aliases round,Dataset,ANY-method

methods::setMethod(
  "round",
  signature("Dataset", "ANY"),
  function(x, digits=0) {
    stopifnot(is.numeric(digits))
    ret <- copy(x)
    for(name in names(ret)) {
      serie <- ret[[name]]
      ret[[name]] <- round(serie, digits=digits)
    }
    ret
  })

