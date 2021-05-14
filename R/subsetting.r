#' standard method for accessing indexed elements in a container
#'
#' @rdname subsetting-methods
#' @docType methods
#' @name [
#' @param x Dataset
#' @param i index element identifier
#' @param j dunno what to do with this
#' @param ... just for compliance
#' @param drop just for compliance
NULL

#' @rdname subsetting-methods
#' @aliases [,Dataset,ANY,missing,ANY-method

methods::setMethod(
  "[",
  c("Dataset", "ANY", "missing", "ANY"),
  function(x, i, j, ..., drop = TRUE) {
    out <- Dataset()
    data <- x@data

    if (is.logical(i) && !any(i)) return(Dataset())
    if (is.numeric(i) && length(i) == 0) return(Dataset())

    if (is.numeric(i) | is.logical(i)) {
      aslist <- as.list(x)
      out@data <- hash::hash(aslist[i])
    } else if (is.character(i)) {
      for (name in i) {
        out[name] <- data[[name]]
      }
    } else {
      stop("can't subset")
    }
    out
  })

#' standard method for accessing indexed elements in a container
#'
#' @rdname subsetting-methods
#' @docType methods
#' @name [[
#' @param x Dataset
#' @param i index element identifier
#' @param j dunno what to do with this
#' @param ... just for compliance
#' @param drop just for compliance
NULL

#' @rdname subsetting-methods
#' @aliases [[,Dataset,ANY,missing,ANY-method


methods::setMethod(
  "[[",
  signature("Dataset", "character"),
  function(x, i) {
    out <- list()
    for(name in i) {
      out[[name]] <- x@data[[name]]
    }
    if (length(out) == 1) {
      out <-  out[[1]]
    }
    out
  })

#' standard method for accessing indexed elements in a container
#'
#' @rdname subsetting-methods
#' @docType methods
#' @name [<-
#' @param x Dataset
#' @param i index element identifier
#' @param j dunno what to do with this
#' @param ... just for compliance
#' @param value the value to be set for `i`

NULL

#' @rdname subsetting-methods
#' @aliases [<-,Dataset,ANY,missing,ANY-method

methods::setMethod(
  "[<-",
  c("Dataset", "ANY", "missing", "ANY"),
  function(x, i, j, ..., value) {
    data <- x@data
    data[i] <- value
    x@data <- data
    invisible(x)
  })

#' standard method for accessing indexed elements in a container
#'
#' @rdname subsetting-methods
#' @docType methods
#' @name [[<-
#' @param x Dataset
#' @param i index element identifier
#' @param j dunno what to do with this
#' @param ... just for compliance
#' @param value the value to be set for `i`
NULL

#' @rdname subsetting-methods
#' @aliases [[<-,Dataset,ANY,missing,ANY-method


methods::setMethod(
  "[[<-",
  c("Dataset", "ANY", "missing", "ANY"),
  function(x, i, j, ..., value) {
    x@data[i] <- value
    invisible(x)
  })
