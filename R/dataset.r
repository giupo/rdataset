## Whenever you have a cyclic dependency, without really having one,
## it's due some generics you are defining in your code:
## that's why I'm writing here this reminder. If you ever encounter a
## cyclic dependency, slap this to R console:
##
##  trace(stop, recover)
##
## and then inspect in which frame there's a getGeneric a see which one
## pisses R off.

#' Classe contenitore di dati (serie storiche)
#'
#' @name Dataset-class
#' @rdname Dataset-class
#' @slot data hash::hash containing data
#' @slot url path where data is
#' @export Dataset
#' @exportClass Dataset
#' @importClassesFrom hash hash
#' @importFrom methods new

# requireNamespace("hash", quietly=TRUE)

Dataset <- methods::setClass( # nolint
  "Dataset",
  representation(data = "hash", url = "character"))

.init <- function(x, url = "") { # nolint
  x@data <- hash::hash()
  x
}

methods::setMethod(
  "initialize",
  signature("Dataset"),
  function(.Object, url = "") { # nolint
    .init(.Object, url = url)
  })


#' Returns the length of the object
#'
#' @name length
#' @param x object to retrieve the length
#' @return the length of the object x
#' @rdname length-methods
#' @docType methods
NULL

#' @rdname length-methods
#' @aliases length,Dataset-method

methods::setMethod(
  "length",
  c("Dataset"),
  function(x) {
    length(x@data)
  })

#' Yields the names contained in the Object
#'
#' @rdname names-methods
#' @docType methods
#' @name names
#' @param x object where "names" are from
NULL


#' @rdname names-methods
#' @aliases names,Dataset-method

methods::setMethod(
  "names",
  c("Dataset"),
  function(x) {
    hash::keys(x@data)
  })

#' shows the object on the stdout
#'
#' @rdname show-methods
#' @docType methods
#' @param object object to printout
#' @name show
NULL

#' @rdname show-methods
#' @aliases show,Dataset-method

methods::setMethod(
  "show",
  "Dataset",
  function(object) {
    template <- "Dataset with {{num}} object{{s}}\n"
    num <- length(object@data)
    s <- if(num == 1) "" else "s"
    message <- whisker::whisker.render(template, list(num = num, s = s))
    cat(message)
  })


#' Ritorna il `Dataset` come `list` di oggetti
#'
#' @param x Dataset da convertire in list
#' @param ... per essere compliant su as.list
#' @seealso base::as.list
#' @return una `list` di Timeseries
#' @export

as.list.Dataset <- function(x, ...) as.list(x@data, ...)


#' checks if \code{x} is an object of type \code{Dataset}
#'
#' @name is.dataset
#' @export
#' @param x a generic object
#' @return \code{TRUE} if \code{x} is a \code{Dataset}, \code{FALSE} otherwise

is.dataset <- function(x) inherits(x, "Dataset")

#' Costruisce un dataset.
#'
#' @name dataset
#' @title Dataset OOP
#' @rdname Dataset.Rd
#' @export

#' @param ... Qui si puo specificare
#'            - una stringa con un URL da cui costruire il Dataset
#'            - una lista di oggetti
#' @return Il Dataset richiesto.
#' @examples
#' \dontrun{
#' ds <-Dataset()
#' ds <- Dataset('/path/to/something/useful')
#' given \code{lll} is a list of timeseries
#' ds <- dataset(lll)
#' }

dataset <- function(...) {
  Dataset(...)
}


#' apply the abs function on all the timeseries contained in this Dataset
#'
#' @name abs_ds
#' @param x a Dataset we want to apply the abs
#' @export
#' @return a Dataset with all the timesereis with the abs applied
#' @note fix for 31922

abs_ds <- function(x) {
  l <- as.list(x)
  nomi <- names(l)
  l <- lapply(l, abs)
  names(l) <- nomi
  as.dataset(l)
}



#' Accesses the timeseries/object based on its name
#'
#' @param x dataset
#' @param name nome dell'oggetto da estrarre
#' @export

methods::setMethod(
  "$",
  signature("Dataset"),
  function(x, name) {
    x[[unlist(stringr::str_split(name, " "))]]
  })


methods::setMethod(
  "$",
  signature("Dataset"),
  function(x, name) {
    x[[unlist(stringr::str_split(name, " "))]]
  })
