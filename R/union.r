#' Fonde due dataset in un unico dataset
#' Le serie storiche contenute nel secondo \code{y} sovrascrivono le
#' serie storiche contenute nel primo in caso che l'intersezione dei
#' nomi dei due dataset sia non nulla.
#'
#' @seealso base::union
#' @param x il primo dataset
#' @param y il secondo dataset
#' @return un dataset con l'unione di tutte le serie storiche
#' @export

union.Dataset <- function(x, y) {
  out <- Dataset()
  x <- as.list(x)
  y <- as.list(y)
  for (name in names(y)) {
    out[name] = y[[name]]
  }
  for (name in names(x)) {
    out[name] = x[[name]]
  }
  out
}

#' Fonde due dataset in un unico dataset
#' Le serie storiche contenute nel secondo \code{y} sovrascrivono le
#' serie storiche contenute nel primo in caso che l'intersezione dei
#' nomi dei due dataset sia non nulla.
#'
#' @name union
#' @seealso base::union
#' @param x il primo dataset
#' @param y il secondo dataset
#' @return un dataset con l'unione di tutte le serie storiche
#' @export
#' @docType methods
#' @rdname union-methods

methods::setGeneric(
  "union",
  function(x, y) {
    standardGeneric("union")
  })

#' @rdname union-methods
#' @aliases union,Dataset,Dataset-method

methods::setMethod(
  "union",
  c("Dataset","Dataset"),
  function(x, y) {
    union.Dataset(x, y)
  })

