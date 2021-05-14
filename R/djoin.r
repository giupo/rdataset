
.djoin <- function(historic, x, date) {
  closure <- function(name) {
    ret <- list()
    if(!name %in% names(historic)) {
      warning(name, "not in historic Dataset, adding without joining")
      ret[[name]] <- x[[name]]
    } else {
      ret[[name]] <- tryCatch(
        stats::as.ts(
          tis::mergeSeries(
            historic[[name]],
            stats::window(x[[name]], start = date))),
        error = function(cond) {
          stop(name, ": ", cond)
        })
    }
    ret
  }

  ret <- Dataset()

  # used to pass checks
  name <- NULL

  ret@data <- hash::hash(
    foreach::`%dopar%`(foreach::foreach(
      name = iterators::iter(names(x)),
      .combine = c, .multicombine = TRUE), {
        closure(name)
      }))
  ret
}

#' Effettua il join ad una data per due Dataset
#'
#' Joina i due dataset \code{x} e \code{historic} alla data (\code{date})
#' specificata.
#'
#' @name djoin
#' @export
#' @param historic il dataset storico
#' @param x il dataset nuovo
#' @param date una periodo di join
#' @return Un dataset joinato
#' @docType methods
#' @rdname djoin-methods

methods::setGeneric(
  "djoin",
  function(historic, x, date) {
    standardGeneric("djoin")
  })


#' @rdname djoin-methods
#' @aliases djoin,Dataset,Dataset,ANY-method

methods::setMethod(
  "djoin",
  c("Dataset", "Dataset", "ANY"),
  function(historic, x, date) {
    .djoin(historic, x, date)
  })
