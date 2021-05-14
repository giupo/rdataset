
#' Annualizza l'oggetto 
#'
#' Se le serie contenute sono gia' annuali, non fa nulla e
#' lascia la serie cosi' com'e'
#'
#' @param x Dataset da annualizzare
#' @return un dataset annualizzato
#' @include dataset.r
#' @export
#' @docType methods
#' @rdname annual-methods


methods::setGeneric(
  "annual",
  function(x) {
    standardGeneric("annual")
  })

#' @rdname annual-methods
#' @aliases annual,Dataset-method

methods::setMethod(
  "annual",
  signature("Dataset"),
  function(x) {
    nome <- NULL
    as.dataset(suppressWarnings(foreach::`%dopar%`(
      foreach::foreach(
        nome = iterators::iter(names(x)),
        .multicombine = TRUE, .combine = c), {
          ret <- list()
          serie <- x[[nome]]
          ret[[nome]] <- annual(serie)
          ret
        })))
  })

#' @rdname annual-methods
#' @aliases annual,ts-method

methods::setMethod(
  "annual",
  list(x = "ts"),
  function(x) {
    attributi <- attributes(x)
    if (stats::frequency(x) == 1) {
      return(x)
    }
    stats::aggregate(
      x,
      nfrequency = 1,
      rutils::ifelse(attributi$stock == 1, xts::last, sum))
  })

