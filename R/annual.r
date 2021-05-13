
#' Esegue l'annual del dataset
#'
#' Se le serie contenute sono gia' annuali, non fa nulla e
#' lascia la serie cosi' com'e'
#'
#' @name annual
#' @usage annual(x)
#' @param x Dataset da annualizzare
#' @return un dataset annualizzato
#' @export
#' @exportMethod annual
#' @include dataset.r

methods::setGeneric(
  "annual",
  function(x) {
    standardGeneric("annual")
  })

methods::setMethod(
  "annual",
  signature("Dataset"),
  function(x) {
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

methods::setMethod(
  "annual",
  list(x = "ts"),
  function(x) {
    attributi <- attributes(x)
    if(frequency(x) == 1) {
      return(x)
    }
    aggregate(x, nfrequency = 1,
              ifelse(attributi$stock == 1, xts::last, sum))
  })

