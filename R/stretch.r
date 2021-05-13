#' Allunga tutte le serie del Dataset.
#'
#' La funzione segue la seguente convezione:
#'    - se la serie termina in `C` o `S` e' una condistenza e va allungata
#'      con il medesimo valore
#'    - altrimenti e' un flusso e va allungata con zero
#'
#'
#' @name stretch
#' @usage stretch(x, periodo)
#' @param x nome del Dataset
#' @param periodo coppia `(anno, trimestre)` rispetto cui allungare le serie
#' @return il dataset con le serie allungate
#' @export

methods::setGeneric(
  "stretch",
  function(x, prd) {
    standardGeneric("stretch")
  })


.stretch <- function(x, prd) {
  ret <- list()
  for (name in names(x)) {
    xts::last_char <- substr(name, nchar(name), nchar(name))
    stock <- (xts::last_char == "C" || xts::last_char == "S")
    serie <- x[[name]]
    ret[[name]] <- stretch_it(serie, UPTO=prd, stock=stock)
  }
  ret
}

methods::setMethod(
  "stretch",
  signature("Dataset", "numeric"),
  function(x, prd) {
    as.dataset(.stretch(x, prd))
  })

methods::setMethod(
  "stretch",
  signature("list", "numeric"),
  function(x, prd) {
    .stretch(x, prd)
  })

