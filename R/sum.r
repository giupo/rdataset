#' Esegue il `sum` sul Dataset
#'
#' @param x dataset da sommare
#' @param ... other objects to add
#' @param na.rm Not used, just to be compliant with sum
#' 
#' @export

sum.Dataset <- function(x,..., na.rm = FALSE) { # nolint
  somma <- NULL
  freq <- NULL
  for(name in names(x)) {
    serie <- x[[name]]

    freq <- rutils::ifelse(is.null(freq), stats::frequency(serie), freq)

    if (freq != stats::frequency(serie)) {
      warning(name, " has a different frequency ",
              stats::frequency(serie), " != ", freq, ", skipping ...")
      next
    }

    somma <- rutils::ifelse(is.null(somma), serie, somma + serie)
  }

  somma
}
