#' Esegue il `sum` sul Dataset
#'
#' @export

sum.Dataset <- function(x,..., na.rm = FALSE) { # nolint
  somma <- NULL
  freq <- NULL
  for(name in names(x)) {
    serie <- x[[name]]

    freq <- rutils::ifelse(is.null(freq), frequency(serie), freq)

    if (freq != frequency(serie)) {
      warning(name, " has a different frequency ",
              frequency(serie), " != ", freq, ", skipping ...")
      next
    }

    somma <- rutils::ifelse(is.null(somma), serie, somma + serie)
  }

  somma
}


methods::setMethod(
  "sum",
  c("Dataset", "logical"),
  function(x,..., na.rm = FALSE) { # nolint
    sum.Dataset(x,..., na.rm = na.rm)
  })
