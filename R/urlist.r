#' Ritorna un data.frame con metadati delle serie
#'
#' I metadati ritornati sono start period, end period, freq
#'
#' @name URLIST
#' @usage URLIST(x)
#' @param x un Dataset
#' @return un dataframe con 4 colonne (nome, start, end, freq)
#' @export

methods::setGeneric(
  "URLIST",
  function(x) {
    standardGeneric("URLIST")
  })


methods::setMethod(
  "URLIST",
  signature("Dataset"),
  function(x) {
    closure <- function(name) {
      timeSeries <- x[[name]]
      freq <- frequency(timeSeries)
      starty <- stats::start(timeSeries)[[1]]
      startp <- stats::start(timeSeries)[[2]]
      endy <- stats::end(timeSeries)[[1]]
      endp <- stats::end(timeSeries)[[2]]
      START <- starty + startp/freq
      END <- endy + endp/freq

      data.frame(list(
        name=name, freq = freq, start = START,
        end = END, starty = starty, startp = startp,
        endy = endy, endp = endp))
    }

    foreach::`%dopar%`(foreach::foreach(
      name = iterators::iter(names(x)), .combine=rbind), {
      closure(name)
    })
  })
