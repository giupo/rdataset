#' ritorna un array di date da un index di xts
#'
#' la funzione ha cura di correggere il problema delle date annuali di xts
#' 

date_index <- function(idx, freq) {
  idx <- zoo::as.Date(idx, frac = 1)
  if (freq == 1) idx <- lubridate::`%m+%`(idx, months(12)) - 1
  idx
}
