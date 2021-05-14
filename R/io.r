#' converts a timeseries to a list representation
#'
#' @name to_list
#' @export
#' @param t a timeseries
#' @return a list object

to_list <- function(t) {
  if (stats::is.ts(t)) {
    freq <- stats::frequency(t)
    year <- stats::start(t)[[1]]
    period <- stats::start(t)[[2]]
  } else {
    freq <- 0
    year <- 0
    period <- 0
  }

  numbers <- as.vector(t)

  list(
    freq = freq,
    year = year,
    period = period,
    numbers = numbers)
}

#' converts a list structure to a BIMETS timeseries
#'
#' @name from_list
#' @export
#' @param lista a list object
#' @return a timeseries

from_list <- function(lista) {
  if (is.list(lista)) {
    if (all(c("year", "period", "freq", "numbers") %in% names(lista))) {
      freq <- lista$freq
      year <- lista$year
      period <- lista$period
    } else {
      year <- 0
      period <- 0
      freq <- 0
    }
    payload <- lista$numbers
    if(year == 0 || period == 0 || freq == 0) {
      payload
    } else {
      stats::ts(payload, start = c(year, period), frequency = freq)
    }
  } else {
    lista
  }
}

#' Converts to JSON a timeseries
#'
#' @name to_json
#' @export
#' @param t a bimets timeseries
#' @return a String representation of a timeseries as a JSON object

to_json <- function(t) {
  RJSONIO::toJSON(to_list(t), digits = 20)
}

#' converts a JSON timeseries to a BIMETS timeseries timeseries
#'
#' @name from_json
#' @param json a character array containing a json string
#' @return a BIMETS timeseries
#' @seealso \code{from_list}
#' @export

from_json <- function(json) {
  from_list(RJSONIO::fromJSON(json, nullValue = NA))
}
