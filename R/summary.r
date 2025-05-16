#' Print a summary of this Dataset
#'
#' For each series prints it's stard period, end period and frequency
#'
#' @name shortSummary
#' @param ds a `Dataset` object
#' @export
#' @docType methods
#' @rdname shortSummary-methods

methods::setGeneric(
  "shortSummary",
  function(ds) {
    standardGeneric("shortSummary")
  })

#' @rdname shortSummary-methods
#' @aliases shortSummary,Dataset-method

methods::setMethod(
  "shortSummary",
  signature("Dataset"),
  function(ds) {
    nomi <- c()
    inizio_periodo <- c()
    fine_periodo <- c()
    freqs <- c()
    for(name in names(ds)) {
      series <- ds[[name]]
      startp <- stats::start(series)[[2]]
      starty <- stats::start(series)[[1]]
      endp <- stats::end(series)[[2]]
      endy <- stats::end(series)[[1]]
      freq <- stats::frequency(series)

      nomi <- c(nomi, name)
      inizio_periodo <- c(inizio_periodo, glue::glue("{starty}/{startp}"))
      fine_periodo <- c(fine_periodo, glue::glue("{endy}/{endp}"))
      freqs <- c(freqs, freq) ## per portarla a stringa
    }

    df <- data.frame(
      NOMI = nomi,
      START = inizio_periodo,
      END = fine_periodo,
      FREQ = freqs
    )

    print(df)
    invisible(df)
  })


#' tabs all the series in the Dataset
#'
#' @name fullSummary
#' @export
#' @param ds a `Dataset` instance
#' @docType methods
#' @rdname fullSummary-methods

methods::setGeneric(
  "fullSummary",
  function(ds) {
    standardGeneric("fullSummary")
  })

.fullSummary <-  function(ds, digits = 2) { # nolint
  freq_bins <- hash::hash()
  for(name in names(ds)) {
    series <- round(ds[[name]], digits=digits)
    freq <- as.character(stats::frequency(series))
    series <- xts::as.xts(series)
    if(freq %in% hash::keys(freq_bins)) {
      bin <- freq_bins[[freq]]
      bin[[name]] <- series
      freq_bins[freq] <- bin
    } else {
      container <- list()
      container[[name]] <- series
      freq_bins[freq] <- container
    }
  }

  for(freq in hash::keys(freq_bins)) {
    samefreq <- freq_bins[[freq]]
    print(do.call(cbind, samefreq))
  }
}

#' @rdname fullSummary-methods
#' @aliases fullSummary,Dataset-method

methods::setMethod(
  "fullSummary",
  signature("Dataset"),
  function(ds) {
    .fullSummary(ds)
  })

