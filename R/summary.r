#' Print a summary of this Dataset
#'
#' For each series prints it's stard period, end period and frequency
#'
#' @name shortSummary
#' @rdname shortSummary
#' @usage shortSummary(object)
#' @param object a `Dataset` object
#' @export
#'

methods::setGeneric(
  "shortSummary",
  function(ds) {
    standardGeneric("shortSummary")
  })

#' @rdname shortSummary

methods::setMethod(
  "shortSummary",
  signature("Dataset"),
  function(ds) {
    NOMI <- c()
    STARTP <- c()
    ENDP <- c()
    FREQ <- c()
    for(name in names(ds)) {
      series <- ds[[name]]
      startp <- start(series)[[2]]
      starty <- start(series)[[1]]
      endp <- end(series)[[2]]
      endy <- end(series)[[1]]
      freq <- stats::frequency(series)

      NOMI <- c(NOMI, name)
      STARTP <- c(STARTP, paste0(starty, "/",startp))
      ENDP <- c(ENDP, paste0(endy, "/", endp))
      FREQ <- c(FREQ,paste(freq))
    }

    df <- data.frame(NOMI=NOMI, start = STARTP, END=ENDP, FREQ=FREQ)
    print(df)
    invisible(df)
  })


#' tabs all the series in the Dataset
#'
#' @name fullSummary
#' @usage fullSummary(ds)
#' @export
#' @param ds a `Dataset` instance

methods::setGeneric(
  "fullSummary",
  function(ds) {
    standardGeneric("fullSummary")
  })

.fullSummary <-  function(ds, digits=2) {
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

methods::setMethod(
  "fullSummary",
  signature("Dataset"),
  function(ds) {
    .fullSummary(ds)
  })

