
#' converte il Dataset in un dataframe
#'
#' Ogni riga del dataframe contiene NOME della serie
#' Osservazione, anno, periodo, frequenza e data come
#' timestamp dell'osservazione
#'
#' @export
#' @param x Dataset da convertire in data.frame
#' @param ... solo per compliancy con as.data.frame
#' @return un data.frame con i dati del Dataset
#' @include utils.r

as.data.frame.Dataset <- function(x, ...) {
  x <- lapply(x, xts::as.xts)
  # riapplico come colname il nome della serie (dai names della lista)
  for(name_ in names(x)) {
    data <- x[[name_]]
    freq <- stats::frequency(data)

    colnames(data) <- "obs"
    x[[name_]] <- data

    # add names column
    name <- rep(name_, length(data))

    idx <- date_index(zoo::index(data), freq)
    # add year column
    year <- as.integer(lubridate::year(idx))
    # add period column
    period <- as.integer(lubridate::month(idx) / 12 * freq)
    # add freq column
    freq <- rep(as.integer(freq), length(data))

    df <- as.data.frame(data)
    df <- cbind(df, name, year, period, freq, idx)
    x[[name_]] <- df
  }

  df <-do.call("rbind", x)
  rownames(df) <- NULL
  df
}
