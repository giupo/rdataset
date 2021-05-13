
#' Ritorna una singola serie storica o un sub Dataset di serie storiche
#'
#' @name "["
#' @title Subsetting
#' @rdname subsetting
#' @export
#' @param x il dataset da cui estrarre la/le serie
#' @param i i nomi da cui estrare (stringa o character array)
#' @param j unused here
#' @param ... God knows what this is good for here.
#' @param drop Same as before
#' @return Ritorna un sub-Dataset

methods::setMethod(
  "[",
  c("Dataset", "ANY", "missing", "ANY"),
  function(x, i, j, ..., drop = TRUE) {
    out <- Dataset()
    data <- x@data

    if (is.logical(i) && !any(i)) return(Dataset())
    if (is.numeric(i) && length(i) == 0) return(Dataset())

    if (is.numeric(i) | is.logical(i)) {
      aslist <- as.list(x)
      out@data <- hash::hash(aslist[i])
    } else if (is.character(i)) {
      for (name in i) {
        out[name] <- data[[name]]
      }
    } else {
      stop("can't subset")
    }
    out
  })



#' Ritorna un list o una singola  serie storica
#' 
#' @name "[["
#' @title Subsetting
#' @rdname subsetting
#' @export
#' @param x il dataset da cui estrarre la/le serie
#' @param i i nomi da cui estrare (stringa o character array)
#' @param j unused here
#' @param ... God knows what this is good for here.
#' @param drop Same as before
#' @return Ritorna una serie storica on una list di serie storiche

methods::setMethod(
  "[[",
  signature("Dataset", "character"),
  function(x, i) {
    out <- list()
    for(name in i) {
      out[[name]] <- x@data[[name]]
    }
    if (length(out) == 1) {
      out <-  out[[1]]
    }
    out
  })

#' Imposta, data una stringa ed un oggetto xts, una serie storica nel Dataset
#'
#' @name "[<-"
#' @title Subsetting
#' @rdname subsetting
#' @export
#' @param x un istanza di Dataset
#' @param i una stringa (il nome della serie storica)
#' @param j una stringa (never userd)
#' @param ... God knows what is this good for
#' @param value l'oggetto da impostare

methods::setMethod(
  "[<-",
  c("Dataset", "ANY", "missing", "ANY"),
  function(x, i, j, ..., value) {
    data <- x@data
    data[i] <- value
    x@data <- data
    invisible(x)
  })

#' Imposta, data una stringa ed un oggetto xts, una serie storica nel Dataset
#'
#' @name "[[<-"
#' @title Subsetting
#' @export
#' @param x un istanza di Dataset
#' @param i una stringa (il nome della serie storica)
#' @param j una stringa (never userd)
#' @param ... God knows what is this good for
#' @param value l'oggetto da impostare

methods::setMethod(
  "[[<-",
  c("Dataset", "ANY", "missing", "ANY"),
  function(x, i, j, ..., value) {
    x@data[i] <- value
    invisible(x)
  })
