## Whenever you have a cyclic dependency, without really having one,
## it's due some generics you are defining in your code:
## that's why I'm writing here this reminder. If you ever encounter a
## cyclic dependency, slap this to R console:
##
##  trace(stop, recover)
##
## and then inspect in which frame there's a getGeneric a see which one
## pisses R off.
#' Classe contenitore di dati (serie storiche)
#'
#' @name Dataset
#' @usage Dataset(url, ids)
#' @aliases Dataset-class
#' @slot data hash::hash containing data
#' @slot url path where data is
#' @title Dataset OOP
#' @export Dataset
#' @exportClass Dataset

Dataset <- methods::setClass( # nolint
  "Dataset",
  representation(data = "hash", url = "character"))

.init <- function(x, url = "") { # nolint
  x@data <- hash::hash()
  x
}

methods::setMethod(
  "initialize",
  signature("Dataset"),
  function(.Object, url = "") { # nolint
    .init(.Object, url = url)
  })

#' Ritorna il numero di oggetti contenute nel Dataset
#'
#' @name length
#' @title Elenco nomi Dataset
#' @export

#' @param x un istanza di Dataset
#' @return il numero di serie storiche nel Dataset

methods::setMethod(
  "length",
  c("Dataset"),
  function(x) {
    length(x@data)
  })


#' Ritorna un elenco di nomi di serie storiche serie storiche contenuti
#' nel Dataset
#'
#' @name as.vector
#' @title Elenco nomi Dataset
#' @export
#' @seealso \code{link{rcf::names}}
#' @param x un istanza di Dataset
#' @return Una rappresentazione a lista del Dataset

methods::setMethod(
  "as.vector",
  c("Dataset"),
  function(x) {
    as.list(x@data)
  })

#' Elenco dei nomi di serie contenuti nel Dataset
#'
#' @name names
#' @title Elenco nomi Dataset
#' @export
#' @param x un istanza di Dataset
#' @return un character array contenente i nomi delle serie storiche nel Dataset

methods::setMethod(
  "names",
  c("Dataset"),
  function(x) {
    hash::keys(x@data)
  })

#' Esegue la differenza tra due dataset di oggetti (a patto che la differenza
#' sia definita per gli oggetti)
#' Se un oggetto non e' comune ai due dataset viene lanciato un warning
#'
#' @name "-"
#' @title Differenza tra Dataset
#' @export
#' @param e1 Dataset (primo operando)
#' @param e2 Dataset (secondo operando)
#' @return Il dataset con le differenze

methods::setMethod(
  "-",
  signature(e1 = "Dataset", e2 = "Dataset"),
  function(e1, e2) {
    nomi1 <- names(e1)
    nomi2 <- names(e2)
    common <- intersect(nomi1, nomi2)
    not_common <- union(
      setdiff(nomi1, nomi2),
      setdiff(nomi2, nomi1))

    if (length(common) == 0) {
      stop("No common objects between the datasets")
    }

    if (length(not_common) > 0) {
      lapply(not_common, function(name) {
        warning(paste0(name, " not common, excluded from diff"))
      })
    }

    result <- Dataset()

    data <- suppressWarnings(foreach::`%dopar%`(foreach::foreach(
      nome = iterators::iter(common),
      .multicombine = TRUE,
      .combine = c), {
        ret <- list()
        ret[[nome]] <- e1[[nome]] - e2[[nome]]
        ret
      }))
    # names(data) <- common
    as.dataset(data)
  })

#' Default metodo show per la classe Dataset
#'
#' @name show
#' @title Show Method
#' @seealso \code{Dataset}
#' @param x Dataset da mostrare

methods::setMethod(
  "show",
  "Dataset",
  function(object) {
    template <- "Dataset with {{num}} object{{s}}\n"
    num <- length(object@data)
    s <- if(num == 1) "" else "s"
    message <- whisker::whisker.render(template, list(num = num, s = s))
    cat(message)
  })


#' Ritorna il dataset come una \code{list} di serie storiche
#'
#' @name as.list
#' @aliases as.list
#' @export

#' @param x il Dataset
#' @return la \code{list} contenente le serie storiche

methods::setMethod(
  "as.list",
  signature("Dataset"),
  function(x) as.list.Dataset(x))

#' Ritorna il `Dataset` come `list` di oggetti
#'
#' @name as.list
#' @seealso base::as.list
#' @return una `list` di Timeseries
#' @export

as.list.Dataset <- function(x, ...) as.list(x@data, ...)


#' checks if \code{x} is an object of type \code{Dataset}
#'
#' @name is.dataset
#' @usage is.dataset(x)
#' @export
#' @param x a generic object
#' @return \code{TRUE} if \code{x} is a \code{Dataset}, \code{FALSE} otherwise

is.dataset <- function(x) inherits(x, "Dataset")

#' Costruisce un dataset.
#'
#' @name dataset
#' @title Dataset OOP
#' @rdname Dataset.Rd
#' @export

#' @param ... Qui si puo specificare
#'            - una stringa con un URL da cui costruire il Dataset
#'            - una lista di oggetti
#' @return Il Dataset richiesto.
#' @examples
#' \dontrun{
#' ds <-Dataset()
#' ds <- Dataset('/path/to/something/useful')
#' given \code{lll} is a list of timeseries
#' ds <- dataset(lll)
#' }

dataset <- function(...) {
  params <- list(...)
  class <- "Dataset"

  if ("biss" %in% names(params) && params$biss) { # nocov start
    if (requireNamespace("BItools", quietly = TRUE)) {
      class <- "BissDataset"
      return(new(class, params[[1]]))
    }
    stop("Non c'e' la library BItools")
  } # nocov end

  new(class, ...)
}


#' apply the abs function on all the timeseries contained in this Dataset
#'
#' @name abs_ds
#' @param x a Dataset we want to apply the abs
#' @usage abs_ds(x)
#' @export
#' @return a Dataset with all the timesereis with the abs applied
#' @note fix for 31922

abs_ds <- function(x) {
  l <- as.list(x)
  nomi <- names(l)
  l <- lapply(l, abs)
  names(l) <- nomi
  as.dataset(l)
}



#' Accesses the timeseries/object based on its name
#'
#' @param x dataset
#' @param name nome dell'oggetto da estrarre
#' @export

methods::setMethod(
  "$",
  signature("Dataset"),
  function(x, name) {
    x[[unlist(stringr::str_split(name, " "))]]
  })


methods::setMethod(
  "$",
  signature("Dataset"),
  function(x, name) {
    x[[unlist(stringr::str_split(name, " "))]]
  })
