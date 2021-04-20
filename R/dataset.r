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
  representation(
    data = "hash",
    url = "character"))

.init <- function(.Object, url = NULL, ids = NULL) { # nolint
  if(is.null(url)) {
    raw_list <- list()
    .Object@data <- hash::hash()
    return(.Object)
  }

  .Object@url <- url

  starts_with <- function(x, pattern) grepl(paste0("^", pattern), x)

  if (starts_with(url, "http")) {
    stop("http[s] not Implemented")
  }

  # bypass for future releases
  parsed_url<- list()
  parsed_url$scheme <- NULL

  path <- normalizePath(url)

  raw_list <- if (is_JSON(path)) {
    load_dataset_json(path)
  } else if (is_csv_library(path)) {
    load_dataset_csv(path, ids = ids)
  } else {
    stop(parsed_url$scheme, "not Implemented")
  }

  .Object@data <- hash::hash(as.list(raw_list))
  .Object
}

methods::setMethod(
  "initialize",
  signature("Dataset"),
  function(.Object, url = NULL, ids = NULL) { # nolint
    .init(.Object, url = url, ids = ids)
  })

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
    
    data <- foreach::`%dopar%`(foreach::foreach(
      nome = iterators::iter(common),
      .multicombine = TRUE,
      .combine = c), {
        ret <- list()
        ret[[nome]] <- e1[[nome]] - e2[[nome]]
        ret
      })
    # names(data) <- common
    as.dataset(data)
  })


methods::setMethod(
  "==",
  signature("Dataset", "numeric"),
  function(e1, e2) {
    ret <- vector("logical", length = length(e1))
    names(ret) <- names(e1)
    for (name in names(e1)) {
      ret[[name]] <- all(e1[[name]] == e2)
    }
    ret
  })

methods::setMethod(
  ">",
  signature("Dataset", "numeric"),
  function(e1, e2) {
    ret <- vector("logical", length = length(e1))
    names(ret) <- names(e1)
    for (name in names(e1)) {
      ret[[name]] <- any(e1[[name]] > e2)
    }
    ret
  })

methods::setMethod(
  "<",
  signature("Dataset", "numeric"),
  function(e1, e2) {
    ret <- vector("logical", length = length(e1))
    names(ret) <- names(e1)
    for (name in names(e1)) {
      ret[[name]] <- any(e1[[name]] < e2)
    }
    ret
  })

methods::setMethod(
  "<=",
  signature("Dataset", "numeric"),
  function(e1, e2) {
    ret <- vector("logical", length = length(e1))
    names(ret) <- names(e1)
    for (name in names(e1)) {
      ret[[name]] <- any(e1[[name]] <= e2)
    }
    ret
  })

methods::setMethod(
  ">=",
  signature("Dataset", "numeric"),
  function(e1, e2) {
    ret <- vector("logical", length=length(e1))
    names(ret) <- names(e1)
    for(name in names(e1)) {
      ret[[name]] <- any(e1[[name]] >= e2)
    }
    ret
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
    cat("Dataset ")
    if (length(object@url) > 0) {
      cat(paste0("[url=",object@url, "] "))
    }
    cat(paste0("with ", length(object@data), " objects\n"))
  })

methods::setGeneric (
  "saveDataset",
  function(x, path){
    standardGeneric("saveDataset")
  })


#' Salva un Dataset.
#'
#' @name saveDataset
#' @title Salva un Dataset
#' @export
#' @seealso \code{saveGraph}
#' @param x un Dataset
#' @param path percorso dove salvare il Dataset

methods::setMethod(
  "saveDataset",
  signature("Dataset", "character"),
  function(x, path) {
    .saveDataset(x, path)
  })

.saveDataset <- function(x, path) {
  data <- x@data
  data <- lapply(as.list(data), to_list)
  json_data <- RJSONIO::toJSON(data, digits=20)
  write(json_data, path)
}




methods::setGeneric(
  "djoin",
  function(historic, x, date) {
    standardGeneric("djoin")
  })

#' Joina i due dataset \code{x} e \code{historic} alla data (\code{date})
#' specificata.
#' @name djoin
#' @aliases djoin

#' @export
#' @param x il dataset nuovo
#' @param historic il dataset storico
#' @param date una periodo di join
#' @return Un dataset joinato

methods::setMethod(
  "djoin",
  c("Dataset", "Dataset", "ANY"),
  function(historic, x, date) {
    closure <- function(name) {
      ret <- list()
      if(!name %in% names(historic)) {
        warning(name, "not in historic Dataset, adding without joining")
        ret[[name]] <- x[[name]]
      } else {
        ret[[name]] <- tryCatch(
          stats::as.ts(tis::mergeSeries(historic[[name]],
            window(x[[name]], start = date))),
          error = function(cond) {
            stop(name, ": ", cond)
          })
      }
      ret
    }

    ret <- Dataset()
    ret@data <- hash::hash(
      foreach::`%dopar%`(foreach::foreach(name = iterators::iter(names(x)),
        .combine = c, .multicombine = TRUE), {
        closure(name)
      }))
    ret
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

as.list.Dataset <- function(x, ...) as.list(x@data, ...) #cosa cambia dal precedente? Boh!


#' Fonde due dataset in un unico dataset
#' Le serie storiche contenute nel secondo \code{y} sovrascrivono le
#' serie storiche contenute nel primo in caso che l'intersezione dei
#' nomi dei due dataset sia non nulla.
#'
#' @name union
#' @aliases union
#' @seealso base::union
#' @export

#' @param x il primo dataset
#' @param y il secondo dataset
#' @return un dataset con l'unione di tutte le serie storiche
#' @export

union.Dataset <- function(x, y) { 
  out <- Dataset()
  x <- as.list(x)
  y <- as.list(y)
  for (name in names(y)) {
    out[name] = y[[name]]
  }
  for (name in names(x)) {
    out[name] = x[[name]] 
  }
  out
}

#' Fonde due dataset in un unico dataset
#' Le serie storiche contenute nel secondo \code{y} sovrascrivono le
#' serie storiche contenute nel primo in caso che l'intersezione dei
#' nomi dei due dataset sia non nulla.
#'
#' @name union
#' @aliases union
#' @seealso base::union
#' @exportMethod union
#' @export

#' @param x il primo dataset
#' @param y il secondo dataset
#' @return un dataset con l'unione di tutte le serie storiche

methods::setGeneric(
  "union",
  function(x, y) {
    standardGeneric("union")
  })

methods::setMethod(
  "union",
  c("Dataset","Dataset"),
  function(x, y) {
    union.Dataset(x, y)
  })


#' merges two timeseries datasets
#'
#' @name merge
#' @usage merge(x,y)
#' @param x first dataset
#' @param y second dataset
#' @return a dataset with the timeseries merged
#' @note raises a warning for timeseries names not common in `x` and `y`
#' @export

methods::setMethod(
  "merge",
  c("Dataset", "Dataset"),
  function(x,y) {
    common <- intersect(names(x), names(y))
    not_common <- unique(
      union(
        setdiff(names(x), names(y)),
        setdiff(names(y), names(x))
      ))

    if (length(not_common)) {
      warning("Le seguenti serie non sono comuni: ",
              paste(not_common, collapse=", "))
    }
    ret <- Dataset()
    for(name in common) {
      ret[[name]] <- stats::as.ts(tis::mergeSeries(x[[name]], y[[name]]))
    }
    ret
  })

#' Controlla che il percorso \code{path} contenga una struttura a
#' Grafo sul file system
#'
#' @name is_grafo
#' @aliases is_grafo

#' @export
#' @param path percorso da controllare
#' @return \code{TRUE} se il percorso e' un grafo valido,
#'         altrimenti \code{FALSE}

is_grafo <- function(path) {
  all(c("data","metadati", "tasks") %in% list.files(path))
}

#' Controlla che il percorso \code{path} e' un file JSON
#'
#' @name is_JSON
#' @aliases is_JSON

#' @export
#' @param path percorso da controllare
#' @return \code{TRUE} se il percorso e' un file JSON valido,
#'         altrimenti \code{FALSE}

is_JSON <- function(path) {
  fileInfo <- file.info(path)

  if(is.na(fileInfo$isdir)) {
    stop(paste0(path, " doesn't exist"))
  }

  rutils::ifelse(!fileInfo$isdir,
    tools::file_ext(path) == "json", FALSE)
}

#' Controlla che `path` sia una library CSV
#'
#' @name is_csv_library
#' @usage is_csv_library(path)
#' @param path percorso da controllare
#' @return `TRUE` se e' una library CSV, `FALSE` otherwise
#' @export

is_csv_library <- function(path) {
  csvs <- list.files(
    path, pattern="..csv",    
    full.names=TRUE, ignore.case=TRUE)
  return(length(csvs) > 0)
}

#' load a list of timeseries placed in path as CSV files.
#'
#' @name load_dataset_csv
#' @title internal functions
#' @param path path where CSV files are
#' @param ids names of the series to load
#' @return a named `list` with timeseries
#' @seealso tsRead_nativo
#' @note the path is scanned recursively

load_dataset_csv <- function(path, ids=NULL) {
  csvs <- list.files(
    path,
    pattern="\\.csv$",
    recursive=TRUE,
    full.names=TRUE,
    ignore.case=TRUE)
  
  if(!is.null(ids)) {
    filterWithIds <- Vectorize(function(X) {
      nome <- rutils::.basename(X)
      return(nome %in% ids)
    })

    csvs <- csvs[filterWithIds(csvs)]
  }

  nomi <- unlist(lapply(csvs, function(x) toupper(rutils::.basename(x))))
  
  foreach::`%dopar%`(foreach::foreach(filepath = iterators::iter(csvs), 
    .combine = c, .errorhandling = "remove"), {
    ret <- list()
    name <- toupper(basename(filepath))
    name <- gsub("\\.CSV", "", name)
    ret[[name]] <- tryCatch({
      tsRead_nativo(filepath)
    }, error = function(cond) {
      stop(name, ":", cond)
    })
    ret
  })
}

#' Carica un Dataset da un percorso di Grafo
#'
#' @name load_dataset_grafo
#' @usage load_dataset_grafo(path)
#' @param path percorso da cui caricare il Dataset
#' @return un `Dataset`
#' @note Funzione interna, potrebbe cambiare
#' @title Internal Functions

load_dataset_grafo <- function(path) load_dataset_csv(file.path(path, "data"))

#' Carica un `Dataset` a partire da un file JSON
#'
#' @name load_dataset_json
#' @usage load_dataset_json(path)
#' @param path percorso da cui caricare il Dataset
#' @return un `Dataset`
#' @note Funzione interna, potrebbe cambiare
#' @title Internal Functions

load_dataset_json <- function(path) {
  json_data <- fromJSON(path, nullValue=NA)
  ret <- list()
  for(name in names(json_data)) {
    ret[[name]] <- from_list(data)
  }
  names(ret) <- names(json_data)
  ret
}

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
      freq <- frequency(series)

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
    freq <- as.character(frequency(series))
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

#' Casts to a Dataset
#'
#' @name as.dataset
#' @usage as.dataset(x)
#' @param x a generico object with "[[" method defined and names
#' @param ... forza la creazione di un nuovo dataset, anche se x e' un `Dataset`
#' @return a Dataset with data defined in x
#' @export

methods::setGeneric(
  "as.dataset",
  function(x, ...) {
    standardGeneric("as.dataset")
  })

methods::setMethod(
  "as.dataset",
  signature("list"),
  function (x) {
    ret <- Dataset()
    data <- ret@data
    slot(data, ".xData") <- as.environment(x)
    ret@data <- data
    ret
  })

methods::setMethod(
  "as.dataset",
  signature("Dataset"),
  function(x) {
    x
  })


methods::setMethod(
  "as.dataset",
  signature("character"),
  function(x) {
    if(file.exists(x)) {
      return(Dataset(x))
    } else {
      return(SvnDataset(x))
    }
  })


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

#' Allunga tutte le serie del Dataset.
#'
#' La funzione segue la seguente convezione:
#'    - se la serie termina in `C` o `S` e' una condistenza e va allungata
#'      con il medesimo valore
#'    - altrimenti e' un flusso e va allungata con zero
#'
#'
#' @name stretch
#' @usage stretch(x, periodo)
#' @param x nome del Dataset
#' @param periodo coppia `(anno, trimestre)` rispetto cui allungare le serie
#' @return il dataset con le serie allungate
#' @export

methods::setGeneric(
  "stretch",
  function(x, prd) {
    standardGeneric("stretch")
  })


.stretch <- function(x, prd) {
  ret <- list()
  for (name in names(x)) {
    xts::last_char <- substr(name, nchar(name), nchar(name))
    stock <- (xts::last_char == "C" || xts::last_char == "S")
    serie <- x[[name]]
    ret[[name]] <- stretch_it(serie, UPTO=prd, stock=stock)
  }
  ret
}

methods::setMethod(
  "stretch",
  signature("Dataset", "numeric"),
  function(x, prd) {
    as.dataset(.stretch(x, prd))
  })

methods::setMethod(
  "stretch",
  signature("list", "numeric"),
  function(x, prd) {
    .stretch(x, prd)
  })

.to_csv <- function(x, sep=";", mappa=NULL) {
  x <- as.list(x)
  counter <- 1
  is_interactive <- interactive()
  if (is_interactive) pb <- progress::progress_bar$new(
      format="[:bar] :current/:total (:percent)", total=length(x))
  if (is_interactive) pb$tick(counter)
  mappa_ <- if (is.null(mappa)) {
    function(x) {
      x
    }
  } else {
    function(x) {
      mappa[[x]]
    }
  }
  rows <- character(0)

  for(name in names(x)) {
    counter <- counter + 1
    if (is_interactive) pb$tick(counter)
    tt <- x[[name]]
    mapped_name <- mappa_(name)
    idx <- index(tt)
    for(i in seq_along(idx)) {
      numero <- round(tt[i])
      anno <- idx[i]
      rows <- paste(
        rows,
        paste(
          gsub("\\.", sep, mapped_name),
          anno,
          numero,
          "A",
          "F",
          sep =sep),
        sep = "\n")
    }
  }

  rows
}

methods::setGeneric(
  "to_csv",
  function(x, sep=";", mappa=NULL) {
    standardGeneric("to_csv")
  })

methods::setMethod(
  "to_csv",
  signature("Dataset", "ANY", "ANY"),
  function(x, sep=";", mappa=NULL) {
    .to_csv(x, sep, mappa)
  })


#' Allunga tutte le serie del Dataset.
#'
#' La funzione segue la seguente convezione:
#'    - se la serie termina in `C` o `S` e' una condistenza e va allungata
#'      con il medesimo valore
#'    - altrimenti e' un flusso e va allungata con zero
#'
#'
#' @name to_xlsx
#' @usage to_xlsx(x, path, bycol)
#' @param x nome del Dataset
#' @param periodo percorso del file
#' @param bycol esporta per colonna se `TRUE`, altrimenti per riga
#' @export

methods::setGeneric(
  "to_xlsx",
  function(x, path, bycol=T) {
    standardGeneric("to_xlsx")
  })

methods::setMethod(
  "to_xlsx",
  signature("Dataset", "character", "logical"),
  function(x, path, bycol=T) {
    .to_xlsx(x, path, bycol = bycol)
  })

methods::setMethod(
  "to_xlsx",
  signature("Dataset", "character"),
  function(x, path) {
    .to_xlsx(x, path, bycol = FALSE)
  })


do.call.cbind <- function(lst) {
  while (length(lst) > 1) {
    idxlst <- seq(from = 1, to = length(lst), by = 2)
    lst <- lapply(idxlst, function(i) {
      if (i == length(lst)) { return(lst[[i]]) }
      return(cbind(lst[[i]], lst[[i + 1]]))
    })
  }

  lst[[1]]
}

#' Exporta in xlsx un Dataset
#'
#' @name .to_xlsx
#' @usage .to_xlsx(x, path, bycol)
#' @param x Istanza di Dataset
#' @param path percorso dove esportare i dati
#' @param bycol esporta per colonna se `TRUE`, altrimenti per riga
#' @rdname to_xlsx-internal

.to_xlsx <- function(x, path, bycol=T) {
  stopifnot(requireNamespace("xlsx"))
  x <- as.list(x)
  path <- suppressWarnings(normalizePath(path))
  freqs <- hash::hash()

  freqs_labels <- hash::hash(
    list("1" = "annuali",
         "4" = "trimestrali",
         "12" = "mensili"))

  count <- 0
  is_interactive <- interactive()
  if (is_interactive) pb <- progress::progress_bar$new(
    format = "[:bar] :current/:total (:percent)", total = length(x))

  for (name in names(x)) {
    count <- count + 1
    if (is_interactive) pb$tick(count)
    serie <- x[[name]]
    f <- as.character(frequency(serie))
    if (f %in% hash::keys(freqs)) {
      container <- freqs[[f]]
      container[[name]] <- serie
      freqs[[f]] <- container
    } else {
      container <- list()
      container[[name]] <- serie
      freqs[[f]] <- container
    }
  }

  if(file.exists(path)) {
    unlink(path)
  }

  count <- 0

  if (is_interactive) pb <- progress::progress_bar$new(
    format = "[:bar] :current/:total (:percent)",
    total = length(hash::keys(freqs)))
  
  for (freq in hash::keys(freqs)) {
    count <- count + 1
    if (is_interactive) pb$tick(count)
    container <- freqs[[freq]]
    nomi <- names(container)
    container <- lapply(container, xts::as.xts)
    container <- do.call.cbind(container)
    colnames(container) <- nomi
    if (!bycol) {
      container <- t(container)
      colnames(container) <- gsub("X", "", as.character(colnames(container)))
    }
    sheet_name <- freqs_labels[[freq]]
    xlsx::write.xlsx(container, file = path,
      sheetName = sheet_name, append = TRUE)
  }
}


methods::setMethod(
  "$",
  signature("Dataset"),
  function(x, name) {
    x[[unlist(stringr::str_split(name, " "))]]
  })

#' Esegue il `window` sul Dataset
#'
#' @export window.Dataset

window.Dataset <- function(x, ...) { # nolint
  aslist <- as.list(x)
  params <- list(...)
  start <- params$start
  end <- params$end
  ret <- lapply(aslist, function(y, ...) {
    tryCatch({
      stats::window(y, start = start, end = end)
    }, error = function(cond) {
      y
    })
  })
  as.dataset(ret)
}

#' Questa funzione esegue una deep copy dell'oggetto
#'
#' @name copy
#' @usage copy(x)
#' @param x oggetto da copiare
#' @return una copia di `x`
#' @export
#' @exportMethod copy

methods::setGeneric(
  "copy",
  function(x) {
    standardGeneric("copy")
  })


methods::setMethod(
  "copy",
  signature("Dataset"),
  function(x) {
    ret <- Dataset()
    for (name in names(x)) {
      ret[[name]] <- x[[name]]
    }
    ret@url <- x@url
    ret
  })

methods::setMethod(
  "round",
  signature("Dataset", "ANY"),
  function(x, digits=0) {
    stopifnot(is.numeric(digits))
    ret <- copy(x)
    for(name in names(ret)) {
      serie <- ret[[name]]
      ret[[name]] <- round(serie, digits=digits)
    }
    ret
  }
)



#' Esegue l'annual del dataset
#'
#' Se le serie contenute sono gia' annuali, non fa nulla e
#' lascia la serie cosi' com'e'
#'
#' @name annual
#' @usage annual(x)
#' @param x Dataset da annualizzare
#' @return un dataset annualizzato
#' @export
#' @exportMethod annual

methods::setGeneric(
  "annual",
  function(x) {
    standardGeneric("annual")
  })

methods::setMethod(
  "annual",
  signature("Dataset"),
  function(x) {
    as.dataset(foreach::`%dopar%`(foreach::foreach(nome = iterators::iter(names(x)),
      .multicombine = TRUE, .combine = c), {
      ret <- list()
      serie <- x[[nome]]
      ret[[nome]] <- annual(serie)
      ret
    }))
  })

methods::setMethod(
  "annual",
  list(x = "ts"),
  function(x) {
    attributi <- attributes(x)
    if(frequency(x) == 1) {
      return(x)
    }
    aggregate(x, nfrequency = 1,
      ifelse(attributi$stock == 1, xts::last, sum))
  })


methods::setMethod(
  "sum",
  signature("Dataset", "logical"),
  function(x,..., na.rm = FALSE) { # nolint
    sum.Dataset(x,..., na.rm = na.rm)
  })


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