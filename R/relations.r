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

