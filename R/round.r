
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
  })

