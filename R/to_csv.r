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
