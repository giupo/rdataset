#' Esegue il `window` sul Dataset
#'
#' @importFrom stats window
#' @param x Dataset su cui applicare il window
#' @param ... altri parametri da passare a stats::window
#' @export window

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


#' @rdname window.Dataset
#' @aliases window,Dataset,ANY-method

setMethod(
  "window",
  "Dataset",
  function(x, ...) {
    window.Dataset(x, ...)
  }
)