#' Esegue il `window` sul Dataset
#'
#' @importFrom stats window
#' @param x Dataset su cui applicare il window
#' @param ... altri parametri da passare a stats::window
#' @method window Dataset

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


#' The 'window' generic
#'
#' @param x An object
#' @param ... see params for `stats::window`
#' @seealso `stats::window()`
#' @return An object of the same class as `x`, possibly modified
#' @rdname window-methods

setGeneric(
  "window",
  function(x, ...) {
    standardGeneric("window")
  }
)

#' @rdname window.Dataset
#' @aliases window,Dataset,ANY-method

setMethod(
  "window",
  "Dataset",
  function(x, ...) {
    window.Dataset(x, ...)
  }
)