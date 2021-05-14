#' Esegue il `window` sul Dataset
#'
#' @importFrom stats window
#' @export

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
