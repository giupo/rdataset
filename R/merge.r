#' merges two timeseries datasets
#'
#' @param x first dataset
#' @param y second dataset
#' @param ... just for compliance
#' @return a dataset with the timeseries merged
#' @note raises a warning for timeseries names not common in `x` and `y`
#' @export

merge.Dataset <- function(x, y, ...) {
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
    ret[[name]] <- stats::as.ts(
      tis::mergeSeries(x[[name]], y[[name]]))
  }
  ret
}
