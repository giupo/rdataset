#' Casts to a Dataset
#'
#' @name as.dataset
#' @usage as.dataset(x)
#' @param x a generico object with "[[" method defined and names
#' @param ... forza la creazione di un nuovo dataset, anche se x e' un `Dataset`
#' @return a Dataset with data defined in x
#' @include dataset.r
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
    Dataset(x)
  })


methods::setMethod(
  "as.dataset",
  c("data.frame"),
  function(x) {
    ret <- Dataset()
    by_name <- split(x, x$name)
    lapply(by_name, function(y) {
      name <- y$name[[1]]
      ret[[name]] <- from_data_frame_to_ts(y)
    })
  })


from_data_frame_to_ts <- function(y) {
  data <- y[order(y$idx), ]
  year <- data$years[[1]]
  period <- data$period[[1]]
  freq <- data$freq[[1]]
  ts(data$obs, start = c(year, period), frequency = freq)
}
