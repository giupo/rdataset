#' Hello function
#'
#' @name hello
#' @usage hello()
#' @param name Name to salute
#' @export

hello <- function(name = "") {
  message("Hello", name, "!")
}
