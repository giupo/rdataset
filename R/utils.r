date_index <- function(idx, freq) {
  idx <- zoo::as.Date(idx, frac = 1)
  if (freq == 1) idx <- idx - 1
  idx
}
