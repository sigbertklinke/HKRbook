#' mmstat.range
#'
#' Computes a range from several R objects by union.
#'
#' @param ... R objects
#'
#' @return range
#' @export
#'
#' @examples
#' mmstat.range(-5:5, 0:10) # returns c(-5, 10)
mmstat.range <- function (...) {
  ranges <- list(...)
  isn    <- sapply(ranges, is.numeric)
  if (all(isn)) {
    mins   <- sapply(ranges, min)
    maxs   <- sapply(ranges, max)
  } else {
    mins <- maxs <- NA
  }
  return (c(min(mins), max(maxs)))
}
