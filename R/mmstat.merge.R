#' mmstat.merge
#'
#' Computes a new range from by a union of the two ranges.
#'
#' @param range1 range: first range
#' @param range2 range: second range
#'
#' @return new range
#' @export
#'
#' @examples
#' mmstat.merge(c(0,1), c(0.5, 2)) # returns c(0, 2)
mmstat.merge <- function (range1, range2) {
  return (c(min(range1, range2), max(range1, range2)))
}
