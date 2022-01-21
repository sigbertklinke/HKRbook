#' in_range
#'
#' Checks if `x` is between `lower` and `upper`,
#'
#' @param x numeric: vaklues to check
#' @param lower numeric: lower bound
#' @param upper numeric: upper bound
#' @param rightmost.closed logical: if true then `x<=upper` is checked otherwise `x<upper` (default: `TRUE`)
#' @param left.open logical: if true then `upper<x` is checked otherwise `lower<=x` (default: `FALSE`)
#'
#' @return a logical vector whether `x` is in range or not
#' @export
#'
#' @examples
#' in_range(-1:2, 0, 1)
in_range <- function (x, lower, upper, rightmost.closed = TRUE, left.open = FALSE) {
  findInterval(x, range(c(lower, upper)), rightmost.closed=rightmost.closed, left.open=left.open)==1
}
