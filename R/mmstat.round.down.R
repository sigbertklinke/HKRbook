#' mmstat.round.down
#'
#' Rounds down.
#'
#' @param x numeric: values for rounding
#' @param digits numeric: digits for rounding (default: \code{0})
#'
#' @return down rounded values
#' @export
#'
#' @examples
#' x <- runif(5)
#' cbind(x, mmstat.round.down(x, 1))
mmstat.round.down <- function (x, digits=0) {
  xr <- round(x, digits)
  tf <- (xr>x)
  xr[tf] <- xr[tf]-10^(-digits)
  xr
}
