#' mmstat.axis
#'
#' Based on `range` the position of the labels are determined and the axis is plotted.
#'
#' @inheritParams graphics::axis
#' @param range range: a data range
#' @param ...  further parameters to [graphics::axis]
#'
#' @return adds a axis to a plot
#' @export
#'
#' @examples
#' oldpar <- par(mfrow=c(1,2))
#' plot(iris[,1])
#' plot(iris[,1], axes=FALSE)
#' mmstat.axis(2, iris[,1])
#' par(oldpar)
mmstat.axis <- function(side, range, at, labels, ...) {
  at  <- pretty(range)
  dec <- mmstat.dec(at)
  graphics::axis(side, at=at, labels=sprintf("%.*f", dec$decimal, at), ...)
}
