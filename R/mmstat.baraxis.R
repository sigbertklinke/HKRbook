#' mmstat.baraxis
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
#' x <- 0:15
#' px <- dbinom(x, 10, 0.5)
#' plot(x, px, type="h")
#' plot(x, px, type="h", axes=FALSE)
#' mmstat.baraxis(1, range(x), at=x, labels=as.character(x))
#' par(oldpar)
mmstat.baraxis <- function(side, range, at, labels, ...) {
  pos <- 1+pretty(range)
  graphics::axis(side, at=at[pos], labels=labels[pos], ...)
}
