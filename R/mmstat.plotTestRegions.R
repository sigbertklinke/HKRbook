#' mmstat.plotTestRegions
#'
#' Plots the test regions in a plot
#'
#' @param crit numeric(2): critical value(s)
#' @param xlim numeric(2): the x limits of the plot
#' @param ylim numeric(2): the y limits of the plot
#' @param cex numeric: amount by which plotting text should be magnified relative to the default
#' @param close logical: should the region box be closed by vertical lines (default: \code{FALSE})
#' @param col color: pecification for the default plotting color (default: \code{"black"})
#' @param label unused
#' @param pos unused
#'
#' @return adds test regions to a plot
#' @export
#'
#' @examples
#' x  <- (-30:30)/10
#' px <- dnorm(x)
#' plot(x, px, type="l", ylim=c(-0.25, max(px)), xlim=range(x))
#' mmstat.plotTestRegions(crit=c(-1.96, +1.96), xlim=range(x), ylim=c(-0.2, -0.1), cex=1)
mmstat.plotTestRegions <- function (crit, xlim, ylim, cex, close=FALSE, col="black", label=NULL, pos=1) {
  graphics::lines(xlim, c(ylim[1], ylim[1]), col=col)
  graphics::lines(xlim, c(ylim[2], ylim[2]), col=col)
  if (close) {
    graphics::lines(c(xlim[1],xlim[1]), ylim, col=col)
    graphics::lines(c(xlim[2],xlim[2]), ylim, col=col)
  }
  cu <- max(crit[1], xlim[1])
  if (crit[1]>=xlim[1]) {
    graphics::lines(c(cu,cu), ylim, col=col)
    graphics::text((cu+xlim[1])/2, mean(ylim), mmstat.math("\\\"&H[1];\\\""), cex=cex, col=col)
  }
  co <- min(crit[2], xlim[2])
  if (crit[2]<=xlim[2]) {
    graphics::lines(c(co,co), ylim, col=col)
    graphics::text((co+xlim[2])/2, mean(ylim), mmstat.math("\\\"&H[1];\\\""), cex=cex, col=col)
  }
  graphics::text((co+cu)/2, mean(ylim), mmstat.math("\\\"&H[0];\\\""), cex=cex, col=col)
#  if (!is.null(text)) { # what is this good for?
#    if (pos==2) graphics::text(xlim[1], mmstat.pos(ylim, -0.25), label, col=col, cex=cex, pos=4)
#    if (pos==4) graphics::text(xlim[2], mmstat.pos(ylim, -0.25), label, col=col, cex=cex, pos=2)
#  }
}
