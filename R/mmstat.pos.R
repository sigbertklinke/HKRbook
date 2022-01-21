#' mmstat.pos
#'
#' Returns a linear interpolation based on \code{minmax}.
#'
#' @param minmax numeric(2): range to interpolate between
#' @param pos numeric: proportion(s) to interpolate, usually between zero and one
#'
#' @return interpolated values
#' @export
#'
#' @examples
#' mmstat.pos(c(0,360), 0.5)
mmstat.pos <- function (minmax, pos) {
  min(minmax)+diff(minmax)*pos
}
