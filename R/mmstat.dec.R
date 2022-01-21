#' mmstat.dec
#'
#' Computes the number of the significant digits based on the smallest non-zero difference of the sorted data.
#'
#' @param x numeric: data vector
#' @param ord index: subset of the ordered data (default: \code{NULL})
#'
#' @return The number of significant digits and (the subset of) the order of the data.
#' @export
#'
#' @examples
#' x <- rnorm(20)
#' d <- mmstat.dec(x)
#' # create strings so that they are unique (if they were)
#' sprintf("%.*f", d$dec, x)
mmstat.dec <- function(x, ord=NULL) {
  order <- order(x)
  df    <- diff(x[order])
  dec   <- Inf
  if (sum(df>0)) { # take only positive differences
    df  <- min(df[df>0])
    dec <- max(0, ceiling(-log10(df)))
  }
  list(decimal = dec, order = if (is.null(ord)) order else order(order[ord]))
}
