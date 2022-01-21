#' stopif
#'
#' A equivalent to \code{stopifnot}: if \code{cond} is \code{TRUE} then a error is thrown.
#'
#' @param cond logical: condition to test
#' @param txt character: error message
#'
#' @return nothing
#' @export
#'
#' @examples
#' if (interactive()) stopif(1+1==2, "1+1 can not be 2, this is fake science!")
stopif   <- function (cond, txt) { if (cond) stop(txt) }
