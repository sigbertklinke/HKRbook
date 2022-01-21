#' is.ASCII
#'
#' Checks if \code{txt} contains only ASCII characters.
#'
#' @param txt character: text to check
#'
#' @return logical
#' @export
#'
#' @examples
#' is.ASCII("Congratulations")
#' is.ASCII("Herzlichen Glückwunsch")
is.ASCII <- function (txt) { all(charToRaw(txt) <= as.raw(127)) }
