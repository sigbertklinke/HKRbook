#' ucfirst
#'
#' Uppercases the first character in \code{txt}.
#'
#' @param txt character:
#'
#' @return character
#' @export
#'
#' @examples
#' ucfirst("hello world")
ucfirst <- function (txt) { return (paste0(toupper(substr(txt, 1, 1)), substring(txt, 2))); }
