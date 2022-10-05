#' mmstat.math
#'
#' Returns a math expression based on HTML special characters notation.
#'
#' @param txt character: input text
#'
#' @return expression
#' @export
#'
#' @examples
#' mmstat.math(" &bar(X);~&N(mu[0], sigma^2/n); ")
#' mmstat.math("&H[0];: &mu==mu[0]; vs. &H[1];: &mu!=mu[0]; ")
mmstat.math <- function (txt) {
  dollar <- strsplit(txt, '&', fixed=TRUE)[[1]]
  if (length(dollar)<2) return(txt)
  res <- paste0('expression(paste("', dollar[1], '"')
  for (i in 2:length(dollar)) {
    percent <- strsplit(dollar[i], ';', fixed=TRUE)[[1]]
    lp      <- length(percent)
    if (lp==1) res <- paste0(res, ',"', percent[1], '"')
    else {
      if (lp>2) percent[2] <- paste(percent[2:lp], sep=';')
      res <- paste0(res, ',', percent[1], ',"', percent[2], '"')
    }
  }
  res <- paste0(res, '))')
  eval(parse(text=res))
}
