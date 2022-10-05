#' mmstat.sliderInput
#'
#' A modified \code{sliderInput} for \code{mmstat} which supports user defined tick marks.
#'
#' @param ... parameters for [shiny::sliderInput]
#'
#' @return the HTML output
#' @export
#'
#' @examples
#' ticks <- c(80, 85, 90, 95, 98, 99, 99.5, 99.9)
#' mmstat.sliderInput("id", "label", min=1, max=length(ticks), value=3, step=1, ticks=ticks)
mmstat.sliderInput <- function (...) {
  iapply <- function (l) {
    ret <- l
    if (is.list(l)) {
      if (!is.null(ret$name) && (ret$name=='input')) {
        ret$attribs[['data-values']] <- ticks
      } else {
        for (i in seq(ret)) ret[[i]] <- iapply(ret[[i]])
      }
    }
    ret
  }
  #
  args <- list(...)
  if ((utils::compareVersion(mmstat$shiny, "0.11")>=0) && !is.null(args$ticks) && length(args$ticks)) {
    ticks      <- paste0(args$ticks, collapse=',')
    args$ticks <- TRUE
    suppressWarnings(html       <- do.call('sliderInput', args))
    html       <- iapply(html)
  } else {
    html      <- do.call('sliderInput', args)
  }
  html
}
