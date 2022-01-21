#' mmstat.warn
#'
#' Writes a warning text into the log object in the internal mmstat object.
#'
#' @param cond logical: condition to test
#' @param txt character: text to write if \code{cond} is true
#'
#' @return nothing
#' @export
#'
#' @examples
#' mmstat.warn(TRUE, "just a true seen")
mmstat.warn <-  function (cond, txt) {
    if ((mmstat$debug > 0) && cond)
      mmstat$log <- cbind(sprintf("%s (Warn): %s", date(), txt), mmstat$log)
  }
