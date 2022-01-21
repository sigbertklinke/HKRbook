#' mmstat.log
#'
#' Writes a message into the internal log.
#'
#' @param txt character: message to write
#'
#' @return nothing
#' @export
#'
#' @examples
#' mmstat.log("Test")
mmstat.log  <- function (txt) {
  if (mmstat$debug>1) mmstat$log <- cbind(sprintf("%s (Info): %s", date(), txt), mmstat$log)
}
