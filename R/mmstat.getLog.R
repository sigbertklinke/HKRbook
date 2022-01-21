#' mmstat.getLog
#'
#' Returns the internal log message as HTML. In a Shiny app the log message are updated every 100 milliseconds
#'
#' @param session session object
#'
#' @return HTML code
#' @export
#'
#' @examples
#' # will work only in Shiny app:
#' # mmstat.getLog(session)
mmstat.getLog <- function (session) {
  if (!mmstat$debug) return ("")
  invalidateLater(100, session)
  paste0('<hr>', paste(mmstat$log, collapse="<br>"))
}
