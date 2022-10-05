#' mmstat.lang
#'
#' Loads a \href{https://www.gnu.org/software/gettext/manual/html_node/PO-Files.html}{PO file} for a translation into the internal environment.
#'
#' @param pof character: file name
#'
#' @return nothing
#' @export
#'
#' @examples
#' mmstat.lang()
mmstat.lang <- function(pof=NULL) {
  msgfn  <- if (is.null(pof)) system.file("po", "default.po", package="HKRbook") else pof
  mmstat.warn (!file.exists(msgfn), sprintf("File '%s' does not exist", msgfn))
  msg    <- paste(readLines(msgfn), collapse=" ")
  msgid  <- regmatches(msg, gregexpr('msgid\\s*".*?"', msg))
  tmp    <- strsplit(msgid[[1]], '"')
  msgid  <- sapply(tmp, function (vec) { paste0(vec[2:length(vec)]) } )
  msgstr <- regmatches(msg, gregexpr('msgstr\\s*".*?"', msg))
  tmp    <- strsplit(msgstr[[1]], '"')
  msgstr <- sapply(tmp, function (vec) { paste0(vec[2:length(vec)]) } )
  mmstat$messages <- list(id=msgid, str=msgstr)
}
