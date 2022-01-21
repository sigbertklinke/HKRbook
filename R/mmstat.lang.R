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
#mmstat.lang <- function(deflang=NULL) {
# pof   <- list.files(path=system.file("po", package="HKRbook"), pattern="*.po$")
# pof   <- list.files(path="inst/po", pattern="*.po$", full.names = TRUE)
#  if (is.null(deflang)) {
#    lang <- sapply(strsplit(pof, '.', fixed=T), function(elem) { elem[1] })
#    pat  <- paste0('_', lang, '$')
#    path <- getwd()
#    path <- strsplit(path, '/', fixed=T)[[1]]
#    pos  <- -1
#    lind <- -1
#    for (i in seq(pat)) {
#      p <- grep(pat[i], path)
#      if (length(p)) {
#        p <- max(p)
#        if (p>pos) { pos <- p; lind <- i; }
#      }
#    }
#  } else {
#    lind <- match (paste0(deflang, '.po'), pof)
#    if (is.na(lind)) lind <- (-1)
#  }
#  msgfn <- ifelse(lind>0, pof[lind], "default.po")
#  msgfn  <- if (is.null(pof)) "inst/po/default.po" else pof
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
