#' gettext
#'
#' Returns a translation from loaded PO-file. If the message is not found in the PO-file then original text will be returned.
#'
#' @param msg character: message(s) to translate
#' @param utype character: how to return the translated message as vector or named list
#'
#' @return translated messages
#' @export
#'
#' @examples
#' msgs <- c("two.sided", "less", "greater")
#' gettext(msgs)
#' # for use in Shiny "choices"
#' gettext(msgs, "name")
#' gettext(msgs, "numeric")
gettext <- function (msg, utype="vector") {
  type <- pmatch(utype, c("name", "numeric"))
  if (is.na(type)) {
    #print(paste('Msg:', msg))
    ret <- paste0(ifelse(mmstat$debug>2, '?', ''), msg)
    #print(paste('Ret:', ret))
    pos <- match(msg, mmstat$messages$id)
    ind <- (1:length(pos))[!is.na(pos)]
    ret[ind] <- mmstat$messages$str[pos[ind]]
    #print(paste('Out:', ret))
    #for (i in 1:length(pos)) {
    #  if (!is.na(pos[i])) ret[i] <- mmstat$messages$str[pos[i]]
    #}
  } else if (type==1) {
    ret        <- as.list(msg)
    names(ret) <- gettext(msg)
  } else if (type==2) {
    ret        <- as.list(seq(msg))
    names(ret) <- gettext(msg)
  }
  return (ret)
}
