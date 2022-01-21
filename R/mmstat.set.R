#' @rdname mmstat.set
#' @title mmstat.set
#' @description  `mmstat.set` sets one (or more) parameter to the internal environment. `mmstat.get` return one or more parameters from the internal environment.
#'
#' @param ... named parameters with values or names
#'
#' @return nothing
#' @export
#'
#' @examples
#' mmstat.set(debug=0)
#' mmstat.get("debug")
#' mmstat.get("debug", "shiny") # returns a list
mmstat.set <- function (...) {
  #  mmstat$UI <- NULL
  args  <- list(...)
  if (length(args)) {
    nargs <- names(args)
    if (is.null(nargs)) nargs <- rep("", length(args))
    tf <- nargs %in% mmstat$readonly
    for (i in 1:length(args)) {
      if (tf[i]) {
        warning(sprintf("'%s' is readonly, new value ignored", nargs[i]))
      } else {
        mmstat[[nargs[i]]] <- args[[i]]
      }
    }
  }
}

#' @rdname mmstat.set
#' @export
mmstat.get <- function (...) {
  name <- as.character(unlist(list(...)))
  if (length(name)==1) return(mmstat[[name]])
  ret <- list()
  for (ni in name) ret[[ni]] <- mmstat[[ni]]
  ret
}
