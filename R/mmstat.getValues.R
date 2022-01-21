#' @rdname mmstat.getValues
#' @title mmstat.getValues
#' @description `mmstat.getValues` returns a list with named elements. If the parameter is \code{NULL} then a default value stored
#' \code{local} will be used.
#' `mmstat.getValue` returns a value. If the parameter is \code{NULL} or \code{NA} then \code{def} will be returned
#' @param local list: default values for the named parameter
#' @param val value for a parameter
#' @param def default value for a parameter
#' @param ... list of named parameters
#'
#' @return a list of requested parameters
#' @export
#'
#' @examples
#' def <- list(a=3)
#' mmstat.getValues(def, b=3, a=NULL)
#' mmstat.getValue(NA, 5)
#' mmstat.getValue(NULL, 5)
#' mmstat.getValue(3, 5)
mmstat.getValues <- function (local, ...) {
  ret  <- list(...)
  nret <- names(ret)
  if (!is.null(local)) nret <- unique(nret, names(local))
  for (name in nret) {
    if (is.null(ret[[name]]) || (length(ret[[name]])==0)) {
      if (is.null(local[[name]])) {
        stopif (is.null(mmstat$UI[[name]]$value) && is.null(mmstat$UI[[name]]$selected),
                paste0('mmstat.getValues: no default value(s) for "', name, '"'))
        if (is.null(mmstat$UI[[name]]$value)) {
          ret[[name]] <- mmstat$UI[[name]]$selected
        } else {
          ret[[name]] <- mmstat$UI[[name]]$value
        }
      } else {
        ret[[name]] <- local[[name]]
      }
    }
    if (!is.null(mmstat$UI[[name]]$call)) {
      if (mmstat$UI[[name]]$call=='mmstat.sliderInput') {
        if ((utils::compareVersion(mmstat$shiny, '0.11')>=0) && !is.null(mmstat$UI[[name]]$ticks)) {
          ret[[name]] <- ret[[name]]+1
        }
      }
    }
  }
  ret
}

#' @rdname mmstat.getValues
#' @export
mmstat.getValue <- function (val, def) {
  if (length(val) && !is.na(val)) return(val)
  return(def)
}
