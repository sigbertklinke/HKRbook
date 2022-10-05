#' mmstat.attrVar
#'
#' Returns the parameters for a variable. If `type="numeric"` then descriptive measures will be returned.
#' Otherwise absolute and relative frequencies will be returned. For using a subset of observation set \code{index}.
#'
#' @param var vector: values of a mmstat variable
#' @param type character: type of values, allowed are `numvars`, `binvars`, `ordvars` or `facvars`
#' @param index integer: observation numbers to use for computation, default is to use all observations
#'
#' @return descriptive measures
#' @export
#'
#' @examples
#' # make sure that no other data sets are loaded
#' mmstat.set(datasets=NULL)
#' mmstat.getDataNames(mmstat.rds("CARS"))
#' # summary of first numeric variable in first data set in mmstat
#' var <- mmstat.getVar(1, 1, 'numeric')
#' mmstat.attrVar(var, "numeric")
#' # summary of first factor variable in first data set in mmstat
#' var <- mmstat.getVar(1, 1, 'factor')
#' mmstat.attrVar(var, 'factor')
mmstat.attrVar <- function(var, type, index=NULL) {
  ret <- var
  if (is.null(index)) index=1:length(var$values) else {
    ret$values <- var$values[index]
    ret$n      <- length(index)
  }
  if (type=='numeric') {
    if (is.null(var$jitter)) ret$jitter <- stats::runif(length(index)) else  ret$jitter <- var$jitter[index]
    ret$mean   <- base::mean(var$values[index], na.rm=TRUE)
    ret$median <- stats::median(var$values[index], na.rm=TRUE)
    ret$sd     <- stats::sd(var$values[index], na.rm=TRUE)
    ret$var    <- stats::var(var$values[index], na.rm=TRUE)
    ret$range  <- base::range(var$values[index], na.rm=TRUE)
    ret$iqr    <- stats::IQR(var$values[index], na.rm=TRUE)
    ret$quart  <- stats::quantile(var$values[index], c(0.25, 0.75), na.rm=TRUE)
    ret$min    <- base::min(var$values[index], na.rm=TRUE)
    ret$max    <- base::max(var$values[index], na.rm=TRUE)
  }
  if ((type=='binary') || (type=='ordered') || (type=='factor')) {
    ret$tab    <- table(var$values[index], useNA="ifany")
    ret$prop   <- prop.table(ret$tab)
  }
  ret
}
