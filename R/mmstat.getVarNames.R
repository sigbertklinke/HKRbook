#' mmstat.getVarNames
#'
#' Returns the variable names of a specific type from a mmstat data set.
#'
#' @param dataname character: name of data set
#' @param vartype character: type of variable, either \code{numeric},  \code{ordered},  \code{factor}, or \code{binary}
#' @param which integer: index number
#'
#' @return a vector or element of variable names which have the type \code{vartype}
#' @export
#'
#' @examples
#' # Load CAR data set into mmstat
#' mmstat.getDataNames(mmstat.rds("CARS"))
#' # Extract names of numeric variables
#' mmstat.getVarNames(1, "numeric")
mmstat.getVarNames <- function(dataname, vartype, which=NULL) {
  vars <- NULL
  if (vartype=='numeric') vars <- 'numvars'
  if (vartype=='ordered') vars <- 'ordvars'
  if (vartype=='factor')  vars <- 'facvars'
  if (vartype=='binary')  vars <- 'binvars'
  stopif(is.null(vars), 'mmstat.getVarNames: Variable type "%s" unknown')
  if (is.null(which)) return(gettext(mmstat$dataset[[dataname]][[vars]], "name"))
#  if (length(which==1)) return(gettext(mmstat$dataset[[dataname]][[vars]][which]))
  if (length(which==1)) return(mmstat$dataset[[dataname]][[vars]][which])
  return(gettext(mmstat$dataset[[dataname]][[vars]][which], "name"))
}
