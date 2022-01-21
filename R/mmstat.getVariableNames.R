#' mmstat.getVariableNames
#'
#' Returns all variable names of data set stored in the internal environment.
#'
#' @param name character or numeric: name or index of data set
#'
#' @return vector of names
#' @export
#'
#' @examples
#' # Delete all stored data sets
#' mmstat.set(datasets=NULL)
#' # Load CAR data set into mmstat
#' mmstat.getDataNames(mmstat.rds("CARS"))
#' # Extract names of all variables
#' mmstat.getVariableNames(1)
mmstat.getVariableNames <- function(name) {
  gettext(mmstat$dataset[[name]]$allvars, "name")
}
