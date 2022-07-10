#' mmstat.getVar
#'
#' Returns the from a data set a variable from the given type.
#'
#' @param dataname integer: number of data set
#' @param varname integer: number of variable
#' @param vartype character: variable type, one of `numeric`, `binary`, `ordered`, or `factor`
#' @param na.action function: indicate what should happen when the data contain NAs (default: [stats::na.omit])
#'
#' @return a variable of the given type
#' @export
#'
#' @examples
#' # make sure that no other data sets are loaded
#' mmstat.set(datasets=NULL)
#' mmstat.getDataNames(mmstat.rds("CARS"))
#' # summary of first numeric variable in first data set in mmstat
#' str(mmstat.getVar(1, 1, 'numeric'))
#' # summary of first factor variable in first data set in mmstat
#' str(mmstat.getVar(1, 1, 'factor'))
mmstat.getVar <- function (dataname=NULL, varname=NULL, vartype=NULL, na.action=stats::na.omit) {
  match.name <- function(needle, haystack) {
    if (is.null(needle)) return(NA);
    return(match(needle, haystack))
  }

  # which variable type do we need
  if (is.null(vartype)) vartype <- mmstat$vartype
  deftype <- c('numeric', 'binary', 'ordered', 'factor')
  type    <- pmatch(vartype, deftype)
  stopif (is.na(type), paste0('mmstat.getVar: Unknown variable type: ', vartype))
  # get dataset
  pos <- match.name(dataname, names(mmstat$dataset))
  if (is.na(pos)) pos <- 1  # assume that all datasets contain
  dataname <- names(mmstat$dataset)[pos]
  if (is.null(mmstat$dataset[[dataname]])) stop(sprintf(gettext("Data file '%s' is missing"), dataname))
  # get variable
  type <- c("numvars", "binvars", "ordvars", "facvars")[type]
  pos  <- match.name(varname, mmstat$dataset[[dataname]][[type]])
  if (is.na(pos)) pos <- 1  # assume that every data set contains a variable of correct type
  varname <- mmstat$dataset[[dataname]][[type]][pos]
  # get var values
  dataset <- mmstat$dataset[[dataname]]$data
  if (inherits(dataset, "data.frame")) values  <- na.action(dataset[[varname]])
  if (inherits(dataset, "ts")) values  <- na.action(dataset[,varname])
  var     <- list(values   = values,
                  n        = length(values),
                  name     = varname,
                  sub      = paste(gettext("Dataset:"), gettext(dataname)),
                  xlab     = gettext(varname),
                  data     = dataname,
                  dataname = gettext(dataname)
  )
  var <- mmstat.attrVar(var, vartype)
  var
}
