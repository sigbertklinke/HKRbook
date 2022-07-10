#' mmstat.getDatasets
#'
#' Reads data set(s) into the mmstat object.
#'
#' @param ... character: file name(s) of RDS data file(s)
#'
#' @return the names of the data set(s)
#' @export
#'
#' @examples
#' # not used, deprecated??
mmstat.getDatasets <- function (...) {
  is.binary <- function(v) {
    x <- unique(v)
    length(x) - sum(is.na(x)) == 2L
  }

  files          <- list(...)
  dataset        <- vector("list", length(files))
  names(dataset) <- files
  mmstat$dataset <- dataset
  for (i in seq(files)) {
    file    <- files[[i]]
    data    <- readRDS(paste0(file, ".rds"))
    if (inherits(data, "data.frame")) {
      allvars <- names(data)
      mmstat$dataset[[file]] <<-list(data=data, allvars=allvars,
                                     numvars=allvars[sapply(data, is.numeric)],
                                     ordvars=allvars[sapply(data, is.ordered)],
                                     facvars=allvars[sapply(data, is.factor)],
                                     binvars=allvars[sapply(data, is.binary)])
    }
    if (inherits(data, "ts")) {
      allvars <- colnames(data)
      mmstat$dataset[[file]] <<-list(data=data, allvars=allvars, numvars=allvars)
    }
  }
  names(dataset)[1]
}
