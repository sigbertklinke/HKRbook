#' mmstat.getDataNames
#'
#' Returns the names of data sets and stores them in the internal environment.
#' The name of the data set is base name without extension.
#'
#' @param ... character: names of the data sets.
#'
#' @return the names of the data sets
#' @export
#'
#' @examples
#' files <- mmstat.rds("HAIR.EYE.COLOR", "TITANIC")
#' mmstat.getDataNames(files)
mmstat.getDataNames <- function (...) {
  is.binary <- function(v) {
    x <- unique(v)
    length(x) - sum(is.na(x)) == 2L
  }

  #browser()
  files <- unlist(list(...))
  if (length(files)==0) return(names(mmstat$dataset)[1])

  name_dataset   <- sub(".rds$", "", basename(files))
  mmstat$dataset <- list()
  for (i in seq(files)) {
    file    <- files[i]
    if (is.null(mmstat$dataset$file)) {
      data    <- readRDS(file)
#      data    <- readRDS(paste0("inst/rds/", file, ".rds"))
      if (inherits(data, "data.frame")) {
        allvars <- names(data)
        mmstat$dataset[[name_dataset[i]]] <- list(data=data, allvars=allvars,
                                                 numvars=allvars[sapply(data, is.numeric)],
                                      ordvars=allvars[sapply(data, is.ordered)],
                                      facvars=allvars[sapply(data, is.factor)],
                                      binvars=allvars[sapply(data, is.binary)])
      }
      if (inherits(data, "ts")) {
        allvars <- colnames(data)
        mmstat$dataset[[name_dataset[i]]] <-list(data=data, allvars=allvars, numvars=allvars)
      }
    }
  }
  gettext(names(mmstat$dataset), "name")
}
