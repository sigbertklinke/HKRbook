#' toRDS
#'
#' Saves one or more data sets in RDS format to a temporary directory (\code{tmpdir()}).
#' Data sets must have the class \code{ts} or something that can be converted
#' to a data frame, e.g. \code{matrix}, \code{table}, etc.
#'
#' @param ... data sets to save
#'
#' @return returns the name of the created files
#' @export
#'
#' @examples
#' toRDS(Titanic) # saves to tempdir/Titanic.rds
toRDS <- function(...) {
  getNames <- function(...) {
    l <- substitute(list(...))
    r <- names(l)
    if (is.null(r)) r <- rep("", length(l))
    if (length(r)>1) {
      for (i in 2:length(r)) {
        if (r[i]=="") r[i] <- as.character(l[[i]])
      }
    }
    r[-1]
  }
  #
  args   <- list(...)
  fnames <- getNames(...)
  stopifnot(length(args)==length(fnames))
  files <- NULL
  if (length(args)) {
    files  <- paste0(tempdir(), "/", fnames, ".rds")
    for (i in 1:length(files)) {
      if ("table" %in% class(args[[i]]))  args[[i]]  <- table2dataframe(args[[i]])
      if (!("ts" %in% class(args))) args[[i]] <- as.data.frame(args[[i]])
      #browser()
      saveRDS(args[[i]], files[i])
    }
  }
  files
}
