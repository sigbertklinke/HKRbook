#' mmstat.rds
#'
#' Returns the full file names of all or specific data set that come with the package.
#'
#' @param ... names of data sets
#'
#' @return full file names
#' @export
#'
#' @examples
#' mmstat.rds()                            # return all RDS file that come with the package
#' mmstat.rds("HAIR.EYE.COLOR", "TITANIC") # location of specific data sets
mmstat.rds <- function(...) {
  args <- unlist(list(...))
  path <- system.file("rds", package="HKRbook")
  if (length(args)) {
    files <- paste0(path, "/", args, ".rds")
    files <- files[file.exists(files)]
  } else {
    files <- list.files(path, pattern="*.rds$", full.names = TRUE)
  }
  files
}
