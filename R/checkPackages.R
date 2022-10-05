#' checkPackages
#'
#' Checks if a package is installed without loading it. Returns a logical vector with \code{TRUE} or \code{FALSE} for each package checked.
#'
#' @param ... character: name(s) of package
#' @param add character: names of default packages to check (default: \code{c("highlight", "formatR", "shiny", "shinydashboard", "shinydashboardPlus", "DT")})
#'
#' @return \code{TRUE} if successful otherweise an error will be thrown
#' @export
#'
#' @examples
#' checkPackages("graphics", add=NULL)          # checks if 'graphics' is installed
#' if (interactive()) checkPackages("graphics") # checks if 'graphics', 'shiny', ... are installed
checkPackages <- function(..., add=c("highlight", "formatR", "shiny", "shinydashboard", "shinydashboardPlus", "DT")) {
  # install=FALSE) {
  pkgs <- as.character(unlist(list(...)))
  if (length(add)) pkgs <- c(pkgs, as.character(add))
  ret <- structure(rep(NA, length(pkgs)), names=pkgs)
  for (pkg in pkgs) {
    ret[pkg] <- nzchar(system.file(package=pkg))
  }
  if (any(!ret)) {
#    if (is.na(install)) {
      cat("Missing package(s): ", paste0(names(ret)[!ret], collpase=", "))
      stop(sprintf("Please call first:\n install.packages(c(%s))", paste0("'", names(ret)[!ret], "'", collapse=", ")))
#    }
#    if (install) {
#      cat("Missing package(s): ", paste0(names(pkg)[!pkg], collpase=", "))
#      yn <- askYesNo("Install package(s)?", default=FALSE)
#      if (isTRUE(yn)) {
#        install.packages(names(pkg)[!pkg])
#        ret <- checkPackages(names(pkg), install=NA)
#      } else stop("Packages are required, execution stopped")
#    }
  }
  ret
}
