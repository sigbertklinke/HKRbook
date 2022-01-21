#' men_norm
#'
#' Visualization of the density and the cumulative distribution function of
#' a normal distribution.
#'
#' @param mean numeric: mean
#' @param sd2 numeric: variance
#'
#' @return nothing
#' @import shiny
#' @import shinydashboardPlus
#' @export
#'
#' @examples
#' if (interactive()) men_norm()
#' if (interactive()) men_norm(1, 0.5)
men_norm <- function(mean=0, sd2=1) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages()
  if (!all(pkgs)) stop(sprintf("Package '%s' not installed", names(pkgs)[!pkgs]))
  shinyOptions(mmstat=c(mean=mean, sd2=sd2))
  source(system.file("app", "men_norm", "app.R", package = "HKRbook"), local = TRUE, chdir = TRUE)$value
}
