#' men_poi
#
#' Visualization of the probability mass and the cumulative distribution function of
#' a Poisson distribution.
#'
#' @param lambda numeric: (non-negative) mean
#'
#' @return nothing
#' @import shiny
#' @import shinydashboardPlus
#' @importFrom shinydashboard sidebarMenu menuItem dashboardBody
#' @export
#'
#' @examples
#' if (interactive()) men_poi()
#' if (interactive()) men_poi(3)
men_poi <- function(lambda = 5) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages()
  if (!all(pkgs)) stop(sprintf("Package '%s' not installed", names(pkgs)[!pkgs]))
  shinyOptions(mmstat=list(distrd='POIS', lambda=lambda))
  source(system.file("app", "men_ddist", "app.R", package = "HKRbook"), local = TRUE, chdir = TRUE)$value
}
