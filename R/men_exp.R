#' men_exp
#'
#' Visualization of the density and the cumulative distribution function of
#' a exponential distribution.
#'
#' @param rate numeric: rate
#'
#' @return nothing
#' @import shiny
#' @import shinydashboardPlus
#' @importFrom shinydashboard sidebarMenu menuItem dashboardBody
#' @export
#'
#' @examples
#' if (interactive()) men_exp()
#' if (interactive()) men_exp(3)
men_exp <- function(rate = 1) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages()
  if (!all(pkgs)) stop(sprintf("Please call first:\n install.packages(c(%s))", paste0("'", names(pkgs)[!pkgs], "'", collapse=", ")))
  shinyOptions(mmstat=c(rate=rate))
  source(system.file("app", "men_exp", "app.R", package = "HKRbook"), local = TRUE, chdir = TRUE)$value
}
