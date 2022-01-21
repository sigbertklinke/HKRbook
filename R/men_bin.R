#' men_bin
#'
#' Visualization of the probability mass and the cumulative distribution function of
#' a binomial distribution.
#'
#' @param size integer: number of trials (zero or more)
#' @param prob numeric: probability of success on each trial
#'
#' @return nothing
#' @import shiny
#' @import shinydashboardPlus
#' @importFrom shinydashboard sidebarMenu menuItem dashboardBody
#' @export
#'
#' @examples
#' if (interactive()) men_bin()
#' if (interactive()) men_bin(20, 0.25)
men_bin <- function(size = 10, prob = 0.5) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages()
  if (!all(pkgs)) stop(sprintf("Package '%s' not installed", names(pkgs)[!pkgs]))
  shinyOptions(mmstat=list(distrd='BINOM', size=size, prob=prob))
  source(system.file("app", "men_ddist", "app.R", package = "HKRbook"), local = TRUE, chdir = TRUE)$value
}
