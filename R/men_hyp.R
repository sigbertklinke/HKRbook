#' men_hyp
#'
#' Visualization of the probability mass and the cumulative distribution function of
#' a hypergeometric distribution.
#'
#' @param N integer: the number of black and white balls in the urn
#' @param M integer: the number of white balls in the urn
#' @param n integer: the number of balls drawn from the urn
#'
#' @return nothing
#' @import shiny
#' @import shinydashboardPlus
#' @importFrom shinydashboard sidebarMenu menuItem dashboardBody
#' @export
#'
#' @examples
#' if (interactive()) men_hyp()
#' if (interactive()) men_hyp(50, 25, 10)
men_hyp <- function(N = 60,
                    M = 30,
                    n = 20) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages()
  if (!all(pkgs)) stop(sprintf("Package '%s' not installed", names(pkgs)[!pkgs]))
  shinyOptions(mmstat=list(distrd='HYPER', N=N, M=M, n=n))
  source(system.file("app", "men_ddist", "app.R", package = "HKRbook"), local = TRUE, chdir = TRUE)$value
}
