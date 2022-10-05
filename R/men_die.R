#' men_die
#'
#' Shiny app for detecting if a die is fair or unfair.
#'
#' @return nothing
#' @import shiny
#' @import shinydashboardPlus
#' @importFrom shinydashboard sidebarMenu menuItem dashboardBody
#' @export
#'
#' @examples
#' if (interactive()) men_die()
men_die <- function() {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages()
  if (!all(pkgs)) stop(sprintf("Please call first:\n install.packages(c(%s))", paste0("'", names(pkgs)[!pkgs], "'", collapse=", ")))
  source(system.file("app", "men_die", "app.R", package = "HKRbook"), local = TRUE, chdir = TRUE)$value
}
