#' men_cilen
#'
#' Shiny app for a length of a confidence interval for the mean.
#'
#' @return nothing
#' @import shiny
#' @import shinydashboardPlus
#' @importFrom shinydashboard sidebarMenu menuItem dashboardBody
#' @export
#'
#' @examples
#' if (interactive()) men_cilen()
men_cilen <- function() {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages()
  if (!all(pkgs)) stop(sprintf("Please call first:\n install.packages(c(%s))", paste0("'", names(pkgs)[!pkgs], "'", collapse=", ")))
  source(system.file("app", "men_cilen", "app.R", package = "HKRbook"), local = TRUE, chdir = TRUE)$value
}
