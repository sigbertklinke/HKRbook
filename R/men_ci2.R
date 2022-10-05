#' men_ci2
#'
#' Shiny app for a confidence interval for the difference of two means.
#' The data used is considered as a population from which random samples can be drawn.
#' If no data are given then the default data from the book will be used.
#' Otherwise the data will be stored as RDS file in a temporary directory.
#'
#' @param ... one or more data sets
#'
#' @return nothing
#' @import shiny
#' @import shinydashboardPlus
#' @importFrom shinydashboard sidebarMenu menuItem dashboardBody
#' @importFrom shinyWidgets actionGroupButtons
#' @export
#'
#' @examples
#' if (interactive()) men_ci2()
#' if (interactive()) men_ci2(CO2)
men_ci2 <- function(...) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages()
  if (!all(pkgs)) stop(sprintf("Please call first:\n install.packages(c(%s))", paste0("'", names(pkgs)[!pkgs], "'", collapse=", ")))
  shinyOptions(mmstat=toRDS(...))
  source(system.file("app", "men_ci2", "app.R", package = "HKRbook"), local = TRUE, chdir = TRUE)$value
}
