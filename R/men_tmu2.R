#' men_tmu2
#'
#' Shiny app for a test on difference of two true means.
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
#' @export
#'
#' @examples
#' if (interactive()) men_tmu2()
#' if (interactive()) men_tmu2(CO2)
men_tmu2 <- function(...) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages()
  if (!all(pkgs)) stop(sprintf("Please call first:\n install.packages(c(%s))", paste0("'", names(pkgs)[!pkgs], "'", collapse=", ")))
  shinyOptions(mmstat=toRDS(...))
  source(system.file("app", "men_tmu2", "app.R", package = "HKRbook"), local = TRUE, chdir = TRUE)$value
}
