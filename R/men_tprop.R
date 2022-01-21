#' men_tprop
#'
#' Shiny app for test on the proportion.
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
#' if (interactive()) men_tprop()
#' if (interactive()) men_tprop(Titanic)
men_tprop <- function(...) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages()
  if (!all(pkgs)) stop(sprintf("Package '%s' not installed", names(pkgs)[!pkgs]))
  shinyOptions(mmstat=toRDS(...))
  source(system.file("app", "men_tprop", "app.R", package = "HKRbook"), local = TRUE, chdir = TRUE)$value
}
