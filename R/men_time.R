#' men_time
#'
#' Shiny app for classical time series analysis
#' If no data are given then the default data from the book will be used.
#' Otherwise the data will be stored as RDS file in a temporary directory.
#'
#' @param ... one or more time series
#'
#' @return nothing
#' @import shiny
#' @import shinydashboardPlus
#' @importFrom shinydashboard sidebarMenu menuItem dashboardBody
#' @export
#'
#' @examples
#' if (interactive()) men_time()
#' if (interactive()) men_time(co2)
men_time <- function(...) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages()
  if (!all(pkgs)) stop(sprintf("Please call first:\n install.packages(c(%s))", paste0("'", names(pkgs)[!pkgs], "'", collapse=", ")))
  shinyOptions(mmstat=toRDS(...))
  source(system.file("app", "men_time", "app.R", package = "HKRbook"), local = TRUE, chdir = TRUE)$value
}
