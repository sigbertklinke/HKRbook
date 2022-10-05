#' Correlation
#'
#' Shiny app for the correlation coefficients between two numeric variables.
#' If no data are given then the default data from the book will be used.
#' Otherwise the data will be stored as RDS file in a temporary directory.
#'
#' @param ... one or more data sets
#'
#' @return nothing
#' @import shiny
#' @import shinydashboardPlus
#' @importFrom shinydashboard sidebarMenu menuItem dashboardBody
#' @importFrom scatterplot3d scatterplot3d
#'
#' @export
#'
#' @examples
#' if (interactive()) men_corr()
#' if (interactive()) men_corr(iris)
men_corr <- function (...) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages("scatterplot3d")
  if (!all(pkgs)) stop(sprintf("Please call first:\n install.packages(c(%s))", paste0("'", names(pkgs)[!pkgs], "'", collapse=", ")))
  shinyOptions(mmstat=toRDS(...))
  source(system.file("app", "men_corr", "app.R", package = "HKRbook"), local = TRUE, chdir = TRUE)$value
}
