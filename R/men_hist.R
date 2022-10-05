#' men_hist
#'
#' Shiny app for visualizing a univariate numeric variable as histögram.
#' If no data are given then the default data from the book will be used.
#' Otherwise the data will be stored as RDS file in a temporary directory.  #'
#' @param ...  one or more data sets
#'
#' @return nothing
#' @import shiny
#' @import shinydashboardPlus
#' @importFrom shinydashboard sidebarMenu menuItem dashboardBody
#' @export
#'
#' @examples
#' if (interactive()) men_hist()
#' if (interactive()) men_hist(iris)
men_hist <- function(...) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages()
  if (!all(pkgs)) stop(sprintf("Please call first:\n install.packages(c(%s))", paste0("'", names(pkgs)[!pkgs], "'", collapse=", ")))
  shinyOptions(mmstat=toRDS(...))
  source(system.file("app", "men_hist", "app.R", package = "HKRbook"), local = TRUE, chdir = TRUE)$value
}
