#' men_vis
#'
#' Shiny app for visualizing the univariate numeric  variable, e.g. boxplot, stripchart, histogram,
#' and cumulative distribution function.
#' If no data are given then the default data from the book will be used.
#' Otherwise the data will be stored as RDS file in a temporary directory.
#'
#' @param ...  one or more data sets
#'
#' @return nothing
#' @import shiny
#' @import shinydashboardPlus
#' @importFrom shinydashboard sidebarMenu menuItem dashboardBody
#' @export
#'
#' @examples
#' if (interactive()) men_vis()
#' if (interactive()) men_vis(iris)
men_vis <- function(...) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages()
  if (!all(pkgs)) stop(sprintf("Please call first:\n install.packages(c(%s))", paste0("'", names(pkgs)[!pkgs], "'", collapse=", ")))
  shinyOptions(mmstat=toRDS(...))
  source(system.file("app", "men_vis", "app.R", package = "HKRbook"), local = TRUE, chdir = TRUE)$value
}
