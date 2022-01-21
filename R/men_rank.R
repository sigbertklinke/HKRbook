#' men_rank
#'
#' Shiny app for the rank correlation coefficients between two ordered variables.
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
#' if (interactive()) men_rank()
#' if (interactive()) {
#'   data("plantTraits", package="cluster")
#'   men_rank(plantTraits)
#' }
men_rank <- function(...) {
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(resetpar(oldpar))
    pkgs <- checkPackages()
    if (!all(pkgs)) stop(sprintf("Package '%s' not installed", names(pkgs)[!pkgs]))
    shinyOptions(mmstat=toRDS(...))
    source(system.file("app", "men_rank", "app.R", package = "HKRbook"), local = TRUE, chdir = TRUE)$value
  }
