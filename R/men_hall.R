#' men_hall
#'
#' Shiny app for the \href{https://en.wikipedia.org/wiki/Monty_Hall_problem}{Monty Hall problem}:
#'
#' Suppose you're on a game show, and you're given the choice of three doors: Behind one door is a car; behind the others, goats. You pick a door, say No. 1, and the host, who knows what's behind the doors, opens another door, say No. 3, which has a goat. He then says to you, "Do you want to pick door No. 2?" Is it to your advantage to switch your choice?
#'
#' @param pointdoor integer: to which door to point (default: \code{1})
#' @param afteropen integer: play strategy 1=keep door, 2=change door (default: \code{1})
#'
#' @return nothing
#' @import shiny
#' @import shinydashboardPlus
#' @importFrom shinydashboard sidebarMenu menuItem dashboardBody
#' @export
#'
#' @examples
#' if (interactive()) men_hall()
#' if (interactive()) men_hall(4, 2)
men_hall <- function(pointdoor=1, afteropen=1) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages()
  if (!all(pkgs)) stop(sprintf("Package '%s' not installed", names(pkgs)[!pkgs]))
  shinyOptions(mmstat=c(pointdoor=pointdoor, afteropen=afteropen))
  source(system.file("app", "men_hall", "app.R", package = "HKRbook"), local = TRUE, chdir = TRUE)$value
}
