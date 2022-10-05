#' resetpar
#'
#' Resets the par if necessary.
#'
#' @param oldpar graphical parameters
#'
#' @return nothing
#' @export
#'
#' @examples
#' par("mar")
#' oldpar <- par(no.readonly = TRUE)
#' par(mar = c(0,0,0,0))
#' par("mar")
#' resetpar(oldpar)
#' par("mar")
resetpar <- function(oldpar) {
  suppressWarnings({
    curpar <- graphics::par(no.readonly = TRUE)
    if (!identical(oldpar, curpar)) graphics::par(oldpar)
  })
}
