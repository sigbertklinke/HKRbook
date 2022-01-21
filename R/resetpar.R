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
#' # no examples
resetpar <- function(oldpar) {
  suppressWarnings({
    curpar <- graphics::par(no.readonly = TRUE)
    if (!identical(oldpar, curpar)) graphics::par(oldpar)
  })
}
