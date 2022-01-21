#' mmstat.ui.update
#'
#' Call for a update of an underlying Shiny UI element (\code{selectInput}, ...).
#'
#' @param inputId character: the input slot called
#' @param ... further parameters given to the call
#'
#' @return whatever the update to the underlying Shiny UI element returns
#' @export
#'
#' @examples
#' mmstat.ui.elem(inputId="alpha", type="significance")
#' mmstat.ui.call("alpha")
mmstat.ui.update <- function(inputId, ...) {
  elem <- mmstat$UI[[inputId]]
  what <- elem$update
  if (!is.null(what)) {
    args <- list(...)
    for (name in names(elem)) { if (is.null(args[[name]])) args[[name]] <- elem[[name]] }
    args$call    <- NULL
    args$update  <- NULL
    args$type    <- NULL
    args$vartype <- NULL
    if ((what=='updateSelectInput') || (what=='updateCheckboxGroupInput')) args$value <- NULL
    do.call(what, args)
  }
}
