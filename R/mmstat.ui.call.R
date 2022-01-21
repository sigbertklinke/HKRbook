#' mmstat.ui.call
#'
#' Calls the underlying Shiny UI element (\code{selectInput}, ...).
#'
#' @param inputId character: the input slot called
#' @param ... further parameters given to the call
#'
#' @return whatever the call to the underlying Shiny UI element returns
#' @export
#'
#' @examples
#' mmstat.ui.elem(inputId="alpha", type="significance")
#' mmstat.ui.call("alpha")
mmstat.ui.call <- function(inputId, ...) {
  elem <- mmstat$UI[[inputId]]
  what <- elem$call
  args <- list(...)
  for (name in names(elem)) { if (is.null(args[[name]])) args[[name]] <- elem[[name]] }
  args$call    <- NULL
  args$update  <- NULL
  args$type    <- NULL
  args$vartype <- NULL
  if ((what=='selectInput') || (what=='checkboxGroupInput')) args$value <- NULL
  if (what=='actionGroupButtons') args$inputId <- NULL
  do.call(what, args)
}
