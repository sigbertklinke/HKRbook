#' mmstat.getLog
#'
#' Returns the internal log message as HTML. In a Shiny app the log message are updated every 100 milliseconds
#'
#' @param session session object
#'
#' @return HTML code
#' @export
#'
#' @examples
#' # will work only in A Shiny app
#' if (interactive()) {
#'   require("shiny")
#'   ui <- fluidPage(
#'     titlePanel("getLog example"),
#'        sidebarLayout(sidebarPanel(
#'          actionButton("quit", "Quit")),
#'          mainPanel(textOutput("log"))
#'    )
#'  )
#' #
#'   server <- function(input, output, session) {
#'     observeEvent(input$quit, { stopApp() })
#'     output$log <- renderText({ mmstat.getLog(session) })
#'   }
#' #
#'   shinyApp(ui, server)
#' }
mmstat.getLog <- function (session) {
  if (!mmstat$debug) return ("")
  invalidateLater(100, session)
  paste0('<hr>', paste(mmstat$log, collapse="<br>"))
}
