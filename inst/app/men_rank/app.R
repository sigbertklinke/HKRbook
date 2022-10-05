#' men_rank
suppressPackageStartupMessages({
  library("shiny")
  library("shinyWidgets")
  library("shinydashboard")
  library("shinydashboardPlus")
  library("HKRbook")
  library("scatterplot3d")
})
#
#oldpar <- graphics::par(no.readonly = TRUE)
#on.exit(resetpar(oldpar))
#
files <- getShinyOption("mmstat")
if (is.null(files)) files <-  mmstat.rds("ALLBUS1994-TRUST", "ALLBUS2002-TRUST", "ALLBUS2012-TRUST", "ALLBUS1992-ECON",
                                         "ALLBUS2002-ECON", "ALLBUS2012-ECON")
#
mmstat.set(vartype = 'ordered',
           UI = NULL,
           dataset = NULL) # reset everything
#
mmstat.ui.elem(
  'coeff',
  'checkboxGroupInput',
  label    = gettext("Show coefficient(s)"),
  choices  = gettext(c("SHOW.SPEARMAN", "SHOW.KENDALL"), "name"),
  value    = character()
)
mmstat.ui.elem(
  "dataset",
  "dataSet",
  choices = mmstat.getDataNames(files)
)
mmstat.ui.elem(
  "variableYSelect",
  "variable1",
  vartype = "ordered",
  label    = gettext("Select column variable"),
  selected = mmstat.getVarNames(1, 'ordered', 1)
)
mmstat.ui.elem(
  "variableXSelect",
  "variable1",
  vartype = "ordered",
  label    = gettext("Select row variable"),
  selected = mmstat.getVarNames(1, 'ordered', 2)
)
mmstat.ui.elem("cex",        "fontSize")

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "MM*Stat",
                    leftUi = tagList(
                      dropdownBlock(
                        id    = "data",
                        title = gettext("Data choice"),
                        icon  = NULL,
                        badgeStatus = NULL,
                        uiOutput("variableYSelectUI"),
                        uiOutput("variableXSelectUI"),
                        uiOutput("datasetUI")
                      ),
                      dropdownBlock(
                        id    = "options",
                        title = gettext("Options"),
                        badgeStatus = NULL,
                        uiOutput("cexUI")
                      )
                    )
    ),
    dashboardSidebar(minified = FALSE, uiOutput("coeffUI")),
    dashboardBody( fluidRow(column(
      width = 12,
      box(width=12,
        title = gettext("Rank correlation"),
        status = "primary",
        htmlOutput("contingencyTable")
      )
    )))
  )
  ,
  server = function(input, output, session) {

    output$coeffUI           <- renderUI({ mmstat.ui.call("coeff") })
    output$datasetUI         <- renderUI({ mmstat.ui.call("dataset") })
    output$variableXSelectUI <- renderUI({ mmstat.ui.call("variableXSelect") })
    output$variableYSelectUI <- renderUI({ mmstat.ui.call("variableYSelect") })
    output$cexUI             <- renderUI({ mmstat.ui.call("cex") })

    observe({
      inp     <- mmstat.getValues(NULL, dataset = input$dataset)
      ordvars <- mmstat.getVarNames(inp$dataset, 'ordered')
      updateSelectInput(
        session,
        "variableYSelect",
        choices = ordvars,
        selected = ordvars[1]
      )
      updateSelectInput(
        session,
        "variableXSelect",
        choices = ordvars,
        selected = ordvars[2]
      )
    })

    output$contingencyTable <- renderText({
      inp <- mmstat.getValues(
        NULL,
        dataset = input$dataset,
        coeff = input$coeff,
        variableXSelect = input$variableXSelect,
        variableYSelect = input$variableYSelect,
        cex = input$cex
      )
      varx  <-
        mmstat.getVar(isolate(inp$dataset), varname = inp$variableXSelect)
      vary  <-
        mmstat.getVar(isolate(inp$dataset), varname = inp$variableYSelect)
      tab   <- table(varx$values, vary$values)
      vars  <- c(paste(gettext("Columns:"), gettext(vary$name)),
                 paste(gettext("Rows:"), gettext(varx$name)))
      lines <- NULL
      for (i in seq(inp$coeff)) {
        if (inp$coeff[i] == "SHOW.SPEARMAN") {
          lines <-
            c(lines, sprintf(
              gettext("Spearman rank correlation r<sub>s</sub>=%.3f"),
              stats::cor(
                as.numeric(varx$values),
                as.numeric (vary$values),
                method = "s"
              )
            ))
        }
        if (inp$coeff[i] == "SHOW.KENDALL") {
          lines <-
            c(lines, sprintf(
              gettext("Kendalls rank correlation &tau;=%.3f"),
              stats::cor(
                as.numeric(varx$values),
                as.numeric(vary$values),
                method = "k"
              )
            ))
        }
      }
      toHTML(htmlTable(
        tab,
        vars = vars,
        lines = lines,
        cex = inp$cex,
        title = gettext(inp$dataset)
      ))
    })

    output$logText <- renderText({ mmstat.getLog(session) })
  },
  onStart = function() {
    oldpar <- par(no.readonly = TRUE)
    onStop(function() { resetpar(oldpar) })
  }
)
