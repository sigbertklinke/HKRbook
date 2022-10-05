#' men_asso
suppressPackageStartupMessages({
  library("shiny")
  library("shinyWidgets")
  library("shinydashboard")
  library("shinydashboardPlus")
  library("HKRbook")
})
#
##oldpar <- graphics::par(no.readonly = TRUE)
##on.exit(resetpar(oldpar))
#
files <- getShinyOption("mmstat")
if (is.null(files)) files <- mmstat.rds("HAIR.EYE.COLOR", "TITANIC")
mmstat.set(vartype = 'factor', UI=NULL, dataset=NULL) # reset everything
mmstat.ui.elem(
  "coeff",
  'checkboxGroupInput',
  label    = gettext("Show coefficient(s)"),
  choices  = gettext(
    c(
      "SHOW.CHISQUARE",
      "SHOW.CONTINGENCY",
      "SHOW.CORRECTED.CONTINGENCY",
      "SHOW.CRAMERS.V"
    ),
    "name"
  ),
  value    = character()
)
mmstat.ui.elem("dataset", "dataSet", choices = mmstat.getDataNames(files))
mmstat.ui.elem(
  "variableYSelect",
  "variable1",
  vartype = "factor",
  label    = gettext("Select column variable"),
  selected = mmstat.getVarNames(1, 'factor', 1)
)
mmstat.ui.elem(
  "variableXSelect",
  "variable1",
  vartype = "factor",
  label    = gettext("Select row variable"),
  selected = mmstat.getVarNames(1, 'factor', 2)
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
    dashboardSidebar(minified=FALSE, uiOutput("coeffUI")),
    dashboardBody( fluidRow(column(
      width = 12,
      box(width=12,
        title = gettext("Associations"),
        status = "primary",
        htmlOutput("contingencyTable")
      )
    )))
  )

  #shinyUI(fluidPage(
  #  div(class="navbar navbar-static-top",
  #      div(class = "navbar-inner",
  #          fluidRow(column(6, div(class = "brand pull-left", gettext("Associations"))),
  #                   column(2, checkboxInput("showcoeff", gettext("Coefficients"), TRUE)),
  #                   column(2, checkboxInput("showdata", gettext("Data choice"), TRUE)),
  #                   column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),
  #
  #  sidebarLayout(
  #    sidebarPanel(
  #      conditionalPanel(
  #        condition = 'input.showcoeff',
  #        uiOutput("coeffUI")
  #      ),
  #      conditionalPanel(
  #        condition = 'input.showdata',
  #        hr(),
  #        uiOutput("datasetUI"),
  #        uiOutput("variableYSelectUI"),
  #        uiOutput("variableXSelectUI")
  #      ),
  #      conditionalPanel(
  #        condition = 'input.showoptions',
  #        hr(),
  #        uiOutput("cexUI")
  #      )
  #    ),
  #    mainPanel(htmlOutput("contingencyTable"))),
  #
  #  htmlOutput("logText")
  #))
  ,
  server = function(input, output, session) {
    output$coeffUI           <- renderUI({ mmstat.ui.call('coeff') })
    output$datasetUI         <- renderUI({ mmstat.ui.call('dataset') })
    output$cexUI             <- renderUI({ mmstat.ui.call('cex') })
    output$variableXSelectUI <- renderUI({ mmstat.ui.call('variableXSelect') })
    output$variableYSelectUI <- renderUI({ mmstat.ui.call('variableYSelect') })

    observe({
      inp  <- mmstat.getValues(NULL, dataset = input$dataset)
      fvar <- mmstat.getVarNames(inp$dataset, 'factor')
      updateSelectInput(
        session,
        "variableYSelect",
        choices  = fvar,
        selected = fvar[1]
      )
      updateSelectInput(
        session,
        "variableXSelect",
        choices  = fvar,
        selected = fvar[2]
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
      chisq <- stats::chisq.test(tab)
      C     <- sqrt(chisq$statistic / (chisq$statistic + sum(tab)))
      k     <- min(nrow(tab), ncol(tab))
      V     <- sqrt(chisq$statistic / sum(tab) / (k - 1))
      for (i in seq(inp$coeff)) {
        if (inp$coeff[i] == "SHOW.CHISQUARE") {
          lines <-
            c(lines, sprintf(
              gettext("Chi square &chi;<sup>2</sup>=%.3f"),
              chisq$statistic
            ))
        }
        if (inp$coeff[i] == "SHOW.CONTINGENCY") {
          lines <-
            c(lines, sprintf(gettext(
              "Contingency coefficient C=%.3f"
            ), C))
        }
        if (inp$coeff[i] == "SHOW.CORRECTED.CONTINGENCY") {
          lines <-
            c(lines, sprintf(
              gettext(
                "Corrected contingency coefficient C<sub>c</sub>=%.3f"
              ),
              C * sqrt(k / (k - 1))
            ))
        }
        if (inp$coeff[i] == "SHOW.CRAMERS.V") {
          lines <- c(lines, sprintf(gettext("Cram&eacute;r\'s V=%.3f"), V))
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

    output$logText <- renderText({
      mmstat.getLog(session)
    })
  },
  onStart = function() {
    oldpar <- par(no.readonly = TRUE)
    onStop(function() { resetpar(oldpar) })
  }
)

