#' men_regr
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
if (is.null(files)) files <- files <- mmstat.rds("USCRIME", "CARS", "DECATHLON")
#
mmstat.set(vartype = 'numeric', UI=NULL, dataset=NULL) # reset everything
#
mmstat.ui.elem(
  "show",
  'checkboxGroupInput',
  label    = gettext("Show"),
  choices  = gettext(
    c("SHOW.REGRESSION", "SHOW.VALUES", "SHOW.CONFIDENCE"),
    "name"
  ),
  value    = character()
)
mmstat.ui.elem("dataset",
               "dataSet",
               choices = mmstat.getDataNames(files))
mmstat.ui.elem(
  "variableYSelect",
  "variable1",
  vartype  = "numeric",
  label    = gettext("Select dependent variable (Y)"),
  selected = mmstat.getVarNames(1, 'numeric', 2)
)
mmstat.ui.elem(
  "variableXSelect",
  "variable1",
  vartype  = "numeric",
  label    = gettext("Select independent variable (X)"),
  selected = mmstat.getVarNames(1, 'numeric', 1)
)
mmstat.ui.elem("cex",  "fontSize")

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
    dashboardSidebar(minified=FALSE,
                     uiOutput("showUI")
    ),
    dashboardBody( fluidRow(column(
      width = 12,
      box(width=12,
        title = gettext("Simple linear regression"),
        status = "primary",
        plotOutput("scatterPlot"),
        verbatimTextOutput("outputR")
      )
    )))
  ),
  server = function(input, output, session) {

    output$showUI <- renderUI({ mmstat.ui.call("show") })
    output$datasetUI <- renderUI({ mmstat.ui.call("dataset") })
    output$cexUI <- renderUI({ mmstat.ui.call("cex") })

    output$variableXSelectUI <- renderUI({
      inp  <- mmstat.getValues(NULL, dataset = input$dataset)
      mmstat.ui.call('variableXSelect', choices = mmstat.getVarNames(inp$dataset, 'numeric'))
    })

    output$variableYSelectUI <- renderUI({
      inp  <- mmstat.getValues(NULL, dataset = input$dataset)
      mmstat.ui.call('variableYSelect', choices = mmstat.getVarNames(inp$dataset, 'numeric'))
    })

    observe({
      inp  <- mmstat.getValues(NULL, dataset = input$dataset)
      nvar <- mmstat.getVarNames(inp$dataset, 'numeric')
      updateSelectInput(
        session,
        "variableYSelect",
        choices  = nvar,
        selected = nvar[2]
      )
      updateSelectInput(
        session,
        "variableXSelect",
        choices  = nvar,
        selected = nvar[1]
      )
    })

    output$scatterPlot <- renderPlot({
      #browser()
      inp <- mmstat.getValues(NULL, dataset = input$dataset, variableXSelect = input$variableXSelect, variableYSelect = input$variableYSelect, show = input$show, cex = input$cex)
      varx <- mmstat.getVar(isolate(inp$dataset), varname = inp$variableXSelect, na.action = stats::na.pass)
      vary <- mmstat.getVar(isolate(inp$dataset), varname = inp$variableYSelect, na.action = stats::na.pass)
      mmcol <- mmstat.get("col")
      if (is.na(pmatch('SHOW.CONFIDENCE', inp$show))) {
        plot (
          varx$values,
          vary$values,
          xlab = gettext(inp$variableXSelect),
          ylab = gettext(inp$variableYSelect),
          cex.axis = inp$cex,
          cex.lab = inp$cex,
          cex.main = inp$cex,
          cex.sub = inp$cex,
          pch = 19,
          sub = gettext(varx$sub)
        )
        if (!is.na(pmatch('SHOW.REGRESSION', inp$show))) {
          lreg <- stats::lm (vary$values ~ varx$values)
          if (inp$variableXSelect != inp$variableYSelect) {
            graphics::abline(lreg, col = mmcol[[1]], lwd = 2)
          } else {
            graphics::abline(
              h = mean(vary$values, na.rm = T),
              col = mmcol[[1]],
              lwd = 2
            )
          }
        }
      } else {
        rx   <- range(varx$values, na.rm = T)
        df   <- data.frame(y = vary$values, x = varx$values)
        lreg <- stats::lm(y ~ x, data = df)
        x    <- rx[1] + diff(rx) * (0:200) / 200
        df   <- data.frame(x = x)
        y    <- stats::predict(lreg, df, interval = "pred")
        if (inp$variableXSelect != inp$variableYSelect)
          ylim <- range(c(range(y), vary$values), na.rm=TRUE)
        else
          ylim <- range(vary$values, na.rm=TRUE)
        plot (
          varx$values,
          vary$values,
          xlab = gettext(inp$variableXSelect),
          ylab = gettext(inp$variableYSelect),
          cex.axis = inp$cex,
          cex.lab = inp$cex,
          cex.main = inp$cex,
          cex.sub = inp$cex,
          pch = 19,
          xlim = rx,
          ylim = ylim
        )
        if (inp$variableXSelect != inp$variableYSelect) {
          graphics::abline(lreg, col = mmcol[[1]], lwd = 2)
        } else {
          graphics::abline(
            h = mean(vary$values, na.rm = T),
            col = mmcol[[1]],
            lwd = 2
          )
        }
        if (inp$variableXSelect != inp$variableYSelect) {
          graphics::lines(x, y[, 'lwr'], lwd = 1, col = mmcol[[2]])
          graphics::lines(x, y[, 'upr'], lwd = 1, col = mmcol[[2]])
          y <- stats::predict(lreg, df, interval = "conf")
          graphics::lines(x, y[, 'lwr'], lwd = 1, col = mmcol[[1]])
          graphics::lines(x, y[, 'upr'], lwd = 1, col = mmcol[[1]])
          pos <- "topright"
          if (lreg$coefficient[2] > 0)
            pos <- "topleft"
          graphics::legend(
            pos,
            legend = c(mmstat.math(" &hat(Y)[h]; "), mmstat.math(" &Y[h]; ")),
            lwd = 1,
            col = c(mmcol[[1]], mmcol[[2]]),
            title = gettext("C.I. for")
          )
        }
      }
    })

    output$outputR <- renderPrint({
      inp <- mmstat.getValues(
        NULL,
        dataset = input$dataset,
        variableXSelect = input$variableXSelect,
        variableYSelect = input$variableYSelect,
        show = input$show
      )
      varx <-
        mmstat.getVar(
          isolate(inp$dataset),
          varname = inp$variableXSelect,
          na.action = stats::na.pass
        )
      vary <-
        mmstat.getVar(
          isolate(inp$dataset),
          varname = inp$variableYSelect,
          na.action = stats::na.pass
        )
      xn   <- gsub('[ ()]+', ".", gettext(inp$variableXSelect))
      yn   <- gsub('[ ()]+', ".", gettext(inp$variableYSelect))
      assign(xn, varx$values)
      assign(yn, vary$values)
      lreg <- eval(parse(text = sprintf("lm(%s~%s)", yn, xn)))
      if (!is.na(pmatch('SHOW.VALUE', inp$show))) {
        print(summary(lreg))
      } else  if (!is.na(pmatch('SHOW.REGRESSION', inp$show))) {
        print(lreg)
      }
    })

    output$logText <- renderText({ mmstat.getLog(session) })
  },
  onStart = function() {
    oldpar <- par(no.readonly = TRUE)
    onStop(function() { resetpar(oldpar) })
  }
)

