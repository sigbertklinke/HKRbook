#' men_tab
suppressPackageStartupMessages({
  library("shiny")
  library("shinyWidgets")
  library("shinydashboard")
  library("shinydashboardPlus")
  library("HKRbook")
})
#
#oldpar <- graphics::par(no.readonly = TRUE)
#on.exit(resetpar(oldpar))
#
files <- getShinyOption("mmstat")
if (is.null(files)) files <- mmstat.rds("HAIR.EYE.COLOR", "TITANIC")
#browser()
mmstat.set(vartype = 'factor', UI=NULL, dataset=NULL) # reset everything
#
mmstat.ui.elem(
  "freq",
  'radioButtons',
  label    = gettext("Show frequencies"),
  choices = gettext(c("SHOW.COMMON.ABS", "SHOW.COMMON.REL", "SHOW.COND.ROW", "SHOW.COND.COL", "SHOW.CHI2.RESIDUAL"), "name"),
  selected= "SHOW.COMMON.ABS"
)
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
    dashboardSidebar(sidebarMenu(
      #        menuItem(
      #          text = gettext("Tables"),
      #          startExpanded = TRUE,
      uiOutput("freqUI"),
      uiOutput("coeffUI")
      #        )
    )),
    dashboardBody( fluidRow(column(
      width = 12,
      box(width=12,
        title = gettext("Frequency table/Crosstable"),
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
    output$freqUI            <- renderUI({ mmstat.ui.call("freq") })
    output$coeffUI           <- renderUI({ mmstat.ui.call("coeff") })
    output$datasetUI         <- renderUI({ mmstat.ui.call("dataset") })
    output$cexUI             <- renderUI({ mmstat.ui.call("cex") })
    output$variableXSelectUI <- renderUI({ mmstat.ui.call("variableXSelect") })
    output$variableYSelectUI <- renderUI({ mmstat.ui.call("variableYSelect") })

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
        freq    = input$freq,
        coeff   = input$coeff,
        variableXSelect = input$variableXSelect,
        variableYSelect = input$variableYSelect,
        cex = input$cex
      )
      varx <- mmstat.getVar(isolate(inp$dataset), varname = inp$variableXSelect)
      vary <- mmstat.getVar(isolate(inp$dataset), varname = inp$variableYSelect)
      tab   <- table(varx$values, vary$values)
      vars  <- c(paste(gettext("Columns:"), gettext(vary$name)),
                 paste(gettext("Rows:"), gettext(varx$name)))
      #

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
      #
      if (inp$freq=="SHOW.COMMON.ABS") {
        htab <- htmlTable(tab, vars=vars, title = gettext(inp$dataset),
                          rowsum=gettext("Sum"), colsum=gettext("Sum"),
                          total=sum(tab), cex=inp$cex, lines=lines)
      }
      if (inp$freq=="SHOW.COMMON.REL") {
        htab <- htmlTable(prop.table(tab), vars=vars, title = gettext(inp$dataset),
                          rowsum=gettext("Sum"), colsum=gettext("Sum"),
                          total=1, fmt="%.3f", cex=inp$cex, lines=lines)
      }
      if (inp$freq=="SHOW.COND.ROW") {
        htab <- htmlTable(tab/rowSums(tab), vars=vars, title = gettext(inp$dataset),
                          rowsum=gettext("Sum"), fmt="%.3f", total=1, cex=inp$cex, lines=lines)
        htab <- hm_colmargin(htab, 1:ncol(tab), value=colSums(tab)/sum(tab), fmt="%.3f", text_align="right")
      }
      if (inp$freq=="SHOW.COND.COL") {
        htab <- htmlTable(scale(tab, center=FALSE, scale=colSums(tab)), vars=vars, title = gettext(inp$dataset),
                          colsum=gettext("Sum"), fmt="%.3f", total=1, cex=inp$cex, lines=lines)
        htab <- hm_rowmargin(htab, 1:nrow(tab), value=rowSums(tab)/sum(tab), fmt="%.3f", text_align="right")
      }
      if (inp$freq=="SHOW.CHI2.RESIDUAL") {
        chi2 <- chisq.test(tab)
        htab <- htmlTable(chi2$residuals, vars=vars, title = gettext(inp$dataset), fmt="%.3f", total=sprintf("%.3f", sum(chi2$residuals^2)), cex=inp$cex, lines=lines,
                          colsum=gettext("Squared sum"), rowsum=gettext("Squared sum"))
        htab <- hm_rowmargin(htab, 1:nrow(tab), value=rowSums(chi2$residuals^2), fmt="%.3f", text_align="right")
        htab <- hm_colmargin(htab, 1:ncol(tab), value=colSums(chi2$residuals^2), fmt="%.3f", text_align="right")
      }
      toHTML(htab)
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
