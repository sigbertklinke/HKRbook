#' men_dot
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
files <- getShinyOption("mmstat")
if (is.null(files)) files <- mmstat.rds("CARS", "USCRIME")
#
mmstat.set(vartype = "numeric", UI = NULL, dataset = NULL)  # reset everything
mmstat.ui.elem("dataset",    "dataSet",     choices = mmstat.getDataNames(files))
mmstat.ui.elem("variable",   "variable1",   vartype = "numeric")
#
dpc <- gettext(c("overplot", "jitter", "stack"), "name")
mmstat.ui.elem("method", "selectInput", label = gettext("Select a Dotplot type"), choices = dpc, selected = "overplot")
mmstat.ui.elem("addmean", "checkboxInput", label = gettext("Add mean (aquamarine, dotted)"), value = FALSE)
mmstat.ui.elem("addmedian", "checkboxInput", label = gettext("Add median (blue, dashed)"), value = FALSE)
mmstat.ui.elem("addrange", "checkboxInput", label = gettext("Add range (orange, dotted)"), value = FALSE)
mmstat.ui.elem("addiqr", "checkboxInput", label = gettext("Add interquartile range (pink, dashed)"), value = FALSE)
mmstat.ui.elem("cex", "fontSize")
#
shinyApp(
  ui = dashboardPage(
    dashboardHeader(title="MM*Stat",
                    leftUi = tagList(
                      dropdownBlock(id = "data",    title = gettext("Data choice"), badgeStatus = NULL,
                                    uiOutput("variableUI"), uiOutput("datasetUI")),
                      dropdownBlock(id = "options", title = gettext("Options"),     badgeStatus = NULL,
                                    uiOutput("cexUI"))
                    )),
    dashboardSidebar(minified=FALSE,
                     uiOutput("methodUI"),
                     uiOutput("addmeanUI"),
                     uiOutput("addmedianUI"),
                     uiOutput("addrangeUI"),
                     uiOutput("addiqrUI")),
    dashboardBody(
      fluidRow(
        column(width = 12,
               box(width=12,title=gettext("Dotplot with parameters"), status="primary",
                   plotOutput("distPlot"))
        )
      )
    )
  )
  ,
  server = function(input, output, session) {

    output$methodUI    <- renderUI({ mmstat.ui.call("method") })
    output$addmedianUI <- renderUI({ mmstat.ui.call("addmedian") })
    output$addrangeUI  <- renderUI({ mmstat.ui.call("addrange") })
    output$addiqrUI    <- renderUI({ mmstat.ui.call("addiqr") })
    output$addmeanUI   <- renderUI({ mmstat.ui.call("addmean") })
    output$datasetUI   <- renderUI({ mmstat.ui.call("dataset") })
    output$cexUI       <- renderUI({ mmstat.ui.call("cex") })

    output$variableUI <- renderUI({
      inp  <- mmstat.getValues(NULL, dataset = input$dataset)
      mmstat.ui.call('variable',
                     choices = mmstat.getVarNames(inp$dataset, 'numeric'))
    })

    getVar <- reactive({
      mmstat.log(paste('getVar'))
      var         <- mmstat.getVar(isolate(input$dataset), input$variable)
      var$ticks   <- mmstat.ticks(var$n, nmin=30)
      dec         <- mmstat.dec(0.1*c(0, var$sd/sqrt(max(var$ticks))))
      var$decimal <- dec$decimal
      var
    })

    output$distPlot <- renderPlot({
      #browser()
      var <- getVar()
      inp <- mmstat.getValues(NULL, method = input$method, cex = input$cex, addmean = input$addmean,
                              addmedian = input$addmedian, addrange = input$addrange, addiqr = input$addiqr)
      graphics::stripchart(var$values,
                           method=inp$method,
                           main=sprintf(gettext("Dotplot (%s) of %s"), tolower(gettext(inp$method)), var$name),
                           xlab=var$xlab,
                           sub=var$sub,
                           cex.axis=inp$cex,
                           cex.lab=inp$cex,
                           cex.main=1.2*inp$cex,
                           cex.sub=inp$cex,
                           axes=F)
      usr <- graphics::par("usr")
      mmstat.axis(1, usr[1:2], cex.axis=inp$cex)
      box()
      mmcol <- mmstat.get("col")
      if (inp$addmean || inp$addmedian || inp$addrange || inp$addiqr) {
        pos <- mmstat.pos(usr[3:4], 0.05)
        if (inp$addmean) {
          graphics::abline(v=var$mean, lwd=3, lty="dotted", col=mmcol[[1]])
          graphics::text(var$mean, pos, sprintf("%.*f", var$decimal, var$mean), col=mmcol[[1]], pos=4-var$pos, cex=inp$cex)
        }
        if (inp$addrange) {
          graphics::rect(var$range[1], 0.87, var$range[2], 1.13, lwd=3, lty="dotted", border=mmcol[[2]])
          graphics::text(var$range[2]-0.02*(usr[2]-usr[1]), 0.87, pos=1,  sprintf("%.*f", var$decimal, diff(var$range)), col=mmcol[[2]], cex=inp$cex)
        }
        if (inp$addmedian) {
          graphics::abline(v=var$median, lwd=3, lty="dashed", col=mmcol[[3]])
          graphics::text(var$median, pos, sprintf("%.*f", var$decimal, var$median), col=mmcol[[3]], pos=2+var$pos, cex=inp$cex)
        }
        if (inp$addiqr) {
          graphics::rect(var$quart[1], 0.87, var$quart[2], 1.13, lwd=3, lty="dashed", border=mmcol[[4]])
          graphics::text(var$quart[2]-0.02*(usr[2]-usr[1]), 0.87, pos=1, sprintf("%.*f", var$decimal, diff(var$quart)), col=mmcol[[4]], cex=inp$cex)
        }
      }
    })

    output$logText <- renderText({ mmstat.getLog(session) })
  },
  onStart = function() {
    oldpar <- par(no.readonly = TRUE)
    onStop(function() { resetpar(oldpar) })
  }
)

