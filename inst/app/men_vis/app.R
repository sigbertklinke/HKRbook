#' men_vis
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
if (is.null(files)) files <-  mmstat.rds("USCRIME", "CARS", "DECATHLON")
# Set parameter
mmstat.set(vartype = "numeric", UI = NULL, dataset = NULL)

#
dpc <-  gettext(c("overplot", "jitter", "stack"), "name")
#
mmstat.ui.elem("method", "selectInput", label = gettext("Select a Dotplot type"), choices = dpc, selected = "overplot")
mmstat.ui.elem("bins", "sliderInput", label = gettext("Histogram: choose number of bins"), min = 1, max = 50,
               value = 30)
mmstat.ui.elem("addmean", "checkboxInput", label = gettext("Add mean (aquamarine, dotted)"), value = FALSE)
mmstat.ui.elem("addmedian", "checkboxInput", label = gettext("Add median (blue, dashed)"), value = FALSE)
mmstat.ui.elem("obs", "checkboxInput", label = gettext("Show observations"), value = FALSE)
mmstat.ui.elem("dataset",    "dataSet",     choices = mmstat.getDataNames(files))
mmstat.ui.elem("variable",   "variable1",   vartype = "numeric")
mmstat.ui.elem("cex",        "fontSize")

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "MM*Stat",
                    leftUi = tagList(
                      dropdownBlock(id = "data",    title = gettext("Data choice"), badgeStatus = NULL,
                                    uiOutput("variableUI"), uiOutput("datasetUI")),
                      dropdownBlock(id = "options", title = gettext("Options"),     badgeStatus = NULL,
                                    uiOutput("cexUI"))
                    )),
    dashboardSidebar(minified=FALSE,
                     uiOutput("methodUI"),
                     uiOutput("binsUI"),
                     uiOutput("addmeanUI"),
                     uiOutput("addmedianUI")
    ),
    dashboardBody(fluidPage(
      fluidRow(
        box(title = gettext("Variable visualizations"),
          status = "primary",
          width = 6,
          plotOutput("plotStrip", height = "300px")
        ),
        box(width = 6,
          title = '',
          plotOutput("plotHist", height = "300px")
        )
      ),
      fluidRow(box(width = 6, plotOutput("plotBox", height = "300px")
      ),
      box(width = 6, plotOutput("plotEcdf", height = "300px")
      ))
      #        ,
      #        fluidRow(box(width=12,width=12, htmlOutput("distText")))
    ))
  ),
  server = function(input, output, session) {
    output$methodUI    <- renderUI({ mmstat.ui.call("method") })
    output$binsUI      <- renderUI({ mmstat.ui.call("bins") })
    output$addmeanUI   <- renderUI({ mmstat.ui.call("addmean") })
    output$addmedianUI <- renderUI({ mmstat.ui.call("addmedian") })
    output$obsUI       <- renderUI({ mmstat.ui.call("obs") })
    output$datasetUI   <- renderUI({ mmstat.ui.call("dataset") })
    output$cexUI       <- renderUI({ mmstat.ui.call("cex") })

    output$variableUI <- renderUI({
      inp  <- mmstat.getValues(NULL, dataset = input$dataset)
      mmstat.ui.call('variable',
                     choices = mmstat.getVarNames(inp$dataset, 'numeric'))
    })

    getVar <- reactive({
      var <- mmstat.getVar(isolate(input$dataset), input$variable, "numeric")
      dec <- mmstat.dec(c(var$mean, var$median))
      var[['decimal']] <- dec$decimal
      var[['pos']]     <- 2 * (var$mean < var$median)
      var
    })

    output$plotStrip <- renderPlot({
      inp <- mmstat.getValues(
        NULL,
        method    = input$method,
        addmean   = input$addmean,
        addmedian = input$addmedian,
        cex       = input$cex
      )
      var <- getVar()
      graphics::par(mar = c(3.1, 4.1, 4.1, 2.1))
      graphics::stripchart(
        var$values,
        inp$method,
        xlim = var$range,
        axes = F,
        main = sprintf(gettext("Dotplot (%s)"), tolower(gettext(inp$method))),
        cex.axis = inp$cex,
        cex.lab = inp$cex,
        cex.main = 1.2 * inp$cex,
        cex.sub = inp$cex,
        xlab = var$xlab
      )
      usr <- graphics::par("usr")
      mmstat.axis(1, usr[1:2], cex.axis = inp$cex)
      box()
      if (inp$addmean)
        graphics::abline(
          v = var$mean,
          col = mmstat$col[[1]],
          lwd = 3,
          lty = "dotted"
        )
      if (inp$addmedian)
        graphics::abline(
          v = var$median,
          col = mmstat$col[[3]],
          lwd = 3,
          lty = "dashed"
        )
    })

    output$plotHist <- renderPlot({
      var  <- getVar()
      inp <- mmstat.getValues(
        NULL,
        bins      = input$bins,
        addmean   = input$addmean,
        addmedian = input$addmedian,
        obs       = input$obs,
        cex       = input$cex
      )
      bins <-
        seq(var$min, var$max, length.out = as.numeric(inp$bins) + 1)
      graphics::par(mar = c(3.1, 4.1, 4.1, 2.1))
      graphics::hist(
        var$values,
        xlim = var$range,
        breaks = bins,
        col = "grey",
        xlab = var$xlab,
        main = gettext("Histogram"),
        ylab = gettext("Absolute frequency"),
        cex.axis = inp$cex,
        cex.lab = inp$cex,
        cex.main = 1.2 * inp$cex,
        cex.sub = inp$cex,
        axes = F
      )
      usr <- graphics::par("usr")
      mmstat.axis(1, usr[1:2], cex.axis = inp$cex)
      mmstat.axis(2, usr[3:4], cex.axis = inp$cex)
      box()
      if (inp$addmean)
        graphics::abline(
          v = var$mean,
          col = mmstat$col[[1]],
          lwd = 3,
          lty = "dotted"
        )
      if (inp$addmedian)
        graphics::abline(
          v = var$median,
          col = mmstat$col[[3]],
          lwd = 3,
          lty = "dashed"
        )
      if (inp$obs)
        graphics::rug(var$values, lwd = 1)
    })

    output$plotBox <- renderPlot({
      inp <- mmstat.getValues(
        NULL,
        bins      = input$bins,
        addmean   = input$addmean,
        addmedian = input$addmedian,
        obs       = input$obs,
        cex       = input$cex
      )
      var  <- getVar()
      graphics::par(mar = c(3.1, 4.1, 4.1, 2.1))
      graphics::boxplot(
        var$values,
        horizontal = T,
        ylim = var$range,
        axes = F,
        xlab = var$xlab,
        cex.axis = inp$cex,
        cex.lab = inp$cex,
        cex.main = 1.2 * inp$cex,
        cex.sub = inp$cex,
        main = gettext("Boxplot")
      )
      usr <- graphics::par("usr")
      mmstat.axis(1, usr[1:2], cex.axis = inp$cex)
      box()
      if (inp$addmean)
        graphics::abline(
          v = var$mean,
          col = mmstat$col[[1]],
          lwd = 3,
          lty = "dotted"
        )
      if (inp$addmedian)
        graphics::abline(
          v = var$median,
          col = mmstat$col[[3]],
          lwd = 3,
          lty = "dashed"
        )
      if (inp$obs)
        graphics::rug(var$values, lwd = 1)
    })

    output$plotEcdf <- renderPlot({
      inp <- mmstat.getValues(
        NULL,
        bins      = input$bins,
        addmean   = input$addmean,
        addmedian = input$addmedian,
        obs       = input$obs,
        cex       = input$cex
      )
      var  <- getVar()
      graphics::par(mar = c(3.1, 4.1, 4.1, 2.1))
      plot(
        stats::ecdf(var$values),
        xlim = var$range,
        main = gettext(" Ecdf "),
        xlab = var$xlab,
        cex.axis = inp$cex,
        cex.lab = inp$cex,
        cex.main = 1.2 * inp$cex,
        cex.sub = inp$cex,
        axes = F
      )
      usr <- graphics::par("usr")
      mmstat.axis(1, usr[1:2], cex.axis = inp$cex)
      mmstat.axis(2, usr[3:4], cex.axis = inp$cex)
      box()
      if (inp$addmean)
        graphics::abline(
          v = var$mean,
          col = mmstat$col[[1]],
          lwd = 3,
          lty = "dotted"
        )
      if (inp$addmedian)
        graphics::abline(
          v = var$median,
          col = mmstat$col[[3]],
          lwd = 3,
          lty = "dashed"
        )
    })

    output$distText <- renderUI({
      mmstat.log("called 'distText'")
      var  <- getVar()
      inp  <- mmstat.getValues(NULL, cex=input$cex)
      htmlTemplate(system.file("template", gettext("men_vis.html"), package="HKRbook"),
                   cex=inp$cex, xlab=var$xlab, dataname=var$dataname, dec=var$decimal,
                   mean=var$mean, median=var$median, min=var$min, max=var$max,
                   q25=var$quart[1], q75=var$quart[2], sd=var$sd, var=var$var,
                   iqr=var$iqr, range=diff(var$range))
    })

    output$logText <- renderText({ mmstat.getLog(session) })

  },
  onStart = function() {
    oldpar <- par(no.readonly = TRUE)
    onStop(function() { resetpar(oldpar) })
  }
)
