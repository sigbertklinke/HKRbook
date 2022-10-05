#' men_parn
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
if (is.null(files)) files <- mmstat.rds("USCRIME", "CARS", "BOSTONHOUSING")

mmstat.set(vartype = 'numeric', UI=NULL, dataset=NULL) # reset everything
dpc <- gettext(c("MEAN", "MEDIAN", "STDDEV", "IQR"), "name")

mmstat.ui.elem("param", "selectInput", label = gettext("Select parameter"), choices = dpc, value = "MEAN")
mmstat.ui.elem("size",       "sampleSize")
mmstat.ui.elem('go',         'drawSample')
mmstat.ui.elem('speed',      'speedSlider')
mmstat.ui.elem("dataset", "dataSet", choices = mmstat.getDataNames(files))
mmstat.ui.elem("variable",   "variable1", vartype = "numeric")
mmstat.ui.elem("cex",        "fontSize")

drawIqrBoxWithPoints <-
  function (x,
            jitter,
            ylim,
            box.param = NULL,
            points.param = NULL) {
    if (is.list(points.param) ||
        is.null(points.param) || points.param) {
      points.param$x <- x
      points.param$y <- ylim[1] + diff(ylim) * jitter
      suppressWarnings(do.call(graphics::points, points.param))
    }
    if (is.list(box.param) || is.null(box.param) || box.param) {
      args <- box.param
      q <- stats::quantile(x, c(0.25, 0.5, 0.75), na.rm = T)
      args$xleft   <- q[1]
      args$xright  <- q[3]
      args$ybottom <- ylim[1]
      args$ytop    <- ylim[2]
      suppressWarnings(do.call(graphics::rect, args))
      args   <- box.param
      args$x <- c(q[2], q[2])
      args$y <- ylim
      if (!is.null(args$border)) {
        args$col <- args$border
        args['border'] <- NULL
      }
      suppressWarnings(do.call(graphics::lines, args))
    }
  }


shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "MM*Stat",
                    leftUi = tagList(
                      dropdownBlock(
                        id    = "data",
                        title = gettext("Data choice"),
                        icon  = NULL,
                        badgeStatus = NULL,
                        uiOutput("variableUI"),
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
      menuItem(
        text = gettext("Sample parameter"),
        startExpanded = TRUE,
        uiOutput("paramUI"),
        uiOutput("sizeUI")
      ),
      menuItem(
        text = gettext("Specify speed"),
        startExpanded = TRUE,
        uiOutput("goUI"),
        uiOutput("speedUI")
      )
    )),
    dashboardBody( fluidRow(column(
      width = 12,
      box(width=12,
        title = gettext("Distribution of sample parameters"),
        status = "primary",
        plotOutput("samplePlot"),
        plotOutput("outputSamplePlot", height = "200px")
      )
    )))
  )

  #shinyUI(fluidPage(
  #
  #  div(class="navbar navbar-static-top",
  #      div(class = "navbar-inner",
  #          fluidRow(column(4, div(class = "brand pull-left", gettext("Distribution of sample parameters"))),
  #                   column(2, checkboxInput("showsample", gettext("Sample parameter"), TRUE)),
  #                   column(2, checkboxInput("showspeed", gettext("Specify speed"), FALSE)),
  #                   column(2, checkboxInput("showdata", gettext("Data choice"), FALSE)),
  #                   column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),
  #
  #  sidebarLayout(
  #    sidebarPanel(
  #      conditionalPanel(
  #        condition = 'input.showsample',
  #        uiOutput("paramUI"),
  #        br(),
  #        uiOutput("sizeUI"),
  #        br(),
  #        uiOutput("goUI")
  #      ),
  #      conditionalPanel(
  #        condition = 'input.showspeed',
  #        br(),
  #        uiOutput("speedUI")
  #      ),
  #      conditionalPanel(
  #        condition = 'input.showdata',
  #        hr(),
  #        uiOutput("datasetUI"),
  #        uiOutput("variableUI")
  #      ),
  #      conditionalPanel(
  #        condition = 'input.showoptions',
  #        hr(),
  #        uiOutput("cexUI")
  #      )
  #      ),
  #
  #      mainPanel(plotOutput("samplePlot"),
  #                plotOutput("outputSamplePlot", height = "200px"))),
  #
  #      htmlOutput("logText")
  #  ))

  ,
  server = function(input, output, session) {

    rv <- reactiveValues(param=c())

    output$paramUI <- renderUI({ mmstat.ui.call("param") })
    output$goUI    <- renderUI({ mmstat.ui.call("go") })
    #  output$resetUI    <- renderUI({ mmstat.ui.call('reset') })
    output$speedUI   <- renderUI({ mmstat.ui.call("speed") })
    output$datasetUI <- renderUI({ mmstat.ui.call("dataset") })
    output$cexUI     <- renderUI({ mmstat.ui.call("cex") })

    output$variableUI <- renderUI({
      inp  <- mmstat.getValues(NULL, dataset = input$dataset)
      mmstat.ui.call("variable", choices = mmstat.getVarNames(inp$dataset, "numeric"))
    })

    output$sizeUI <- renderUI({
      var <- getVar()
      mmstat.ui.call("size", ticks = var$ticks, max = length(var$ticks))
    })

    getVar <- reactive({
      inp           <- mmstat.getValues(NULL, dataset = input$dataset, variable = input$variable)
      var           <- mmstat.getVar(inp$dataset, inp$variable)
      var$ticks     <- mmstat.ticks(var$n, nmin = 30)
      dec           <- mmstat.dec(c(var$mean, var$median))
      var$decimal   <- dec$decimal
      var[['pos']]  <- 2 * (var$mean < var$median)
      mmstat.set(param=c())
      var
    })

    getSize   <- reactive({
      var  <- getVar()
      inp <- mmstat.getValues(NULL, param = input$param, size = input$size)
      if (inp$param == "MEAN")   rv$param <- var$mean
      if (inp$param == "MEDIAN") rv$param <- var$median
      if (inp$param == "STDDEV") rv$param <- var$sd
      if (inp$param == "IQR")    rv$param <- var$iqr
      var$ticks[inp$size]
    })

    drawSample <- reactive ({
      input$go
      inp <- mmstat.getValues(NULL, speed = input$speed, param = input$param)
      if (inp$speed > 0) invalidateLater(500/inp$speed, session)
      var <- getVar()
      repeat {
          # ensure at least two samples
          size <- getSize()
          index <- sample(var$n, size = size, replace = T)
          sample <- var$values[index]
          isolate({
            if (inp$param == "MEAN")   rv$param <- c(rv$param, mean(sample))
            if (inp$param == "MEDIAN") rv$param <- c(rv$param, stats::median(sample))
            if (inp$param == "STDDEV") rv$param <- c(rv$param, stats::sd(sample))
            if (inp$param == "IQR")    rv$param <- c(rv$param, stats::IQR(sample))
          })
          if (length(rv$param) > 2) break
      }
      index
    })

    #output$populationPlot <- renderPlot({
    #  var    <- getVar()
    #  inp    <- mmstat.getValues(NULL, param  = input$param, cex = input$cex)
    #  par (mar=c(5,0,2,0))
    #  hist(var$values, breaks="FD", freq=F, main=sprintf(gettext("Distribution of %s"), var$xlab), axes=F, xlab="", ylab="",
    #       cex.axis=inp$cex, cex.lab=inp$cex, cex.main=1.2*inp$cex, cex.sub=inp$cex)
    #  rug(var$values)
    #  usr <- par('usr')
    #  mmstat.axis(1, usr[1:2], cex.axis=inp$cex)
    #  box()
    #  usr <- par('usr')
    #  if (inp$param=="MEAN") {
    #    lty <- "dotted"
    #    col <- mmstat$col[[1]]
    #    abline(v=var$mean, lwd=3, lty=lty, col=col)
    #    text(var$mean, 0.95*usr[4], pos=4, sprintf("%.*f", var$dec, var$mean), col=col, cex=inp$cex)
    #  }
    #  if (inp$param=="MEDIAN") {
    #    lty <- "dashed"
    #    col <- mmstat$col[[3]]
    #   abline(v=var$median, lwd=3, lty=lty, col=col)
    #    text(var$median, 0.95*usr[4], pos=4, sprintf("%.*f", var$dec, var$median), col=col, cex=inp$cex)
    #  }
    #  if (inp$param=="STDDEV") {
    #    lty <- "dotted"
    #    col <- mmstat$col[[2]]
    #    rect(var$mean-0.5*var$sd, 0, var$mean+0.5*var$sd, usr[4]/3, lwd=3, lty=lty, border=col)
    #    text(var$mean, usr[4]/3, pos=3, sprintf("%.*f", var$dec, var$sd), col=col, cex=inp$cex)
    #  }
    #  if (inp$param=="IQR") {
    #    lty <- "dashed"
    #    col <- mmstat$col[[4]]
    #    rect(var$quart[1], 0, var$quart[2], usr[4]/3, lwd=3, lty=lty, border=col)
    #    text(var$median, usr[4]/3, pos=3, sprintf("%.*f", var$dec, diff(var$quart)), col=col, cex=inp$cex)
    #  }
    #  legend("topright", legend=inp$param, lwd=3, lty=lty, col=col, cex=inp$cex)
    #})


    output$samplePlot <- renderPlot({
      mmstat.log(sprintf('samplePlot %s', input$param))
      var  <- getVar()
      inp  <- mmstat.getValues(NULL, param = input$param, cex = input$cex)
      drawSample()
      mmcol <- mmstat.get('col')
      graphics::par (mar = c(5, 0, 2, 0))
      graphics::hist(
        rv$param,
        breaks = "Scott",
        freq = F,
        axes = F,
        xlab = var$xlab,
        ylab = "",
        main = sprintf(
          gettext("Distribution of %s based on %.0f samples of size n=%.0f"),
          inp$param,
          length(rv$param),
          getSize()
        ),
        cex.axis = inp$cex,
        cex.lab = inp$cex,
        cex.main = 1.2 * inp$cex,
        cex.sub = inp$cex
      )
      graphics::rug(rv$param)
      usr <- graphics::par('usr')
      mmstat.axis(1, usr[1:2], cex.axis = inp$cex)
      box()
      if (inp$param == "MEAN") {
        lty <- "dotted"
        col <- mmcol[[1]]
        graphics::abline(
          v = var$mean,
          lwd = 3,
          lty = lty,
          col = col
        )
        graphics::text(
          var$mean,
          0.95 * usr[4],
          pos = 4,
          sprintf("%.*f", var$dec, var$mean),
          col = col,
          cex = inp$cex
        )
      }
      if (inp$param == "MEDIAN") {
        lty <- "dashed"
        col <- mmcol[[3]]
        graphics::abline(
          v = var$median,
          lwd = 3,
          lty = lty,
          col = col
        )
        graphics::text(
          var$median,
          0.95 * usr[4],
          pos = 4,
          sprintf("%.*f", var$dec, var$median),
          col = col,
          cex = inp$cex
        )
      }
      if (inp$param == "STDDEV") {
        lty <- "dotted"
        col <- mmcol[[2]]
        graphics::abline(
          v = var$sd,
          lwd = 3,
          lty = lty,
          col = col
        )
        graphics::text(
          var$sd,
          0.95 * usr[4],
          pos = 4,
          sprintf("%.*f", var$dec, var$sd),
          col = col,
          cex = inp$cex
        )
      }
      if (inp$param == "IQR") {
        lty <- "dashed"
        col <- mmcol[[4]]
        graphics::abline(
          v = diff(var$quart),
          lwd = 3,
          lty = lty,
          col = col
        )
        graphics::text(
          diff(var$quart),
          0.95 * usr[4],
          pos = 4,
          sprintf("%.*f", var$dec, diff(var$quart)),
          col = col,
          cex = inp$cex
        )
      }
    })

    output$outputSamplePlot <- renderPlot({
      var   <- getVar()
      index <- drawSample()
      inp <- mmstat.getValues(NULL, cex = input$cex, param = input$param)
      graphics::par (mar = c(5, 0, 2, 0))
      mmcol <- mmstat.get('col')
      plot(
        range(var$values),
        c(-0.05, 1.0),
        type = "n",
        axes = F,
        main = gettext("Population and sample"),
        xlab = var$xlab,
        sub = var$sub,
        cex.axis = inp$cex,
        cex.lab = inp$cex,
        cex.main = 1.2 * inp$cex,
        cex.sub = inp$cex
      )
      usr <- graphics::par("usr")
      mmstat.axis(1, usr[1:2], cex.axis = inp$cex)
      drawIqrBoxWithPoints(
        var$values,
        var$jitter,
        ylim = c(0, 0.45),
        box.param = list(border = mmcol[[1]], lwd =
                           2),
        points.param = list(
          col = mmcol[[9]],
          pch = 19,
          cex = 0.5 * inp$cex
        )
      )
      #    if (inp$sigma) {
      #      drawIqrBoxWithPoints(var$values[index], var$jitter[index], ylim=0.5+c(0, 0.45*sqrt(length(index)/var$n)),
      #                           box.param=FALSE,
      #                           points.param=list(col=mmstat$col[[10]], pch=19, cex=0.5*inp$cex))
      #    } else {
      drawIqrBoxWithPoints(
        var$values[index],
        var$jitter[index],
        ylim = 0.5 + c(0, 0.45 * sqrt(length(index) / var$n)),
        box.param = list(border = mmcol[[2]], lwd =
                           2),
        points.param = list(
          col = mmcol[[10]],
          pch = 19,
          cex = 0.5 * inp$cex
        )
      )
      #    }
      box()
      #    if (inp$param=="MEAN"){
      #      lty <- "dotted"
      #      col <- mmstat$col[[1]]
      #      abline(v=var$mean, lwd=3, lty=lty, col=col)
      #      text(var$mean, 0.95*usr[4], pos=4, sprintf("%.*f", var$dec, var$mean), col=col, cex=inp$cex)
      #    }
      #    if (inp$param=="MEDIAN") {
      #      lty <- "dashed"
      #      col <- mmstat$col[[3]]
      #      abline(v=var$median, lwd=3, lty=lty, col=col)
      #      text(var$median, 0.95*usr[4], pos=4, sprintf("%.*f", var$dec, var$median), col=col, cex=inp$cex)
      #    }
      #    if (inp$param=="STDDEV"){
      #      lty <- "dotted"
      #      col <- mmstat$col[[2]]
      #      abline(v=var$sd, lwd=3, lty=lty, col=col)
      #      text(var$sd, 0.95*usr[4], pos=4, sprintf("%.*f", var$dec, var$sd), col=col, cex=inp$cex)
      #    }
      #    if (inp$param=="IQR") {
      #      lty <- "dashed"
      #      col <- mmstat$col[[4]]
      #      abline(v=diff(var$quart), lwd=3, lty=lty, col=col)
      #      text(diff(var$quart), 0.95*usr[4], pos=4, sprintf("%.*f", var$dec, diff(var$quart)), col=col, cex=inp$cex)
      #    }
    })

    output$logText <- renderText({ mmstat.getLog(session) })
  },
  onStart = function() {
    oldpar <- par(no.readonly = TRUE)
    onStop(function() { resetpar(oldpar) })
  }
)
