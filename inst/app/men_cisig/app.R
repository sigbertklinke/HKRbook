#' men_cisig
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
if (is.null(files)) files <- mmstat.rds("NORM", "BOSTONHOUSING", "USCRIME")
#
mmstat.set(vartype = "numeric", UI = NULL, dataset = NULL, confint = list())  # reset everything
mmstat.ui.elem("conflevel",  "confidenceLevel")
mmstat.ui.elem(inputId="go_reset",
               "actionGroupButtons",
               inputIds=c("go", "reset"),
               labels=gettext(c("Draw sample", "Reset")))
#  mmstat.ui.elem("go",         "drawSample")
#  mmstat.ui.elem('reset',
#                 'actionButton',
#                 label = gettext("Reset"),
#                 value = 0)
mmstat.ui.elem("speed", "speedSlider")
mmstat.ui.elem("dataset", "dataSet", choices = mmstat.getDataNames(files))
mmstat.ui.elem("variable", "variable1", vartype = "numeric")
mmstat.ui.elem("size", "sampleSize")
mmstat.ui.elem("cex", "fontSize")

CIsigma <- function (x, oma, xlim = NULL) {
  n     <- length(x)
  varx  <- stats::var(x)
  upper <- varx * (n - 1) / stats::qchisq((1 - oma) / 2, n - 1)
  lower <- varx * (n - 1) / stats::qchisq(1 - (1 - oma) / 2, n - 1)
  if (is.null(xlim)) {
    lim <- c(lower, upper)
  } else {
    lim <- mmstat.merge(xlim, c(lower, upper))
  }
  list(
    upper = upper,
    lower = lower,
    var = varx,
    oma = oma,
    n = n,
    xlim = lim
  )
}

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
      q <- stats::quantile(x, c(0.25, 0.5, 0.75))
      args$xleft   <- q[1]
      args$xright  <- q[3]
      args$ybottom <- ylim[1]
      args$ytop    <- ylim[2]
      suppressWarnings(do.call(graphics::rect, args))
      args <- box.param
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
                        badgeStatus = NULL,
                        uiOutput("variableUI"),
                        uiOutput("datasetUI")
                      ),
                      dropdownBlock(
                        id    = "options",
                        title = gettext("Options"),
                        badgeStatus = NULL,
                        uiOutput("cexUI")
                      ))),
    dashboardSidebar(minified=FALSE,
                     uiOutput("conflevelUI"),
                     uiOutput("sizeUI"),
                     uiOutput("go_resetUI"),
                     uiOutput("speedUI")
    ),
    dashboardBody( fluidRow(column(
      width = 12,
      box(width=12,
        title = gettext("Confidence intervals for the variance"),
        status = "primary",
        plotOutput("outputConfPlot"),
        plotOutput("outputSamplePlot", height = "200px")
      ))))),
  server = function(input, output, session) {
    output$conflevelUI <- renderUI({
      mmstat.ui.call('conflevel')
    })
    output$sigmaUI     <- renderUI({
      mmstat.ui.call('sigma')
    })
    output$go_resetUI        <- renderUI({
      mmstat.ui.call('go_reset')
    })
    output$speedUI     <- renderUI({
      mmstat.ui.call('speed')
    })
    output$datasetUI   <- renderUI({
      mmstat.ui.call('dataset')
    })
    output$cexUI       <- renderUI({
      mmstat.ui.call('cex')
    })

    output$sizeUI <- renderUI({
      var <- getVar()
      mmstat.ui.call('size',
                     ticks = var$ticks,
                     max = length(var$ticks))
    })

    output$variableUI <- renderUI({
      inp  <- mmstat.getValues(NULL, dataset = input$dataset)
      mmstat.ui.call('variable',
                     choices = mmstat.getVarNames(inp$dataset, 'numeric'))
    })

    getVar <- reactive({
      inp         <-
        mmstat.getValues(NULL,
                         dataset = isolate(input$dataset),
                         variable = input$variable)
      var         <- mmstat.getVar(inp$dataset, inp$variable)
      var$ticks   <- mmstat.ticks(var$n, nmin = 30)
      dec         <- mmstat.dec(0.1 * c(0, var$sd / sqrt(max(var$ticks))))
      var$decimal <- dec$decimal
      var
    })

    getSize <- reactive({
      var <- getVar()
      inp <- mmstat.getValues(NULL, size = input$size)
      var$ticks[inp$size]
    })

    resetCI <- reactive ({
      input$reset
      mmstat.set(confint=vector("list", 0))
    })

    drawSample <- reactive ({
      inp <- mmstat.getValues (
        list(go=0, reset=0),
        go         = input$go,
        reset      = input$reset,
        speed      = input$speed,
        conflevel  = input$conflevel
      )
      if (inp$speed > 0)
        invalidateLater(500 / inp$speed, session)
      var      <- getVar()
      index    <-
        sample(length(var$values),
               size = getSize(),
               replace = T)
      sample   <- var$values[index]
      confint  <- mmstat.get("confint")
      nci      <- length(confint)
      xlim     <- if (nci > 0) confint[[nci]]$xlim else NULL

      confint[[nci + 1]] <- CIsigma(sample, mmstat.get("UI")$conflevel$ticks[inp$conflevel] / 100, xlim)
      mmstat.set(confint=confint)
      nci      <- length(confint)
      index
    })

    output$outputConfPlot <- renderPlot({
      mmstat.log(sprintf('outputConfPlot'))
      resetCI()
      var   <- getVar()
      index <- drawSample()
      inp   <- mmstat.getValues(NULL, cex = input$cex)
      confint <- mmstat.get("confint")
      mmcol   <- mmstat.get("col")
      nci   <- length(confint)
      if (nci) {
        graphics::par (mar = c(2, 0, 2, 0))
        plot (
          0,
          0,
          type = "n",
          xlim = confint[[nci]]$xlim,
          ylim = c(1.5, 2.0 + 0.2 * nci),
          axes = F,
          cex = 0.5,
          col = mmcol[[1]],
          main = sprintf(gettext("%i confidence interval(s)"), nci),
          cex.axis = inp$cex,
          cex.lab = inp$cex,
          cex.main = 1.2 * inp$cex,
          cex.sub = inp$cex
        )
        graphics::text(
          var$var,
          1.5,
          sprintf("%.*f", var$decimal, var$var),
          col = mmcol[[1]],
          pos = 4,
          cex = inp$cex
        )
        usr <- graphics::par("usr")
        mmstat.axis(1, usr[1:2], cex.axis = inp$cex)
        seqi <- 1:nci
        lwd  <- 2 - (nci > 10)
        for (i in seqi) {
          yi  <- 1.95 + 0.2 * i
          rng <- c(confint[[i]]$lower, confint[[i]]$upper)
          col <-
            ifelse((rng[1] - var$var) > 0 ||
                     (rng[2] - var$var) < 0,
                   'black',
                   mmcol[[2]])
          graphics::lines(rng,
                          c(yi, yi),
                          lwd = lwd,
                          lty = 'solid',
                          col = col)
          graphics::lines(
            c(rng[1], rng[1]),
            c(yi - 0.05, yi + 0.05),
            lwd = lwd,
            lty = 'solid',
            col = col
          )
          graphics::lines(
            c(rng[2], rng[2]),
            c(yi - 0.05, yi + 0.05),
            lwd = lwd,
            lty = 'solid',
            col = col
          )
          graphics::lines(
            c(confint[[i]]$var, confint[[i]]$var),
            c(yi - 0.05, yi + 0.05),
            lwd = lwd,
            lty = 'solid',
            col = col
          )
        }
        size  <- sapply(confint, '[[', 'n')
        index <- 1 + c(0, which(diff(size) != 0))
        posx  <- mmstat.pos(usr[1:2], c(0.05, 0.95))
        graphics::text(
          posx[1],
          1.95 + 0.2 * index,
          labels = sprintf('%.0f', size[index]),
          cex = inp$cex
        )
        oma  <- sapply(confint, '[[', 'oma')
        index <- 1 + c(0, which(diff(oma) != 0))
        graphics::text(posx[2],
                       1.95 + 0.2 * index,
                       labels = sprintf('%.3f', oma[index]),
                       cex = inp$cex)
        graphics::axis(
          3,
          at = posx,
          labels = c(expression('n'), expression(alpha)),
          cex.axis = inp$cex
        )
        graphics::abline(
          v = var$var,
          col = mmcol[[1]],
          lwd = 3,
          lty = "dotted"
        )
        box()
      }
    })

    output$outputSamplePlot <- renderPlot({
      mmstat.log(sprintf('outputSamplePlot'))
      var   <- getVar()
      index <- drawSample()
      inp   <- mmstat.getValues(NULL, cex = input$cex)
      mmcol <- mmstat.get("col")
      graphics::par (mar = c(5, 0, 2, 0))
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
        box.param = list(border = mmcol[[1]], lwd = 2),
        points.param = list(
          col = mmcol[[9]],
          pch = 19,
          cex = 0.25 * inp$cex
        )
      )

      drawIqrBoxWithPoints(
        var$values[index],
        var$jitter[index],
        ylim = 0.5 + c(0, 0.45 * sqrt(length(index) / var$n)),
        box.param = list(border = mmcol[[2]], lwd =
                           2),
        points.param = list(
          col = mmcol[[10]],
          pch = 19,
          cex = 0.25 * inp$cex
        )
      )

      box()
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

