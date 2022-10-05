#' men_tmu1
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
if (is.null(files)) files <- mmstat.rds("BOSTONHOUSING", "USCRIME")
mmstat.set(vartype = 'numeric',
           UI = NULL,
           dataset = NULL) # reset everything
#
mmstat.ui.elem("test",       "testHypotheses")
mmstat.ui.elem("mu0",        "sliderInput", label = HTML(gettext("Hypothetical mean (&mu;<sub>0</sub>)")))
mmstat.ui.elem("alpha",      "significance")
mmstat.ui.elem("size",       "sampleSize")
mmstat.ui.elem("go",         "drawSample")
mmstat.ui.elem("dataset",    "dataSet",     choices = mmstat.getDataNames(files))
mmstat.ui.elem("variable",   "variable1",   vartype = "numeric")
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
      suppressWarnings(do.call('points', points.param))
    }
    if (is.list(box.param) || is.null(box.param) || box.param) {
      args <- box.param
      q <- stats::quantile(x, c(0.25, 0.5, 0.75))
      args$xleft   <- q[1]
      args$xright  <- q[3]
      args$ybottom <- ylim[1]
      args$ytop    <- ylim[2]
      suppressWarnings(do.call('rect', args))
      args <- box.param
      args$x <- c(q[2], q[2])
      args$y <- ylim
      if (!is.null(args$border)) {
        args$col <- args$border
        args['border'] <- NULL
      }
      suppressWarnings(do.call('lines', args))
    }
  }

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
                     uiOutput("mu0UI"),
                     uiOutput("alphaUI"),
                     uiOutput("sizeUI"),
                     uiOutput("goUI")
    ),
    dashboardBody( fluidRow(column(
      width = 12,
      box(width=12,
        title = gettext("Test of mean"),
        status = "primary",
        plotOutput("outputTestPlot"),
        plotOutput("outputSamplePlot", height = "200px")
      )
    )))
  ),
  server = function(input, output, session) {
    output$alphaUI   <- renderUI({ mmstat.ui.call("alpha") })
    output$goUI      <- renderUI({ mmstat.ui.call("go") })
    output$datasetUI <- renderUI({ mmstat.ui.call("dataset") })
    output$cexUI     <- renderUI({ mmstat.ui.call("cex") })

    output$mu0UI <- renderUI({
      var   <- getVar()
      #inp   <- mmstat.getValues(NULL, alpha=input$alpha)
      alpha <- mmstat.get("UI")$alpha$ticks[1]
      mmstat.ui.call(
        'mu0',
        min   = mmstat.round.down(max(
          var$min, stats::qnorm(min(alpha) / 200, var$mean, var$sd / sqrt(30))
        ), var$decimal),
        max   = mmstat.round.up(min(
          var$max, stats::qnorm(1 - min(alpha) / 200, var$mean, var$sd / sqrt(30))
        ), var$decimal),
        value = round(var$mean, var$decimal),
        step  = 10 ^ (-var$decimal)
      )
    })

    output$sizeUI <- renderUI({
      var <- getVar()
      mmstat.ui.call('size',
                     ticks = var$ticks,
                     max = length(var$ticks))
    })

    output$variableUI <- renderUI({
      mmstat.log('variableUI')
      inp  <- mmstat.getValues(NULL, dataset = input$dataset)
      mmstat.ui.call('variable',
                     choices = mmstat.getVarNames(inp$dataset, 'numeric'))
    })

    getVar <- reactive({
      mmstat.log(paste("getVar"))
      var <- mmstat.getVar(isolate(input$dataset), input$variable)
      var$ticks <- mmstat.ticks(var$n, nmin = 30)
      dec <- mmstat.dec(0.1 * c(0, var$sd/sqrt(max(var$ticks))))
      var$decimal <- dec$decimal
      var
    })

    drawSample <- reactive ({
      inp    <- mmstat.getValues(NULL, go = input$go, size = input$size)
      var    <- getVar()
      index  <- sample(var$n, var$ticks[inp$size], replace = T)
      values <- var$values[index]
      list(
        values = values,
        jitter = stats::runif(length(values)),
        index  = index,
        range  = range(values),
        mean   = mean(values),
        sd     = stats::sd(values),
        n      = length(values)
      )
    })

    output$outputTestPlot <- renderPlot({
      mmstat.log('outputTestPlot')
      var    <- getVar()
      sample <- drawSample()
      mmcol  <- mmstat.get("col")
      inp  <- mmstat.getValues(
        list(mu0 = var$mean),
        alpha = input$alpha,
        mu0 = input$mu0,
        cex = input$cex
      )
      if (is.list(sample)) {
        xq.mu <- inp$mu0
        xq.sd <- sample$sd / sqrt(sample$n)
        xlim  <-
          range(stats::qnorm(c(0.00001, 0.99999),  inp$mu0, var$sd / sqrt(30)),
                stats::qnorm(c(0.00001, 0.99999), var$mean, var$sd / sqrt(30)))
        #xlim  <- c(max(var$min, xlim[1]), min(var$max, xlim[2]))
        x  <- mmstat.pos(xlim, (0:300) / 300)
        y  <- stats::dnorm(x, xq.mu, var$sd / sqrt(sample$n))
        my <- max(y)
        ylim <- my * c(-0.5, 1)
        graphics::par (mar = c(2, 0, 2, 0))
        plot (
          x,
          y,
          type = 'l',
          xlab = "",
          xlim = xlim,
          ylim = ylim,
          axes = F,
          main = mmstat.math("&H[0];: &mu==mu[0]; vs. &H[1];: &mu!=mu[0]; "),
          ylab = "",
          cex.axis = inp$cex,
          cex.lab = inp$cex,
          cex.main = 1.2 * inp$cex,
          cex.sub = inp$cex
        )
        graphics::abline(h = 0)
        mmstat.axis(1, xlim, cex.axis = inp$cex)
        usr <- graphics::par('usr')
        xp <- stats::qnorm(0.75, xq.mu, xq.sd)
        yp <- stats::dnorm(xp, xq.mu, xq.sd)
        graphics::text(
          xp,
          yp,
          mmstat.math(" &bar(X);~&N(mu[0], sigma^2/n); "),
          pos = 4,
          cex = inp$cex
        )

        c  <- stats::qnorm(1 - mmstat.get("alpha")[inp$alpha] / 200)
        mmstat.plotTestRegions(
          inp$mu0 + var$sd / sqrt(sample$n) * c(-c, c),
          xlim = usr[1:2],
          ylim = -my * c(0.05, 0.25),
          cex = inp$cex,
          col = mmcol[[9]],
          label = mmstat.math(gettext(" &sigma; known")),
          pos = 2 + 2 * (var$mean > inp$mu0)
        )
        c  <- stats::qt(1 - mmstat.get("alpha")[inp$alpha] / 200, sample$n - 1)
        mmstat.plotTestRegions(
          inp$mu0 + sample$sd / sqrt(sample$n) * c(-c, c),
          xlim = usr[1:2],
          ylim = -my * c(0.30, 0.50),
          cex = inp$cex,
          col = mmcol[[10]],
          label = mmstat.math(gettext(" &sigma; estimated")),
          pos = 2 + 2 * (var$mean > inp$mu0)
        )

        graphics::abline(v = xq.mu,
                         col = "gray",
                         lwd = 2)
        graphics::abline(
          v = sample$mean,
          col = mmcol[[2]],
          lwd = 2,
          lty = "dotted"
        )
        graphics::abline(
          v = var$mean,
          col = mmcol[[1]],
          lwd = 2,
          lty = "dotted"
        )
        ypos <- mmstat.pos(usr[3:4], 0.95)
        graphics::text(
          sample$mean,
          ypos,
          sprintf('%.*f',  var$decimal, sample$mean),
          pos = 4 - 2 * (sample$mean < var$mean),
          col = mmcol[[2]],
          cex = inp$cex
        )
        graphics::text(
          var$mean,
          ypos,
          sprintf('%.*f', var$decimal, var$mean),
          pos = 2 + 2 * (sample$mean < var$mean),
          col = mmcol[[1]],
          cex = inp$cex
        )
        box()
      }
    })

    output$outputSamplePlot <- renderPlot({
      mmstat.log(sprintf('outputSamplePlot'))
      var   <- getVar()
      samp  <- drawSample()
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
        box.param = list(border = mmcol[[1]], lwd =
                           2),
        points.param = list(
          col = mmcol[[9]],
          pch = 19,
          cex = 0.5 * inp$cex
        )
      )
      drawIqrBoxWithPoints(
        samp$values,
        samp$jitter,
        ylim = 0.5 + c(0, 0.45 * sqrt(length(samp$values) / var$n)),
        box.param = list(border = mmcol[[2]], lwd =
                           2),
        points.param = list(
          col = mmcol[[10]],
          pch = 19,
          cex = 0.5 * inp$cex
        )
      )

      box()
    })

    output$logText <- renderText({ mmstat.getLog(session) })
  },
  onStart = function() {
    oldpar <- par(no.readonly = TRUE)
    onStop(function() { resetpar(oldpar) })
  }
)
