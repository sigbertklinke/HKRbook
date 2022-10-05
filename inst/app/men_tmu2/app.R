#' men_tmu2
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
if (is.null(files)) files <- mmstat.rds("ALLBUS2012-GENERAL", "ALLBUS2004-GENERAL", "ALLBUS2002-GENERAL")
#
mmstat.set(vartype = "numeric", UI = NULL, dataset = NULL)  # reset everything
#
mmstat.ui.elem("test",       "testHypotheses")
mmstat.ui.elem("alpha",      "significance")
mmstat.ui.elem("size1",      "sampleSize", label = HTML(gettext("Select sample size for group 1 (n<sub>1</sub>)")))
mmstat.ui.elem("size2",      "sampleSize", label = HTML(gettext("Select sample size for group 2 (n<sub>2</sub>)")))
mmstat.ui.elem("go",         "drawSample")
mmstat.ui.elem("dataset",    "dataSet", choices = mmstat.getDataNames(files))
mmstat.ui.elem("variable",   "variable1", vartype = "numeric")
mmstat.ui.elem("group",      "variable1", vartype = "binary", label = gettext("Select a group variable"))
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
      q <- stats::quantile(x, c(0.25, 0.5, 0.75), na.rm = T)
      box.param$xleft   <- q[1]
      box.param$xright  <- q[3]
      box.param$ybottom <- ylim[1]
      box.param$ytop    <- ylim[2]
      suppressWarnings(do.call(graphics::rect, box.param))
      box.param$x <- c(q[2], q[2])
      box.param$y <- ylim
      if (!is.null(box.param$border))
        box.param$col <- box.param$border
      box.param$xleft   <- NULL
      box.param$xright  <- NULL
      box.param$ybottom <- NULL
      box.param$ytop    <- NULL
      box.param$border  <- NULL
      suppressWarnings(do.call(graphics::lines, box.param))
    }
  }

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "MM*Stat",
                    leftUi = tagList(
                      dropdownBlock(id = "data",    title = gettext("Data choice"), badgeStatus = NULL,
                                    uiOutput("variableUI"),
                                    uiOutput("groupUI"),
                                    uiOutput("datasetUI")),
                      dropdownBlock(id = "options", title = gettext("Options"),     badgeStatus = NULL,
                                    uiOutput("cexUI"))
                    )),
    dashboardSidebar(minified=FALSE,
                     uiOutput("alphaUI"),
                     uiOutput("size1UI"),
                     uiOutput("size2UI"),
                     uiOutput("goUI")
    ),
    dashboardBody( fluidRow(column(
      width = 12,
      box(width=12,
        title = gettext("Test of two means"),
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

    output$size1UI <- renderUI({
      #browser()
      var <- getVar()
      grp <- getGroup()
      mmstat.ui.call("size1", ticks = mmstat.ticks(grp$tab[1], nmin = 30), max = length(var$ticks))
    })

    output$size2UI <- renderUI({
      var   <- getVar()
      grp   <- getGroup()
      ticks <- mmstat.ticks(grp$tab[2], nmin = 30)
      mmstat.ui.call('size2', ticks = ticks, max = length(ticks))
    })

    output$variableUI <- renderUI({
      inp <- mmstat.getValues(NULL, dataset = input$dataset)
      mmstat.ui.call("variable", choices = mmstat.getVarNames(inp$dataset, "numeric"))
    })

    output$groupUI <- renderUI({
      inp <- mmstat.getValues(NULL, dataset = input$dataset)
      mmstat.ui.call("group", choices = mmstat.getVarNames(inp$dataset, "binary"))
    })

    getVar <- reactive({
      inp         <- mmstat.getValues(NULL, dataset = isolate(input$dataset), variable = input$variable)
      var         <- mmstat.getVar(inp$dataset, inp$variable, "numeric")
      var$ticks   <- mmstat.ticks(var$n, nmin = 30)
      dec         <- mmstat.dec(0.1 * c(0, var$sd / sqrt(max(var$ticks))))
      var$decimal <- dec$decimal
      var
    })

    getGroup <- reactive({
      inp <- mmstat.getValues(NULL, group = input$group, dataset = isolate(input$dataset))
      var <- mmstat.getVar(inp$dataset, inp$group, 'binary')
      var
    })

    drawSample <- reactive ({
      inp    <- mmstat.getValues(NULL, go = input$go, size1 = input$size1, size2 = input$size2)
      var    <- getVar()
      grp    <- getGroup()
      ticks  <- mmstat.ticks(grp$tab[1], nmin = 30)
      index  <- (1:var$n)[grp$values == levels(grp$values)[1]]
      index1 <- sample(grp$tab[1], ticks[inp$size1], replace = T)
      sampl1 <- mmstat.attrVar (var, 'numeric', index[index1])
      ticks  <- mmstat.ticks(grp$tab[2], nmin = 30)
      index  <- (1:var$n)[grp$values == levels(grp$values)[2]]
      index2 <- sample(grp$tab[2], ticks[inp$size2], replace = T)
      sampl2 <- mmstat.attrVar (var, 'numeric', index[index2])
      list(sampl1, sampl2)
    })

    output$outputTestPlot <- renderPlot({
      mmstat.log('outputTestPlot')
      var     <- getVar()
      grp     <- getGroup()
      samples <- drawSample()
      mmcol   <- mmstat.get("col")
      inp  <- mmstat.getValues(NULL, alpha = input$alpha, cex = input$cex)
      grp1 <- grp$values == levels(grp$values)[1]
      grp2 <- grp$values == levels(grp$values)[2]
      mupop1  <- mean(var$values[grp1], na.rm = T)
      mupop2  <- mean(var$values[grp2], na.rm = T)
      s2pop1  <- var(var$values[grp1], na.rm = T)
      s2pop2  <- var(var$values[grp2], na.rm = T)
      n1      <- length(samples[[1]]$values)
      n2      <- length(samples[[2]]$values)
      sgpop   <- sqrt(s2pop1 / n1 + s2pop2 / n2)
      xlim  <- stats::qnorm(c(0.001, 0.999), mean = 0, sd = sqrt(s2pop1/30 + s2pop2/30))
      x     <- xlim[1] + diff(xlim) * (0:300) / 300
      y     <- stats::dnorm(x, mean = 0, sd = sgpop)
      my    <- max(y)
      ylim  <- my * c(-0.5, 1)
      graphics::par (mar = c(5, 0, 2, 0))
      plot (
        x,
        y,
        type = 'l',
        xlab = "",
        xlim = xlim,
        ylim = ylim,
        axes = F,
        main = mmstat.math("&H[0];: &mu[1]==mu[2]; vs. &H[1];: &mu[1]!=mu[2]; "),
        ylab = "",
        cex.axis = inp$cex,
        cex.lab = inp$cex,
        cex.main = 1.2 * inp$cex,
        cex.sub = inp$cex
      )
      usr <- graphics::par('usr')
      s12 <- samples[[1]]$var
      s22 <- samples[[2]]$var
      # equal variances
      oma  <- mmstat.get("UI")$alpha$ticks[inp$alpha] / 100
      dist <-
        stats::qt(1 - oma / 2, n1 + n2 - 2) * sqrt(((n1 - 1) * s12 + (n2 - 1) * s22) /
                                                     (n1 + n2 - 2) * (1 / n1 + 1 / n2))
      mmstat.plotTestRegions(
        c(-dist, dist),
        xlim = usr[1:2],
        ylim = -my * c(0.05, 0.25),
        cex = inp$cex,
        col = mmcol[[9]],
        label = mmstat.math(gettext(" &sigma[1]==sigma[2]; ")),
        pos = 4
      )
      # unequal variances
      df   <-
        floor((s12 / n1 + s22 / n2) ^ 2 / (s12 ^ 2 / n1 ^ 2 / (n1 - 1) + s22 ^
                                             2 / n2 ^ 2 / (n2 - 1)))
      dist <- stats::qt(1 - oma / 2, df) * sqrt(s12 / n1 + s22 / n2)
      mmstat.plotTestRegions(
        c(-dist, dist),
        xlim = usr[1:2],
        ylim = -my * c(0.30, 0.50),
        cex = inp$cex,
        col = mmcol[[10]],
        label = mmstat.math(gettext(" &sigma[1]!=sigma[2]; ")),
        pos = 4
      )
      #
      mmstat.axis(1, xlim, cex.axis = inp$cex)
      graphics::abline(
        v = samples[[1]]$mean - samples[[2]]$mean,
        col = mmcol[[2]],
        lwd = 2,
        lty = "dotted"
      )
      graphics::abline(
        v = mupop1 - mupop2,
        col = mmcol[[1]],
        lwd = 2,
        lty = "dotted"
      )
      graphics::abline(v = 0, col = "gray", lwd = 2)

      xp <- stats::qnorm(0.75, 0, sgpop)
      yp <- stats::dnorm(xp, 0, sgpop)
      graphics::text(
        xp,
        yp,
        mmstat.math(
          " &bar(X)[1]-bar(X)[2]%~~%N(0*','*sigma[1]^2/n[1]+sigma[2]^2/n[2]); "
        ),
        pos = 4,
        cex = inp$cex
      )
      box()
    })

    output$outputSamplePlot <- renderPlot({
      mmstat.log(sprintf('outputSamplePlot'))
      var     <- getVar()
      grp     <- getGroup()
      samples <- drawSample()
      mmcol   <- mmstat.get("col")
      inp   <- mmstat.getValues(NULL, cex = input$cex)
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
      lgrp <- levels(grp$values)
      grp1 <- grp$values == lgrp[1]
      drawIqrBoxWithPoints(
        var$values[grp1],
        var$jitter[grp1],
        ylim = c(0, 0.2),
        box.param = list(border = mmcol[[1]], lwd =
                           2),
        points.param = list(
          col = mmcol[[9]],
          pch = 19,
          cex = 0.5 * inp$cex
        )
      )
      graphics::text(usr[2],
                     0.1,
                     paste(gettext(lgrp[1]), '(1)'),
                     pos = 2,
                     cex = 0.75 * inp$cex)
      grp2 <-  grp$values == lgrp[2]
      drawIqrBoxWithPoints(
        var$values[grp2],
        var$jitter[grp2],
        ylim = 0.25 + c(0.0, 0.2),
        box.param = list(border = mmcol[[1]], lwd =
                           2),
        points.param = list(
          col = mmcol[[9]],
          pch = 19,
          cex = 0.5 * inp$cex
        )
      )
      graphics::text(usr[2],
                     0.35,
                     paste(gettext(lgrp[2]), '(2)'),
                     pos = 2,
                     cex = 0.75 * inp$cex)
      drawIqrBoxWithPoints(
        samples[[1]]$values,
        samples[[1]]$jitter,
        ylim = 0.5 + c(0, 0.2 * sqrt(length(
          samples[[1]]$values
        ) / grp$tab[1])),
        box.param = list(border = mmcol[[2]], lwd =
                           2),
        points.param = list(
          col = mmcol[[10]],
          pch = 19,
          cex = 0.5 * inp$cex
        )
      )
      drawIqrBoxWithPoints(
        samples[[2]]$values,
        samples[[2]]$jitter,
        ylim         = 0.75 + c(0, 0.2 * sqrt(length(samples[[2]]$values)/grp$tab[2])),
        box.param    = list(border = mmcol[[2]], lwd = 2),
        points.param = list(col = mmcol[[10]], pch = 19, cex = 0.5 * inp$cex)
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
