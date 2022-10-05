# men_ci2
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
if (is.null(files)) files <- mmstat.rds("ALLBUS2012-GENERAL", "ALLBUS2004-GENERAL", "ALLBUS2002-GENERAL")

mmstat.set(vartype = "numeric", UI = NULL, dataset = NULL, confint = list())  # reset everything
mmstat.ui.elem("conflevel",  "confidenceLevel")
mmstat.ui.elem(
  "varequal",
  "checkboxInput",
  label = gettext("Equal variances assumed"),
  value = FALSE
)
mmstat.ui.elem("size1",      "sampleSize",  label = HTML(gettext(
  "Select sample size for group 1 (n<sub>1</sub>)"
)))
mmstat.ui.elem("size2",      "sampleSize",  label = HTML(gettext(
  "Select sample size for group 2 (n<sub>2</sub>)"
)))
mmstat.ui.elem(inputId="go_reset",
               actionGroupButtons,
               inputIds=c("go", "reset"),
               labels=gettext(c("Draw sample", "Reset")))
#  mmstat.ui.elem("go",         "drawSample")
#  mmstat.ui.elem('reset',
#                 'actionButton',
#                 label = gettext("Reset"),
#                 value = 0)
mmstat.ui.elem('speed',      'speedSlider')
mmstat.ui.elem("dataset", "dataSet", choices = mmstat.getDataNames(files))
mmstat.ui.elem("variable",   "variable1",   vartype = "numeric")
#browser()

mmstat.ui.elem("group", "variable1", vartype = "binary", label = gettext("Select a group variable"))
mmstat.ui.elem("cex",        "fontSize")

CItwomeans  <- function (x1,
                         x2,
                         oma,
                         var.equal = F,
                         xlim = NULL) {
  n1  <- length(x1)
  s12 <- stats::var(x1)
  n2  <- length(x2)
  s22 <- stats::var(x2)
  if (var.equal) {
    dist <-
      stats::qt(1 - (1 - oma) / 2, n1 + n2 - 2) * sqrt(((n1 - 1) * s12 + (n2 - 1) *
                                                          s22) / (n1 + n2 - 2) * (1 / n1 + 1 / n2))
  } else {
    df   <-
      floor((s12 / n1 + s22 / n2) ^ 2 / (s12 ^ 2 / n1 ^ 2 / (n1 - 1) + s22 ^
                                           2 / n2 ^ 2 / (n2 - 1)))
    dist <- stats::qt(1 - (1 - oma) / 2, df) * sqrt(s12 / n1 + s22 / n2)
  }
  upper <- mean(x1) - mean(x2) + dist
  lower <- mean(x1) - mean(x2) - dist
  if (is.null(xlim)) {
    lim <- c(lower, upper)
  } else {
    lim   <- mmstat.merge(xlim, c(lower, upper))
  }
  list(
    upper = upper,
    lower = lower,
    mean = mean(x1) - mean(x2),
    oma = oma,
    n1 = n1,
    n2 = n2,
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
      q <- stats::quantile(x, c(0.25, 0.5, 0.75), na.rm = T)
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
                        uiOutput("groupUI"),
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
        text = gettext("Confidence interval parameter"),
        startExpanded = TRUE,
        uiOutput("conflevelUI"),
        uiOutput("varequalUI"),
        uiOutput("size1UI"),
        uiOutput("size2UI")
      ),
      menuItem(
        text = gettext("Sample drawing"),
        startExpanded = TRUE,
        uiOutput("go_resetUI"),
        #          uiOutput("goUI"),
        #          uiOutput("resetUI"),
        uiOutput("speedUI")
      )
      #        menuItem(
      #          text = gettext("Data choice"),
      #          startExpanded = FALSE,
      #          uiOutput("datasetUI"),
      #          uiOutput("variableUI"),
      #          uiOutput("groupUI")
      #       ),
      #        menuItem(
      #          text = gettext("Options"),
      #          startExpanded = FALSE,
      #          uiOutput("cexUI")
      #        )
    )),
    dashboardBody( fluidRow(column(
      width = 12,
      box(width=12,
        title = gettext("Confidence intervals for the difference of two means"),
        status = "primary",
        plotOutput("outputConfPlot"),
        plotOutput("outputSamplePlot", height = "200px")
      )
    )))
  )
  #shinyUI(fluidPage(
  #
  #    div(class="navbar navbar-static-top",
  #        div(class = "navbar-inner",
  #            fluidRow(column(4, div(class = "brand pull-left", gettext("Confidence intervals for the difference of two means"))),
  #                     column(2, checkboxInput("showtest", gettext("Confidence interval parameter"), TRUE)),
  #                     column(2, checkboxInput("showsample", gettext("Sample drawing"), TRUE)),
  #                     column(2, checkboxInput("showdata", gettext("Data choice"), FALSE)),
  #                     column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),
  #
  #    sidebarLayout(
  #      sidebarPanel(
  #        conditionalPanel(
  #          condition = 'input.showtest',
  #          uiOutput("conflevelUI"),
  #          br(),
  #          uiOutput("varequalUI"),
  #          br(),
  #          uiOutput("size1UI"),
  #          br(),
  #          uiOutput("size2UI")
  #        ),
  #        conditionalPanel(
  #          condition = 'input.showsample',
  #          hr(),
  #          uiOutput("goUI"),
  #          uiOutput("resetUI"),
  #          uiOutput("speedUI")
  #        ),
  #        conditionalPanel(
  #          condition = 'input.showdata',
  #          hr(),
  #          uiOutput("datasetUI"),
  #          uiOutput("variableUI"),
  #          uiOutput("groupUI")
  #        ),
  #        conditionalPanel(
  #          condition = 'input.showoptions',
  #          hr(),
  #          uiOutput("cexUI")
  #        )
  #      ),
  #
  #      mainPanel(plotOutput("outputConfPlot"),
  #                plotOutput("outputSamplePlot", height = "200px"))),
  #
  #      htmlOutput("logText")
  #  ))

  ,
  server = function(input, output, session) {
    output$conflevelUI <- renderUI({
      mmstat.ui.call('conflevel')
    })
    output$varequalUI  <- renderUI({
      mmstat.ui.call('varequal')
    })
    output$go_resetUI        <- renderUI({
      mmstat.ui.call('go_reset')
    })
    #      output$goUI        <- renderUI({
    #        mmstat.ui.call('go')
    #      })
    #      output$resetUI     <- renderUI({
    #        mmstat.ui.call('reset')
    #      })
    output$speedUI     <- renderUI({
      mmstat.ui.call('speed')
    })
    output$datasetUI   <- renderUI({
      mmstat.ui.call('dataset')
    })
    output$cexUI       <- renderUI({
      mmstat.ui.call('cex')
    })

    output$size1UI <- renderUI({
      var <- getVar()
      grp <- getGroup()
      ticks <- mmstat.ticks(grp$tab[1], nmin = 30)
      mmstat.ui.call('size1', ticks = ticks, max = length(ticks))
    })

    output$size2UI <- renderUI({
      var   <- getVar()
      grp   <- getGroup()
      ticks <- mmstat.ticks(grp$tab[2], nmin = 30)
      mmstat.ui.call('size2', ticks = ticks, max = length(ticks))
    })

    output$variableUI <- renderUI({
      inp  <- mmstat.getValues(NULL, dataset = input$dataset)
      mmstat.ui.call('variable',
                     choices = mmstat.getVarNames(inp$dataset, 'numeric'))
    })

    output$groupUI <- renderUI({
      inp  <- mmstat.getValues(NULL, dataset = input$dataset)
      mmstat.ui.call('group',
                     choices = mmstat.getVarNames(inp$dataset, 'binary'))
    })

    getVar <- reactive({
      inp         <-
        mmstat.getValues(NULL,
                         dataset = isolate(input$dataset),
                         variable = input$variable)
      var         <-
        mmstat.getVar(inp$dataset, inp$variable, 'numeric')
      var$ticks   <- mmstat.ticks(var$n, nmin = 30)
      dec         <- mmstat.dec(0.1 * c(0, var$sd / sqrt(max(var$ticks))))
      var$decimal <- dec$decimal
      var
    })

    getGroup <- reactive({
      inp <-
        mmstat.getValues(NULL,
                         group = input$group,
                         dataset = isolate(input$dataset))
      var <- mmstat.getVar(inp$dataset, inp$group, 'binary')
      var
    })

    resetCI <- reactive ({
      input$reset
      mmstat.set(confint=list())
    })

    drawSample <- reactive ({
      inp    <-
        mmstat.getValues(
          list(go=0, reset=0),
          go = input$go,
          size1 = input$size1,
          size2 = input$size2,
          conflevel = input$conflevel,
          varequal = input$varequal,
          speed = input$speed
        )
      if (inp$speed > 0)
        invalidateLater(500 / inp$speed, session)
      var    <- getVar()
      grp    <- getGroup()
      ticks  <- mmstat.ticks(grp$tab[1], nmin = 30)
      index  <- (1:var$n)[grp$values == levels(grp$values)[1]]
      index1 <- sample(grp$tab[1], ticks[inp$size1], replace = T)
      sampl1 <- mmstat.attrVar (var, 'numeric', index[index1])
      sampl1$group <- levels(grp$values)[1]
      ticks  <- mmstat.ticks(grp$tab[2], nmin = 30)
      index  <- (1:var$n)[grp$values == levels(grp$values)[2]]
      index2 <- sample(grp$tab[2], ticks[inp$size2], replace = T)
      sampl2 <- mmstat.attrVar (var, 'numeric', index[index2])
      sampl2$group <- levels(grp$values)[2]
      #
      confint  <- mmstat.get("confint")
      nci      <- length(confint)
      if (nci > 0)
        xlim <- confint[[nci]]$xlim
      else
        xlim = NULL
      clal     <- mmstat.get("UI")$conflevel$ticks[inp$conflevel] / 100
      confint[[nci + 1]] <- CItwomeans(sampl1$values, sampl2$values, clal, inp$varequal, xlim)
      mmstat.set(confint=confint)
      list(sampl1, sampl2)
    })

    output$outputConfPlot <- renderPlot({
      resetCI()
      var     <- getVar()
      grp     <- getGroup()
      samples <- drawSample()
      inp     <- mmstat.getValues(NULL, cex = input$cex)
      confint <- mmstat.get("confint")
      nci     <- length(confint)
      meandiff <- mean(var$values[grp$values == samples[[1]]$group]) - mean(var$values[grp$values == samples[[2]]$group])
      mmcol   <- mmstat.get("col")
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
          meandiff,
          1.5,
          sprintf("%.*f", var$decimal, meandiff),
          col = mmcol[[1]],
          pos = 4,
          cex = inp$cex
        )
        usr <- graphics::par("usr")
        mmstat.axis(1, usr[1:2], cex.axis = inp$cex)
        #   points(sample, 1.5+0.5*jitter[index], pch=19, col=mmcol[[1]])
        seqi <- 1:nci
        lwd  <- 2 - (nci > 10)
        for (i in seqi) {
          yi  <- 1.95 + 0.2 * i
          rng <- c(confint[[i]]$lower, confint[[i]]$upper)
          col <-
            ifelse((rng[1] > meandiff) ||
                     (rng[2] < meandiff),
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
            c(confint[[i]]$mean, confint[[i]]$mean),
            c(yi - 0.05, yi + 0.05),
            lwd = lwd,
            lty = 'solid',
            col = col
          )
        }
        n1  <- sapply(confint, '[[', 'n1')
        n2  <- sapply(confint, '[[', 'n2')
        index <- 1 + c(0, which((diff(n1) != 0) | (diff(n2) != 0)))
        # str(index)
        posx  <- mmstat.pos(usr[1:2], c(0.05, 0.95))
        graphics::text(
          posx[1],
          1.95 + 0.2 * index,
          labels = sprintf('%.0f;%.0f', n1[index], n2[index]),
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
          labels = c(expression('n1;n2'), expression(alpha)),
          cex.axis = inp$cex
        )
        graphics::abline(
          v = meandiff,
          col = mmcol[[1]],
          lwd = 3,
          lty = "dotted"
        )
        box()
      }
    })

    output$outputSamplePlot <- renderPlot({
      var     <- getVar()
      grp     <- getGroup()
      samples <- drawSample()
      inp   <- mmstat.getValues(NULL, cex = input$cex)
      mmcol   <- mmstat.get("col")
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
                     paste(gettext(samples[[1]]$group), '(1)'),
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
                     paste(gettext(samples[[2]]$group), '(2)'),
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
        ylim = 0.75 + c(0, 0.2 * sqrt(length(
          samples[[2]]$values
        ) / grp$tab[2])),
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

    output$logText <- renderText({
      mmstat.getLog(session)
    })
  },
  onStart = function() {
    oldpar <- par(no.readonly = TRUE)
    onStop(function() { resetpar(oldpar) })
  }
)

