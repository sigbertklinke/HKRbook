#' men_tprop
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
files<- getShinyOption("mmstat")
if (is.null(files)) files <-  mmstat.rds("CREDIT", "BOSTONHOUSING")
#
mmstat.set(vartype = 'binary', UI = NULL, dataset = NULL)  # reset everything
#
mmstat.ui.elem(
  "test",
  'radioButtons',
  label   = gettext("Choose test type"),
  choices = gettext(c('two.sided', 'less', 'greater'), "name"),
  selected   = 'two.sided'
)
mmstat.ui.elem(
  'pi0',
  'sliderInput',
  label     = HTML(gettext(
    "Hypothetical proportion (&pi;<sub>0</sub>)"
  )),
  min       = 0,
  max       = 1,
  value     = 0.5,
  step      = 0.05
)
mmstat.ui.elem('alpha', 'significance')
mmstat.ui.elem('size',  'sampleSize')
mmstat.ui.elem("go",         "drawSample")
mmstat.ui.elem("dataset",    "dataSet",     choices = mmstat.getDataNames(files))
mmstat.ui.elem("variable",   "variable1",   vartype = "binary")
mmstat.ui.elem("cex",        "fontSize")

drawVariableValues <-
  function (x,
            jitter = NULL,
            ylim,
            box.param = NULL,
            points.param = NULL,
            param = NULL) {
    mmcol <- mmstat.get("col")
    if (is.numeric(x)) {
      if (is.list(points.param) ||
          is.null(points.param) || points.param) {
        points.param$x <- x
        points.param$y <- ylim[1] + diff(ylim) * jitter
        suppressWarnings(do.call('points', points.param))
      }
      if (is.list(box.param) || is.null(box.param) || box.param) {
        q <- stats::quantile(x, c(0.25, 0.5, 0.75))
        box.param$xleft   <- q[1]
        box.param$xright  <- q[3]
        box.param$ybottom <- ylim[1]
        box.param$ytop    <- ylim[2]
        suppressWarnings(do.call('rect', box.param))
        box.param$x <- c(q[2], q[2])
        box.param$y <- ylim
        if (!is.null(box.param$border)) {
          box.param$col    <- box.param$border
          box.param$border <- NULL
        }
        suppressWarnings(do.call('lines', box.param))
      }
    }
    if (is.factor(x)) {
      tab <- prop.table(table(x))
      xp  <- as.numeric(c(0, cumsum(tab)))
      level       <- param$level
      param$level <- NULL
      for (i in seq(tab)) {
        args         <- param
        args$xleft   <- xp[i]
        args$xright  <- xp[i + 1]
        args$ybottom <- ylim[1]
        args$ytop    <- ylim[2]
        args$col     <- mmcol[[i + param$col]]
        suppressWarnings(do.call(graphics::rect, args))
        if (!is.null(level) && level) {
          args        <- param
          args$x      <- mean((xp[i:(i + 1)]))
          args$y      <- mean(ylim)
          args$labels <- gettext(names(tab)[i])
          args$level  <- NULL
          args$border <- NULL
          suppressWarnings(do.call(graphics::text, args))
        }
      }
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
                      )
                    )
    ),
    dashboardSidebar(minified=FALSE,
                     uiOutput("testUI"),
                     uiOutput("pi0UI"),
                     uiOutput("alphaUI"),
                     uiOutput("sizeUI"),
                     uiOutput("goUI")
    ),
    dashboardBody( fluidRow(column(
      width = 12,
      box(width=12,
        title = gettext("Test of proportion"),
        status = "primary",
        plotOutput("outputPlot"),
        htmlOutput("distText")
      )
    )))
  ) ,
  server = function(input, output, session) {
    output$testUI <- renderUI({ mmstat.ui.call("test") })
    output$pi0UI <- renderUI({ mmstat.ui.call("pi0") })
    output$alphaUI <- renderUI({ mmstat.ui.call("alpha") })
    output$goUI <- renderUI({ mmstat.ui.call("go") })
    output$datasetUI <- renderUI({ mmstat.ui.call("dataset") })
    output$cexUI <- renderUI({ mmstat.ui.call("cex") })

    output$variableUI <- renderUI({
      mmstat.log('variableUI')
      inp  <- mmstat.getValues(NULL, dataset = input$dataset)
      mmstat.ui.call('variable',
                     choices = mmstat.getVarNames(inp$dataset, 'binary'))
    })

    output$sizeUI <- renderUI({
      var  <- getVar()
      mmstat.ui.call('size',
                     ticks = var$ticks,
                     max = length(var$ticks))
    })

    getVar <- reactive({
      mmstat.log('getVar')
      var         <-
        mmstat.getVar(isolate(input$dataset), input$variable)
      var$ticks   <- mmstat.ticks(var$n, nmin = 40)
      var$decimal <- 2
      var
    })

    drawSample <- reactive ({
      mmstat.log('drawSample')
      input$go
      inp    <- mmstat.getValues(NULL, size = input$size)
      var    <- getVar()
      index  <- sample(var$n, var$ticks[inp$size], replace = T)
      values <- var$values[index]
      tab    <- table(values)
      sample <- list(
        values = values,
        n      = length(values),
        tab    = tab,
        prop   = prop.table(tab)
      )
      sample
    })

    doTest <- reactive({
      var    <- getVar()
      sample <- drawSample()
      inp    <-
        mmstat.getValues(
          NULL,
          pi0 = input$pi0,
          test = input$test,
          alpha = input$alpha
        )
      test   <-
        list(
          pi0 = inp$pi0,
          statistic = sample$prop[1],
          cond = sample$n * inp$pi0 * (1 - inp$pi0),
          var = inp$pi0 * (1 - inp$pi0) / sample$n,
          alpha = inp$alpha,
          distribution = mmstat.math(" &hat(Pi)%~~%N(pi[0], pi[0](1-pi[0])/n); ")
        )
      if (inp$test == 'two.sided') {
        test$h0.html    <- '&pi;=&pi;<sub>0</sub>'
        test$h1.html    <- '&pi;&ne;&pi;<sub>0</sub>'
        test$hypotheses <-
          mmstat.math("&H[0];: &pi==pi[0]; vs. &H[1];: &pi!=pi[0]; ")
        test$cu         <-
          inp$pi0 - stats::qnorm(1 - mmstat.get("alpha")[inp$alpha] / 200) * sqrt(inp$pi0 * (1 -
                                                                                        inp$pi0) / sample$n)
        test$co         <-
          inp$pi0 + stats::qnorm(1 - mmstat.get("alpha")[inp$alpha] / 200) * sqrt(inp$pi0 * (1 -
                                                                                        inp$pi0) / sample$n)
      }
      if (inp$test == 'less') {
        test$h0.html    <- '&pi;&ge;&pi;<sub>0</sub>'
        test$h1.html    <- '&pi;&lt;&pi;<sub>0</sub>'
        test$hypotheses <- mmstat.math("&H[0];: &pi>=pi[0]; vs. &H[1];: &pi<pi[0]; ")
        test$cu         <- inp$pi0 - stats::qnorm(1 - mmstat.get("alpha")[inp$alpha]/100) * sqrt(inp$pi0 * (1 - inp$pi0)/sample$n)
        test$co         <- +Inf
      }
      if (inp$test == 'greater') {
        test$h0.html    <- "&pi;&le;&pi;<sub>0</sub>"
        test$h1.html    <- "&pi;&gt;&pi;<sub>0</sub>"
        test$hypotheses <- mmstat.math("&H[0];: &pi<=pi[0]; vs. &H[1];: &pi>pi[0]; ")
        test$cu         <- -Inf
        test$co         <- inp$pi0 + stats::qnorm(1 - mmstat.get("alpha")[inp$alpha]/100) * sqrt(inp$pi0 * (1 - inp$pi0)/sample$n)
      }
      test$decision     <- ifelse((sample$prop[1] < test$cu) || (sample$prop[1] > test$co),
                                  gettext("Reject H<sub>0</sub>"),
                                  gettext("Can not reject H<sub>0</sub>"))
      test$cond.text    <-
        ifelse(sample$n * inp$pi0 * (1 - inp$pi0) < 9,
               mmstat.math(
                 gettext(
                   "Normal approximation of test statistics &hat(Pi); is NOT applicable!"
                 )
               ),
               "")
      test
    })

    readHTML <- reactive({
      var    <- getVar()
      sample <- drawSample()
      test   <- doTest()
      inp    <- mmstat.getValues(NULL, cex = input$cex)
      np1p <- sample$n * inp$pi0 * (1 - inp$pi0)
      html <- htmlTemplate(system.file("template", "men_tprop.html", package="HKRbook"),
                           cex   = inp$cex,
                           h1    = test$h1.html,
                           h0    = test$h0.html,
                           pi0   = test$pi0,
                           ex    = test$pi0,
                           varx  = test$var,
                           cond  = test$cond,
                           alpha = mmstat.get("alpha")[test$alpha],
                           cu    = test$cu,
                           co    = test$co,
                           statistic = test$statistic,
                           decision = test$decision,
                           sprop = sample$prop[1],
                           ssize = sample$n,
                           dataname = var$dataname,
                           varname = var$xlab,
                           pprop = var$prop[1],
                           psize = var$n
      )
      html
    })

    output$outputPlot <- renderPlot({
      mmstat.log('outputPlot')
      var    <- getVar()
      sample <- drawSample()
      test   <- doTest()
      inp    <-
        mmstat.getValues(
          NULL,
          alpha = input$alpha,
          pi0 = input$pi0,
          cex = input$cex,
          test = input$test
        )
      xi <- (0:100) / 100
      sd <- sqrt(inp$pi0 * (1 - inp$pi0) / sample$n)
      yi <- stats::dnorm(xi, mean = inp$pi0, sd = sd)
      ya <- 0.4 * max(yi)
      ylim <- c(-ya, max(yi))
      xlim <- c(0, 1)
      graphics::par (mar = c(5, 0, 2, 0))
      plot(
        xi,
        yi,
        type = "l",
        xlab = mmstat.math(" &pi; "),
        ylab = "",
        ylim = ylim,
        xlim = xlim,
        axes = "F",
        cex.axis = inp$cex,
        cex.lab = inp$cex,
        cex.main = 1.2 * inp$cex,
        cex.sub = inp$cex
      )
      graphics::title(
        main = test$hypotheses,
        sub = test$cond.text,
        col.sub = "red",
        cex = inp$cex
      )
      mmstat.axis(1, c(0, 1))
      #    mmstat.axis(2, c(0,max(yi)))
      box()
      usr <- graphics::par("usr")

      xt <- stats::qnorm(ifelse(inp$pi0 <= 0.5, 0.7, 0.3),
                         mean = inp$pi0,
                         sd = sd)
      yt <- stats::dnorm(xt, mean = inp$pi0, sd = sd)
      graphics::text(xt,
                     yt,
                     test$distribution,
                     pos = 4,
                     cex = inp$cex)

      #   if (inp$population) {
      #      lines(c(0, var$prop[1]), -c(ya, ya), col=mmstat$col[[1]], lwd=2, lty="solid")
      #      text(1, -ya, sprintf('%.*f',  3, var$prop[1]), pos=4, col=mmstat$col[[1]], cex=inp$cex)
      #    }
      #    if (inp$sample) {
      #      lines(c(0, sample$prop[1]), -0.5*c(ya, ya), col=mmstat$col[[2]], lwd=2, lty="dotted")
      #      text(1, -0.5*ya, sprintf('%.*f', 3, sample$prop[1]), pos=4, col=mmstat$col[[2]], cex=inp$cex)
      #    }

      mmstat.plotTestRegions(
        c(test$cu, test$co),
        xlim = c(0, 1),
        ylim = -ya * c(0.05, 0.4),
        cex = inp$cex,
        close = T
      )
      drawVariableValues(
        as.factor(var$values),
        ylim = -ya * c(0.75, 1),
        param = list(
          cex = inp$cex,
          col = 0,
          border = NA,
          level = T
        )
      )
      drawVariableValues(
        as.factor(sample$values),
        ylim = -ya * c(0.45, 0.45 + 0.25 * sqrt(sample$n / var$n)),
        param = list(
          cex = inp$cex,
          col = 8,
          border = NA,
          level = F
        )
      )
      graphics::abline(v = inp$pi0, col = "grey")

    })

    output$distText <- renderUI({
      mmstat.log("called 'distText'")
      html <- readHTML()
      html
    })

    output$logText <- renderText({ mmstat.getLog(session) })
  },
  onStart = function() {
    oldpar <- par(no.readonly = TRUE)
    onStop(function() { resetpar(oldpar) })
  }
)
