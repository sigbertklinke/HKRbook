#' men_time
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
if (is.null(files)) files <- mmstat.rds("TELEPHONE", "INDEX-BASIC-RENT-BERLIN")
#
mmstat.set(vartype = 'numeric', UI=NULL, dataset=NULL, r2=NULL) # reset everything
#
mmstat.ui.elem(
  'trend',
  'radioButtons',
  label    = gettext("Choose trend type"),
  choices  = gettext(
    c(
      'NO.TREND',
      'LIN.TREND',
      'EXP.TREND',
      'MOV.AVE.3',
      'MOV.AVE.5',
      'MOV.AVE.7'
    ),
    "name"
  ),
  selected = 'NO.TREND'
)
mmstat.ui.elem(
  'season',
  'radioButtons',
  label    = gettext("Choose season type"),
  choices  = gettext(c(
    'NO.SEASON', 'ADD.SEASON', 'MUL.SEASON'
  ), "name"),
  selected = 'NO.SEASON'
)
mmstat.ui.elem(
  'quality',
  'checkboxInput',
  label   = gettext("Show model quality"),
  value   = FALSE
)
mmstat.ui.elem(
  'dataset',
  'dataSet',
  label   = gettext("Select set of  time series set"),
  choices = mmstat.getDataNames(files)
)
mmstat.ui.elem(
  "variable",
  "variable1",
  label   = gettext("Select times series"),
  vartype = "numeric"
)
mmstat.ui.elem("cex",        "fontSize")

add.r2 <-
  function (trendmodel = "NO.TREND",
            seasonality = "NO.SEASON",
            value = NA) {
    rn <- paste0(gettext(trendmodel), ' - ', gettext(seasonality))
    local.r2 <- mmstat.get("r2")
    if (is.null(local.r2)) {
      local.r2 <- value
      names(local.r2) <- rn
    } else {
      rnr <- names(local.r2)
      pos <- match(rn, rnr)
      if (is.na(pos)) {
        local.r2 <- c(local.r2, value)
        names(local.r2) <- c(rnr, rn)
      } else {
        local.r2[pos] <- value
      }
    }
    rnr             <- names(local.r2)
    ind             <- order(local.r2, decreasing = T)
    local.r2        <- local.r2[ind]
    names(local.r2) <- rnr[ind]
    mmstat.set(r2=local.r2)
  }

decompose.ts <-
  function (ts,
            trendmodel = "NO.TREND",
            seasonality = "NO.SEASON") {
    enlarge <- function (x, n) {
      l <- length(x)
      xn <- rep(x, 1 + ceiling(n %/% l))
      xn[1:n]
    }
    # trend estimation
    trend <- rep(0, length(ts))
    t     <- 1:length(ts)
    if (trendmodel == "LIN.TREND") {
      trend <- stats::fitted(stats::lm(ts ~ t))
    }
    if (trendmodel == "EXP.TREND") {
      xt    <- log(ts)
      trend <- exp(stats::fitted(stats::lm(xt ~ t)))
    }
    if (trendmodel == 'MOV.AVE.3') {
      trend <- stats::filter(ts, rep(1 / 3, 3))
    }
    if (trendmodel == 'MOV.AVE.5') {
      trend <- stats::filter(ts, rep(1 / 5, 5))
    }
    if (trendmodel == 'MOV.AVE.7') {
      trend <- stats::filter(ts, rep(1 / 7, 7))
    }
    random   <- ts - trend
    seasonal <- rep(NA, length(ts))
    figure   <- rep(NA, length(ts))
    season   <- 1 + (t %% stats::frequency(ts))
    if (seasonality == 'ADD.SEASON') {
      adj      <- tapply(ts - trend, season, mean, na.rm = T)
      seasonal <- enlarge(adj, length(ts))
      figure   <- trend + seasonal
      random   <- ts - figure
    }
    if (seasonality == 'MUL.SEASON') {
      adj      <- tapply(ts / trend, season, mean, na.rm = T)
      seasonal <- enlarge(adj, length(ts))
      figure   <- trend * seasonal
      random   <- ts - figure
    }
    ret <- list(
      ts = ts,
      trend = ts(trend, start = stats::start(ts), frequency = stats::frequency(ts)),
      seasonal = ts(seasonal, start = stats::start(ts), frequency = stats::frequency(ts)),
      random = ts(random, start = stats::start(ts), frequency = stats::frequency(ts)),
      figure = ts(figure, start = stats::start(ts), frequency = stats::frequency(ts)),
      trendmodel = trendmodel,
      seasonality = seasonality,
      r2 = 1 - stats::var(random, na.rm = T) / stats::var(ts)
    )
    add.r2(trendmodel, seasonality, ret$r2)
    ret
  }

plot.tsd <- function (fit, cex) {
  ylim <- range(fit$ts, na.rm = T)

  if (fit$trendmodel != 'NO.TREND') {
    ylim2 <- range(fit$trend, na.rm = T)
    ylim  <- c(min(ylim, ylim2, na.rm = T), max(ylim, ylim2, na.rm = T))
  }
  bcex <- 1
  if (fit$seasonality != 'NO.SEASON') {
    #oldpar <- graphics::par()
    graphics::layout(matrix(1:3, ncol = 1), heights = c(2, 1, 1))
    graphics::par(mai = c(0.7, oldpar$mai[2], 0, oldpar$mai[4]))
    bcex <- 1.5
  }
  if (fit$trendmodel == 'NO.TREND') {
    plot(fit$ts, axes = F, xlab = "")
    graphics::axis(1, cex.axis = bcex * cex)
    mmstat.axis(2, ylim, cex.axis = bcex * cex)
    graphics::title(xlab = gettext("Time"), cex.lab = bcex * cex)
    box()
  } else {
    stats::ts.plot(fit$ts, fit$trend, gpars = list(
      col = c("black", "red"),
      axes = F,
      xlab = ""
    ))
    graphics::axis(1, cex.axis = bcex * cex)
    mmstat.axis(2, ylim, cex.axis = bcex * cex)
    graphics::title(xlab = gettext("Time"), cex.lab = bcex * cex)
    box()
  }
  if (fit$seasonality != 'NO.SEASON') {
    graphics::par(mai = c(0, oldpar$mai[2], 0, oldpar$mai[4]))
    #plot(fit$trend, ylim=range(fit$trend, na.rm=T), ylab=gettext("trend"), xaxt = "n")
    ylim <- range(fit$seasonal, na.rm = T)
    plot(fit$seasonal,
         ylim = ylim,
         axes = F,
         ylab = "")
    mmstat.axis(2, ylim, cex.axis = bcex * cex)
    graphics::title(ylab = gettext("seasonal"), cex.lab = bcex * cex)
    box()
    #par(mai=c(1,oldpar$mai[2], 0, oldpar$mai[4]))
    ylim <- range(fit$random, na.rm = T)
    plot(fit$random,
         ylim = ylim,
         ylab = "",
         axes = F)
    mmstat.axis(2, ylim, cex.axis = bcex * cex)
    graphics::title(ylab = gettext("random"), cex.lab = bcex * cex)
    box()
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
    dashboardSidebar(minified=FALSE,
                     uiOutput("trendUI"),
                     uiOutput("seasonUI"),
                     uiOutput("qualityUI")
    ),
    dashboardBody(fillPage(column(
      width = 12,
      box(width=12,
        title = gettext("Time series analysis"),
        status = "primary",
        plotOutput("outputPlot"),
        verbatimTextOutput("outputTest")
      )
    )))
  ),
  server = function(input, output, session) {
    output$trendUI    <- renderUI({ mmstat.ui.call("trend") })
    output$qualityUI  <- renderUI({ mmstat.ui.call("quality") })
    output$datasetUI  <- renderUI({ mmstat.ui.call("dataset") })
    output$variableUI <- renderUI({ mmstat.ui.call("variable") })
    output$cexUI      <- renderUI({ mmstat.ui.call("cex") })

    observe({
      inp <- mmstat.getValues(NULL, dataset = input$dataset)

      mmstat.ui.update(
        'variable',
        session = session,
        choices = mmstat.getVarNames(inp$dataset, 'numeric')
      )
    })

    output$seasonUI <- renderUI({
      var  <- getVar()
      if (stats::frequency(var$values) == 1)
        return (
          tags$p(gettext(
            "Frequency of time series is one, therefore no seasonality estimation is possible!"
          ), style="padding:20px;")
        )
      mmstat.ui.call('season')
    })

    getVar <- reactive({
      mmstat.log(paste('getVar'))
      inp         <-
        mmstat.getValues(NULL,
                         dataset = input$dataset,
                         variable = input$variable)
      var         <- mmstat.getVar(inp$dataset, inp$variable)
      var$ticks   <- mmstat.ticks(var$n, nmin = 30)
      dec         <- mmstat.dec(0.1 * c(0, var$sd / sqrt(max(var$ticks))))
      var$decimal <- dec$decimal
      mmstat.set(r2=NULL)
      var
    })

    output$outputPlot <- renderPlot({
      mmstat.log('outputPlot')
      var  <- getVar()
      inp  <- mmstat.getValues(
        NULL,
        trend = input$trend,
        season = input$season,
        cex = input$cex
      )
      if ((stats::frequency(var$values) == 1) ||
          (inp$trend == 'NO.TREND'))
        mmstat.ui.update('season', session = session)
      fit <- decompose.ts(var$values, inp$trend, inp$season)
      plot.tsd(fit, inp$cex)
    })

    output$outputTest <- renderPrint({
      inp  <- mmstat.getValues(NULL, quality = input$quality)
      if (inp$quality) {
        invalidateLater(100, session)
        local.r2 <- as.data.frame(mmstat.get("r2"))
        colnames(local.r2) <- gettext("R^2")
        local.r2
      }
    })

    output$logText <- renderText({ mmstat.getLog(session) })
  },
  onStart = function() {
    oldpar <- par(no.readonly = TRUE)
    onStop(function() { resetpar(oldpar) })
  }
)
