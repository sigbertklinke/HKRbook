#' men_ddist
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
# Set parameter
param  <- getShinyOption("mmstat")
distrd <- if (is.null(param$distrd)) 'BINOM' else param$distrd
distrd <- match.arg(toupper(distrd), c("BINOM", "HYPER", "POIS"))
if (distrd=="BINOM") {
  size   <- mmstat.getValue(param$size, 10)
  prob   <- mmstat.getValue(param$prob, 0.5)
  dparam <- distributionParams(mean=size*prob, sd=sqrt(size*prob*(1-prob)), n=size)
}
if (distrd=="HYPER") {
  N   <- mmstat.getValue(param$N, 60)
  M   <- mmstat.getValue(param$M, 30)
  n   <- mmstat.getValue(param$n, 20)
  dparam <- distributionParams(mean=n*M/N, sd=sqrt(n*M/N*(1-M/N)), n=n, N=N)
}
if (distrd=="POIS") {
  lambda <- mmstat.getValue(param$lambda, 10)
  dparam <- distributionParams(mean=lambda, sd=sqrt(lambda))
}
#
mmstat.set(vartype = 'factor',
           UI      = NULL,
           dataset = NULL,
           dist    = distrd,
           xlim    = NULL,
           ylim    = NULL
) # reset everything
#
radioc <- gettext(c("DPDF", "DCDF"), "name")
mmstat.ui.elem(
  "distribution",
  'radioButtons',
  label = gettext("Select a probability distribution"),
  choices  = gettext(c("BINOM", "HYPER", "POIS"), "name"),
  selected = distrd
)
mmstat.ui.elem(
  "binom.n",
  'sliderInput',
  label = gettext("Number of draws (n)"),
  min   = min(dparam$binom$size, 2),
  max   = max(dparam$binom$size, 30),
  value = dparam$binom$size,
  step = 1
)
mmstat.ui.elem(
  "binom.p",
  'sliderInput',
  label = gettext("Probability of success per draw (p)"),
  min   = 0,
  value = dparam$binom$prob,
  step  = 0.01,
  max   = 1
)
mmstat.ui.elem(
  "hyper.N",
  'sliderInput',
  label   = gettext("Population size (N)"),
  min = min(20, dparam$hyper$N),
  max = max(75, dparam$hyper$N),
  value = dparam$hyper$N,
  step = 1
)
mmstat.ui.elem(
  "hyper.n",
  'sliderInput',
  label   = gettext("Number of draws (n)"),
  min = 0,
  max = max(30, dparam$hyper$n),
  value = dparam$hyper$n,
  step = 1
)
mmstat.ui.elem(
  "hyper.M",
  'sliderInput',
  label   = gettext("Number of success states in the population (M)"),
  min = 0,
  max = max(75, dparam$hyper$M),
  value = dparam$hyper$M,
  step = 1
)
mmstat.ui.elem(
  "pois.lambda",
  'sliderInput',
  label   = HTML(gettext("Parameter (&lambda;)")),
  min = 0,
  max = max(10, ceiling(dparam$pois$lambda)),
  value = round(dparam$pois$lambda, 1),
  step = 0.1
)
mmstat.ui.elem(
  "pdforcdf",
  'radioButtons',
  label   = gettext("Show"),
  choices = radioc,
  selected = "DPDF"
)
mmstat.ui.elem("refit", 'actionButton', label    = gettext("Refit axes"))
mmstat.ui.elem("cex", "fontSize")
#
shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "MM*Stat",
                    leftUi =  tagList(
                      dropdownBlock(
                        id    = "options",
                        title = gettext("Options"),
                        badgeStatus = NULL,
                        uiOutput("cexUI")
                      ))),
    dashboardSidebar(minified=FALSE,
                     uiOutput("distributionUI"),
                     conditionalPanel(condition = "input.distribution=='BINOM'",
                                      uiOutput("binom.nUI"),
                                      uiOutput("binom.pUI")),
                     conditionalPanel(condition = "input.distribution=='HYPER'",
                                      uiOutput("hyper.NUI"),
                                      uiOutput("hyper.MUI"),
                                      uiOutput("hyper.nUI")),
                     conditionalPanel(condition = "input.distribution=='POIS'",
                                      uiOutput("pois.lambdaUI")),
                     uiOutput("pdforcdfUI"),
                     uiOutput("refitUI")
    ),
    dashboardBody( fluidRow(column(
      width = 12,
      box(width=12,
          #        title = gettext("Discrete distributions"),
          status = "primary",
          plotOutput("distPlot")
      ))))
  ),
  server = function(input, output, session) {
    #    output$BINOMUI        <- renderUI({ HTML(mmstat.html(gettext("INTRO_BINOM"))) })
    #    output$HYPERUI        <- renderUI({ HTML(mmstat.html(gettext("INTRO_HYPER"))) })
    #    output$POISUI         <- renderUI({ HTML(mmstat.html(gettext("INTRO_POIS"))) })
    output$pdforcdfUI     <- renderUI({ mmstat.ui.call("pdforcdf") })
    output$refitUI        <- renderUI({ mmstat.ui.call("refit") })
    output$pois.lambdaUI  <- renderUI({ mmstat.ui.call("pois.lambda") })
    output$distributionUI <- renderUI({ mmstat.ui.call("distribution") })
    output$binom.nUI      <- renderUI({ mmstat.ui.call("binom.n") })
    output$binom.pUI      <- renderUI({ mmstat.ui.call("binom.p") })
    output$hyper.nUI      <- renderUI({ mmstat.ui.call("hyper.n") })
    output$hyper.NUI      <- renderUI({ mmstat.ui.call("hyper.N") })
    output$hyper.MUI      <- renderUI({ mmstat.ui.call("hyper.M") })
    output$cexUI          <- renderUI({ mmstat.ui.call("cex") })

    observe({
      if (!is.null(input$hyper.N) && !is.null(input$hyper.M)) {
        val <- if (input$hyper.N<input$hyper.M) input$hyper.N else input$hyper.M
        updateSliderInput(session, 'hyper.M', value=val, max=input$hyper.N)
      }
    })

    cdf <- function(x, height) {
      n <- length(x)
      if (n < 1)
        stop("'x' must have 1 or more non-missing values")
      rval <-
        stats::approxfun(
          x,
          height,
          method = "constant",
          yleft = 0,
          yright = 1,
          f = 0,
          ties = "ordered"
        )
      class(rval) <- c("ecdf", "stepfun", class(rval))
      rval
    }

    refit <- reactive({
      inp <- mmstat.getValues(NULL, refit = input$refit)
      mmstat.set(xlim=NULL, ylim=NULL)
      #      mmstat.set(dist='mmstat')
    })

    getDistribution <- reactive({
      inp <- mmstat.getValues(
        NULL,
        distribution = input$distribution,
        binom.p      = isolate(input$binom.p),
        binom.n      = isolate(input$binom.n),
        hyper.M      = isolate(input$hyper.M),
        hyper.N      = isolate(input$hyper.N),
        hyper.n      = isolate(input$hyper.n),
        pois.lambda  = isolate(input$pois.lambda)
      )
      refit()
      dist <- mmstat.get("dist")
      if (dist != inp$distribution) {
        dparam <- switch(dist,
                         BINOM=distributionParams(inp$binom.n*inp$binom.p,
                                                  inp$binom.n*inp$binom.p*(1-inp$binom.p),
                                                  n=inp$binom.n, N=inp$hyper.N),
                         HYPER=distributionParams(inp$hyper.n*inp$hyper.M/inp$hyper.N,
                                                  inp$hyper.n*inp$hyper.M/inp$hyper.N*(1-inp$hyper.M/inp$hyper.N),
                                                  n=inp$hyper.n, N=inp$hyper.N),
                         POIS=distributionParams(inp$pois.lambda,
                                                 sqrt(inp$pois.lambda),
                                                 n=inp$hyper.n, N=inp$hyper.N)
        )
        # update all sliders
        updateSliderInput(session, inputId = 'binom.n',
                          value = round(dparam$binom$size))
        updateSliderInput(session, inputId = 'binom.p',
                          value = round(dparam$binom$prob, 2))
        updateSliderInput(session, inputId = 'hyper.n',
                          value = round(dparam$hyper$n))
        updateSliderInput(session, inputId = 'hyper.M',
                          value = round(dparam$hyper$M))
        updateSliderInput(session, inputId = 'hyper.N',
                          value = round(dparam$hyper$N))
        updateSliderInput(session, inputId = 'pois.lambda',
                          value = round(dparam$pois$lambda, 1))
        mmstat.set(dist=inp$distribution, xlim=c(0, 0), ylim=c(0, 0))
      }
      inp$distribution
    })

    output$distPlot <- renderPlot({
      inp   <- mmstat.getValues(
        NULL,
        binom.p      = input$binom.p,
        binom.n      = input$binom.n,
        cex          = input$cex,
        distribution = input$distribution,
        hyper.M      = input$hyper.M,
        hyper.N      = input$hyper.N,
        hyper.n      = input$hyper.n,
        pdforcdf     = input$pdforcdf,
        pois.lambda  = input$pois.lambda
      )
      getDistribution()
      xlim <- mmstat.get("xlim")
      ylim <- mmstat.get("ylim")
      switch(
        inp$distribution,
        BINOM = {
          xlim <- mmstat.merge(xlim, c(0, inp$binom.n + 1))
          x       <- 0:xlim[2]
          if (inp$pdforcdf == 'DPDF') {
            height <- stats::dbinom(x, inp$binom.n, inp$binom.p)
            ylim   <- mmstat.merge(ylim, c(0, height))
            mp <-
              graphics::barplot(
                height,
                xlim = 1.3 * xlim,
                ylim = 1.1 * ylim,
                ylab = "f(x)",
                xlab = "x",
                main = sprintf(
                  gettext("Probability mass function of B(n=%.0f, p=%.2f)"),
                  inp$binom.n,
                  inp$binom.p
                ),
                cex.axis = inp$cex,
                cex.lab = inp$cex,
                cex.main = 1.2 * inp$cex,
                cex.sub = inp$cex
              )
            mmstat.baraxis(1, x, mp, sprintf("%.f", x), cex.axis =
                             inp$cex)
          } else {
            ylim <- mmstat.merge(ylim, c(0, 1))
            height <- stats::pbinom(x, inp$binom.n, inp$binom.p)
            plot(
              cdf(x, height),
              xlim = xlim,
              ylim = ylim,
              ylab = "F(x)",
              xlab = "x",
              main = sprintf(
                gettext(
                  "Cumulative distribution function of B(n=%.0f, p=%.2f)"
                ),
                inp$binom.n,
                inp$binom.p
              ),
              cex.axis = inp$cex,
              cex.lab = inp$cex,
              cex.main = 1.2 * inp$cex,
              cex.sub = inp$cex
            )
          }
        },
        HYPER = {
          xlim <- mmstat.merge(xlim, c(0, inp$hyper.N + 1))
          x       <- 0:xlim[2]
          if (inp$hyper.N - inp$hyper.M>=0) {
            if (inp$pdforcdf == 'DPDF') {
              height  <-
                stats::dhyper(x,
                              inp$hyper.M,
                              inp$hyper.N - inp$hyper.M,
                              inp$hyper.n)
              ylim <- mmstat.merge(ylim, c(0, height))
              mp <- graphics::barplot(
                height,
                xlim = 1.3 * xlim,
                ylim = 1.1 * ylim,
                ylab = "f(x)",
                xlab = "x",
                main = sprintf(
                  gettext(
                    "Probability mass function of H(N=%.0f, M=%.0f, n=%.0f)"
                  ),
                  inp$hyper.N,
                  inp$hyper.M,
                  inp$hyper.n
                ),
                cex.axis = inp$cex,
                cex.lab = inp$cex,
                cex.main = 1.2 * inp$cex,
                cex.sub = inp$cex
              )
              mmstat.baraxis(1,
                             xlim,
                             mp,
                             sprintf("%.f", 0:xlim[2]),
                             cex.axis = inp$cex)
            } else {
              ylim <- mmstat.merge(ylim, c(0, 1))
              height <- stats::phyper(x, inp$hyper.M, inp$hyper.N - inp$hyper.M, inp$hyper.n)
              plot(
                cdf(x, height),
                xlim = xlim,
                ylim = ylim,
                ylab = "F(x)",
                xlab = "x",
                main = sprintf(
                  gettext(
                    "Cumulative distribution function of H(N=%.0f, M=%.0f, n=%.0f)"
                  ),
                  inp$hyper.N,
                  inp$hyper.M,
                  inp$hyper.n
                ),
                cex.axis = inp$cex,
                cex.lab = inp$cex,
                cex.main = 1.2 * inp$cex,
                cex.sub = inp$cex
              )
            }
          }
        },
        POIS = {
          xlim <- mmstat.merge(xlim, c(0, round(stats::qpois(0.999, inp$pois.lambda))))
          x       <- 0:xlim[2]
          if (inp$pdforcdf == 'DPDF') {
            height  <- stats::dpois(x, inp$pois.lambda)
            ylim <- mmstat.merge(ylim, c(0, height))
            mp <- graphics::barplot(
              height,
              xlim = 1.3 * xlim,
              ylim = 1.1 * ylim,
              ylab = "f(x)",
              xlab = "x",
              main = mmstat.math(sprintf(
                gettext("Probability mass function of PO(&lambda;=%0.1f)"),
                inp$pois.lambda
              )),
              cex.axis = inp$cex,
              cex.lab = inp$cex,
              cex.main = 1.2 * inp$cex,
              cex.sub = inp$cex
            )
            mmstat.baraxis(1,
                           xlim,
                           mp,
                           sprintf("%.f", 0:xlim[2]),
                           cex.axis = inp$cex)
          } else {
            ylim <- mmstat.merge(ylim, c(0, 1))
            height  <- stats::ppois(x, inp$pois.lambda)
            plot(
              cdf(x, height),
              xlim = xlim,
              ylim = ylim,
              ylab = "F(x)",
              xlab = "x",
              main = mmstat.math(sprintf(
                gettext(
                  "Cumulative distribution function of PO(&lambda;=%0.1f)"
                ),
                inp$pois.lambda
              )),
              cex.axis = inp$cex,
              cex.lab = inp$cex,
              cex.main = 1.2 * inp$cex,
              cex.sub = inp$cex
            )
          }
        }
      )
      mmstat.set(xlim=xlim, ylim=ylim)
      box()
    })

    output$logText <- renderText({
      #mmstat.getLog(session)
    })
  },
  onStart = function() {
    oldpar <- par(no.readonly = TRUE)
    onStop(function() { resetpar(oldpar) })
  }
)

