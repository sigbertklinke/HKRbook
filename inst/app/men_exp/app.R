#' men_exp
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
param <- getShinyOption("mmstat")
rate  <- mmstat.getValue(param['rate'], 1)
stopifnot(in_range(rate, 0, 5))
#
mmstat.set(vartype = 'factor', UI = NULL, dataset = NULL, dist = "mmstat", xlim = NULL, ylim = NULL)  # reset everything

distrc <- gettext(c("EXPD", "NORM"), "name")
distrd <- 'EXPD'
radioc <- gettext(c("CPDF", "CCDF"), "name")

mmstat.ui.elem("distribution", "selectInput", label = gettext("Select a probability distribution"),
               choices = distrc, selected = distrd, value = distrd)
mmstat.ui.elem("exp.lambda", "sliderInput", label = HTML(gettext("Parameter (&lambda;)")),
               min = 0.1, max = 5, value = rate, step = 0.1)
mmstat.ui.elem("norm.mu", "sliderInput", label = HTML(gettext("Mean (&mu;)")),
               min = -3, max = +3, value = 0, step = 0.1)
mmstat.ui.elem("norm.sigma2", "sliderInput", label = HTML(gettext("Variance (&sigma;<sup>2</sup>)")),
               min = 0.1, max = 5, value = 1, step = 0.1)
mmstat.ui.elem("pdforcdf", "radioButtons", label = gettext("Show"), choices = radioc, selected = "CPDF")
mmstat.ui.elem("refit", "actionButton", label = gettext("Refit axes"))
mmstat.ui.elem("cex", "fontSize")
#
shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "MM*Stat",
                    leftUi = tagList(
                      dropdownBlock(
                        id    = "options",
                        title = gettext("Options"),
                        badgeStatus = NULL,
                        uiOutput("cexUI")))),
    dashboardSidebar(minified=FALSE,
                     uiOutput("exp.lambdaUI"),
                     uiOutput("pdforcdfUI"),
                     uiOutput("refitUI")
    ),
    dashboardBody( fluidRow(column(
      width = 12,
      box(width=12,
        title = gettext("Exponential distributions"),
        status = "primary",
        plotOutput("distPlot")
      ))))),
  server = function(input, output, session) {
    #      output$EXPUI <- renderUI({ HTML(mmstat.html(gettext("INTRO_EXP"))) })
    #      output$NORMUI <- renderUI({ HTML(mmstat.html(gettext("INTRO_NORM"))) })

    output$exp.lambdaUI   <- renderUI({ mmstat.ui.call("exp.lambda") })
    output$norm.muUI      <- renderUI({ mmstat.ui.call("norm.mu") })
    output$norm.sigma2UI  <- renderUI({ mmstat.ui.call("norm.sigma2") })
    output$distributionUI <- renderUI({ mmstat.ui.call("distribution") })
    output$pdforcdfUI     <- renderUI({ mmstat.ui.call("pdforcdf") })
    output$refitUI        <- renderUI({ mmstat.ui.call("refit") })
    output$cexUI         <- renderUI({ mmstat.ui.call("cex") })

    refit <- reactive({
      input$refit
      mmstat.set(dist='mmstat')
    })

    output$distPlot <- renderPlot({
      refit()
      inp <- mmstat.getValues(NULL, cex = input$cex, distribution = input$distribution, exp.lambda = input$exp.lambda,
                              norm.sigma2 = input$norm.sigma2, norm.mu = input$norm.mu, pdforcdf = input$pdforcdf)
      mmdist <- mmstat.get("dist")
      xlim   <- mmstat.get("xlim")
      ylim   <- mmstat.get("ylim")
      if (mmdist != inp$distribution) {
        if ((mmdist == 'NORM') &&
            (inp$distribution == 'EXPD')) {
          # switch from NORM to EXPD
          updateSliderInput(session,
                            inputId = 'exp.lambda',
                            value = inp$norm.sigma2)
        }
        if ((mmdist == 'EXPD') &&
            (inp$distribution == 'NORM')) {
          # switch from EXPD to NORM
          updateSliderInput(session,
                            inputId = 'norm.sigma2',
                            value = inp$exp.lambda)
          updateSliderInput(session, inputId = 'norm.mu', value = 0)
        }
        xlim <- ylim <- c(0,0)
        mmdist <- inp$distribution
      }
      switch(inp$distribution,
             EXPD = {
               xlim <- mmstat.merge(xlim, c(0, stats::qexp(0.999, inp$exp.lambda)))
               x       <-  (0:300) / 300 * xlim[2]
               if (inp$pdforcdf == 'CPDF') {
                 height  <- stats::dexp(x, inp$exp.lambda)
                 ylim <- mmstat.merge(ylim, c(0, height))
                 plot(x, height, type = "l", xlim = xlim, ylim = ylim, ylab = "", xlab = "x",
                      main = mmstat.math(sprintf(gettext("Density function f(x) of EX(&lambda;=%.1f)"),
                                                 inp$exp.lambda)), cex.axis = inp$cex, cex.lab = inp$cex, cex.main = 1.2 * inp$cex, cex.sub = inp$cex)
               } else {
                 ylim <- mmstat.merge(ylim, c(0, 1))
                 height  <- stats::pexp(x, inp$exp.lambda)
                 plot(
                   x,
                   height,
                   type = "l",
                   xlim = xlim,
                   ylim = ylim,
                   ylab = "",
                   xlab = "x",
                   main = mmstat.math(sprintf(
                     gettext("Cumulative distribution function F(x) of EX(&lambda;=%.1f)"),
                     inp$exp.lambda
                   )),
                   cex.axis = inp$cex,
                   cex.lab = inp$cex,
                   cex.main = 1.2 * inp$cex,
                   cex.sub = inp$cex
                 )
                 graphics::abline(h = 0, col = "gray70", lty = "dashed")
                 graphics::abline(h = 1, col = "gray70", lty = "dashed")
               }
             },
             NORM = {
               sd <- sqrt(inp$norm.sigma2)
               xlim <-
                 mmstat.merge(xlim, c(
                   stats::qnorm(0.001, inp$norm.mu, sd),
                   stats::qnorm(0.999, inp$norm.mu, sd)
                 ))
               x       <-  xlim[1] + (0:300) / 300 * diff(xlim)
               if (inp$pdforcdf == 'CPDF') {
                 height  <- stats::dnorm(x, inp$norm.mu, sd)
                 ylim <- mmstat.merge(ylim, c(0, height))
                 plot(
                   x,
                   height,
                   type = "l",
                   xlim = xlim,
                   ylim = ylim,
                   ylab = "",
                   xlab = "x",
                   main = mmstat.math(sprintf(
                     gettext(
                       "Density function f(x) of N(&mu;=%.1f,&sigma^2;=%.1f)"
                     ),
                     inp$norm.mu,
                     inp$norm.sigma2
                   )),
                   cex.axis = inp$cex,
                   cex.lab = inp$cex,
                   cex.main = 1.2 * inp$cex,
                   cex.sub = inp$cex
                 )
                 graphics::abline(v = 0, col = "gray70")
               } else {
                 ylim <- mmstat.merge(ylim, c(0, 1))
                 height  <- stats::pnorm(x, inp$norm.mu, sd)
                 plot(
                   x,
                   height,
                   type = "l",
                   xlim = xlim,
                   ylim = ylim,
                   ylab = "",
                   xlab = "x",
                   main = mmstat.math(sprintf(
                     gettext("Cumulative distribution function F(x) of N(&mu;=%.1f,&sigma^2;=%.1f)"),
                     inp$norm.mu,
                     inp$norm.sigma2
                   )),
                   cex.axis = inp$cex,
                   cex.lab = inp$cex,
                   cex.main = 1.2 * inp$cex,
                   cex.sub = inp$cex
                 )
                 graphics::abline(h = 0, col = "gray70", lty = "dashed")
                 graphics::abline(h = 1, col = "gray70", lty = "dashed")
               }
             })
      mmstat.set(xlim=xlim, ylim=ylim, dist=mmdist)
    })

    output$logText <- renderText({ mmstat.getLog(session) })
  },
  onStart = function() {
    oldpar <- par(no.readonly = TRUE)
    onStop(function() { resetpar(oldpar) })
  }
)

