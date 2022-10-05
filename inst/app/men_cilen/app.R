#' men_cilen
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
mmstat.set(vartype = 'numeric', UI=NULL, dataset=NULL) # reset everything
#
mmstat.ui.elem("conflevel",  "confidenceLevel")
mmstat.ui.elem("sigmavar", "sliderInput", label = gettext("Standard deviation"),
               min = 0.1, max = 10, step = 0.1, value = 5)
mmstat.ui.elem("length", "sliderInput", label = gettext("Confidence interval length"),
               min = 0, max = 18, step = 0.1, value = 0)
mmstat.ui.elem("size", "sampleSize", label = gettext("Max. sample size"))
mmstat.ui.elem("cex", "fontSize")

sqr <- function(x) { return(x * x) }

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
                     uiOutput("conflevelUI"),
                     uiOutput("sizeUI"),
                     uiOutput("sigmavarUI"),
                     uiOutput("lengthUI")
    ),
    dashboardBody( fluidRow(column(
      width = 12,
      box(width=12,
        title = gettext("Confidence interval length for the mean"),
        status = "primary",
        plotOutput("outputConfPlot")
      ))))),
  server = function(input, output, session) {
    output$conflevelUI <- renderUI({ mmstat.ui.call("conflevel") })
    output$sigmavarUI  <- renderUI({ mmstat.ui.call("sigmavar") })
    output$lengthUI    <- renderUI({ mmstat.ui.call("length") })
    output$cexUI       <- renderUI({ mmstat.ui.call("cex") })
    output$sizeUI      <- renderUI({ mmstat.ui.call("size",
                                                    ticks = c(20, 50, 100, 200, 500, 1000, 2000),
                                                    max = 7) })

    getSize   <- reactive({
      inp   <- mmstat.getValues(NULL, size = input$size)
      ticks <- c(20, 50, 100, 200, 500, 1000, 2000)
      ticks[inp$size]
    })

    output$outputConfPlot <- renderPlot({
      mmstat.log(sprintf('outputConfPlot'))
      size <- max(30, getSize())
      inp <-
        mmstat.getValues(
          NULL,
          conflevel = input$conflevel,
          sigmavar = input$sigmavar,
          length = input$length,
          cex = input$cex
        )
      nval <- 5:size
      oma  <- mmstat.get("UI")$conflevel$ticks[inp$conflevel] / 100
      ln   <- 2 * inp$sigmavar / sqrt(nval) * stats::qnorm(1 - (1 - oma) / 2)
      lt   <- 2 * inp$sigmavar / sqrt(nval) * stats::qt(1 - (1 - oma) / 2, nval-1)
      plot(
        0,
        0,
        xlim = c(0, size),
        ylim = c(0, mmstat.get("UI")$length$max),
        type = "n",
        xlab = gettext("Sample size"),
        ylab = gettext("Confidence interval length"),
        cex.axis = inp$cex,
        cex.lab = inp$cex,
        cex.main = 1.2 * inp$cex,
        cex.sub = inp$cex
      )

      mmcol <- mmstat.get("col")
      graphics::lines(nval, ln, col = mmcol[[1]], lwd = 2)
      if ((inp$length > min(ln)) && (inp$length < max(ln))) {
        pos <- sum(ln > inp$length)
        graphics::text(
          nval[pos + 1],
          ln[pos + 1],
          labels = sprintf("(%0.f, %.2f)", nval[pos + 1], ln[pos + 1]),
          adj = c(1, 1.5),
          col = mmcol[[1]],
          cex = inp$cex
        )
        graphics::points(nval[pos + 1], ln[pos + 1], pch = 19, col = mmcol[[1]])
      }
      graphics::lines(nval, lt, col = mmcol[[2]], lwd = 2)
      if ((inp$length > min(lt)) && (inp$length < max(lt))) {
        pos <- sum(lt > inp$length)
        graphics::text(
          nval[pos + 1],
          lt[pos + 1],
          labels = sprintf("(%0.f, %.2f)", nval[pos + 1], lt[pos + 1]),
          adj = c(0, -0.5),
          col = mmcol[[2]],
          cex = inp$cex
        )
        graphics::points(nval[pos + 1], lt[pos + 1], pch = 19, col = mmcol[[2]])
      }
      graphics::abline(h = inp$length, col = grDevices::gray(0.2))

      graphics::legend(
        "topright",
        legend = c(mmstat.math(" &sigma; known"), mmstat.math((
          " &sigma; estimated"
        ))),
        lwd = 2,
        col = c(mmcol[[1]], mmcol[[2]]),
        cex = inp$cex
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

