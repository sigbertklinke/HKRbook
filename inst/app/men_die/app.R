#' men_die
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

mmstat.set(vartype = 'numeric', UI=NULL, dataset=NULL) # reset everything
#
mmstat.ui.elem(
  'rolls',
  'sliderInput',
  label   = gettext("Number of rolls:"),
  min = 1,
  max = 20,
  value = 3
)
mmstat.ui.elem("prob", "probability", label = gettext("Probability for six with loaded die:"))
#mmstat.ui.elem(
#  "prob",
#  'sliderInput',
#  label   = gettext("Probability for six with loaded die:"),
#  min = 0,
#  max = 1,
#  step = 1 / 6,
#  value = 2 / 6
#)
mmstat.ui.elem("cex", 'fontSize')
mmstat.ui.elem(
  "sixes",
  'sliderInput',
  label   = gettext("Number of sixes (X):"),
  min = 0,
  max = 3,
  value = 0
)

ddbinom <- function (x, size, prob) {
  if (prob <= 0) {
    return (as.numeric(x == 0))
  }
  if (prob >= 1) {
    return (as.numeric(x == size))
  }
  stats::dbinom(x, size, prob)
}

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
                     uiOutput("rollsUI"),
                     uiOutput("sixesUI"),
                     uiOutput("probUI")
    ),
    dashboardBody( fluidRow(column(
      width = 12,
      box(width=12,
        title = gettext("Die rolling sisters"),
        status = "primary",
        htmlOutput("formula"),
        plotOutput("distPlot")
      ))))),
  server = function(input, output, session) {
    output$rollsUI <- renderUI({ mmstat.ui.call("rolls") })
    output$probUI  <- renderUI({ mmstat.ui.call("prob") })
    output$cexUI   <- renderUI({ mmstat.ui.call("cex") })
    output$sixesUI <- renderUI({ mmstat.ui.call("sixes") })

    output$distPlot <- renderPlot({
      inp <- mmstat.getValues(NULL, cex = input$cex, prob = input$prob, rolls = input$rolls)
      # adapt prob
      inp$prob <- eval(parse(text=mmstat.get("UI")$prob$ticks[inp$prob]))
      #
      t  <- 0:inp$rolls
      w0 <- stats::dbinom(t, inp$rolls, 1 / 6)
      w1 <- ddbinom(t, inp$rolls, inp$prob)
      #
      mmcol <- mmstat.get("col")
      mp <- graphics::barplot(rbind(w1, w0), main = gettext("P(Number of sixes | ... die)"), ylim = c(0, 1), axes = F,
                              col = c(mmcol[[2]], mmcol[[1]]), beside = T,
                              cex.axis = inp$cex, cex.lab = inp$cex, cex.main = 1.2 * inp$cex, cex.sub = inp$cex)
      graphics::legend("topright", gettext(c("loaded die (W=1)", "fair die (W=0)")), cex = inp$cex, fill = c(mmcol[[2]],
                                                                                                             mmcol[[1]]), )
      mp <- colMeans(mp)
      graphics::axis(1, at = mp, labels = sprintf("%.0f", t), cex.axis = inp$cex)
      graphics::axis(2, at = (0:5)/5, labels = sprintf("%.1f", (0:5)/5), cex.axis = inp$cex)
      box()
    })

    output$formula <- renderUI({
      inp <- mmstat.getValues(NULL, cex = input$cex, prob = input$prob, sixes = input$sixes, rolls = input$rolls)
      t  <- 0:inp$rolls
      w0 <- stats::dbinom(t, inp$rolls, 1 / 6)
      w1 <- ddbinom(t, inp$rolls, inp$prob)
      p1 <- w1[1 + inp$sixes] / (w1[1 + inp$sixes] + w0[1 + inp$sixes])
      p0 <- w0[1 + inp$sixes] / (w1[1 + inp$sixes] + w0[1 + inp$sixes])
      mmcol <- mmstat.get("col")
      withMathJax('Assume you have either a loaded die (W=1) or a fair die (W=0). What is the probability of having a loaded',
                  sprintf('or fair die if we have rolled %.0f sixes on %.f rolls?', inp$sixes, inp$rolls),
                  sprintf('$$\\color{%s}{\\scriptsize{', mmcol[[2]]),
                  sprintf('P(W=1|X=%.0f)=', inp$sixes),
                  sprintf('\\frac{P(X=%.0f|W=1)P(W=1)}{P(X=%.0f|W=0)P(W=0)+P(X=%.0f|W=1)P(W=1)}=',
                          inp$sixes, inp$sixes, inp$sixes),
                  sprintf('\\frac{%.3f\\cdot 0.5}{%.3f\\cdot 0.5+%.3f\\cdot 0.5}=',
                          w1[1 + inp$sixes], w0[1 + inp$sixes], w1[1 + inp$sixes]),
                  sprintf('%0.3f', p1),
                  '}}$$',
                  sprintf('$$\\color{%s}{\\scriptsize{', mmcol[[1]]),
                  sprintf('P(W=0|X=%.0f)=', inp$sixes),
                  sprintf('\\frac{P(X=%.0f|W=0)P(W=0)}{P(X=%.0f|W=0)P(W=0)+P(X=%.0f|W=1)P(W=1)}=',
                          inp$sixes, inp$sixes, inp$sixes),
                  sprintf('\\frac{%.3f\\cdot 0.5}{%.3f\\cdot 0.5+%.3f\\cdot 0.5}=',
                          w0[1 + inp$sixes], w0[1 + inp$sixes], w1[1 + inp$sixes]),
                  sprintf('%0.3f', p0),
                  '}}$$'
      )
      #        paste0(
      #          sprintf(
      #            '<table style="font-size:%.0f%%"><tr style="color:%s"><td>',
      #            90 * inp$cex,
      #            mmcol[[2]]
      #          ),
      #          sprintf('P(W=1|X=%.0f)=', inp$sixes),
      #          '</td><td align="center">',
      #          sprintf(
      #            'P(X=%.0f|W=1)*P(W=1)<img src="dorange.png" style="width:100%%;height:1px">P(X=%.0f|W=0)*P(W=0)+P(X=%.0f|W=1)*P(W=1)',
      #            inp$sixes,
      #            inp$sixes,
      #            inp$sixes
      #          ),
      #          '</td><td>=</td><td align="center">',
      #          sprintf(
      #            '%.3f*0.5<img src="dorange.png" style="width:100%%;height:1px">%.3f*0.5+%.3f*0.5',
      #            w1[1 + inp$sixes],
      #            w0[1 + inp$sixes],
      #            w1[1 + inp$sixes]
      #          ),
      #          '</td><td>=</td><td align="center">',
      #          sprintf('%0.3f', p1),
      #          '</td></tr><tr><td><br><br></td></tr>',
      #          sprintf('<tr style="color:%s"><td>', mmcol[[1]]),
      #          sprintf('P(W=0|X=%.0f)=', inp$sixes),
      #          '</td><td align="center">',
      #          sprintf(
      #            'P(X=%.0f|W=0)*P(W=0)<img src="daquamarine.png" style="width:100%%;height:1px">P(X=%.0f|W=0)*P(W=0)+P(X=%.0f|W=1)*P(W=1)',
      #            inp$sixes,
      #            inp$sixes,
      #            inp$sixes
      #          ),
      #          '</td><td>=</td><td align="center">',
      #          sprintf(
      #            '%.3f*0.5<img src="daquamarine.png" style="width:100%%;height:1px">%.3f*0.5+%.3f*0.5',
      #            w0[1 + inp$sixes],
      #            w0[1 + inp$sixes],
      #            w1[1 + inp$sixes]
      #          ),
      #          '</td><td>=</td><td align="center">',
      #          sprintf('%0.3f', p0),
      #          '</td></tr><tr><td><br><br></td></tr></table>'
      #        )
    })

    output$logText <- renderText({ mmstat.getLog(session) })
  },
  onStart = function() {
    oldpar <- par(no.readonly = TRUE)
    onStop(function() { resetpar(oldpar) })
  }
)
