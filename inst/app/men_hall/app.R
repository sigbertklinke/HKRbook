#' men_hall
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
pointdoor <- mmstat.getValue(param['pointdoor'], 1)
afteropen <- mmstat.getValue(param['afteropen'], 1)
addResourcePath('www', system.file('www', package='HKRbook'))
pdc <-
  gettext(c(
    "to left door",
    "to middle door",
    "to right door",
    "randomly to a door"
  ),
  "num")
aoc <- gettext(c('keep door', 'change door'), "num")
stopifnot(pointdoor %in% seq(length(pdc)))
stopifnot(afteropen %in% seq(length(aoc)))

mmstat.ui.elem("pointdoor", "radioButtons", selected = pointdoor, label = gettext("1. Guest points"), choices = pdc)
mmstat.ui.elem("afteropen", "radioButtons", selected = afteropen, label = gettext("3. Guest decides to"), choices = aoc)
mmstat.ui.elem("guest", "actionButton", label = gettext("Guest"), style = "display:inline-block")
mmstat.ui.elem("host", "actionButton", label = gettext("Host"), style = "display:inline-block")
mmstat.ui.elem("speed", "speedSlider")
mmstat.ui.elem("cex", "fontSize")

ndoor <- 3

points <- function(deal) {
  out <- rep('/www/Empty.png', 3)
  if (!is.na(deal$guest))
    out[deal$guest] <- '/www/PointTo.png'
  out
}

doors <- function(deal) {
  out <- rep('/www/GoatClose.png', 3)
  if (!is.na(deal$price))
    out[deal$price] <- '/www/CarClose.png'
  if (!is.na(deal$host))
    out[deal$host] <- '/www/GoatOpen.png'
  if (!is.na(deal$final)) {
    out[deal$final] <-
      if (deal$final == deal$price)
        '/www/CarOpen.png'
    else
      '/www/GoatOpen.png'
  }
  out
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

                     uiOutput("pointdoorUI"),
                     htmlOutput("hostopensUI"),
                     uiOutput("afteropenUI"),
                     uiOutput("guestUI", inline = TRUE),
                     uiOutput("hostUI", inline = TRUE),
                     uiOutput("speedUI")
    ),
    dashboardBody( fluidRow(column(
      width = 12,
      box(width=12,
        title = gettext("Let's Make a Deal"),
        status = "primary",
        plotOutput("distPlot"),
        htmlOutput('imagePlot')
      ))))),
  server = function(input, output, session) {

    output$hostUI <- renderUI({ mmstat.ui.call("host") })
    output$guestUI     <- renderUI({ mmstat.ui.call("guest") })
    output$cexUI       <- renderUI({ mmstat.ui.call("cex") })
    output$pointdoorUI <- renderUI({ mmstat.ui.call("pointdoor") })
    output$afteropenUI <- renderUI({ mmstat.ui.call("afteropen") })
    output$speedUI     <- renderUI({ mmstat.ui.call("speed") })
    output$hostopensUI <- renderUI({ HTML(paste(HTML('&nbsp;&nbsp;&nbsp;'), tags$b(gettext("2. Host opens door with a goat")))) })

    deal <- reactiveValues(success = logical(0), price = NA, guest = NA, host = NA, final = NA, trial = 0)
    # price: the door number where the price is
    # guest: the door number the guest first points to
    # host:  the door number which the show master opens
    # final: the door number where guest finally points to
    observeEvent(input$host, {
      isolate({
        door      <- setdiff(1:ndoor, c(deal$price, deal$guest))
        deal$host <- if (length(door) == 1)
          door
        else
          sample(door, 1)
        if (input$afteropen == 1)
          deal$final <- deal$guest
        if (input$afteropen == 2)
          deal$final <- setdiff(1:ndoor, c(deal$host, deal$guest))
        deal$success[deal$trial] <- (deal$final == deal$price)
      })
    })

    observeEvent(input$guest, {
      isolate({
        inp    <- mmstat.getValues(NULL, pointdoor = input$pointdoor)
        deal$trial <- deal$trial + 1
        deal$price <- sample(ndoor, 1)
        if (inp$pointdoor == 1)
          deal$guest <- 1
        if (inp$pointdoor == 2)
          deal$guest <- 2
        if (inp$pointdoor == 3)
          deal$guest <- 3
        if (inp$pointdoor == 4)
          deal$guest <- sample(3, 1)
        deal$host  <- NA
        deal$final <- NA
      })
    })

    observeEvent(input$pointdoor, {
      deal$success <- logical(0)
      deal$price   <- NA
      deal$guest   <- NA
      deal$host    <- NA
      deal$final   <- NA
      deal$trial   <- 0
    })

    observeEvent(input$afteropen, {
      deal$success <- logical(0)
      deal$price   <- NA
      deal$guest   <- NA
      deal$host    <- NA
      deal$final   <- NA
      deal$trial   <- 0
    })

    observe({
      inp    <-
        mmstat.getValues(
          NULL,
          speed = input$speed,
          pointdoor = input$pointdoor,
          afteropen = input$afteropen
        )
      if (inp$speed > 0) {
        # guest
        isolate({
          deal$trial <- deal$trial + 1
          deal$price <- sample(ndoor, 1)
          if (inp$pointdoor == 1)
            deal$guest <- 1
          if (inp$pointdoor == 2)
            deal$guest <- 2
          if (inp$pointdoor == 3)
            deal$guest <- 3
          if (inp$pointdoor == 4)
            deal$guest <- sample(3, 1)
          # host
          door      <- setdiff(1:ndoor, c(deal$price, deal$guest))
          deal$host <- if (length(door) == 1)
            door
          else
            sample(door, 1)
          if (input$afteropen == 1)
            deal$final <- deal$guest
          if (input$afteropen == 2)
            deal$final <- setdiff(1:ndoor, c(deal$host, deal$guest))
          deal$success[deal$trial] <- (deal$final == deal$price)
          #
        })
        invalidateLater(2000 / inp$speed, session)
      }
    })

    output$distPlot <- renderPlot({
      input <- mmstat.getValues(NULL, cex = input$cex)
      main <- sprintf(gettext("Trial %.0f"), 1 + length(deal$success))
      plot(
        cumsum(deal$success) / seq(length(deal$success)),
        xlim = c(1, deal$trial),
        ylim = c(0, 1),
        pch = 19,
        xlab = gettext("Trial"),
        ylab = gettext("Relative frequency of successful trials"),
        main = main,
        cex.axis = input$cex,
        cex.lab = input$cex,
        cex.main = 1.2 * input$cex,
        cex.sub = input$cex
      )
      graphics::abline(h = (1:2) / 3, col = "gray20")
    })

    output$imagePlot <- renderText({
      d <- doors(deal)
      p <- points(deal)
      paste0(
        '<table width="100%"><tr align="center"><td width="10%"><h1>',
        deal$trial,
        '.</h1></td><td width="30%"><img src="',
        d[1],
        '"></td><td width="30%"><img src="',
        d[2],
        '"></td><td width="30%"><img src="',
        d[3],
        '"></td></tr><tr align="center"><td width="10%"></td><td width="30%"><img src="',
        p[1],
        '"></td><td width="30%"><img src="',
        p[2],
        '"></td><td width="30%"><img src="',
        p[3],
        '"></td></tr></table>'
      )
    })

    output$logText <- renderText({ mmstat.getLog(session) })
  },
  onStart = function() {
    oldpar <- par(no.readonly = TRUE)
    onStop(function() { resetpar(oldpar) })
  }
)

