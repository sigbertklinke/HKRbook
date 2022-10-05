#' men_corr
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
if (is.null(files)) files <- mmstat.rds("USCRIME", "CARS", "DECATHLON")
#
mmstat.set(vartype = 'numeric', UI=NULL, dataset=NULL, last.permute=0, ct=0, coeff=NULL) # reset everything
mmstat.ui.elem(
  "graph",
  'radioButtons',
  label    = gettext("Select a scatterplot type"),
  choices  = gettext(
    c("2D.SCATTERPLOT", "3D.SCATTERPLOT", "SCATTERPLOTMATRIX"),
    "name"
  ),
  selected = "2D.SCATTERPLOT"
)
mmstat.ui.elem("permute", 'actionButton',
               label    = gettext("Cycle axes"))
mmstat.ui.elem("coeff", "checkboxGroupInput", label = gettext("Show coefficient(s)"),
               choices = gettext(c("SHOW.BRAVAIS.PEARSON", "SHOW.SPEARMAN"), "name"), value = character())
mmstat.ui.elem("dataset", "dataSet", choices = mmstat.getDataNames(files))
mmstat.ui.elem("variableSelect", "variableN", vartype = "numeric", selected = mmstat.getVarNames(1, vartype = "numeric", which=1:2))
mmstat.ui.elem("cex",        "fontSize")

buildList <- function(depth, v) {
  #stopif(depth>=length(v), "Recursion error")
  if (depth) {
    ret <- buildList(depth - 1, v)
    mat <- matrix(NA,
                  nrow = length(v) * nrow(ret),
                  ncol = ncol(ret) + 1)
    for (i in seq(v)) {
      ind1 <- (1 + (i - 1) * nrow(ret)):(i * nrow(ret))
      ind2 <- 1 + (1:ncol(ret))
      mat[ind1, ind2] <- ret
      mat[(1 + (i - 1) * nrow(ret)):(i * nrow(ret)), 1] <- v[i]
    }
    ind <- apply(mat, 1, function(x, m) {
        length(unique(x)) == m
    }, m = depth + 1)
    return(mat[ind, ])
  }
  return(as.matrix(v, ncol = 1))
}

panel.cor <- function(x, y, cex) {
  coeff <- mmstat.get("coeff")
  nc <- length(coeff)
  if (nc) {
    usr    <- graphics::par("usr")
    method <- c("SHOW.BRAVAIS.PEARSON"="pearson", "SHOW.SPEARMAN"="spearman")
    prefix <- c("SHOW.BRAVAIS.PEARSON"="r_xy", "SHOW.SPEARMAN"="r_s")
    graphics::par(usr = c(0, 1, 0, 1))
    r <- rep(NA, nc)
    for (i in 1:nc) r[i] <- stats::cor(x, y, method=method[coeff[i]])
    rt  <- sprintf("%s=%+.3f", prefix[coeff], r)
    yt  <- (1:nc)/(nc+1)
    graphics::text(0.5, yt, rt, cex = cex)
  } else {
    graphics::points(x, y, cex = 0.75, pch = 19)
  }
}

#ct           <- 0
#last.permute <- 0

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "MM*Stat",
                    leftUi = tagList(
                      dropdownBlock(
                        id    = "data",
                        title = gettext("Data choice"),
                        badgeStatus = NULL,
                        uiOutput("variableSelectUI"),
                        uiOutput("datasetUI")
                      ),
                      dropdownBlock(
                        id    = "options",
                        title = gettext("Options"),
                        badgeStatus = NULL,
                        uiOutput("cexUI")
                      ))),
    dashboardSidebar(minified=FALSE,
                     uiOutput("graphUI"),
                     uiOutput("permuteUI"),
                     uiOutput("coeffUI")
    ),
    dashboardBody( fluidRow(column(
      width = 12,
      box(width=12,
        title = gettext("Scatterplots and correlation"),
        status = "primary",
        plotOutput("scatterPlot")
      ))))),
  server = function(input, output, session) {
    output$graphUI   <- renderUI({ mmstat.ui.call("graph") })
    output$coeffUI   <- renderUI({ mmstat.ui.call("coeff") })
    output$permuteUI <- renderUI({ mmstat.ui.call("permute") })
    output$datasetUI <- renderUI({ mmstat.ui.call("dataset") })
    output$cexUI     <- renderUI({ mmstat.ui.call("cex") })

    output$variableSelectUI <- renderUI({
      inp  <- mmstat.getValues(NULL, dataset = input$dataset)
      ret <- mmstat.ui.call('variableSelect',
                            choices = mmstat.getVarNames(inp$dataset, 'numeric'))
      ret
    })

    #  output$variableSelectUI <- renderUI({
    #    mmstat.log('variableSelectUI')
    #    inp  <- mmstat.getValues(NULL, dataset=input$dataset, variableSelect=isolate(input$variableSelect))
    #    args <- mmstat$UI$variableSelect
    #    args$value   <- NULL
    #    args$choices <- gettext(mmstat$dataset[[inp$dataset]]$numvars, "name")
    #    do.call('selectInput', args)
    #  })

    observe({
      inp = mmstat.getValues(NULL, dataset = input$dataset)
      updateSelectInput(
        session,
        "variableSelect",
        choices = mmstat.getVarNames(inp$dataset, 'numeric'),
        selected = mmstat.getVarNames(inp$dataset, 'numeric')[1:2]
      )
    })

    cycleList <- reactive({
      inp <- mmstat.getValues(NULL,
                              graph = input$graph,
                              variableSelect = input$variableSelect)
      maxdepth <- length(inp$variableSelect)
      if (inp$graph == "2D.SCATTERPLOT") maxdepth <- 2
      if (inp$graph == "3D.SCATTERPLOT") maxdepth <- 3
      if (inp$graph == "SCATTERPLOTMATRIX") maxdepth <- min(maxdepth, 6)
      return (buildList(maxdepth - 1, inp$variableSelect))
    })

    output$scatterPlot <- renderPlot({
      inp <- mmstat.getValues(
        NULL,
        dataset = input$dataset,
        graph = input$graph,
        permute = input$permute,
        coeff = input$coeff,
        variableSelect = input$variableSelect,
        cex = input$cex
      )
      last.permute <- mmstat.get("last.permute")
      ct           <- mmstat.get("ct")
      coeff        <- mmstat.get("coeff")
      clist <- cycleList()
      if (nrow(clist) == 0) {
        plot(
          0,
          0,
          xlim = c(-0.1, 0.1),
          ylim = c(-0.1, 0.1),
          type = "n",
          axes = F,
          xlab = "",
          ylab = "",
          pch = 19,
          cex = 0.5
        )
        graphics::text(0, 0, gettext("Please select two, three or more variables!"), cex = 1.5 * inp$cex)
      } else {
        if (inp$permute > last.permute) {
          # button was pressed
          last.permute <- inp$permute
          ct <- ct + 1
        }
        ct <- ct %% nrow(clist)
        varx <- mmstat.getVar(isolate(inp$dataset), varname = clist[ct + 1, 1], na.action = stats::na.pass)
        if (varx$name == clist[ct + 1, 1]) {
          if (inp$graph == '2D.SCATTERPLOT') {
            vary <- mmstat.getVar(inp$dataset, varname = clist[ct + 1, 2], na.action = stats::na.pass)
            plot (
              varx$values,
              vary$values,
              xlab = gettext(clist[ct + 1, 1]),
              ylab = gettext(clist[ct + 1, 2]),
              cex.axis = inp$cex,
              cex.lab = inp$cex,
              cex.main = inp$cex,
              cex.sub = inp$cex,
              pch = 19,
              sub = varx$sub
            )
            if (length(inp$coeff)) {
              main <- rep('', length(inp$coeff))
              for (i in seq(inp$coeff)) {
                if (inp$coeff[i] == "SHOW.BRAVAIS.PEARSON") {
                  main[i] <- sprintf(" &r[xy];=%+.2f", stats::cor(varx$values, vary$values, use = "c"))
                }
                if (inp$coeff[i] == "SHOW.SPEARMAN") {
                  main[i] <- sprintf(" &r[s];=%+.2f", stats::cor(varx$values, vary$values, use = "c", method = "s"))
                }
              }
              graphics::title(main = mmstat.math(paste(main, collapse = ", ")))
              #("topright", c(legend1, legend2), ncol=2)
            }
          }
          if (inp$graph == '3D.SCATTERPLOT') {
            vary <- mmstat.getVar(isolate(inp$dataset), varname = clist[ct + 1, 2], na.action = stats::na.pass)
            varz <- mmstat.getVar(isolate(inp$dataset), varname = clist[ct + 1, 3], na.action = stats::na.pass)
            scatterplot3d (
              varx$values,
              vary$values,
              varz$values,
              pch = 19,
              #                cex.symbols = 0.75,
              cex.axis = 0.8 * inp$cex,
              cex.lab = inp$cex,
              cex.main = inp$cex,
              highlight.3d = T,
              xlab = gettext(clist[ct + 1, 1]),
              ylab = gettext(clist[ct + 1, 2]),
              zlab = gettext(clist[ct + 1, 3]),
              sub = varx$sub
            )
            if (length(inp$coeff))
              graphics::title(main = gettext("Coefficients are not available for 3D Scatterplot"))
          }
          if (inp$graph == 'SCATTERPLOTMATRIX') {
            dat <- data.frame(varx$values)
            for (i in 2:length(clist[ct + 1, ])) {
              var <- mmstat.getVar(inp$dataset, varname = clist[ct + 1, i], na.action = stats::na.pass)
              dat[, i] <- var$values
            }
            colnames(dat) <- gettext(clist[ct + 1, ])
            coeff  <- inp$coeff
            graphics::pairs(as.matrix(dat), labels=colnames(dat),
                            cex.labels=inp$cex, cex=inp$cex,
                            upper.panel=panel.cor)
            #splom(
            #  ~ dat,
            #  pch = 19,
            #  col = "black",
            #  as.matrix = T,
            #  cex = 1 / sqrt(ncol(dat)),
            #  varname.cex = 1.5 * inp$cex / inp$cex / (ncol(dat) ^ (1 / 3)),
            #  axis.text.cex = ifelse(ncol(dat) < 5, inp$cex / (ncol(dat) ^ (1 / 3)), 0),
            #  coeff = inp$coeff,
            #  cex.coeff = 1.5 * inp$cex / (ncol(dat) ^ (1 / 3)),
            #  sub = varx$sub,
            #  panel = function(x,
            #                   y,
            #                   i,
            #                   j,
            #                   cex,
            #                   coeff,
            #                   cex.coeff,
            #                   ...) {
            #    if (length(coeff)) {
            #      if (i > j) {
            #        graphics::points(x,
            #                     y,
            #                     cex = cex,
            #                     pch = 19,
            #                     col = "black")
            #      } else {
            #        main <- rep('', length(inp$coeff))
            #        for (i in seq(inp$coeff)) {
            #          if (inp$coeff[i] == "SHOW.BRAVAIS.PEARSON") {
            #            main[i] <- sprintf(" &r[xy];=%+.2f", stats::cor(x, y, use = "c"))
            #          }
            #          if (inp$coeff[i] == "SHOW.SPEARMAN") {
            #            main[i] <- sprintf(" &r[s];=%+.2f",
            #                               stats::cor(x, y, use = "c", method = "s"))
            #          }
            #        }
            #        xpos <- mean(range(x))
            #        ypos <-
            #          min(y) + diff(range(y)) * c(1, 3, 5) / (2 * length(coeff))
            #        for (i in seq(main))
            #          panel.text(xpos, ypos[i], mmstat.math(main[i]), cex = cex.coeff)
            #      }
            #    } else {
            #      graphics::points(x,
            #                   y,
            #                   cex = cex,
            #                   pch = 19,
            #                   col = "black")
            #    }
            #  }
            #)
          }
        }
      }
      mmstat.set(last.permute=last.permute, ct=ct, coeff=coeff)
    })

    output$logText <- renderText({ mmstat.getLog(session) })
  },
  onStart = function() {
    oldpar <- par(no.readonly = TRUE)
    onStop(function() { resetpar(oldpar) })
  }
)
