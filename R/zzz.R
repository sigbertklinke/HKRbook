mmstat <- new.env()

.onLoad <- function(...) {
  mmstat$debug <- 1
  mmstat$readonly <- c("", "shiny", "col", "alpha", "fun", "readonly")
  mmstat$shiny    <- as.character(utils::packageVersion("shiny"))
  mmstat$col      <- list(daquamarine="#1B9E77",  dorange="#D95F02",   dblue="#7570B3",  dpink="#E7298A",
                          dgreen="#66A61E",       dyellow="#E6AB02", 	dgold="#A6761D", 	dgray="#666666",
                          laquamarine="#66C2A5",  lorange="#FC8D62",   lblue="#8DA0CB",  lpink="#E78AC3",
                          lgreen="#A6D854",     	 lyellow="#FFD92F", 	lgold="#E5C494", 	lgray="#B3B3B3")
  mmstat$alpha    <- c(0.1, 0.25, 0.5, 1, 2.5, 5, 10, 20)
  mmstat$UI       <- NULL
  mmstat$dataset  <- NULL
  mmstat$fun   <- list(sampleSize = list(call   = 'mmstat.sliderInput',
                                         update = 'updateSliderInput',
                                         label  = expression(gettext("Sample size (n)")),
                                         step   = 1,
                                         min    = 1,
                                         value  = 1,
                                         ticks  = TRUE,
                                         max    = expression(length(ticks)),
                                         value  = expression(as.numeric(utils::compareVersion(mmstat$shiny, "0.11")<0))),
                       drawSample = list(call   = 'actionButton',
                                         label  = expression(gettext("Draw sample")),
                                         value  = 0),
                       testHypotheses = list(call    = 'radioButtons',
                                             label   = expression(gettext("Choose test type")),
                                             choices = expression(gettext(c("two.sided", "less", "greater"), "name")),
                                             value   = 'two.sided'),
                       significance = list(call   =  'mmstat.sliderInput',
                                           update = 'updateSliderInput',
                                           ticks  = expression(c(0.1, 0.25, 0.5, 1, 2, 5, 10, 20)),
                                           label  = expression(HTML(gettext("Select significance level (&alpha;)"))),
                                           step   = 1,
                                           min    = 1,
                                           max    = expression(length(elem$ticks)),
                                           value  = expression(6-as.numeric(utils::compareVersion(mmstat$shiny, "0.11")>=0))),
                        confidenceLevel = list(call   = 'mmstat.sliderInput',
                                               update = 'updateSliderInput',
                                               ticks  = expression(c(80, 85, 90, 95, 98, 99, 99.5, 99.9)),
                                               label  = expression(HTML(gettext("Select confidence level (1-&alpha;)"))),
                                               step   = 1,
                                               min    = 1,
                                               max    = expression(length(elem$ticks)),
                                               value  = expression(4-as.numeric(utils::compareVersion(mmstat$shiny, "0.11")>=0))),
                        dataSet = list(call     = 'selectInput',
                                       label    = expression(gettext("Select a data set")),
                                       choices  = expression(mmstat.getDataNames(gsub('.rds$', '', list.files(pattern='*.rds')))),
                                       selected = expression(mmstat.getDataNames())),
                        variable1 = list(call     = 'selectInput',
                                         label    = expression(gettext("Select a variable")),
                                         choices  = expression(mmstat.getVarNames(1, elem$vartype)),
                                         selected = expression(mmstat.getVarNames(1, elem$vartype, 1))),
                        variableN = list(call     = 'selectInput',
                                         multiple = TRUE,
                                         label    = expression(gettext("Select variable(s)")),
                                         choices  = expression(mmstat.getVarNames(1, elem$vartype))),
                        fontSize = list(call   = 'mmstat.sliderInput',
                                        update = 'updateSliderInput',
                                        label  = expression(gettext("Font size")),
                                        min    = 1,
                                        max    = 1.5,
                                        step   = 0.05,
                                        value  = expression(min)),
                        speedSlider = list(call   = 'mmstat.sliderInput',
                                           update = 'updateSliderInput',
                                           label  = expression(list(NULL)),
                                           min    = 0,
                                           max    = 5,
                                           step   = 1,
                                           value  = expression(min)),
                        actionGroupButtons = list(call      = 'actionGroupButtons',
                                                  update    = NULL,
                                                  labels    = expression(as.character(seq(inputIds))),
                                                  status    = "default",
                                                  size      = "normal",
                                                  direction = "horizontal",
                                                  fullwidth = FALSE),
                       probability        =  list(call   = 'mmstat.sliderInput',
                                                  update = 'updateSliderInput',
                                                  label  = expression(HTML(gettext("Select confidence level (1-&alpha;)"))),
                                                  step   = 1/6,
                                                  min    = 0,
                                                  max    = 1,
                                                  ticks  = expression(as.character(MASS::fractions(seq(min, max, by=step)))),
                                                  value  = expression(as.numeric(utils::compareVersion(mmstat$shiny, "0.11")>=0)))
  )
  mmstat.log("Starting app")
  mmstat.lang()
  Sys.setlocale("LC_ALL", gettext("LC_ALL"))
}

#' zzz
#'
#' Checks if all necessary packages are installed.
#'
#' @return a logical vector which of the required packages are available
#' @importFrom highlight header_html
#' @importFrom formatR tidy_source
#' @importFrom DT tableHeader
#' @importFrom utils capture.output
#' @export
#'
#' @examples
#' zzz()
zzz <- function() {
  h <- header_html(document= FALSE)              # highlight
  t <- capture.output(tidy_source(text = "1+1")) # formatR
  t <- tableHeader(1:2)                          # DT
  pkgs <- c("highlight", "formatR", "shiny", "shinydashboard", "shinydashboardPlus", "DT")
  ret <- structure(rep(FALSE, length(pkgs)), names=pkgs)
  for (pkg in pkgs) ret[pkg] <- nzchar(system.file(package=pkg))
  ret
}
