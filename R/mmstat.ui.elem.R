#' @title mmstat.ui.elem
#' @description Adds a new UI element to the app interface. The following types from Shiny are allowed:
#' * \code{\link[shiny:actionButton]{actionButton}},
#' * \code{\link[shiny:checkboxInput]{checkboxInput}},
#' * \code{\link[shiny:checkboxGroupInput]{checkboxGroupInput}},
#' * \code{\link[shiny:dateInput]{dateInput}},
#' * \code{\link[shiny:dateRangeInput]{dateRangeInput}},
#' * \code{\link[shiny:fileInput]{fileInput}},
#' * \code{\link[shiny:helpText]{helpText}},
#' * \code{\link[shiny:numericInput]{numericInput}},
#' * \code{\link[shiny:radioButtons]{radioButtons}},
#' * \code{\link[shiny:selectInput]{selectInput}},
#' * \code{\link[shiny:sliderInput]{sliderInput}},
#' * \code{\link[shiny:submitButton]{submitButton}}, and
#' * \code{\link[shiny:textInput]{textInput}}.
#'
#' Additionally some standard statistical UI elements are supported (links go to the Shiny element used):
#'
#' * \code{\link[shiny:sliderInput]{sampleSize}},
#' * \code{\link[shiny:actionButton]{drawSample}},
#' * \code{\link[shiny:sliderInput]{speedSlider}},
#' * \code{\link[shiny:sliderInput]{confidenceLevel}},
#' * \code{\link[shiny:sliderInput]{significance}},
#' * \code{\link[shiny:radioButtons]{testHypotheses}},
#' * \code{\link[shiny:selectInput]{dataSet}},
#' * \code{\link[shiny:selectInput]{variable1}},
#' * \code{\link[shiny:selectInput]{variableN}}, and
#' * \code{\link[shiny:sliderInput]{fontSize}}.
#'
#' Partially these elements have default settings which can be overwritten.
#'
#' @param inputId character: input slot that will be used to access the value
#' @param type character: element type
#' @param ... further named parameter to Shiny UI elements
#'
#' @return nothing
#' @export
#'
#' @examples
#' mmstat.ui.elem(inputId="alpha", type="significance")
mmstat.ui.elem <- function (inputId, type, ...) {
  found        <- FALSE
  elem         <- list(...)
  elem$inputId <- inputId
  elem$type    <- as.character(substitute(type))
  shinytypes   <- c('actionButton', 'checkboxInput', 'checkboxGroupInput', 'dateInput', 'dateRangeInput', 'fileInput',
                    'helpText', 'numericInput', 'radioButtons', 'selectInput', 'sliderInput',
                    'submitButton', 'textInput')
  mmstattypes  <- c('sampleSize', 'drawSample', 'speedSlider',
                    'confidenceLevel',
                    'significance', 'testHypotheses',
                    'dataSet', 'variable1', 'variableN',
                    'fontSize',
                    'probability',
                    'actionGroupButtons')
  pos <- pmatch(elem$type, shinytypes)
  if (!is.na(pos)) {
    if (elem$type=='actionButton') {
      if (is.null(elem$value)) elem$value <- 0
    }
    elem$call <- shinytypes[pos]
  } else {
    pos <- pmatch(elem$type, mmstattypes)
    stopif(is.na(pos), sprintf('mmstat.ui.elem: Type "%s" unknown', elem$type))
    pos <- which(names(mmstat$fun)==elem$type)
    if (length(pos)==1) {
      nparam <- names(mmstat$fun[[pos]])
      for (i in seq(length(nparam))) {
        if (is.null(elem[[nparam[i]]])) {
          elem[[nparam[i]]] <- mmstat$fun[[pos]][[nparam[i]]]
          if (inherits(elem[[nparam[i]]], "expression")) elem[[nparam[i]]] <- eval(elem[[nparam[i]]], envir=list2env(elem))
        }
      }
    }
    #      if (is.null(elem$label)) elem$label <- gettext("Sample size (n)")
    #      elem$call   <- 'mmstat.sliderInput'
    #      elem$update <- 'updateSliderInput'
    #      elem$step   <- 1
    #      elem$min    <- 1
    #      elem$ticks  <- TRUE
    #      #elem$value  <- 1
    #      if (is.null(elem$value)) elem$value <- as.numeric(utils::compareVersion(mmstat$shiny, "0.11")<0)
    #    }
    #    if (elem$type=='drawSample') { # button to draw a new sample
    #      elem$call  <- 'actionButton'
    #      if (is.null(elem$label)) elem$label <- gettext("Draw sample")
    #      if (is.null(elem$value)) elem$value <- 0
    #    }
    #    if (elem$type=='testHypotheses') { # radiobuttons for two and one-sided tests
    #      elem$call  <- 'radioButtons'
    #      if (is.null(elem$label)) elem$label <- gettext("Choose test type")
    #      elem$choices = gettext(c("two.sided", "less", "greater"), "name")
    #      if (is.null(elem$value)) elem$value <- 'two.sided'
    #    }
    #    if (elem$type=='significance') { # special slider for significance level
    #      elem$call   <- 'mmstat.sliderInput'
    #      elem$update <- 'updateSliderInput'
    #      if (is.null(elem$ticks)) elem$ticks <- c(0.1, 0.25, 0.5, 1, 2, 5, 10, 20)
    #      if (is.null(elem$label)) elem$label <- HTML(gettext("Select significance level (&alpha;)"))
    #      elem$step  <- 1
    #      elem$min   <- 1
    #      elem$max   <- length(elem$ticks)
    ##      elem$value <- 6
    #      if (is.null(elem$value)) elem$value <- 6-as.numeric(utils::compareVersion(mmstat$shiny, "0.11")>=0)
    #    }
    #    if (elem$type=='confidenceLevel')  { # special slider for confidence level
    #      elem$call   <- 'mmstat.sliderInput'
    #      elem$update <- 'updateSliderInput'
    #      if (is.null(elem$ticks)) elem$ticks <- c(80, 85, 90, 95, 98, 99, 99.5, 99.9)
    #      if (is.null(elem$label)) elem$label <- HTML(gettext("Select confidence level (1-&alpha;)"))
    #      elem$step  <- 1
    #      elem$min   <- 1
    #      elem$max   <- length(elem$ticks)
    #      elem$value <- 4
    #      if (is.null(elem$value)) elem$value <- 4-as.numeric(utils::compareVersion(mmstat$shiny, "0.11")>=0)
    #    }
    #    if (elem$type=='dataSet') { # selectInput to select between datasets
    #      elem$call  <- 'selectInput'
    #      if (is.null(elem$label))    elem$label    <- gettext("Select a data set")
    #      if (is.null(elem$choices))  elem$choices  <- mmstat.getDataNames(gsub('.rds$', '', list.files(pattern='*.rds')))
    #      if (is.null(elem$selected)) elem$selected <- mmstat.getDataNames()
    #    }
    #    if (elem$type=='variable1') { # selectInput to select one variable
    #      elem$call  <- 'selectInput'
    #      if (is.null(elem$label))    elem$label    <- gettext("Select a variable")
    #      if (is.null(elem$choices))  elem$choices  <- mmstat.getVarNames(1, elem$vartype)
    #      if (is.null(elem$selected)) elem$selected <- mmstat.getVarNames(1, elem$vartype, 1)
    #    }
    #    if (elem$type=='variableN') { # selectInput to select one variable
    #      elem$call     <- 'selectInput'
    #      elem$multiple <- TRUE
    #      if (is.null(elem$label))   elem$label   <- gettext("Select variable(s)")
    #      if (is.null(elem$choices)) elem$choices <- mmstat.getVarNames(1, elem$vartype)
    #    }
    #    if (elem$type=='fontSize') { # sliderInput for choosing font size
    #      elem$call   <- 'mmstat.sliderInput'
    #      elem$update <- 'updateSliderInput'
    #      if (is.null(elem$label)) elem$label <- gettext("Font size")
    #      if (is.null(elem$min))   elem$min   <- 1
    #      if (is.null(elem$max))   elem$max   <- 1.5
    #      if (is.null(elem$step))  elem$step  <- 0.05
    #      if (is.null(elem$value)) elem$value <- elem$min
    #    }
    #    if (elem$type=='speedSlider') { # sliderInput for choosing font size
    #      elem$call   <- 'mmstat.sliderInput'
    #      elem$update <- 'updateSliderInput'
    #      if (is.null(elem$label)) elem$label <- list(NULL)
    #      if (is.null(elem$min))   elem$min   <- 0
    #      if (is.null(elem$max))   elem$max   <- 5
    #      if (is.null(elem$step))  elem$step  <- 1
    #      if (is.null(elem$value)) elem$value <- elem$min
    #    }
    #    if (elem$type=='actionGroupButtons') { # actionGroupButtons from shinyWidgets
    #      elem$call     <- 'actionGroupButtons'
    #      elem$update   <- NULL
    #      if (is.null(elem$labels))    elem$labels    <- as.character(seq(elem$inputIds))
    #      if (is.null(elem$status))    elem$status    <- "default"
    #      if (is.null(elem$size))      elem$size      <- "normal"
    #      if (is.null(elem$direction)) elem$direction <- "horizontal"
    #      if (is.null(elem$fullwidth)) elem$fullwidth <- FALSE
    #    }
    #  }
  }
  if (is.null(elem$update)) {
    if (elem$type!='actionGroupButtons') elem$update <- paste0('update', ucfirst(elem$call))
  }
  mmstat$UI[[inputId]] <- elem
}
