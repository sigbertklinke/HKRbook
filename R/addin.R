#' reformatAddin
#'
#' Reformats selected text with formatR
#'
#' @importFrom utils adist
#' @export
reformatAddin <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  # For the first selection only
  sel      <- context$selection[[1]]
  blanks   <- gsub("\\S.*$", "", sel$text)
  old_text <- sel$text
  Encoding(old_text) <- "UTF-8"
  #
  fmtsrc   <- formatR::tidy_source (text=old_text,
                                    width.cutoff=110-nchar(blanks),
                                    output=FALSE)
  #
  new_text <- paste0(blanks, unlist(strsplit(fmtsrc$text.tidy, split = "\n", fixed=TRUE)), collapse="\n")
  rstudioapi::insertText(location = sel$range,
                         text = as.character(new_text),
                         id = context$id)
}

#' onelineAddin
#'
#' Each top level expression will become a single line
#'
#' @export
onelineAddin <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  # For the first selection only
  sel      <- context$selection[[1]]
  blanks   <- gsub("\\S.*$", "", sel$text)
  old_text <- sel$text
  Encoding(old_text) <- "UTF-8"
  #
  expr <- try(parse(text=old_text), silent = TRUE)
  if ("try-error" %in% class(expr)) return()
  new_text <- c()
  for (e in expr) new_text <- c(new_text, paste0(blanks, paste(trimws(as.character(deparse(e))), collapse=" ")))
  #
  rstudioapi::insertText(location = sel$range,
                         text = paste0(new_text, collapse="\n"),
                         id = context$id)
}

#' parseAddin
#'
#' Parses the selected code
#'
#' @export
parseAddin <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  parse(text=context$selection[[1]])
}

#' elemAddin
#'
#' Adds an mmstat.ui.elem call if possible.
#'
#' @export
elemAddin <- function() {
  makeParams <- function(pl, subset=NULL) {
    if (!is.null(subset)) pl <- pl[subset]
    apos        <- ifelse(sapply(pl, class)=="character", '"', '')
    paramv      <- ifelse(sapply(pl, class)=="NULL", "NULL", sapply(pl, as.character))
    paste0(names(pl), " =  ", apos, paramv, apos, collapse=", ")
  }
  #
  breakLineAtComma <- function(txt, width=100, prefix='') {
    txt <- strsplit(txt, ',', fixed=TRUE)
    for (i in seq(length(txt))) {
       txti <- NULL
       while (length(txt[[i]])) {
         ni  <- cumsum(nchar(txt[[i]]))
         pos <- which(ni<width)
         if (length(pos)<2) {
           txti     <- c(txti, txt[[i]][1])
           txt[[i]] <- txt[[i]][-1]
         } else {
           txti <- c(txti, paste0(txt[[i]][pos], collapse=","))
           txt[[i]] <- txt[[i]][-pos]
         }
       }
       pre      <- ifelse(seq(length(txti))==1, '', prefix)
       txt[[i]] <- paste0(pre, txti, collapse=",\n")
    }
    paste0(unlist(txt), collapse="\n")
  }
  #
  types <- list('actionButton'       = c(shiny::actionButton),
                'checkboxInput'      = c(shiny::checkboxInput),
                'checkboxGroupInput' = c(shiny::checkboxGroupInput),
                'dateInput'          = c(shiny::dateInput),
                'dateRangeInput'     = c(shiny::dateRangeInput),
                'fileInput'          = c(shiny::fileInput),
                'helpText'           = c(shiny::helpText),
                'numericInput'       = c(shiny::numericInput),
                'radioButtons'       = c(shiny::radioButtons),
                'selectInput'        = c(shiny::selectInput),
                'sliderInput'        = c(shiny::sliderInput),
                'submitButton'       = c(shiny::submitButton),
                'textInput'          = c(shiny::textInput),
                # HKRbook
                'sampleSize'         = c(HKRbook::mmstat.ui.elem, shiny::sliderInput),
                'drawSample'         = c(HKRbook::mmstat.ui.elem, shiny::actionButton),
                'speedSlider'        = c(HKRbook::mmstat.ui.elem, shiny::sliderInput),
                'confidenceLevel'    = c(HKRbook::mmstat.ui.elem, shiny::sliderInput),
                'significance'       = c(HKRbook::mmstat.ui.elem, shiny::sliderInput),
                'testHypotheses'     = c(HKRbook::mmstat.ui.elem, shiny::radioButtons),
                'dataSet'            = c(HKRbook::mmstat.ui.elem, shiny::selectInput),
                'variable1'          = c(HKRbook::mmstat.ui.elem, shiny::selectInput),
                'variableN'          = c(HKRbook::mmstat.ui.elem, shiny::selectInput),
                'fontSize'           = c(HKRbook::mmstat.ui.elem, shiny::sliderInput),
                # ShinyWidgets
                'actionGroupButtons' = c(shinyWidgets::actionGroupButtons)
  )
  context <- rstudioapi::getActiveDocumentContext()
  # For the first selection only
  sel  <- context$selection[[1]]
  d    <- stringdist::stringdistmatrix(tolower(sel$text), tolower(names(types)), method="lcs")
  pos  <- which(d==min(d))
  if (length(pos)>1) {
    new_text <- paste0(names(types)[pos], collapse=" ")
  } else {
    params <- list()
    for (i in seq(length(types[[pos]]))) params <- c(params, formals(types[[pos]][[i]]))
    params[duplicated(names(params))]   <- NULL
    dots         <- which(names(params)=="...")
    params[dots] <- NULL
    params$type  <- names(types)[pos]
    pos <- which(names(mmstat$fun)==params$type)
    if (length(pos)==1) {
      for (name in names(params)) {
        if (!is.null(mmstat$fun[[pos]][[name]])) params[[name]] <- mmstat$fun[[pos]][[name]]
      }
    }
    uparams     <- c(names(params)[sapply(params, class)=="name"], "type")
    dparams     <- setdiff(names(params), uparams)
    new_text    <- paste0(breakLineAtComma(paste0("mmstat.ui.elem(", makeParams(params, uparams), ")"), prefix=" "), "\n# ",
                          paste0(breakLineAtComma(makeParams(params, dparams), prefix="#")))
  }
  rstudioapi::insertText(location = sel$range, text = new_text, id = context$id)
}
