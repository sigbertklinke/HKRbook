#' @rdname toHTML
#' @title toHTML
#' @description Returns a HTMl representation of a matrix and optionally shows the result in the browser.
#' If you decide to view the result in a browser then the HTML will be written to a temporary file and
#' [utils::browseURL()] called
#'
#' @param x html_matrix object
#' @param browser logical: show HTML in a browser (default: \code{FALSE})
#' @param ... further parameters to [utils::browseURL()]
#'
#' @md
#' @return html_matrix object
#' @importFrom utils browseURL
#' @importFrom tools toHTML
#' @export
#'
#' @examples
#' library("tools")
#' m    <- matrix(1:12, ncol=4)
#' hm   <- html_matrix(m)
#' html <- toHTML(hm, browser=interactive())
toHTML.html_matrix <- function(x, browser=FALSE, ...)  {
  style <- function(l) {
    use <- setdiff(names(l), c("tooltip", "value", "fmt", ""))
    if (length(use)==0) return('')
    use2 <- gsub("_", "-", use, fixed=TRUE)
    txt <- ' style="'
    for (k in seq(use2)) {
      txt <- paste0(txt, paste0(use2[k], ':', as.character(l[[use[k]]]), ';'))
    }
    paste0(txt, '"')
  }
  #
  checki <- function(xi) {
    if (is.null(xi$value)) stop("component 'value' not found")
    if (is.null(xi$fmt)) { # set default values
      if (mode(xi$value)=="numeric") xi$fmt <- "%f"
      if (mode(xi$value)=="logical") xi$fmt <- "%f"
      if (mode(xi$value)=="character") xi$fmt <- "%s"
      stopif(is.null(xi$fmt))
      stopifnot(mode(xi$fmt)=="character")
    }
    xi
  }
  check <- function(x, single=FALSE) {
    if (!is.null(x)) {
      if (single) {
        x <- checki(x)
      } else {
        for (i in seq(length(x))) x[[i]] <- checki(x[[i]])
      }
    }
    x
  }
  #
  tooltip    <- attr(x, "tooltip", TRUE)
  tabletitle <- if (is.null(tooltip)) '' else sprintf(' title="%s"', tooltip)
  html    <- paste0("<table", style(attr(x, "table")), tabletitle, ">\n")
  tr      <- attr(x, "tr", TRUE)
  title   <- check(attr(x, "title", TRUE), TRUE)
  rows    <- check(attr(x, "rownames", TRUE))
  cols    <- check(attr(x, "colnames", TRUE))
  rmvalue <- check(attr(x, "rowmargin", TRUE))
  rmtitle <- check(attr(x, "rowmargintitle", TRUE), TRUE)
  rmargin <- !(is.null(rmvalue) && is.null(rmtitle))
  empty   <- list(value='', fmt='%s', background_color="#999999", font_weight="bold")
  if (rmargin && is.null(rmtitle)) rmtitle <- empty
  if (rmargin && is.null(rmvalue)) rmvalue <- rep(list(empty), rows)
  header  <- check(attr(x, "header", TRUE))
  if (!is.null(header)) {
    tcol   <- ncol(x)+1+rmargin
    for (i in seq(length(header))) html <- paste0(html, "<tr><td colspan=\"", tcol, "\"", style(header[[i]]), ">", sprintf(header[[i]]$fmt, header[[i]]$value), "</td></tr>")
  }
  for (r in 0:nrow(x)) {
    html <- paste0(html, "<tr", style(tr[[r+1]]), ">")
    for (c in 0:ncol(x)) {
      if (r) {
        if (c) {
          html <- paste0(html, "<td", style(x[[r,c]]), ">", sprintf(x[[r,c]]$fmt, x[[r,c]]$value), "</td>")
        } else {
          html <- paste0(html, "<td", style(rows[[r]]), ">", sprintf(rows[[r]]$fmt, rows[[r]]$value), "</td>")
        }
      } else {
        if (c) {
          html <- paste0(html, "<td", style(cols[[c]]), ">", sprintf(cols[[c]]$fmt, cols[[c]]$value), "</td>")
        } else {
          html <- paste0(html, "<td", style(title), ">", sprintf(title$fmt, title$value), "</td>")
        }
      }
    }
    if (rmargin) {
      #browser()
      if (r) {
        html <- paste0(html, "<td", style(rmvalue[[r]]), ">", sprintf(rmvalue[[r]]$fmt, rmvalue[[r]]$value), "</td>")
      } else {
        html <- paste0(html, "<td", style(rmtitle), ">", sprintf(rmtitle$fmt, rmtitle$value), "</td>")
      }
    }
    html <- paste0(html, "</tr>\n")
  }
  cmvalue <- check(attr(x, "colmargin", TRUE))
  cmtitle <- check(attr(x, "colmargintitle", TRUE), TRUE)
  total   <- check(attr(x, "total", TRUE), TRUE)
  cmargin <- !(is.null(cmvalue) && is.null(cmtitle) && is.null(total))
  if (cmargin && is.null(cmtitle)) cmtitle <- empty
  if (cmargin && is.null(cmvalue)) cmvalue <- rep(list(empty), cols)
  if (cmargin && is.null(total))   total <- empty
  if (cmargin) {
    html <- paste0(html, "<tr><td", style(cmtitle), ">", sprintf(cmtitle$fmt, cmtitle$value), "</td>")
    for (c in 1:ncol(x)) {
      html <- paste0(html, "<td", style(cmvalue[[c]]), ">", sprintf(cmvalue[[c]]$fmt, cmvalue[[c]]$value), "</td>")
    }
    html <- paste0(html, "<td", style(total), ">", sprintf(total$fmt, total$value), "</td></tr>")
  }
  footer <- check(attr(x, "footer", TRUE))
  if (!is.null(footer)) {
    tcol   <- ncol(x)+1+!is.null(attr(x, "rowmargin", TRUE))
    for (i in 1:length(footer)) html <- paste0(html, "<tr><td colspan=\"", tcol, "\"", style(footer[[i]]), ">", sprintf(footer[[i]]$fmt, footer[[i]]$value), "</td></tr>")
  }
  html <- paste0(html, "</table>")
  if (browser) {
    file <- tempfile(fileext=".html")
    writeLines(html, file)
    browseURL(file, ...)
  }
  html
}

#' @rdname toHTML
#' @export
toHTML.table <- function(x, browser=FALSE, ...)  { toHTML(html_matrix(x), browser=browser, ...) }

#' @rdname toHTML
#' @export
toHTML.matrix <- function(x, browser=FALSE, ...)  { toHTML(html_matrix(x), browser=browser, ...) }
