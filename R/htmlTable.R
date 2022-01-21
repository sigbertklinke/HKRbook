#' htmlTable
#'
#' Creates a HTML table from a two dimensional table object.
#'
#' @param tab two dimensional table object
#' @param vars character: names of row and column variable
#' @param lines character: final line (default: \code{NULL})
#' @param cex numeric: font size (default: \code{1})
#' @param title character: table title  (default: \code{''})
#' @param rowsum character: add row sums at the right (default: \code{NULL})
#' @param colsum character: add column sums at the bottom (default: \code{NULL})
#' @param fmt character: format string for \code{sprintf} (default: \code{"\%.0f"})
#' @param total character: add the grand total at the bottom left (default: \code{NULL})
#' @param ... further parameters given to \code{html_matrix}
#'
#' @return html_matrix object
#' @export
#'
#' @examples
#' htab <- htmlTable(apply(Titanic,1:2,sum), c("Sex", "Class"), title="Titanic")
#' toHTML(htab, browser=interactive())
htmlTable <- function (tab, vars=NULL, lines=NULL, cex=1, title='', rowsum = NULL, colsum = NULL, fmt="%.0f", total=NULL, ...) {
  stopifnot(length(dim(tab))==2)
  #browser()
  htab <- html_matrix(tab, ...)
  htab <- hm_table(htab, text_align="right", width="100%", font_size=sprintf("%.0f%%", 100*cex))
  htab <- hm_title(htab, title, text_align="left", fmt="%s")
  htab <- zebra(htab)
  htab <- hm_cell(htab, fmt=fmt)
  if (!is.null(vars)) {
    htab <- hm_header(htab, 1, value=vars[1], text_align="right", fmt="%s")
    htab <- hm_footer(htab, 1, value=vars[2], text_align="left", fmt="%s")
    if (!is.null(lines)) {
      htab <- hm_footer(htab, 2, value="<hr>")
      htab <- hm_footer(htab, 2+seq(lines), value=lines, text_align="left")
    }
  } else if (!is.null(lines)) {
    htab <- hm_footer(htab, 1, value="<hr>")
    htab <- hm_footer(htab, 1+seq(lines), value=lines, text_align="left")
  }
  if (!is.null(rowsum)) {
    htab <- hm_rowmargintitle(htab, value=rowsum, text_align="right", fmt="%s")
    t    <- rowSums(tab)
    htab <- hm_rowmargin(htab, 1:length(t), value=t, text_align="right", fmt=fmt)
  }
  if (!is.null(colsum)) {
    htab <- hm_colmargintitle(htab, value=colsum, text_align="left", fmt="%s")
    t    <- colSums(tab)
    htab <- hm_colmargin(htab, 1:length(t), value=t, text_align="right", fmt=fmt)
  }
  if (!is.null(total)) {
    htab <- hm_total(htab, value=total, text_align="right", fmt="%s")
  }
  htab
}
