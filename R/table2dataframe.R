#' table2dataframe
#'
#' Converts a table to a full data frame.
#'
#' @param tab table: contingency table
#' @param ...  further parameters given to [base::as.data.frame.table]
#'
#' @md
#' @return a data frame with `sum(tab)` rows and `length(dim(tab))` cols
#' @export
#'
#' @examples
#' table2dataframe(Titanic)
table2dataframe <- function(tab, ...) {
  stopifnot("table" %in% class(tab))
  df    <- as.data.frame(tab, ...)
  index <- rep(1:nrow(df), df$Freq)
  df[index,-ncol(df)]
}
