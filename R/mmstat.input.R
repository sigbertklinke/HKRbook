mmstat.input <- function (input) { # not used anywhere
  if (mmstat$debug>1) {
    ni  <-names(input)
    for (i in seq(ni)) {
      out <- utils::capture.output(input[[ni[i]]])
      out[1] <- paste0(ni[i], ': ', out[1])
      for (j in seq(out)) mmstat$log <<- cbind(sprintf("%s (Input): %s", date(), out[j]), mmstat$log)
    }
  }
}
