mmstat.htest <- function (...) { # not used

  addnames <- function (txt1, txt2) {
    if (length(txt1)) {
      cont <- txt2 %in% txt1
      ret  <- c(txt1, txt2[!cont])
    } else {
      ret <- txt2
    }
    ret
  }

  htest <- list(method      = list(attr=NULL,         names='',                     fmt="%s",   lines=0),
                alternative = list(attr=NULL,         names='Alternative:',         fmt="%s",   lines=0),
                null.value  = list(attr='names',      names=vector("character", 0), fmt="%.4f", lines=1),
                data.name   = list(attr=NULL,         names='Data:',                fmt="%s",   lines=0),
                estimate    = list(attr='names',      names=vector("character", 0), fmt="%.4f", lines=0),
                conf.int    = list(attr='conf.level', names=vector("character", 0), fmt="%s",   lines=1),
                statistic   = list(attr='names',      names=vector("character", 0), fmt="%.4f", lines=0),
                parameter   = list(attr='names',      names=vector("character", 0), fmt="%.0f", lines=0),
                p.value     = list(attr=NULL,         names='p-value:',             fmt="%.4f", lines=0)
  )

  tests  <- list(...)
  nhtest <- names(htest)
  nrow   <- vector("numeric", length(htest))
  lines  <- 0
  for (j in seq(nhtest)) {
    name <- nhtest[j]
    attr <- htest[[nhtest[j]]]$attr
    if (!is.null(attr)) {
      # find all names
      for (i in seq(tests)) {
        htest[[name]]$names <- addnames(htest[[name]]$names, attr(tests[[i]][[name]], attr))
      }
    }
    # grab all values
    nrow[j] <- length(htest[[name]]$names)
    htest[[name]]$tab <- matrix('', nrow=nrow[j], ncol=length(tests))
    for (i in seq(tests)) {
      telem <- tests[[i]][[name]]
      if (!is.null(telem)) {
        if (is.null(attr)) {
          htest[[name]]$tab[1, i] <- sprintf(htest[[name]]$fmt, telem)
        } else if (attr=='conf.level') {
          htest[[name]]$tab[match(as.character(attr(telem, attr)), htest[[name]]$names), i] <-
            paste0('[', round(telem[1],4), ', ', round(telem[2],4), ']')
        } else {
          htest[[name]]$tab[match(as.character(attr(telem, attr)), htest[[name]]$names), i] <-
            sprintf(htest[[name]]$fmt, telem)
        }
      }
    }
    if (!is.null(attr)) {
      if (attr=='conf.level') {
        htest[[name]]$names <- sprintf("%.1f%% CI", 100*as.numeric(htest[[name]]$names))
      }
    }
    lines <- lines+htest[[name]]$lines
  }
  tab <- matrix('', nrow=sum(nrow)+lines, ncol=1+length(tests))
  pos <- 1
  for (j in seq(nhtest)) {
    name <- nhtest[j]
    len  <- length(htest[[name]]$names)
    tab[pos:(pos+len-1), 1] <- htest[[name]]$names
    tab[pos:(pos+len-1), 2:(1+length(tests))] <- htest[[name]]$tab
    pos <- pos+len+htest[[name]]$lines
  }
  maxlen <- apply(nchar(tab), 2, max)
  for (j in seq(tab)) {
    if (j<=nrow(tab))
      tab[j] <- sprintf('%-*s', maxlen[1+((j-1)%/%nrow(tab))], tab[j])
    else
      tab[j] <- sprintf('%*s', maxlen[1+((j-1)%/%nrow(tab))], tab[j])
  }
  paste(apply(tab, 1, paste, collapse="   "), collapse="\n")
}
