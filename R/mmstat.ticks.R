#' mmstat.ticks
#'
#' Returns tick marks for a log based scale between \code{nmin} and \code{nin}.
#'
#' @param nin integer: maximun of scale
#' @param nmin integer: minimun of scale
#' @param tin integer: number of desired tick marks
#'
#' @return vector of tick marks
#' @export
#'
#' @examples
#' mmstat.ticks(506)
mmstat.ticks <- function (nin, nmin=3, tin=11) {
  nmax <- nin
  nt   <- tin-1
  repeat {
    n           <- nmin*exp((0:nt)/nt*log(nmax/nmin))
    pow         <- 10^trunc(log10(n))
    fsd         <- n%/%pow
    ssd         <- (n%%pow)%/%(pow/10)
    ssd[pow==1] <- 0
    ssd[ssd<3]  <- 0
    ssd[(ssd>2)&(ssd<8)] <- 5
    fsd[ssd>7]  <- fsd[ssd>7]+1
    ssd[ssd>7]  <- 0
    nret        <- fsd*pow+ssd*pow/10
    if(nret[nt+1]>nmax) nret[nt+1]<-nret[nt+1]-pow[nt+1]/2
    if (length(unique(nret))==nt+1) return(nret)
    nt <- nt-1
  }
}
