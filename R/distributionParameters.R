#' distributionParams
#'
#' Computes approximate distribution parameters for the binomial, hypergeometric,
#' Poisson, Exponential and normal distribution for a given mean (and standard deviation).
#' With the sample and the population size the computation can be influenced.
#'
#' @param mean numeric: mean
#' @param sd numeric: standard deviation (only used for the normal distribution)
#' @param n integer: sample size (default: \code{30})
#' @param N integer: population size (default: \code{60})
#'
#' @return a list of parameters for each distribution
#' @export
#'
#' @examples
#' # Compute approx. paramaters for a binomial distribution
#' distributionParams(mean=30*0.5, sd=sqrt(30*0.5*0.5))
distributionParams <- function(mean, sd, n=30, N=60) {
  list(binom  = list(size=n, prob=mean/n),   # Expectation only
       hyper  = list(N=N, M=mean*N/n, n=n),  # Expectation only
       pois   = list(lambda=mean),           # Expectation only
       exp    = list(rate=1/mean),           # Expectation only
       norm   = list(mean=mean, sd=sd)
  )
}
