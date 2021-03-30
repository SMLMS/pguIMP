#' @title dLogLikelihood
#'
#' @description
#' Calculates the logLikelihood.
#'
#' @param  x
#' The x-value.
#' (numeric)
#'
#' @param pars
#' Numeric vector with two entries
#' 1: The expextation value.
#' 2: The standard deviation.
#' (numeric)
#'
#' @return
#' The logLikelihood.
#' (numeric)
#'
#' @examples
#' y <- dLogLikelihood(x=5, pars = c(mu=0.0, sigma=1.0))
#'
#' @import stats
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

dLogLikelihood = function(x = "numeric", pars = c(mu=0.0, sigma=1.0)){
  mu <- pars[1]
  sigma <- abs(pars[2])
  y <- stats::dnorm(x,mu,sigma,log = TRUE)
  return(-sum(y))
}
