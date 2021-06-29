#' @title dLogLikelihood
#' @description
#' Calculates the log Likelihood of a normally distributed event.
#' @param  x The x-value(numeric)
#' @param pars Numeric vector with two entries c(mu, sigma).
#' Where mu is the expectation value and sigma is the standard deviation.
#' (numeric)
#' @return
#' The logLikelihood.
#' (numeric)
#' @examples
#' y <- pguIMP::dLogLikelihood(x=5, pars = c(mu=0.0, sigma=1.0))
#' @importFrom stats dnorm
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#' @export
#'

dLogLikelihood = function(x = "numeric", pars = c(mu=0.0, sigma=1.0)){
  mu <- pars[1]
  sigma <- abs(pars[2])
  y <- stats::dnorm(x,mu,sigma,log = TRUE)
  return(-sum(y))
}
