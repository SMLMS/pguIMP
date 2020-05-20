#' @title dLogLikelihood
#'
#' @description
#' Calculates the logLikelihood.
#'
#' @param  x
#' The x-value.
#' (numeric)
#'
#' @param mu
#' The expextation value.
#' (numeric)
#'
#' @param sigma
#' The standard deviation.
#' (numeric)
#'
#' @param log
#' Indicate to calculate the log likelihood
#' (logical)
#'
#' @return
#' The logLikelihood.
#' (numeric)
#'
#' @examples
#' y <- dLogLikelihood(x=5, mu=0.0, sigma=1.0)
#'
#' @import stats
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

dLogLikelihood = function(x="numeric", mu="numeric",sigma="numeric", log=TRUE){
  y <- stats::dnorm(x,mu,sigma)
  return(sum(log(y)))
}
