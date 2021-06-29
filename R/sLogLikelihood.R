#' @title dLogLikelihood
#'
#' @description
#' Calculates bbmle snmor function.
#'
#' @param mu
#' The expextation value.
#' (numeric)
#'
#' @param sigma
#' The standard deviation.
#' (numeric)
#'
#' @return
#' the bbmle::snorm results.
#'
#' @examples
#' y <- sLogLikelihood (mu=0.0, sigma=1.0)
#'
#' @importFrom bbmle snorm
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

sLogLikelihood = function(mu="numeric", sigma="numeric"){
  bbmle::snorm(mean=mu, sd=sigma)
}
