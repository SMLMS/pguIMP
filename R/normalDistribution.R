#' @title normalDistribution
#'
#' @description
#' Represents a normal distribution.
#'
#' @details
#' Calculates p(x | mu, sigma). Where p is a normal distribution
#' with user defined expectation value `mu`
#' and standard deviation `sigma`.
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
#' @return
#' The probability of event x.
#' (numeric)
#'
#' @examples
#' y <- normalDistribution(x=5, mu=0.0, sigma=1.0)
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

normalDistribution = function(x="numeric", mu="numeric",sigma="numeric"){
  y <- 1.0/(sigma*sqrt(2.0*pi)) *exp(-1.0 * ((x-mu)^2)/(2.0*sigma^2))
  return(y)
}
