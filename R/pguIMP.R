#' @title IMPgui
#' @description Run shiny based gui of pguIMP.
#'
#' @return shiny application object
#'
#' @import shiny
#'
#' @include dLogLikelihood.R
#' @include sLogLikelihood.R
#' @include transposeTibble.R
#' @include normalDistribution.R
#'
#' @examples
#' pguIMP::IMPgui()
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

IMPgui <- function() {
  appDir <- system.file("application", package = "pguIMP")
  if (appDir == "") {
    stop("Could not find application. Try re-installing `pguIMP`.", call. = FALSE)
  }
  appDir %>%
  shiny::shinyAppDir() %>%
    shiny::runApp(launch.browser = TRUE)
}
