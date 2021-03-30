#' @title IMPgui
#' @description Run shiny based gui of pguIMP.
#'
#' @return shiny application object
#'
#' @import shiny
#' @import shinyWidgets
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
  vwr = shiny::dialogViewer('pguIMP', width = 1600, height = 1200)
  appDir %>%
  shiny::shinyAppDir() %>%
    shiny::runGadget(viewer = vwr)
    # shiny::runApp(
    #   port = getOption("shiny.port"),
    #   launch.browser = getOption("shiny.launch.browser", interactive()),
    #   host = getOption("shiny.host", "127.0.0.1"),
    #   workerId = "",
    #   quiet = FALSE,
    #   display.mode = c("auto", "normal", "showcase"),
    #   test.mode = getOption("shiny.testmode", FALSE)
    # )
}
