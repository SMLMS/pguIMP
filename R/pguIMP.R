#' @title pguIMP
#' @description Reproducible cleaning of biomedical laboratory data using methods of visualization, error correction and transformation implemented as interactive R-notebooks.
#' @details
#' A graphical data preprocessing toolbox, named “pguIMP”, that includes a fixed sequence of preprocessing steps to enable error-free data preprocessing interactively.
#' By implementing contemporary data processing methods including machine learning-based imputation procedures, the creation of corrected and cleaned bioanalytical datasets is ensured, which preserve data structures such as clusters better than resulting with classical methods.
#' @return shiny application object
#'
#' @importFrom magrittr %>%
#' @importFrom  shiny dialogViewer runApp runGadget shinyAppDir
#'
#' @include dLogLikelihood.R
#' @include sLogLikelihood.R
#' @include transposeTibble.R
#' @include normalDistribution.R
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pguIMP <- function() {
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
