#' @title main
#' @description Run shiny based gui of pguIMP.
#'
#' @examples
#' pguIMP::main()
#'
#' @import shiny
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'


# source(file = "R/pguGlobals.R", local=TRUE)

main <- function() {
  appDir <- system.file("gui", package = "pguIMP")
  if (appDir == "") {
    stop("Could not find gui directory. Try re-installing `pguIMP`.", call. = FALSE)
  }
  shiny::runApp("gui", launch.browser=TRUE)
}

# main()
