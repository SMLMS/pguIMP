#' Shiny app server object
#'
#' @import shiny
#' @import shinydashboard
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

# create the shiny application user interface header.

header <- shinydashboard::dashboardHeader(
  title = "PGU IMP"
  # shinydashboard::dropdownMenuOutput("helpMenu")
)
