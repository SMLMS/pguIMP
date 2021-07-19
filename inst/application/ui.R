#' Shiny app server object
#'
#' @importFrom shinydashboard dashboardPage
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

source(file = 'header.R', local=TRUE)
source(file = 'sidebar.R', local=TRUE)
source(file = 'body.R', local=TRUE)
# create the shiny application user interface
ui <- shinydashboard::dashboardPage(
  header,
  sidebar,
  body
)
