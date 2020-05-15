## ui.R ##
library(shiny)
library(shinydashboard)
library(shinyjs)
source(file = "../gui/pguHeader.R", local=TRUE)
source(file = "../gui/pguSidebar.R", local=TRUE)
source(file = "../gui/pguBody.R", local=TRUE)


ui <- dashboardPage(
  header,
  sidebar,
  body
)
