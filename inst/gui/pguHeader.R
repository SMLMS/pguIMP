library(shiny)
library(shinydashboard)

header <- shinydashboard::dashboardHeader(
  title = "PGU Cleaner",
  shinydashboard::dropdownMenuOutput("helpMenu")
)
