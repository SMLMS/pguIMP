library(shiny)
library(shinydashboard)
library(shinyjs)

sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    id = "menue",
    shinydashboard::menuItem("Import",
                             tabName = "tab_import",
                             icon = shiny::icon("download")),
    shinydashboard::menuItem("Filter",
                             tabName = "tab_filter",
                             icon = shiny::icon("filter")
    ),
    shinydashboard::menuItem("Explore",
                             tabName = "tab_explore",
                             icon = shiny::icon("compass")
    ),
    shinydashboard::menuItem("LOQ",
                             icon = shiny::icon("exchange-alt"),
                             shinydashboard::menuSubItem("Detect",
                                                         icon = shiny::icon("microscope"),
                                                         tabName = "tab_detect_loq"),
                             shinydashboard::menuSubItem("Mutate",
                                                         icon = shiny::icon("sitemap"),
                                                         tabName = "tab_mutate_loq")
    ),
    shinydashboard::menuItem("Transformation",
                             tabName = "tab_trafo",
                             icon = shiny::icon(name = "random", class = "fas"),
                             shinydashboard::menuSubItem("Detect",
                                                         icon = shiny::icon("microscope"),
                                                         tabName = "tab_detect_trafo"),
                             shinydashboard::menuSubItem("Mutate",
                                                         icon = shiny::icon("sitemap"),
                                                         tabName = "tab_mutate_trafo")
    ),
    shinydashboard::menuItem("Imputation",
                             tabName = "tab_impute",
                             icon = shiny::icon("fill-drip"),
                             shinydashboard::menuSubItem("Detect",
                                                         icon = shiny::icon("microscope"),
                                                         tabName = "tab_detect_impute"),
                             shinydashboard::menuSubItem("Mutate",
                                                         icon = shiny::icon("sitemap"),
                                                         tabName = "tab_mutate_impute")
    ),
    shinydashboard::menuItem("Outliers",
                             tabName = "tab_revise",
                             icon = shiny::icon("microscope"),
                             shinydashboard::menuSubItem("Detect",
                                                         icon = shiny::icon("microscope"),
                                                         tabName = "tab_detect_outliers"),
                             shinydashboard::menuSubItem("Mutate",
                                                         icon = shiny::icon("sitemap"),
                                                         tabName = "tab_mutate_outliers")
    ),
    shinydashboard::menuItem("Analysis",
                             tabName = "tab_analyis",
                             icon = shiny::icon("chart-line"),
                             shinydashboard::menuSubItem("Correlation",
                                                         icon = shiny::icon("chart-bar"),
                                                         tabName = "tab_analysis_correlation"),
                             shinydashboard::menuSubItem("Regression",
                                                         icon = shiny::icon("chart-line"),
                                                         tabName = "tab_analysis_regression")
    ),
    shinydashboard::menuItem("Export",
                             # tabName = "tab_export",
                             icon = shiny::icon("upload"),
                             shinyjs::useShinyjs(),
                             shiny::downloadButton(
                               'dbExport',
                               width = "100%",
                               labe = h5('Download')
                             )
    )
    
  )
)
