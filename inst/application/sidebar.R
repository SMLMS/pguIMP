#' Shiny app server object
#'
#' @import shiny
#' @import shinydashboard
#' @import shinyjs
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

# create the shiny application user interface sidebar.

sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    id = "menue",
    shinydashboard::menuItem("Upload",
                             tabName = "tab_import",
                             icon = shiny::icon("upload")),
    shinydashboard::menuItem("Filter",
                             tabName = "tab_filter",
                             icon = shiny::icon("filter")
    ),
    shinydashboard::menuItem("Explore",
                             tabName = "tab_explore",
                             icon = shiny::icon("compass")
    ),
    shinydashboard::menuItem("Crop LOQ",
                             tabName = "tab_loq",
                             icon = shiny::icon("crop-alt"),
                             shinydashboard::menuSubItem("Define",
                                                         icon = shiny::icon("upload"),
                                                         tabName = "tab_define_loq"),
                             shinydashboard::menuSubItem("Detect",
                                                         icon = shiny::icon("microscope"),
                                                         tabName = "tab_detect_loq"),
                             shinydashboard::menuSubItem("Mutate",
                                                         icon = shiny::icon("dna"),
                                                         tabName = "tab_mutate_loq")
    ),
    shinydashboard::menuItem("Transform",
                             tabName = "tab_trafo",
                             icon = shiny::icon(name = "random", class = "fas"),
                             # shinydashboard::menuSubItem("Detect",
                             #                             icon = shiny::icon("microscope"),
                             #                             tabName = "tab_detect_trafo"),
                             shinydashboard::menuSubItem("Mutate",
                                                         icon = shiny::icon("dna"),
                                                         tabName = "tab_mutate_trafo"),
                             shinydashboard::menuSubItem("Normalize",
                                                         icon = shiny::icon("compress"),
                                                         tabName = "tab_normalize_trafo")
    ),
    shinydashboard::menuItem("Impute",
                             tabName = "tab_impute",
                             icon = shiny::icon("brain"),
                             shinydashboard::menuSubItem("Missing statistics",
                                                         icon = shiny::icon("question-circle"),
                                                         tabName = "tab_missings_impute"),
                             shinydashboard::menuSubItem("Detect anomalies",
                                                         icon = shiny::icon("microscope"),
                                                         tabName = "tab_outliers_impute"),
                             shinydashboard::menuSubItem("Mutate",
                                                         icon = shiny::icon("dna"),
                                                         tabName = "tab_mutate_impute")
    ),
    shinydashboard::menuItem("Validate",
                             tabName = "tab_analysis_validation",
                             icon = shiny::icon("vials")
    ),
    # shinydashboard::menuItem("Analysis",
    #                          tabName = "tab_analyis",
    #                          icon = shiny::icon("chart-pie"),
    #                          shinydashboard::menuSubItem("Validation",
    #                                                      icon = shiny::icon("vials"),
    #                                                      tabName = "tab_analysis_validation"),
    #                          shinydashboard::menuSubItem("Correlation",
    #                                                      icon = shiny::icon("chart-bar"),
    #                                                      tabName = "tab_analysis_correlation"),
    #                          shinydashboard::menuSubItem("Regression",
    #                                                      icon = shiny::icon("chart-line"),
    #                                                      tabName = "tab_analysis_regression")
    # ),
    shinydashboard::menuItem("Export",
                             tabName = "tab_export",
                             icon = shiny::icon("file-excel"),
                             shinyjs::useShinyjs(),
                             shiny::downloadButton(
                               'dbExport',
                               width = "100%",
                               labe = h5('Download')
                             )
    )
    # shinydashboard::menuItem("Report",
    #                          tabName = "tab_report",
    #                          icon = shiny::icon("file-pdf"),
    #                          shinyjs::useShinyjs(),
    #                          shiny::downloadButton(
    #                            'dbReport',
    #                            width = "100%",
    #                            labe = h5('Download')
    #                          )
    # )

  )
)
