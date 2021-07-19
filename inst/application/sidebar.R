#' Shiny app server object
#'
#' @importFrom shiny downloadButton icon
#' @importFrom shinydashboard dashboardSidebar menuItem menuSubItem sidebarMenu
#' @importFrom shinyWidgets switchInput
#' @importFrom shinyjs useShinyjs
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
                             shinydashboard::menuSubItem("Upload LOQs",
                                                         icon = shiny::icon("upload"),
                                                         tabName = "tab_upload_loq"),
                             shinydashboard::menuSubItem("Define LOQs",
                                                         icon = shiny::icon("user-edit"),
                                                         tabName = "tab_define_loq"),
                             shinydashboard::menuSubItem("Detect LOQ Outliers",
                                                         icon = shiny::icon("microscope"),
                                                         tabName = "tab_detect_loq"),
                             shinydashboard::menuSubItem("Substitute LOQ Outliers",
                                                         icon = shiny::icon("dna"),
                                                         tabName = "tab_mutate_loq")
    ),
    shinydashboard::menuItem("Transform",
                             tabName = "tab_trafo",
                             icon = shiny::icon(name = "random", class = "fas"),
                             # shinydashboard::menuSubItem("Detect",
                             #                             icon = shiny::icon("microscope"),
                             #                             tabName = "tab_detect_trafo"),
                             shinydashboard::menuSubItem("Transform Data",
                                                         icon = shiny::icon("dna"),
                                                         tabName = "tab_mutate_trafo"),
                             shinydashboard::menuSubItem("Normalize Data",
                                                         icon = shiny::icon("compress"),
                                                         tabName = "tab_normalize_trafo")
    ),
    shinydashboard::menuItem("Impute",
                             tabName = "tab_impute",
                             icon = shiny::icon("brain"),
                             shinydashboard::menuSubItem("Detect Missings",
                                                         icon = shiny::icon("question-circle"),
                                                         tabName = "tab_missings_impute"),
                             shinydashboard::menuSubItem("Detect Anomalies",
                                                         icon = shiny::icon("microscope"),
                                                         tabName = "tab_outliers_impute"),
                             shinydashboard::menuSubItem("Substitute Imputation Sites",
                                                         icon = shiny::icon("dna"),
                                                         tabName = "tab_mutate_impute")
    ),
    shinydashboard::menuItem("Validate Preprocessing",
                             tabName = "tab_analysis_validation",
                             icon = shiny::icon("vials")
    ),
    shinydashboard::menuItem("Export",
                             tabName = "tab_export",
                             icon = shiny::icon("file-excel"),
                             shinyjs::useShinyjs(),
                             shiny::downloadButton(
                               'dbExport',
                               width = "100%",
                               labe = h5('Download')
                             )

    ),
    shinyWidgets::switchInput(inputId = "switch.help",
                              label = "Help",
                              size = "mini",
                              onLabel = "Show",
                              offLabel = "Off",
                              offStatus = TRUE,
                              value = FALSE
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
