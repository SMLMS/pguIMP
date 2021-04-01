#' Shiny app server object
#'
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @import shinyjs
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

# create the shiny application user interface body.
menueColumnWidth <- 3
dataColumnWidth <- 12 - menueColumnWidth
halfDataColumnWidth <- as.integer(dataColumnWidth/2)

body <- shinydashboard::dashboardBody(shinydashboard::tabItems(
  shinydashboard::tabItem(
    tabName = "tab_import",
    shiny::fluidPage(
      width = 12,
      title = "Import",
      shiny::fluidRow(
        shiny::column(
          width = menueColumnWidth,
          shiny::h1("Upload"),
          shiny::fileInput(
            inputId = "fi.import",
            label = h5(" Select file "),
            accept = c(".csv", ".txt", ".xls", ".xlsx"),
            width = "100%"
          ),
          shiny::numericInput(
            inputId = "ni.importSheetIndex",
            label = shiny::h5("Select sheet"),
            value = 1,
            min = 1,
            step = 1,
            width = "100%"
          ),
          shiny::selectInput(
            inputId = "si.importSeparator",
            label = shiny::h5("Separator"),
            choices = c(",", ";", "tab"),
            selected = ",",
            width = "100%"
          ),
          shiny::numericInput(
            inputId = "ni.importSkip",
            label = shiny::h5("Skip rows"),
            value = 0,
            min = 0,
            step = 1,
            width = "100%"
          ),
          shiny::selectInput(
            inputId = "si.importColnames",
            label = shiny::h5("Column names"),
            choices = c("FALSE", "TRUE"),
            selected = "TRUE",
            width = "100%"
          ),
          shiny::selectInput(
            inputId = "si.importNaChar",
            label = shiny::h5("Na character"),
            choices = c("na", "nan", "Na"),
            selected = "na",
            width = "100%"
          ),
          shiny::actionButton("ab.importReset",
                              label = h5("Reset"),
                              width = "100%"
          )
        ),
        shiny::column(
          width = dataColumnWidth,
          shiny::htmlOutput(outputId = "html.uploadHelp"),
          shiny::h3("Data types"),
          DT::dataTableOutput(
            outputId = "tbl.importDataTypes",
            width = "100%"),
          shiny::hr(),
          shiny::h3("Data summary"),
          DT::dataTableOutput(
            outputId = "tbl.importDataStatistics",
            width = "100%"
          ),
          shiny::br(),
          shiny::h3("Missings summary"),
          DT::dataTableOutput(
            outputId = "tbl.importMissingsStatistics",
            width = "100%"
          )
        )
      )
    )),

  shinydashboard::tabItem(
    tabName = "tab_filter",
    shiny::fluidPage(
      width = 12,
      title = "Filter",
      shiny::fluidRow(
        shiny::column(
          width = menueColumnWidth,
          shiny::h1("Filter"),
          shiny::actionButton(
              inputId = "ab.filterSet",
              label = "Filter",
              width = "100%"
          ),
          shiny::actionButton(
            inputId = "ab.filterInvSet",
            label = "Filter inverse",
            width = "100%"
          ),
          shiny::actionButton(
            inputId = "ab.filterReset",
            label = "Reset",
            width = "100%"
          )
        ),
        shiny::column(
          width = dataColumnWidth,
          shiny::htmlOutput(outputId = "html.filterHelp"),
          shiny::h3("Select Filter"),
          DT::dataTableOutput("tbl.filterSelect", width = "100%"),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::h3("Statistics"),
              DT::dataTableOutput("tbl.filterStatistics", width = "100%")
            ),
            shiny::column(
              width = 6,
              shiny::h3("Missings"),
              DT::dataTableOutput("tbl.filterMissings", width = "100%")
            )
          )
        )
      )
    )
  ),

  shinydashboard::tabItem(
    tabName = "tab_explore",
    fluidPage(
      width = 12,
      title = "Explore",
      shiny::fluidRow(
        shiny::column(
          width = menueColumnWidth,
          shiny::h1("Explore"),
          selectInput(
            "si.exploreAbs",
            label = h5("Abscissa"),
            choices = list(),
            selected = 1,
            width = "100%"
          ),
          selectInput(
            "si.exploreOrd",
            label = h5("Ordinate"),
            choices = list(),
            selected = 1,
            width = "100%"
          )
        ),
        shiny::column(
          width = dataColumnWidth,
          shiny::htmlOutput(outputId = "html.exploreHelp"),
          shiny::h3("Feature Scatter Plot"),
          shiny::plotOutput("plt.exploreGraphic"),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::h3("Abscissa Statistics"),
              shiny::plotOutput("plt.exploreAbscissaGraphic"),
              shiny::br(),
              DT::dataTableOutput("tbl.exploreAbscissaStatistics")
            ),
            shiny::column(
              width = 6,
              shiny::h3("Ordinate Statistics"),
              shiny::plotOutput("plt.exploreOrdinateGraphic"),
              shiny::br(),
              DT::dataTableOutput("tbl.exploreOrdinateStatistics")
            )
          )
        )
      )
    )
  ),

  shinydashboard::tabItem(
    tabName = "tab_upload_loq",
    shiny::fluidPage(
      width = 12,
      title = "Upload LOQs",
      shiny::column(
        width = menueColumnWidth,
        shiny::h1("Import LOQs"),
        shiny::fileInput(
          inputId = "fi.LoqImport",
          label = h5("Select file"),
          accept = c(".csv", ".txt", ".xls", ".xlsx"),
          width = "100%"
        ),
        shiny::numericInput(
          inputId = "ni.LoqSheetIndex",
          label = shiny::h5("Select sheet"),
          value = 1,
          min = 1,
          step = 1,
          width = "100%"
        ),
        shiny::selectInput(
          inputId = "si.LoqSeparator",
          label = shiny::h5("Separator"),
          choices = c(",", ";", "tab"),
          selected = ",",
          width = "100%"
        ),
        shiny::numericInput(
          inputId = "ni.LoqSkip",
          label = shiny::h5("Skip rows"),
          value = 0,
          min = 0,
          step = 1,
          width = "100%"
        ),
        shiny::selectInput(
          inputId = "si.LoqColnames",
          label = shiny::h5("Column names"),
          choices = c("FALSE", "TRUE"),
          selected = "TRUE",
          width = "100%"
        ),
        shiny::selectInput(
          inputId = "si.LoqNaChar",
          label = shiny::h5("Na character"),
          choices = c("na", "nan", "Na"),
          selected = "na",
          width = "100%"
        ),
        shiny::actionButton(
          inputId = "ab.LoqUploadReset",
          label = "Reset",
          width = "100%"
        )
      ),
      shiny::column(
        width = dataColumnWidth,
        shiny::htmlOutput(outputId = "html.uploadLOQHelp"),
        shiny::h3("LOQ values"),
        DT::dataTableOutput("tbl.loqUploadValues")
      )
    )
  ),

  shinydashboard::tabItem(
    tabName = "tab_define_loq",
    shiny::fluidPage(
      width = 12,
      title = "Define LOQs",
      shiny::column(
        width = menueColumnWidth,
        shiny::h1("Define LOQs"),
        shiny::selectInput(
          inputId = "si.LoqDefineFeature",
          label = shiny::h5("Feature"),
          choices = list(),
          selected = 1,
          width = "100%"
        ),
        shiny::numericInput(
          inputId = "ni.LoqDefineLLOQ",
          label = h5("LLOQ"),
          value = NA,
          width = "100%"
        ),
        shiny::numericInput(
          inputId = "ni.LoqDefineULOQ",
          label = h5("ULOQ"),
          value = NA,
          width = "100%"
        ),
        shiny::actionButton(
          inputId = "ab.LoqDefineSet",
          label = "set",
          width = "100%"
        ),
        shiny::actionButton(
          inputId = "ab.LoqDefineSetGlobally",
          label = "set globally",
          width = "100%"
        ),
        shiny::actionButton(
          inputId = "ab.LoqDefineReset",
          label = "Reset",
          width = "100%"
        )
      ),
      shiny::column(
        width = dataColumnWidth,
        shiny::htmlOutput(outputId = "html.defineLOQHelp"),
        shiny::h3("LOQ values"),
        DT::dataTableOutput("tbl.loqDefineValues")
      )
    )
  ),

  shinydashboard::tabItem(
    tabName = "tab_detect_loq",
    shiny::fluidPage(
      width = 12,
      title = "Detect LOQ",
      shiny::fluidRow(
        shiny::column(
          width = menueColumnWidth,
          shiny::h1("Detect LOQ Outliers"),
          shiny::selectInput(
            "si.loqDetectFeature",
            width = '100%',
            label = h5("Feature"),
            choices = list(),
            selected = 1
          ),
          shiny::selectInput(
            "si.loqNaHandling",
            width = '100%',
            label = h5("NA Handling"),
            choices = list(),
            selected = 1
          ),
          shiny::hr(),
          shiny::actionButton(
            inputId = "ab.detectLoq",
            label = "Detect Outliers",
            width = "100%"
          ),
          shiny::actionButton(
            inputId = "ab.resetDetectLoq",
            label = "Reset",
            width = "100%"
          )
        ),
        shiny::column(
          width = dataColumnWidth,
          shiny::htmlOutput(outputId = "html.detectLOQHelp"),
          shiny::h3("Define LOQ"),
          DT::dataTableOutput("tbl.loqDefine", width = "100%"),
          shiny::br(),
          shiny::h3("LOQ Outliers Distribution"),
          shiny::plotOutput("plt.loqDetectStatistics"),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              h3("Feature Plot"),
              shiny::plotOutput("plt.loqDetectFeature")
            ),
            shiny::column(
              width = 6,
              h3("Feature Data"),
              DT::dataTableOutput("tbl.loqDetectFeature")
            )
          ),
          shiny::br(),
          shiny::br(),
          shiny::h3("LOQ Outliers Statistics"),
          DT::dataTableOutput("tbl.loqDetectStatistics"),
          shiny::br(),
          shiny::h3("LOQ Outliers"),
          DT::dataTableOutput("tbl.loqDetectOutlier")
        )
      )
    )
  ),

  shinydashboard::tabItem(
    tabName = "tab_mutate_loq",
    shiny::fluidPage(
      width = 12,
      title = "Substitute LOQ Outliers",
      shiny::fluidRow(
        shiny::column(
          width = menueColumnWidth,
          shiny::h1("Substitute LOQ Outliers"),
          shiny::selectInput(
            "si.loqMutateFeature",
            width = '100%',
            label = h5("Feature"),
            choices = list(),
            selected = 1
          ),
          shiny::selectInput(
            "si.lloqSubstitute",
            width = '100%',
            label = h5("LLOQ Substitute"),
            choices = list(),
            selected = 1
          ),
          shiny::selectInput(
            "si.uloqSubstitute",
            width = '100%',
            label = h5("ULOQ Substitute"),
            choices = list(),
            selected = 1
          ),
          shiny::hr(),
          shiny::actionButton(
            inputId = "ab.mutateLoq",
            label = "Substitute Outliers",
            width = "100%"
          ),
          shiny::actionButton(
            inputId = "ab.resetMutateLoq",
            label = "Reset",
            width = "100%"
          )
        ),
        shiny::column(
          width = dataColumnWidth,
          shiny::htmlOutput(outputId = "html.substituteLOQHelp"),
          shiny::h3("LOQ Outliers Distribution"),
          shiny::plotOutput("plt.loqMutateStatistics"),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              h3("Feature Plot"),
              shiny::plotOutput("plt.loqMutateFeature")
            ),
            shiny::column(
              width = 6,
              h3("Feature Data"),
              DT::dataTableOutput("tbl.loqMutateFeature")
            )
          ),
          shiny::br(),
          shiny::br(),
          shiny::h3("Substituted LOQ Outliers Data"),
          DT::dataTableOutput("tbl.loqMutateData")
        )
      )
    )
  ),

  shinydashboard::tabItem(
    tabName = "tab_mutate_trafo",
    fluidPage(
      width = 12,
      title = "Transformation",
      shiny::fluidRow(
        shiny::column(
          width = menueColumnWidth,
          shiny::h1("Transform"),
          selectInput(
            "si.trafoMutateFeature",
            label = h5("Feature"),
            choices = list(),
            selected = 1,
            width = "100%"
          ),
          shiny::selectInput(
            "si.trafoMutateType",
            label = h5("Transformation Type"),
            choices = list(),
            selected = 1,
            width = "100%"
          ),
          shiny::numericInput(
            "ni.trafoMutateLambdaLOP",
            label = h5("lambda LOP"),
            value = 1,
            min = -2,
            max = 2,
            step = 0.01,
            width = "100%"
          ),
          shiny::checkboxInput(
            "cb.trafoMutateMirror",
            label = h5("Mirror"),
            value = FALSE
          ),
          shiny::actionButton(
            inputId = "ab.trafoMutateSetGlobal",
            label = "Initialize Analysis",
            width = "100%"
          ),
          shiny::actionButton(
            inputId = "ab.trafoMutateFeature",
            label = "Transform Feature",
            width = "100%"
          ),
          shiny::actionButton(
            inputId = "ab.trafoMutateReset",
            label = "Reset",
            width = "100%"
          )
        ),
        shiny::column(
          width = dataColumnWidth,
          shiny::htmlOutput(outputId = "html.transformHelp"),
          shiny::h3("Feature Transformation"),
          shiny::plotOutput("plt.trafoMutateFeature", height = "600"),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::h3("Model Parameter"),
              DT::dataTableOutput("tbl.trafoMutateFeatureParameter")
              ),
            shiny::column(
              width = 6,
              shiny::h3("Model Quality"),
              DT::dataTableOutput("tbl.trafoMutateFeatureQuality")
              )
          ),
          shiny::br(),
          shiny::br(),
          shiny::h3("Global Transformation"),
          shiny::h3("Transformation Parameter"),
          DT::dataTableOutput("tbl.trafoMutateGlobalParameter"),
          shiny::br(),
          shiny::br(),
          shiny::h3("Model Parameter"),
          DT::dataTableOutput("tbl.trafoMutateGlobalModel"),
          shiny::br(),
          shiny::br(),
          shiny::h3("Model Quality"),
          DT::dataTableOutput("tbl.trafoMutateGlobalQuality"),
          shiny::br(),
          shiny::br(),
          shiny::h3("Test Results"),
          DT::dataTableOutput("tbl.trafoMutateGlobalTests"),
          shiny::br(),
          shiny::br(),
          shiny::h3("Transformed Data"),
          DT::dataTableOutput("tbl.trafoMutateGlobalData")
        )
      )
    )
  ),

  shinydashboard::tabItem(
    tabName = "tab_normalize_trafo",
    shiny::fluidPage(
      width = 12,
      title = "Normalization",
      shiny::fluidRow(
        shiny::column(
          width = menueColumnWidth,
          shiny::h1("Normalize"),
          selectInput(
            "si.trafoNormFeature",
            label = h5("Feature"),
            choices = list(),
            selected = 1,
            width = "100%"
          ),
          selectInput(
            "si.trafoNormMethod",
            label = h5("Normalization Type"),
            choices = list(),
            selected = 1,
            width = "100%"
          ),
          shiny::hr(),
          shiny::actionButton(
            inputId = "ab.trafoNormMutate",
            label = "Normalize",
            width = "100%"
          ),
          shiny::actionButton(
            inputId = "ab.trafoNormReset",
            label = "Reset",
            width = "100%"
          )
        ),
        shiny::column(
          width = dataColumnWidth,
          shiny::htmlOutput(outputId = "html.normalizeHelp"),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              h3("Feature Plot"),
              shiny::plotOutput("plt.trafoNormFeature")
            ),
            shiny::column(
              width = 6,
              h3("Feature Statistics"),
              DT::dataTableOutput("tbl.trafoNormFeatureStatistics")
            )
          ),
          shiny::br(),
          shiny::br(),
          shiny::h3("Model parameters"),
          DT::dataTableOutput("tbl.trafoNormParameter"),
          shiny::br(),
          shiny::h3("Normalized data statistics"),
          DT::dataTableOutput("tbl.trafoNormStatistics"),
          shiny::br(),
          shiny::h3("Normalized data"),
          DT::dataTableOutput("tbl.trafoNormData")
        )
      )
    )
  ),

  shinydashboard::tabItem(
    tabName = "tab_missings_impute",
    shiny::fluidPage(
      width = 12,
      title = "Detect Missings",
      shiny::fluidRow(
        shiny::column(
          width = menueColumnWidth,
          shiny::h1("Detect Missings"),
          shiny::hr(),
          shiny::actionButton(
            inputId = "ab.imputeMissingsDetect",
            label = "Detect Missings",
            width = "100%"
          )
        ),
        shiny::column(
          width = dataColumnWidth,
          shiny::htmlOutput(outputId = "html.missingsHelp"),
          shiny::h3("Missings Statistics"),
          shiny::plotOutput("plt.imputeMissingsSummary"),
          shiny::br(),
          shiny::br(),
          shiny::h3("Missings Statistics"),
          DT::dataTableOutput("tbl.imputeMissingsStatistics"),
          shiny::br(),
          shiny::br(),
          shiny::h3("Missings Distribution"),
          DT::dataTableOutput("tbl.imputeMissingsDistribution"),
          shiny::br(),
          shiny::br(),
          shiny::hr(),
          shiny::h3("Missings Vs. Exsitings"),
          shiny::plotOutput("plt.imputeMissingsPairs"),
          shiny::br(),
          shiny::br(),
          shiny::h3("Missings Statistics Mean (SD)"),
          DT::dataTableOutput("tbl.imputeMissingsCharacteristics"),
          shiny::br(),
          shiny::br(),
          shiny::hr(),
          shiny::h3("Missings Details"),
          DT::dataTableOutput("tbl.imputeMissingsDetail")
        )
      )
    )
  ),

  shinydashboard::tabItem(
    tabName = "tab_outliers_impute",
    shiny::fluidPage(
      width = 12,
      title = "Detect Anomalies",
      shiny::fluidRow(
        shiny::column(
          width = menueColumnWidth,
          shiny::h1("Detect Anomalies"),
          shiny::selectInput(
            "si.imputeOutliersFeature",
            label = h5("Feature"),
            choices = list(),
            selected = 1,
            width = "100%"
          ),
          shiny::selectInput(
            "si.imputeOutliersMethod",
            label = h5("Detection Method"),
            choices = list(),
            selected = 1,
            width = "100%"
          ),
          shiny::hr(),
          shiny::h3("Grubbs parameters"),
          shiny::numericInput(
            "ni.imputeOutliersAlpha",
            label = h5("alpha"),
            value = 0.05,
            min = 0.01,
            max = 0.99,
            step = 0.01,
            width = "100%"
          ),
          shiny::hr(),
          shiny::h3("DBSCAN parameters"),
          shiny::numericInput(
            "ni.imputeOutliersEpsilon",
            label = h5("espilon"),
            value = 0.1,
            min = 0.01,
            step = 0.01,
            width = "100%"
          ),
          shiny::numericInput(
            "ni.imputeOutliersMinSamples",
            label = h5("minSamples"),
            value = 5,
            min = 1,
            max = 10,
            step = 1,
            width = "100%"
          ),
          shiny::hr(),
          shiny::h3("SVM parameters"),
          shiny::numericInput(
            "ni.imputeOutliersGamma",
            label = h5("gamma"),
            value = 0.05,
            min = 0.01,
            step = 0.01,
            width = "100%"
          ),
          shiny::numericInput(
            "ni.imputeOutliersNu",
            label = h5("nu"),
            value = 0.95,
            min = 0.01,
            max = 0.99,
            step = 0.01,
            width = "100%"
          ),
          shiny::hr(),
          shiny::h3("KNN parameters"),
          shiny::numericInput(
            "ni.imputeOutliersCutoff",
            label = h5("cutoff"),
            value = 0.99,
            min = 0.01,
            max = 0.99,
            step = 0.01,
            width = "100%"
          ),
          shiny::numericInput(
            "ni.imputeOutliersK",
            label = h5("k"),
            value = 5,
            min = 1,
            max = 10,
            step = 1,
            width = "100%"
          ),
          shiny::numericInput(
            "ni.imputeOutliersSeed",
            label = h5("anomalies seed"),
            value = 42,
            width = "100%"
          ),
          shiny::hr(),
          shiny::actionButton(
            inputId = "ab.imputeOutliersDetect",
            label = "Detect Anomalies",
            width = "100%"
          ),
          shiny::actionButton(
            inputId = "ab.imputeOutliersReset",
            label = "Reset",
            width = "100%"
          )
        ),
        shiny::column(
          width = dataColumnWidth,
          shiny::htmlOutput(outputId = "html.anomaliesHelp"),
          shiny::h3("Outlier Distribution"),
          shiny::plotOutput("plt.outliersImputeSummary"),
          shiny::hr(),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::h3("Feature Plot"),
              shiny::plotOutput("plt.outliersImputeFeature")
            ),
            shiny::column(
              width = 6,
              shiny::h3("Feature Data"),
              DT::dataTableOutput("tbl.outliersImputeFeature")
            )
          ),
          shiny::hr(),
          shiny::h3("Outlier Statistics"),
          DT::dataTableOutput("tbl.outliersImputeStatistics"),
          shiny::hr(),
          shiny::h3("Outlier Details"),
          DT::dataTableOutput("tbl.outliersImputeDetail")
        )
      )
    )
  ),

  shinydashboard::tabItem(
    tabName = "tab_mutate_impute",
    shiny::fluidPage(
      width = 12,
      title = "Substitute Imputation Sites",
      shiny::fluidRow(
        shiny::column(
          width = menueColumnWidth,
          shiny::h1("Substitute Imputation Sites"),
          shiny::selectInput(
            "si.imputeMutateFeature",
            label = h5("Feature"),
            width = "100%",
            choices = list(),
            selected = 1
          ),
          shiny::selectInput(
            "si.imputeMutateMethod",
            label = h5("Substitution Method"),
            width = "100%",
            choices = list(),
            selected = 1
          ),
          shiny::numericInput(
            "ni.imputeMutateNumberOfNeighbors",
            label = h5("number of neighbors"),
            width = "100%",
            min = 2,
            max = 10,
            step =1,
            value = 3
          ),
          shiny::numericInput(
            "ni.imputeMutatePredFrac",
            label = h5("fraction of predictors"),
            width = "100%",
            min = 0,
            max = 1,
            step =0.1,
            value = 1.0
          ),
          shiny::numericInput(
            "ni.imputeMutateOutfluxThr",
            label = h5("outflux threshold"),
            width = "100%",
            min = 0.1,
            max = 1.0,
            step = 0.01,
            value = 0.5
          ),
          shiny::numericInput(
            "ni.imputeMutateSeed",
            label = h5("imputation seed"),
            width = "100%",
            min = 1,
            max = 1000,
            step = 1,
            value = 42
          ),
          shiny::numericInput(
            "ni.imputeMutateIterations",
            label = h5("Iterations"),
            width = "100%",
            min = 1,
            max = 10,
            step = 1,
            value = 4
          ),
          shiny::hr(),
          shiny::actionButton(
            inputId = "ab.imputeMutateMutate",
            label = "Substitute imputation sites",
            width = "100%"
          ),
          shiny::actionButton(
            inputId = "ab.imputeMutateReset",
            label = "reset imputation settings",
            width = "100%"
          )
        ),
        shiny::column(
          width = dataColumnWidth,
          shiny::htmlOutput(outputId = "html.substituteImpHelp"),
          shiny::h3("Imputation Site Heatmap"),
          shiny::plotOutput("plt.imputeMutateSummary"),
          shiny::hr(),
          shiny::h3("Flux plot"),
          shiny::plotOutput("plt.imputeMutateFlux"),
          shiny::hr(),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::h3("Feature Plot"),
              shiny::plotOutput("plt.imputeMutateFeatureDetail")
            ),
            shiny::column(
              width = 6,
              shiny::h3("Feature Data"),
              DT::dataTableOutput("tbl.imputeMutateFeatureDetail")
            )
          ),
          shiny::br(),
          shiny::br(),
          shiny::h3("Imputation Site Statistics"),
          DT::dataTableOutput("tbl.imputeMutateStatistics"),
          shiny::br(),
          shiny::br(),
          shiny::h3("Imputation Site Distribution"),
          DT::dataTableOutput("tbl.imputeMutateDistribution"),
          shiny::br(),
          shiny::br(),
          shiny::h3("Imputation Site Detail"),
          DT::dataTableOutput("tbl.imputeMutateDetail"),
          shiny::br(),
          shiny::br(),
          shiny::h3("Imputation Site Data"),
          DT::dataTableOutput("tbl.imputeMutateData")
        )
      )
    )
  ),

  shinydashboard::tabItem(
    tabName = "tab_analysis_validation",
    shiny::fluidPage(
      width = 12,
      title = "Validate Preprocessing",
      shiny::fluidRow(
        shiny::column(
          width = menueColumnWidth,
          shiny::h1("Validate Imputation Procedure"),
          shiny::selectInput(
            "si.analysisValidationFeature",
            label = h5("Feature"),
            width = "100%",
            choices = list(),
            selected = 1
          ),
          shiny::hr(),
          shiny::actionButton(
            inputId = "ab.validation",
            label = "Validate",
            width = "100%"
          )
        ),
        shiny::column(
          width = dataColumnWidth,
          shiny::htmlOutput(outputId = "html.validateHelp"),
          shiny::h3("Validation analysis results"),
          shiny::plotOutput("plt.analysisValidationFeature"),
          shiny::hr(),
          shiny::br(),
          shiny::br(),
          shiny::h3("Test results"),
          DT::dataTableOutput("tbl.analysisValidationTest"),
          shiny::br(),
          shiny::br(),
          shiny::h3("Central moments"),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::h3("Raw data"),
              DT::dataTableOutput("tbl.centralMomentsOrg")
            ),
            shiny::column(
              width = 6,
              shiny::h3("Imputed data"),
              DT::dataTableOutput("tbl.centralMomentsImp")
            )
          ),
          shiny::h3("Deviation"),
          DT::dataTableOutput("tbl.centralMomentsDelta"),
          shiny::br(),
          shiny::br(),
          shiny::h3("Correlation Validation Analysis"),
          shiny::plotOutput("plt.correlationValidationScatter"),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::h3("Box Plot"),
              shiny::plotOutput("plt.correlationValidationBoxPlot"),
            ),
            shiny::column(
              width = 6,
              shiny::h3("Correlation Deviation Distribution"),
              DT::dataTableOutput("tbl.correlationValidationDeviation")
            ),
          ),
          shiny::br(),
          shiny::h3("Correlation Validation Data"),
          DT::dataTableOutput("tbl.correlationValidationData")
        )
      )
    )
  )

  # shinydashboard::tabItem(
  #   tabName = "tab_analysis_correlation",
  #   shiny::fluidPage(
  #     width = 12,
  #     title = "Correlation",
  #     shiny::fluidRow(
  #       shiny::column(
  #         width = menueColumnWidth,
  #         shiny::h1("Correlation"),
  #         shiny::br(),
  #         shiny::br(),
  #         shiny::actionButton(
  #           inputId = "ab.correlation",
  #           label = h5("calculate"),
  #           width = "100%"
  #         )
  #       ),
  #       shiny::column(
  #         width = dataColumnWidth,
  #         shiny::h3("Pearson"),
  #         shiny::fluidRow(
  #           shiny::column(
  #             width = 6,
  #             shiny::h3("R"),
  #             DT::dataTableOutput("tbl.correlationMatrixR")
  #           ),
  #           shiny::column(
  #             width = 6,
  #             shiny::h3("p value"),
  #             DT::dataTableOutput("tbl.correlationMatrixPPearson")
  #           )
  #         ),
  #         shiny::hr(),
  #         shiny::br(),
  #         shiny::h3("Kendall"),
  #         shiny::fluidRow(
  #           shiny::column(
  #             width = 6,
  #             shiny::h3("Tau"),
  #             DT::dataTableOutput("tbl.correlationMatrixTau")
  #           ),
  #           shiny::column(
  #             width = 6,
  #             shiny::h3("p value"),
  #             DT::dataTableOutput("tbl.correlationMatrixPKendall")
  #           )
  #         ),
  #         shiny::hr(),
  #         shiny::br(),
  #         shiny::h3("Spearman"),
  #         shiny::fluidRow(
  #           shiny::column(
  #             width = 6,
  #             shiny::h3("Rho"),
  #             DT::dataTableOutput("tbl.correlationMatrixRho")
  #           ),
  #           shiny::column(
  #             width = 6,
  #             shiny::h3("p value"),
  #             DT::dataTableOutput("tbl.correlationMatrixPSpearman")
  #           )
  #         )
  #       )
  #     )
  #   )
  # ),
  # shinydashboard::tabItem(
  #   tabName = "tab_analysis_regression",
  #   shiny::fluidPage(
  #     width = 12,
  #     title = "Regression",
  #     shiny::fluidRow(
  #       shiny::column(
  #         width = menueColumnWidth,
  #         shiny::h1("Linear Regression"),
  #         shiny::br(),
  #         shiny::hr(),
  #         shiny::br(),
  #         shiny::selectInput(
  #           "si.regressionAbs",
  #           label = h5("Abscissa"),
  #           width = "100%",
  #           choices = list(),
  #           selected = 1
  #         ),
  #         shiny::selectInput(
  #           "si.regressionOrd",
  #           label = h5("Ordinate"),
  #           width = "100%",
  #           choices = list(),
  #           selected = 1
  #         ),
  #         shiny::br(),
  #         shiny::hr(),
  #         shiny::br(),
  #         shiny::actionButton(
  #           inputId = "ab.regression",
  #           label = h5("calculate"),
  #           width = "100%"
  #         )
  #       ),
  #       shiny::column(
  #         width = dataColumnWidth,
  #         shiny::h3("Model"),
  #         shiny::fluidRow(
  #           shiny::column(
  #             width = 6,
  #             shiny::h3("Analysis"),
  #             shiny::plotOutput("plt.regressionFeature")
  #           ),
  #           shiny::column(
  #             width = 6,
  #             shiny::h3("Parameter"),
  #             DT::dataTableOutput("tbl.regressionFeature")
  #           )
  #         ),
  #         shiny::br(),
  #         shiny::br(),
  #         shiny::h3("Intercept"),
  #         shiny::fluidRow(
  #           shiny::column(
  #             width = 6,
  #             shiny::h3("value"),
  #             DT::dataTableOutput("tbl.regressionIntercept")
  #           ),
  #           shiny::column(
  #             width = 6,
  #             shiny::h3("p value"),
  #             DT::dataTableOutput("tbl.regressionPIntercept")
  #           )
  #         ),
  #         shiny::hr(),
  #         shiny::br(),
  #         shiny::h3("Slope"),
  #         shiny::fluidRow(
  #           shiny::column(
  #             width = 6,
  #             shiny::h3("value"),
  #             DT::dataTableOutput("tbl.regressionSlope")
  #           ),
  #           shiny::column(
  #             width = 6,
  #             shiny::h3("p value"),
  #             DT::dataTableOutput("tbl.regressionPSlope")
  #           )
  #         )
  #       )
  #     )
  #   )
  # )
))



