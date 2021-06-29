#' @title pgu.delegate
#'
#' @description
#' Manages the communication between the shiny gui layer and the classes of the pguIMP package
#'
#' @details
#' Comprises all needed classes from the pguIMP package and manages the communication between the gui and the analysis.
#' This object is used by the shiny based gui and is not for use in individual R-scripts!
#'
#' @format [R6::R6Class] object.
#' @importFrom R6 R6Class
#' @importFrom magrittr %>%
#' @importFrom dplyr all_of any_vars everything filter  filter_all  mutate mutate_if pull rename right_join select select_if slice summarise_all
#' @importFrom DT datatable formatStyle renderDataTable styleEqual
#' @importFrom shiny includeHTML Progress renderPlot renderUI showNotification updateCheckboxInput updateNumericInput updateSelectInput
#' @importFrom tibble add_column as_tibble rownames_to_column tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tools file_path_sans_ext
#'
#' @include pguStatus.R
#' @include pguFile.R
#' @include importDataSet.R
#' @include pguData.R
#' @include pguLimitsOfQuantification.R
#' @include pguFilter.R
#' @include pguExplorer.R
#' @include pguOptimizer.R
#' @include pguTransformator.R
#' @include pguModel.R
#' @include pguNormDist.R
#' @include pguNormalizer.R
#' @include pguMissings.R
#' @include pguMissingsCharacterizer.R
#' @include pguOutliers.R
#' @include pguImputation.R
#' @include pguValidator.R
#' @include pguCorrValidator.R
#' @include pguExporter.R
#' @include pguReporter.R
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pgu.delegate <- R6::R6Class("pgu.delegate",
                          ####################
                          # instance variables
                          ####################
                          private = list(
                            .status = "pgu.status",
                            .fileName = "pgu.file",
                            .loqFileName = "pgu.file",
                            .rawData = "pgu.data",
                            .filterSet = "pgu.filter",
                            .filteredData = "pgu.data",

                            .loq = "pgu.limitsOfQuantification",
                            .loqMutatedData = "pgu.data",
                            .explorer= "pgu.explorer",
                            .optimizer = "pgu.optimizer",
                            .transformator = "pgu.transformator",
                            .model = "pgu.model",
                            .transformedData = "pgu.data",
                            .featureModel = "pgu.normDist",
                            .normalizer = "pgu.normalizer",
                            .normalizedData = "pgu.data",
                            .missings = "pgu.missings",
                            .missingsCharacterizer = "pgu.missingsCharacterizer",
                            .outliers = "pgu.outliers",
                            .imputer = "pgu.imputation",
                            .imputedData = "pgu.data",
                            .cleanedData = "pgu.data",
                            .validator = "pgu.validatior",
                            .corrValidator = "pgu.corrValidator",
                            .exporter = "pgu.exporter",
                            .reporter = "pgu.reporter",

                            #' @description
                            #' Clears the heap and
                            #' indicates that instance of `pgu.delegate` is removed from heap.
                            finalize = function() {
                              print("Instance of pgu.delegate removed from heap")
                            } #function
                          ),
                          ##################
                          # accessor methods
                          ##################
                          active = list(
                            #' @field status
                            #' Returns the instance variable status
                            #' (pguIMP::pgu.status)
                            status = function(){
                              return(private$.status)
                            },
                            #' @field fileName
                            #' Returns the instance variable fileName
                            #' (pguIMP::pgu.file)
                            fileName = function(){
                              return(private$.fileName)
                            },
                            #' @field loqFileName
                            #' Returns the instance variable loqFileName
                            #' (pguIMP::pgu.file)
                            loqFileName = function(){
                              return(private$.loqFileName)
                            },
                            #' @field rawData
                            #' Returns the instance variable rawData
                            #' (pguIMP::pgu.data)
                            rawData = function(){
                              return(private$.rawData)
                            },
                            #' @field filterSet
                            #' Returns the instance variable filterSet
                            #' (pguIMP::pgu.filter)
                            filterSet = function(){
                              return(private$.filterSet)
                            },
                            #' @field filteredData
                            #' Returns the instance variable filteredData
                            #' (pguIMP::pgu.data)
                            filteredData = function(){
                              return(private$.filteredData)
                            },
                            #' @field loq
                            #' Returns the instance variable loq
                            #' (pguIMP::pgu.limitsOfQuantification)
                            loq = function(){
                              return(private$.loq)
                            },
                            #' @field loqMutatedData
                            #' Returns the instance variable loqMutatedData
                            #' (pguIMP::pgu.data)
                            loqMutatedData = function(){
                              return(private$.loqMutatedData)
                            },
                            #' @field explorer
                            #' Returns the instance variable explorer
                            #' (pguIMP::pgu.explorer)
                            explorer = function(){
                              return(private$.explorer)
                            },
                            #' @field optimizer
                            #' Returns the instance variable optimizer
                            #' (pguIMP::pgu.optimizer)
                            optimizer = function(){
                              return(private$.optimizer)
                            },
                            #' @field transformator
                            #' Returns the instance variable transformator
                            #' (pguIMP::pgu.transformator)
                            transformator = function(){
                              return(private$.transformator)
                            },
                            #' @field model
                            #' Returns the instance variable model
                            #' (pguIMP::pgu.model)
                            model = function(){
                              return(private$.model)
                            },
                            #' @field transformedData
                            #' Returns the instance variable transformedData
                            #' (pguIMP::pgu.data)
                            transformedData = function(){
                              return(private$.transformedData)
                            },
                            #' @field featureModel
                            #' Returns the instance variable featureModel
                            #' (pguIMP::pgu.normDist)
                            featureModel = function(){
                              return(private$.featureModel)
                            },
                            #' @field normalizer
                            #' Returns the instance variable normalizer
                            #' (pguIMP::pgu.normalizer)
                            normalizer = function(){
                              return(private$.normalizer)
                            },
                            #' @field normalizedData
                            #' Returns the instance variable normalizedData
                            #' (pguIMP::pgu.data)
                            normalizedData = function(){
                              return(private$.normalizedData)
                            },
                            #' @field missings
                            #' Returns the instance variable missings
                            #' (pguIMP::pgu.missings)
                            missings = function(){
                              return(private$.missings)
                            },
                            #' @field missingsCharacterizer
                            #' Returns the instance variable missingsCharacterizer
                            #' (pguIMP::pgu.missingsCharacterizer)
                            missingsCharacterizer = function(){
                              return(private$.missingsCharacterizer)
                            },
                            #' @field outliers
                            #' Returns the instance variable outlierd
                            #' (pguIMP::pgu.outliers)
                            outliers = function(){
                              return(private$.outliers)
                            },
                            #' @field imputer
                            #' Returns the instance variable imputer
                            #' (pguIMP::pgu.imputation)
                            imputer = function(){
                              return(private$.imputer)
                            },
                            #' @field imputedData
                            #' Returns the instance variable imputedData
                            #' (pguIMP::pgu.data)
                            imputedData = function(){
                              return(private$.imputedData)
                            },
                            #' @field cleanedData
                            #' Returns the instance variable cleanedData
                            #' (pguIMP::pgu.data)
                            cleanedData = function(){
                              return(private$.cleanedData)
                            },
                            #' @field validator
                            #' Returns the instance variable validator
                            #' (pguIMP::pgu.validator)
                            validator = function(){
                              return(private$.validator)
                            },
                            #' @field corrValidator
                            #' Returns the instance variable corrValidator
                            #' (pguIMP::pgu.corrValidator)
                            corrValidator = function(){
                              return(private$.corrValidator)
                            },
                            #' @field exporter
                            #' Returns the instance variable exporter
                            #' (pguIMP::pgu.exporter)
                            exporter = function(){
                              return(private$.exporter)
                            },
                            #' @field reporter
                            #' Returns the instance variable reporter
                            #' (pguIMP::pgu.reporter)
                            reporter = function(){
                              return(private$.reporter)
                            }
                          ),#acessors
                          ###################
                          # memory management
                          ###################
                          public = list(
                            #' @description
                            #' Creates and returns a new `pgu.delegate` object.
                            #' @param data
                            #' The data to be analyzed.
                            #' (tibble::tibble)
                            #' @return
                            #' A new `pgu.delegate` object.
                            #' (pguIMP::pgu.delegate)
                            initialize = function(data = "tbl_df") {
                              print("Instance of pgu.delegate allocated")
                              private$.status <- pgu.status$new()
                              private$.fileName <- pgu.file$new()
                              private$.loqFileName <- pgu.file$new()
                              private$.rawData <- pgu.data$new()
                              private$.loq <- pgu.limitsOfQuantification$new()
                              #private$.metadata <- pgu.data$new()
                              private$.filterSet <- pgu.filter$new()
                              private$.filteredData <- pgu.data$new()
                              #private$.filteredMetadata <- pgu.data$new()
                              private$.explorer <- pgu.explorer$new()
                              private$.loqMutatedData <- pgu.data$new()
                              private$.optimizer <- pgu.optimizer$new()
                              private$.transformator <- pgu.transformator$new()
                              private$.model <- pgu.model$new()
                              private$.transformedData <- pgu.data$new()
                              private$.featureModel <- pgu.normDist$new()
                              private$.normalizer <- pgu.normalizer$new()
                              private$.normalizedData <- pgu.data$new()
                              private$.missings <- pgu.missings$new()
                              private$.missingsCharacterizer <- pgu.missingsCharacterizer$new()
                              private$.outliers <- pgu.outliers$new()
                              private$.imputer <- pgu.imputation$new()
                              private$.imputedData <- pgu.data$new()
                              private$.cleanedData <- pgu.data$new()
                              private$.validator <- pgu.validator$new()
                              private$.corrValidator <- pgu.corrValidator$new()
                              private$.exporter <- pgu.exporter$new()
                              private$.reporter <- pgu.reporter$new()
                            }, #function



                            ##########################
                            # print instance variables
                            ##########################
                            #' @description
                            #' Prints instance variables of a `pgu.delegate` object.
                            #' @return
                            #' string
                            print = function()
                            {
                              sprintf("\npgu.delegate\n\n") %>%
                                cat()
                              print(self$status)
                              print(self$fileName)
                              print(self$loqFileName)
                              print(self$importer)
                              print(self$rawData)
                              print(self$loq)
                              print(self$metadata)
                              print(self$filterSet)
                              print(self$filteredData)
                              print(self$filteredMetadata)
                              print(self$explorer)
                              print(self$loqMutatedData)
                              print(self$optimizer)
                              print(self$transformator)
                              print(self$model)
                              print(self$transformedData)
                              print(self$featureModel)
                              print(self$normalizer)
                              print(self$normalizedData)
                              print(self$missings)
                              print(self$missingsCharacterizer)
                              print(self$outliers)
                              print(self$imputer)
                              print(self$imputedData)
                              print(self$cleanedData)
                              print(self$validator)
                              print(self$corrValidator)
                              print(self$exporter)
                              print(self$reporter)
                              invisible(self)
                            }, #end pguIMP::pgu.delegate$print()
                            ####################
                            # import functions #
                            ####################
                            #' @description
                            #' Updates the import gui
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_import_gui = function(input, output, session)
                            {
                              shiny::updateNumericInput(session = session,
                                                        inputId = "ni.importSheetIndex",
                                                        value = private$.fileName$sheetIndex)
                              shiny::updateSelectInput(session = session,
                                                       inputId = "si.importSeparator",
                                                       selected = private$.fileName$separator)
                              shiny::updateNumericInput(session = session,
                                                        inputId = "ni.importSkip",
                                                        value =  private$.fileName$skipRows)
                              shiny::updateSelectInput(session = session,
                                                       inputId = "si.importHeader",
                                                       selected = private$.fileName$header)
                              shiny::updateSelectInput(session = session,
                                                       inputId = "si.importNaChar",
                                                       selected = private$.fileName$naChar)
                            }, #end pguIMP::pgu.delegate$updateImportGui()


                            #' @description
                            #' Manages the data upload to the R server.
                            #' Updates the instance class status.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            query_data = function(input, output, session)
                            {
                              if (length(input$fi.import$datapath) > 0){
                                private$.fileName$reset(uploadFileName = input$fi.import$datapath,
                                                        fileName = input$fi.import$name,
                                                        sheetIndex = input$ni.importSheetIndex,
                                                        separator = input$si.importSeparator,
                                                        skipRows = input$ni.importSkip,
                                                        columnName = input$si.importColnames,
                                                        naChar = input$si.importNaChar)
                                private$.fileName$fit()
                                private$.status$update(processName = "dataUploaded", value = TRUE)
                              } #if
                              else{
                                private$.status$update(processName = "dataUploaded", value = FALSE)
                                shiny::showNotification(paste("File name field must not left empty."),type = "error", duration = 10)
                              } #else
                              if((private$.fileName$suffix != "csv") &&
                                 (private$.fileName$suffix != "txt") &&
                                 (private$.fileName$suffix != "xls") &&
                                 (private$.fileName$suffix != "xlsx")){
                                private$.status$update(processName = "dataUploaded", value = FALSE)
                                shiny::showNotification(paste("Invalid file type. Please select a valid file of type '.arff', '.csv', '.txt', '.xls' or '.xlsx'."),type = "error", duration = 10)
                              } #if
                            }, #end pguIMP::pgi.delegate$queryData()

                            #' @description
                            #' Imports uploaded data from the R server into the instance variable rawData.
                            #' Updates the instance class status.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            import_data = function(input, output, session)
                            {
                              if (private$.status$query(processName = "dataUploaded")){
                                tryCatch({
                                  # private$.rawData$setRawData <- private$.importer$importData(self$fileName)
                                  private$.rawData$setRawData <- pguIMP::importDataSet(obj = self$fileName)
                                  private$.rawData$fit()
                                  private$.status$update(processName = "dataImported", value = TRUE)
                                },
                                error = function(e) {
                                  private$.status$update(processName = "dataImported", value = FALSE)
                                  shiny::showNotification(paste(e),type = "error", duration = 10)
                                }#error
                                )#tryCatch
                              }#if
                              else{
                                private$.status$update(processName = "dataUploaded", value = FALSE)
                                shiny::showNotification(paste("No file uploaded to import. Please upload a valid file first."),type = "error", duration = 10)
                              }#else
                            }, #end pguIMP::pgu.delegate$importData()

                            #' @description
                            #' Updates the tbl.importDataTypes table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_import_data_Types_tbl = function(input, output, session)
                            {
                              if(private$.status$query(processName = "dataImported")){
                                output$tbl.importDataTypes <- DT::renderDataTable({self$rawData$classInformation %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '75vh',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("rawData_type") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                }#output
                                )#output
                              }#if
                              else{
                                output$tbl.importDataTypes <- DT::renderDataTable(NULL)
                              }#else
                            }, #end pguIMP::pgu.delegate$update_import_data_Types_tbl

                            #' @description
                            #' Updates the tbl.importDataStatistics table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_import_data_statistics_tbl = function(input, output, session)
                            {
                              if(private$.status$query(processName = "dataImported")){
                                output$tbl.importDataStatistics <- DT::renderDataTable({self$rawData$statistics %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '75vh',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("rawData_statistics") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                }#output
                                )#output
                              }#if
                              else{
                                output$tbl.importDataStatistics <- DT::renderDataTable(NULL)
                              }#else
                            }, #end pguIMP::pgu.delegate$update_import_data_statistics_tbl

                            #' @description
                            #' Updates the tbl.importMissingsStatistics table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_import_missings_statistics_tbl = function(input, output, session)
                            {
                              if(private$.status$query(processName = "dataImported")){
                                output$tbl.importMissingsStatistics <- DT::renderDataTable({self$rawData$missingsStatistics %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '75vh',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("rawData_missings") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                }#output
                                )#output
                              }#if
                              else{
                                output$tbl.importMissingsStatistics <- DT::renderDataTable(NULL)
                              }#else
                            }, #end pguIMP::pgu.delegate$update_import_missings_statistics_tbl


                            ####################
                            # filter functions #
                            ####################
                            #' @description
                            #' Updates the tbl.filter table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_filter_select_tbl = function(input, output, session)
                            {
                              if(private$.status$query(processName = "dataImported")){
                                output$tbl.filterSelect <- DT::renderDataTable({self$rawData$rawData %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      rownames = FALSE,
                                      filter = "top",
                                      selection = list("multiple", target = "column"),
                                      options = list(
                                        stateSave = TRUE,
                                        scrollX = TRUE,
                                        scrollY = '45vh',
                                        paging = FALSE,
                                        autoWidth = TRUE,
                                        columnDefs = list(list(width = '75px', targets = "_all")),
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("filtered_data") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#optins
                                    )#DT::datatable
                                })#output
                              }#if
                              else{
                                output$tbl.filterSelect <- DT::renderDataTable(NULL)
                              }#else
                            }, #end pguIMP::pgu.delegate$update_filter_select_tbl


                            #' @description
                            #' Queries the filter parameters selected by the user in the gui
                            #'  and stores them in the instance variable filterSet.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_filter = function(input, output, session)
                            {
                              if(private$.status$query(processName = "dataImported")){
                                # if (length(input$tbl.filter_rows_all) < 1) {
                                #   private$.filterSet$resetRowIdx(data = self$rawData$rawData)
                                # }#if
                                # else {
                                #   private$.filterSet$setRowIdx <- input$tbl.filter_rows_all
                                # }#else
                                # if (length(input$tbl.filter_columns_selected) < 1) {
                                #   private$.filterSet$resetColIdx(data = self$rawData$rawData)
                                # }#if
                                # else{
                                #   colSelection <- input$tbl.filter_columns_selected - ncol(self$metadata$rawData) + 1
                                #   colSelection <- colSelection[colSelection > 0] + 1
                                #   if(length(colSelection) < 1){
                                #     private$.filterSet$resetColIdx(data = self$rawData$rawData)
                                #   }#if
                                #   else{
                                #     colSelection <- colSelection %>%
                                #       append(1) %>%
                                #       unique() %>%
                                #       sort
                                #     private$.filterSet$setColIdx <- colSelection
                                #   }#else
                                # }#else
                                private$.filterSet$reset(data_df = self$rawData$rawData)
                                private$.filterSet$setRowIdx <- input$tbl.filterSelect_rows_all
                                if (!is.null(input$tbl.filterSelect_columns_selected))
                                {
                                  private$.filterSet$setColIdx <- input$tbl.filterSelect_columns_selected + 1
                                }
                              }#if
                              else{
                                shiny::showNotification(paste("No file imported. Please import a valid file first."),type = "error", duration = 10)
                              }#else
                            }, #end pguIMP::pgu.delegate$update_filter

                            #' @description
                            #' Queries the filter parameters selected by the user in the gui
                            #' inverts them
                            #' and stores them in the instance variable filterSet.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_filter_inverse = function(input, output, session){
                              if(private$.status$query(processName = "dataImported")){
                                # if (length(input$tbl.filter_rows_all) < 1) {
                                #   private$.filterSet$resetRowIdx(data = self$rawData$rawData)
                                # }#if
                                # else {
                                #   private$.filterSet$setRowIdx <- input$tbl.filter_rows_all
                                # }#else
                                # if (length(input$tbl.filter_columns_selected) < 1) {
                                #   private$.filterSet$resetColIdx(data = self$rawData$rawData)
                                # }#if
                                # else{
                                #   colSelection <- input$tbl.filter_columns_selected - ncol(self$metadata$rawData) + 1
                                #   colSelection <- colSelection[colSelection > 0] + 1
                                #   idx <- seq(1,ncol(self$rawData$rawData), 1)
                                #   iverseColSelection <- idx[-c(colSelection)]
                                #   if(length(iverseColSelection) < 1){
                                #     private$.filterSet$resetColIdx(data = self$rawData$rawData)
                                #   }#if
                                #   else{
                                #     iverseColSelection <- iverseColSelection %>%
                                #       append(1) %>%
                                #       unique() %>%
                                #       sort()
                                #     private$.filterSet$setColIdx <- iverseColSelection
                                #   }#else
                                # }#else
                                private$.filterSet$reset(data_df = self$rawData$rawData)
                                private$.filterSet$setRowIdx <- input$tbl.filterSelect_rows_all
                                if (!is.null(input$tbl.filterSelect_columns_selected))
                                {
                                  idx <- seq(1,ncol(self$rawData$rawData), 1)
                                  iverseColSelection <- idx[-c(input$tbl.filterSelect_columns_selected + 1)]
                                  private$.filterSet$setColIdx <- iverseColSelection
                                }
                              }#if
                              else{
                                shiny::showNotification(paste("No file imported. Please import a valid file first."),type = "error", duration = 10)
                              } #else
                            }, #function

                            #' @description
                            #' Generates a filter set that selects the whole data frame.
                            #' Stores them in the instance variable filterSet.
                            #' Updates the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            reset_filter = function(input, output, session){
                              if(private$.status$query(processName = "dataImported")){
                                private$.filterSet$reset(data_df = self$rawData$rawData)
                              }#if
                              else{
                                shiny::showNotification(paste("No file imported. Please import a valid file first."),type = "error", duration = 10)
                              }#else
                            }, #function

                            #' @description
                            #' Filters the data corresponding to the user defined parameters stored in the instance variable filterSet.
                            #' Results are stored in the instance variables filteredData and filteredMetadata.
                            #' Updated the instance variable filterSet.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            filter_data = function(input, output, session)
                            {
                              if(private$.status$query(processName = "dataImported")){
                                private$.filteredData$setRawData <- self$rawData$rawData %>%
                                  self$filterSet$predict()
                                private$.filteredData$fit()
                                private$.status$update(processName = "dataFiltered", value = TRUE)
                              }#if
                              else{
                                shiny::showNotification(paste("No file imported. Please import a valid file first."),type = "error", duration = 10)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the tbl.filterStatistics table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_filter_statistics_tbl = function(input, output, session){
                              if(private$.status$query(processName = "dataFiltered")){
                                output$tbl.filterStatistics <- DT::renderDataTable({self$filteredData$statistics %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '25vh',
                                        paging = FALSE,
                                        autoWidth = TRUE,
                                        columnDefs = list(list(width = '50px', targets = "_all")),
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("filter_statistics") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                })#output
                              }#if
                              else{
                                output$tbl.filterStatistics <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the tbl.filterMissings table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_filter_missings_tbl = function(input, output, session){
                              if(private$.status$query(processName = "dataFiltered")){
                                output$tbl.filterMissings <- DT::renderDataTable({self$filteredData$missingsStatistics %>%
                                    format.data.frame(scientific = FALSE, digits = 2) %>%
                                    DT::datatable(
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '25vh',
                                        paging = FALSE,
                                        autoWidth = TRUE,
                                        columnDefs = list(list(width = '50px', targets = "_all")),
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("filter_missings") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        )) #buttons
                                      )#options
                                    )#DT::datatable
                                })#output
                              }#if
                              else{
                                output$tbl.filterMissings <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #####################
                            # explore functions #
                            #####################

                            #' @description
                            #' Updates the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_exploration_gui = function(input, output, session){
                              if(private$.status$query(processName = "dataFiltered")){
                                private$.explorer$reset(data_df = self$filteredData$rawData,
                                                        abs = "Sample Name",
                                                        ord = "Sample Name"
                                )
                                featureNames <- self$explorer$rawData %>%
                                  colnames()
                                updateSelectInput(session, "si.exploreAbs", choices = featureNames)
                                updateSelectInput(session, "si.exploreAbs", selected = featureNames[1])
                                updateSelectInput(session, "si.exploreOrd", choices = featureNames)
                                updateSelectInput(session, "si.exploreOrd", selected = featureNames[1])
                              }#if
                            }, #function

                            #' @description
                            #' Transfers the information oabout the selected abscissa attribute to the explorer class.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_exploration_abscissa = function(input, output, session){
                              if(private$.status$query(processName = "dataFiltered"))
                              {
                                private$.explorer$setAbscissa <- input$si.exploreAbs
                                private$.explorer$fit()
                              }
                            }, #function

                            #' @description
                            #' Transfers the information oabout the selected ordinate attribute to the explorer class.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_exploration_ordinate = function(input, output, session){
                              if(private$.status$query(processName = "dataFiltered"))
                              {
                                private$.explorer$setOrdinate <- input$si.exploreOrd
                                private$.explorer$fit()
                              }
                            }, #function

                            #' @description
                            #' Updates the exploration abscissa vs. ordinate scatter plot
                            #' corresponding to the respective user defined attributes.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_exploration_graphic = function(input, output, session){
                              if(private$.status$query(processName = "dataFiltered")){
                                output$plt.exploreGraphic <- shiny::renderPlot(
                                  self$explorer$scatterPlot(),
                                  height = 400,
                                  bg="transparent")
                              }#if
                              else{
                                output$plt.exploreGraphic <- shiny::renderPlot(NULL, bg="transparent")
                              }#else
                            }, #function

                            #' @description
                            #' Updates the abscissa compound plot
                            #' corresponding to the respective user defined attributes.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_exploration_abscissa_graphic = function(input, output, session){
                              if(private$.status$query(processName = "dataFiltered")){
                                output$plt.exploreAbscissaGraphic <- shiny::renderPlot(
                                  self$explorer$abscissaPlot(),
                                  height = 400,
                                  bg="transparent"
                                )
                              }#if
                              else{
                                output$plt.exploreAbscissaGraphic <- shiny::renderPlot(NULL, bg="transparent")
                              }#else
                            }, #function

                            #' @description
                            #' Updates the ordinate compound plot
                            #' corresponding to the respective user defined attributes.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_exploration_ordinate_graphic = function(input, output, session){
                              if(private$.status$query(processName = "dataFiltered")){
                                output$plt.exploreOrdinateGraphic <- shiny::renderPlot(
                                  self$explorer$ordinatePlot(),
                                  height = 400,
                                  bg="transparent"
                                )
                              }#if
                              else{
                                output$plt.exploreOrdinateGraphic <- shiny::renderPlot(NULL, bg="transparent")
                              }#else
                            }, #function

                            #' @description
                            #' Updates the numerical abscissa analysis table.
                            #' corresponding to the respective user defined attributes.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_exploration_abscissa_table = function(input, output, session){
                              if(private$.status$query(processName = "dataFiltered")){
                                output$tbl.exploreAbscissaStatistics <- DT::renderDataTable({
                                  self$explorer$abscissaStatistics %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        # scrollY = '75vh',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict(sprintf("abs_%s_statistics", input$si.exploreAbs)) %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                })
                              }#if
                              else{
                                output$tbl.exploreAbscissaStatistics <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the numerical ordinate analysis table.
                            #' corresponding to the respective user defined attributes.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_exploration_ordinate_table = function(input, output, session){
                              if(private$.status$query(processName = "dataFiltered")){
                                output$tbl.exploreOrdinateStatistics <- DT::renderDataTable({
                                  self$explorer$ordinateStatistics %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict(sprintf("ord_%s_statistics", input$si.exploreOrd)) %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                })
                              }#if
                              else{
                                output$tbl.exploreOrdinateStatistics <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            ########################
                            # LOQ Upload functions #
                            ########################
                            #' @description
                            #' Initializes the LOQ object after filtering.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            reset_loq_values = function(input, output, session)
                            {
                              if(private$.status$query(processName = "dataFiltered"))
                              {
                                self$filteredData$numericalAttributeNames %>%
                                  private$.loq$reset()
                                self$update_loq_upload_gui(input, output, session)
                                private$.status$update(processName = "loqImported", value = TRUE)
                              }
                            },

                            #' @description
                            #' Updates the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_loq_upload_gui = function(input, output, session)
                            {
                              if(private$.status$query(processName = "dataFiltered"))
                              {
                                output$tbl.loqUploadValues  <- DT::renderDataTable({
                                  self$loq$loq %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '75vh',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("LOQ_values") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                })
                              }#if
                              else{
                                output$tbl.loqDefineValues <- DT::renderDataTable(NULL)
                              }#else
                            },

                            #' @description
                            #' Manages the loq data upload to the R server.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            query_loq = function(input, output, session)
                            {
                              if(private$.status$query(processName = "dataFiltered")){
                                if (length(input$fi.LoqImport$datapath) > 0){
                                  private$.loqFileName$reset(uploadFileName = input$fi.LoqImport$datapath,
                                                             fileName = input$fi.LoqImport$name,
                                                             sheetIndex = input$ni.LoqSheetIndex,
                                                             separator = input$si.LoqSeparator,
                                                             skipRows = input$ni.LoqSkip,
                                                             columnName = input$si.LoqColnames,
                                                             naChar = input$si.LoqNaChar)
                                  private$.loqFileName$fit()
                                } #if
                                else{
                                  shiny::showNotification(paste("File name field must not left empty."),type = "error", duration = 10)
                                } #else
                                if((private$.fileName$suffix != "csv") &&
                                   (private$.fileName$suffix != "txt") &&
                                   (private$.fileName$suffix != "xls") &&
                                   (private$.fileName$suffix != "xlsx")){
                                  shiny::showNotification(paste("Invalid file type. Please select a valid file of type '.csv', '.txt', '.xls' or '.xlsx'."),type = "error", duration = 10)
                                } #if
                              }
                            }, #end pguIMP::pgi.delegate$query_loq()

                            #' @description
                            #' Imports the loq data upload to the R server.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            import_loq = function(input, output, session)
                            {
                              if(private$.status$query(processName = "dataFiltered"))
                              {
                                private$.loq$reset(attribute_names = self$filteredData$numericalAttributeNames,
                                                   data_df = pguIMP::importDataSet(obj = self$loqFileName))
                                self$update_loq_upload_gui(input, output, session)
                                private$.status$update(processName = "loqImported", value = TRUE)
                              }
                            },

                            ########################
                            # LOQ Define functions #
                            ########################
                            #' @description
                            #' Updates the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_loq_define_gui = function(input, output, session)
                            {
                              if(private$.status$query(processName = "dataFiltered"))
                              {
                                self$update_loq_define_feature(input, output, session)
                                self$update_loq_define_lloq(input, output, session)
                                self$update_loq_define_uloq(input, output, session)
                                self$update_loq_define_table(input, output, session)
                              }
                            },

                            #' @description
                            #' Updates the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_loq_define_feature = function(input, output, session)
                            {
                              if(private$.status$query(processName = "dataFiltered"))
                              {
                                shiny::updateSelectInput(session,
                                                         inputId = "si.LoqDefineFeature",
                                                         choices = self$loq$loq$attribute,
                                                         selected = self$loq$loq$attribute[1])
                              }
                            },

                            #' @description
                            #' Updates the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_loq_define_lloq = function(input, output, session)
                            {
                              if(private$.status$query(processName = "dataFiltered"))
                              {
                                shiny::updateNumericInput(session,
                                                          inputId = "ni.LoqDefineLLOQ",
                                                          value = self$loq$attribute_lloq(attribute = input$si.LoqDefineFeature))
                              }
                            },

                            #' @description
                            #' Updates the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_loq_define_uloq = function(input, output, session)
                            {
                              if(private$.status$query(processName = "dataFiltered"))
                              {
                                shiny::updateNumericInput(session,
                                                          inputId = "ni.LoqDefineULOQ",
                                                          value = self$loq$attribute_uloq(attribute = input$si.LoqDefineFeature))
                              }
                            },

                            #' @description
                            #' Updates the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_loq_define_table = function(input, output, session)
                            {
                              if(private$.status$query(processName = "dataFiltered"))
                              {
                                output$tbl.loqDefineValues  <- DT::renderDataTable({
                                  self$loq$loq %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '75vh',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("LOQ_values") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                })
                              }#if
                              else{
                                output$tbl.loqDefineValues <- DT::renderDataTable(NULL)
                              }#else
                            },

                            #' @description
                            #' Updates the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_loq_define_menu = function(input, output, session)
                            {
                              if(private$.status$query(processName = "dataFiltered"))
                              {
                                self$update_loq_define_lloq(input, output, session)
                                self$update_loq_define_uloq(input, output, session)
                              }
                            },

                            #' @description
                            #' Updates loq class.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            set_loq_define_values = function(input, output, session)
                            {
                              if(private$.status$query(processName = "dataFiltered"))
                              {
                                private$.loq$set_attribute_lloq(attribute = input$si.LoqDefineFeature,
                                                                value = input$ni.LoqDefineLLOQ)
                                private$.loq$set_attribute_uloq(attribute = input$si.LoqDefineFeature,
                                                                value = input$ni.LoqDefineULOQ)
                              }
                            },

                            #' @description
                            #' Updates loq class.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            set_loq_define_values_globally = function(input, output, session)
                            {
                              if(private$.status$query(processName = "dataFiltered"))
                              {
                                for(attribute in self$loq$loq$attribute)
                                {
                                  private$.loq$set_attribute_lloq(attribute = attribute,
                                                                  value = input$ni.LoqDefineLLOQ)
                                  private$.loq$set_attribute_uloq(attribute = attribute,
                                                                  value = input$ni.LoqDefineULOQ)

                                }
                              }
                            },

                            ########################
                            # LOQ Detect functions #
                            ########################
                            #' #' @description
                            #' #' Imports uploaded data from the R server into the instance variable loqData.
                            #' #' Updates the instance class status.
                            #' #' @param input
                            #' #' Pointer to shiny input
                            #' #' @param output
                            #' #' Pointer to shiny output
                            #' #' @param session
                            #' #' Pointer to shiny session
                            #' importLoq = function(input, output, session){
                            #'   if (private$.status$query(processName = "dataImported")){
                            #'     tryCatch({
                            #'       private$.loq$setLoq <- private$.importer$importLoq(self$fileName)
                            #'       private$.status$update(processName = "loqImported", value = TRUE)
                            #'     },
                            #'     error = function(e) {
                            #'       private$.status$update(processName = "loqImported", value = FALSE)
                            #'       shiny::showNotification(paste(e),type = "error", duration = 10)
                            #'     }#error
                            #'     )#tryCatch
                            #'   }#if
                            #'   else{
                            #'     private$.status$update(processName = "loqImported", value = FALSE)
                            #'     shiny::showNotification(paste("No file uploaded to import. Please upload a valid file first."),type = "error", duration = 10)
                            #'   }#else
                            #' }, #function



                            #' @description
                            #' Updates the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_loq_detect_gui = function(input, output, session){
                              if(private$.status$query(processName = "loqImported")){
                                self$update_loq_na_handling(input, output, session)
                                shiny::updateSelectInput(session,
                                                         "si.loqDetectFeature",
                                                         choices = self$filteredData$numericalAttributeNames,
                                                         selected = self$filteredData$numericAttributeNames[1])

                              }#if
                              else{
                                shiny::showNotification(paste("No filtered data set. Please filter data set first."),type = "error", duration = 10)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the si.loqHandling shiny widget
                            #' corresponding to the respective user defined parameter.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_loq_na_handling = function(input, output, session){
                              if(private$.status$query(processName = "loqImported"))
                              {
                                shiny::updateSelectInput(session,
                                                         "si.loqNaHandling",
                                                         choices = self$loq$naHandlingAlphabet,
                                                         selected = self$loq$naHandlingAgent)
                              }
                            }, #function

                            #' @description
                            #' Runs the outlier detection routine of the instance variable outliers.
                            #' Updates the instance class status.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            init_detect_loq = function(input, output, session){
                              if(private$.status$query(processName = "loqImported")){
                                private$.loq$setNaHandlingAgent <- self$loq$naHandlingAlphabet[1]
                                self$filteredData$numerical_data() %>%
                                  private$.loq$fit()
                                private$.status$update(processName = "loqDetected", value = TRUE)
                              }#if
                              else{
                                shiny::showNotification(paste("No filtered data set. Please filter data set first."),type = "error", duration = 10)
                              }#else
                            }, #function

                            #' @description
                            #' Runs the outlier detection routine of the instance variable outliers.
                            #' Updates the instance class status.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            detect_loq = function(input, output, session){
                              if(private$.status$query(processName = "loqImported")){
                                private$.loq$setNaHandlingAgent <- input$si.loqNaHandling
                                self$filteredData$numerical_data() %>%
                                  private$.loq$fit()
                                private$.status$update(processName = "loqDetected", value = TRUE)
                              }#if
                              else{
                                shiny::showNotification(paste("No filtered data set. Please filter data set first."),type = "error", duration = 10)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the numerical loq statistics analysis table
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_loq_detect_statistics_tbl = function(input, output, session){
                              if(private$.status$query(processName = "loqDetected")){
                                output$tbl.loqDetectStatistics <- DT::renderDataTable(
                                  self$loq$loqStatistics %>%
                                    format.data.frame(scientific = FALSE, digits = 3) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("loqStatistics") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.loqDetectStatistics <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the numerical loq table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_loq_detect_outlier_tbl = function(input, output, session){
                              if(private$.status$query(processName = "loqDetected"))
                              {
                                output$tbl.loqDetectOutlier <- DT::renderDataTable(
                                  self$loq$outliers %>%
                                    format.data.frame(scientific = FALSE, digits = 3) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("loqOutliers") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }else{
                                output$tbl.loqDetectOutlier <- DT::renderDataTable(NULL)
                              }

                              # options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
                              # t <- NULL
                              # if(self$status$query(processName = "loqDetected")){
                              #   dfData <- self$filteredMetadata$rawData %>%
                              #     dplyr::right_join(self$filteredData$rawData, by = "Sample Name")
                              #   dfOutlier <- self$loq$outliers
                              #   idx <- dfOutlier[["measurement"]][!duplicated(dfOutlier[["measurement"]])]
                              #   t <- dfData %>%
                              #     dplyr::slice(idx) %>%
                              #     dplyr::mutate_if(is.numeric, round, 3) %>%
                              #     DT::datatable(
                              #       options = list(
                              #         scrollX = TRUE,
                              #         scrollY = '350px',
                              #         paging = FALSE,
                              #         dom = "Blfrtip",
                              #         buttons = list(list(
                              #           extend = 'csv',
                              #           filename = self$fileName$bluntFileName("loqOutlier"),
                              #           text = "Download"
                              #         ))#buttons
                              #       )#options
                              #     )#DT::datatable
                              #   for (featureName in self$filteredData$numericFeatureNames){
                              #     featureOutlier <- dfOutlier %>%
                              #       dplyr::filter(feature == featureName) %>%
                              #       dplyr::mutate_if(is.numeric, round, 3)
                              #     if (nrow(featureOutlier)>0){
                              #       t <- DT::formatStyle(t,
                              #                            featureName,
                              #                            backgroundColor = styleEqual(dfData %>%
                              #                                                           dplyr::select(!!featureName) %>%
                              #                                                           dplyr::slice(featureOutlier[["measurement"]]) %>%
                              #                                                           unlist() %>%
                              #                                                           as.numeric() %>%
                              #                                                           round(digits = 3),
                              #                                                         featureOutlier[["color"]])
                              #
                              #       )#t
                              #     }#if
                              #   }#for
                              # }#if
                              # output$tbl.loqDetectOutlier <- DT::renderDataTable(t)
                            }, #function

                            #' @description
                            #' Updates the loq statistics graphic.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_loq_detect_statistics_graphic = function(input, output, session){
                              if(self$status$query(processName = "loqDetected")){
                                output$plt.loqDetectStatistics <- shiny::renderPlot(
                                  self$loq$plot_loq_distribution(),
                                  height = 400,
                                  bg="transparent"
                                )
                              }#if
                              else{
                                output$plt.loqDetectStatistics <- shiny::renderPlot(NULL, bg="transparent")
                              }#else
                            }, #function

                            #' @description
                            #' Updates the loq feature compound graphic.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_loq_detect_attribute_graphic = function(input, output, session){
                              if(self$status$query(processName = "loqDetected")){
                                output$plt.loqDetectFeature <- shiny::renderPlot(
                                  self$loq$attribute_plot(data_df = self$filteredData$rawData, attribute = input$si.loqDetectFeature),
                                  height = 425,
                                  bg="transparent"
                                )
                              }#if
                              else{
                                output$plt.loqDetectFeature <- shiny::renderPlot(NULL, bg="transparent")
                              }#else
                            }, #function


                            #' @description
                            #' Updates the numerical loq feature table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_loq_detect_attribute_tbl = function(input, output, session){
                              if(private$.status$query(processName = "loqDetected"))
                              {
                                suffix <- sprintf("%s_outliers", input$si.loqDetectFeature)
                                output$tbl.loqDetectFeature <- DT::renderDataTable(
                                  self$loq$attribute_outliers(attribute = input$si.loqDetectFeature) %>%
                                    format.data.frame(scientific = FALSE, digits = 3) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict(suffix) %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }else{
                                output$tbl.loqDetectFeature <- DT::renderDataTable(NULL)
                              }

                              # options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
                              # t <- NULL
                              # if(private$.status$query(processName = "loqDetected")){
                              #   feature <- input$si.loqDetectFeature
                              #   dfData <- self$filteredMetadata$rawData %>%
                              #     dplyr::right_join(self$filteredData$rawData %>%
                              #                         dplyr::select(c("Sample Name", !!feature)),
                              #                       by = "Sample Name")
                              #
                              #   dfOutlier <- self$loq$featureOutlier(feature = feature)
                              #
                              #   t <- dfData %>%
                              #     DT::datatable(options = list(scrollX = TRUE,
                              #                                  scrollY = '300px',
                              #                                  paging = FALSE,
                              #                                  dom = "Blfrtip",
                              #                                  buttons = list(list(
                              #                                    extend = 'csv',
                              #                                    filename = self$fileName$bluntFileName("rawData_type"),
                              #                                    text = "Download"
                              #                                  ))
                              #     ))#DT::datatable
                              #   if (nrow(dfOutlier) > 0){
                              #     t <- DT::formatStyle(
                              #       t,
                              #       feature,
                              #       backgroundColor = DT::styleEqual(dfData %>%
                              #                                          dplyr::select(!!feature) %>%
                              #                                          dplyr::slice(dfOutlier[["measurement"]]) %>%
                              #                                          unlist() %>%
                              #                                          round(digits = 3),
                              #                                        dfOutlier[["color"]]))
                              #   }#if
                              # }#if
                              # output$tbl.loqDetectFeature <- DT::renderDataTable(t)
                            }, #function

                            ########################
                            # LOQ mutate functions #
                            ########################
                            #' @description
                            #' Updates the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_loq_mutate_gui = function(input, output, session)
                            {
                              if(private$.status$query(processName = "loqDetected"))
                              {
                                self$update_lloq_substitute(input, output, session)
                                self$update_uloq_substitute(input, output, session)
                                shiny::updateSelectInput(session,
                                                         "si.loqMutateFeature",
                                                         choices = self$filteredData$numericalAttributeNames,
                                                         selected = self$filteredData$numericalAttributeNames[1])

                              }#if
                              else{
                                shiny::showNotification(paste("No loq outliers detected. Please screen for loq outliers first."),type = "error", duration = 10)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the si.lloqSubstitute shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_lloq_substitute = function(input, output, session)
                            {
                              shiny::updateSelectInput(session,
                                                       "si.lloqSubstitute",
                                                       choices = self$loq$lloqSubstituteAlphabet,
                                                       selected = self$loq$lloqSubstituteAgent)
                            }, #function

                            #' @description
                            #' Updates the si.uloqSubstitute shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_uloq_substitute = function(input, output, session)
                            {
                              shiny::updateSelectInput(session,
                                                       "si.uloqSubstitute",
                                                       choices = self$loq$uloqSubstituteAlphabet,
                                                       selected = self$loq$uloqSubstituteAgent)
                            }, #function

                            #' @description
                            #' Calls the mutation routine of the instance variable loq on the instance variable filteredData.
                            #' The reult is stored in the instance variable loqMutatedData
                            #' Updates the instance class status.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            init_mutate_loq = function(input, output, session)
                            {
                              if(private$.status$query(processName = "loqDetected"))
                              {
                                private$.loq$setLloqSubstituteAgent <- self$loq$lloqSubstituteAlphabet[1]
                                private$.loq$setUloqSubstituteAgent <- self$loq$uloqSubstituteAlphabet[1]

                                private$.loqMutatedData$setRawData <- self$filteredData$numerical_data() %>%
                                  self$loq$predict() %>%
                                  tibble::add_column(self$filteredData$categorical_data()) %>%
                                  dplyr::select(dplyr::all_of(self$filteredData$attributeNames))
                                private$.loqMutatedData$fit()
                                private$.status$update(processName = "loqMutated", value = TRUE)
                              }#if
                              else{
                                shiny::showNotification(paste("No loq outliers detected. Please screen for loq outliers first."),type = "error", duration = 10)
                              }#else
                            }, #function

                            #' @description
                            #' Calls the mutation routine of the instance variable loq on the instance variable filteredData.
                            #' The reult is stored in the instance variable loqMutatedData
                            #' Updates the instance class status.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            mutate_loq = function(input, output, session)
                            {
                              if(private$.status$query(processName = "loqDetected"))
                              {
                                private$.loq$setLloqSubstituteAgent <- input$si.lloqSubstitute
                                private$.loq$setUloqSubstituteAgent <- input$si.uloqSubstitute

                                private$.loqMutatedData$setRawData <- self$filteredData$numerical_data() %>%
                                  self$loq$predict() %>%
                                  tibble::add_column(self$filteredData$categorical_data()) %>%
                                  dplyr::select(dplyr::all_of(self$filteredData$attributeNames))
                                private$.loqMutatedData$fit()
                                private$.status$update(processName = "loqMutated", value = TRUE)
                              }#if
                              else{
                                shiny::showNotification(paste("No loq outliers detected. Please screen for loq outliers first."),type = "error", duration = 10)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the numerical loq mutate outliers table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_loq_mutate_data_tbl = function(input, output, session){
                              if(private$.status$query(processName = "loqMutated"))
                              {
                                output$tbl.loqMutateData <- DT::renderDataTable(
                                  self$loqMutatedData$rawData %>%
                                    format.data.frame(scientific = FALSE, digits = 3) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("loq_mutated_data") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }else{
                                output$tbl.loqMutateData <- DT::renderDataTable(NULL)
                              }
                              # options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
                              # t <- NULL
                              # if(self$status$query(processName = "loqMutated")){
                              #   dfData <- self$filteredMetadata$rawData %>%
                              #     dplyr::right_join(self$loqMutatedData$rawData, by = "Sample Name")
                              #   dfOutlier <- self$loq$outliers
                              #   idx <- dfOutlier[["measurement"]][!duplicated(dfOutlier[["measurement"]])]
                              #   t <- dfData %>%
                              #     dplyr::mutate_if(is.numeric, round, 3) %>%
                              #     DT::datatable(
                              #       options = list(
                              #         scrollX = TRUE,
                              #         scrollY = '350px',
                              #         paging = FALSE,
                              #         dom = "Blfrtip",
                              #         buttons = list(list(
                              #           extend = 'csv',
                              #           filename = self$fileName$bluntFileName("loqData"),
                              #           text = "Download"
                              #         ))#buttons
                              #       )#options
                              #     )#DT::datatable
                              #   for (featureName in self$loqMutatedData$numericFeatureNames){
                              #     featureOutlier <- dfOutlier %>%
                              #       dplyr::filter(feature == featureName) %>%
                              #       dplyr::mutate_if(is.numeric, round, 3)
                              #     if (nrow(featureOutlier)>0){
                              #       t <- DT::formatStyle(t,
                              #                            featureName,
                              #                            backgroundColor = styleEqual(dfData %>%
                              #                                                           dplyr::select(!!featureName) %>%
                              #                                                           dplyr::slice(featureOutlier[["measurement"]]) %>%
                              #                                                           unlist() %>%
                              #                                                           as.numeric() %>%
                              #                                                           round(digits = 3),
                              #                                                         featureOutlier[["color"]])
                              #
                              #       )#t
                              #     }#if
                              #   }#for
                              # }#if
                              # output$tbl.loqMutateData <- DT::renderDataTable(t)
                            }, #function

                            #' @description
                            #' Updates the loq mutate statistics graphic.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_loq_mutate_statistics_graphic = function(input, output, session)
                            {
                              if(self$status$query(processName = "loqMutated"))
                              {
                                output$plt.loqMutateStatistics <- shiny::renderPlot(
                                  self$loq$plot_loq_distribution(),
                                  height = 400,
                                  bg="transparent"
                                )#output
                              }#if
                              else{
                                output$plt.loqMutateStatistics <- shiny::renderPlot(NULL, bg="transparent")
                              }#else
                            }, #function

                            #' @description
                            #' Updates the loq mutate feature graphic.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_loq_mutate_attribute_graphic = function(input, output, session)
                              {
                              if(self$status$query(processName = "loqMutated"))
                                {
                                output$plt.loqMutateFeature <- shiny::renderPlot(
                                  self$loq$attribute_plot(data_df = self$loqMutatedData$rawData, attribute = input$si.loqMutateFeature),
                                  height = 425,
                                  bg="transparent"
                                )#output
                              }#if
                              else{
                                output$plt.loqMutateFeature <- shiny::renderPlot(NULL, bg="transparent")
                              }#else
                            }, #function

                            #' @description
                            #' Updates the numeric loq mutate feature table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            init_loq_mutate_attribute_tbl = function(input, output, session){
                              if(private$.status$query(processName = "loqMutated"))
                              {
                                suffix <- sprintf("%s_outliers_mutated", self$loq$loq$attribute[1])
                                data_df <- self$loq$attribute_outliers(attribute = self$loq$loq$attribute[1])
                                idx <- dplyr::pull(data_df, instance)

                                data_df <- data_df %>%
                                  dplyr::mutate(mutated = dplyr::pull(self$loqMutatedData$rawData, self$loq$loq$attribute[1])[idx])

                                output$tbl.loqMutateFeature <- DT::renderDataTable(
                                  data_df %>%
                                    format.data.frame(scientific = FALSE, digits = 3) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict(suffix) %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }else{
                                output$tbl.loqMutateFeature <- DT::renderDataTable(NULL)
                              }
                            },

                            #' @description
                            #' Updates the numeric loq mutate feature table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_loq_mutate_attribute_tbl = function(input, output, session){
                              if(private$.status$query(processName = "loqMutated"))
                              {
                                suffix <- sprintf("%s_outliers_mutated", input$si.loqMutateFeature)
                                data_df <- self$loq$attribute_outliers(attribute = input$si.loqMutateFeature)
                                idx <- dplyr::pull(data_df, instance)

                                data_df <- data_df %>%
                                  dplyr::mutate(mutated = dplyr::pull(self$loqMutatedData$rawData, input$si.loqMutateFeature)[idx])

                                output$tbl.loqMutateFeature <- DT::renderDataTable(
                                  data_df %>%
                                    format.data.frame(scientific = FALSE, digits = 3) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict(suffix) %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }else{
                                output$tbl.loqMutateFeature <- DT::renderDataTable(NULL)
                              }

                              # options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
                              # t <- NULL
                              # if(self$status$query(processName = "loqMutated")){
                              #   feature <- input$si.loqMutateFeature
                              #   dfData <- self$filteredMetadata$rawData %>%
                              #     dplyr::right_join(self$loqMutatedData$rawData %>%
                              #                         dplyr::select(c("Sample Name", !!feature)),
                              #                       by = "Sample Name")
                              #
                              #   dfOutlier <- self$loq$featureOutlier(feature = feature)
                              #
                              #   t <- dfData %>%
                              #     DT::datatable(options = list(scrollX = TRUE,
                              #                                  scrollY = '300px',
                              #                                  paging = FALSE,
                              #                                  dom = "Blfrtip",
                              #                                  buttons = list(list(
                              #                                    extend = 'csv',
                              #                                    filename = self$fileName$bluntFileName("rawData_type"),
                              #                                    text = "Download"
                              #                                  ))#buttons
                              #     ))#DT::datatable
                              #   if (nrow(dfOutlier) > 0){
                              #     t <- DT::formatStyle(
                              #       t,
                              #       feature,
                              #       backgroundColor = DT::styleEqual(dfData %>%
                              #                                          dplyr::select(!!feature) %>%
                              #                                          dplyr::slice(dfOutlier[["measurement"]]) %>%
                              #                                          unlist() %>%
                              #                                          round(digits = 3),
                              #                                        dfOutlier[["color"]])
                              #       )#t
                              #   }#if
                              # }#if
                              # output$tbl.loqMutateFeature <- DT::renderDataTable(t)
                            }, #function

                            #############################################
                            # Trafo Detect functions (Parameter Wizard) #
                            #############################################
                            #' @description
                            #' Calls the optimize routine of the instance variable optimizer on the instance variable loqMutatedData.
                            #' Updates the instance class status.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            optimizeTrafoParameter = function(input, output, session){
                              if(self$status$query(processName = "loqMutated")){
                                trafoAlphabet <- c("none")
                                if(input$cb.wizardLog){
                                  trafoAlphabet <- c(trafoAlphabet, "log2", "logNorm", "log10")
                                }#if
                                if(input$cb.wizardRoot){
                                  trafoAlphabet <- c(trafoAlphabet, "squareRoot", "cubeRoot")
                                }#if
                                if(input$cb.wizardArcsine){
                                  trafoAlphabet <- c(trafoAlphabet, "arcsine")
                                }#if
                                if(input$cb.wizardInverse){
                                  trafoAlphabet <- c(trafoAlphabet, "inverse")
                                }#if
                                if(input$cb.wizardTLOP){
                                  trafoAlphabet <- c(trafoAlphabet, "tukeyLOP")
                                }#if
                                if(input$cb.wizardBoxCox){
                                  trafoAlphabet <- c(trafoAlphabet, "boxCox")
                                }#if
                                # data in obj ändern
                                private$.optimizer$resetOptimizer(data = self$loqMutatedData$numerical_ata())
                                private$.optimizer$setTrafoAlphabet <- trafoAlphabet
                                private$.optimizer$setMirror <- input$cb.wizardMirror
                                progress <- shiny::Progress$new(session, min = 1, max = length(self$optimizer$trafoAlphabet)*2)
                                progress$set(message = "optimizing transformation parameters ...", value = 1)
                                private$.optimizer$optimize(data = self$loqMutatedData$numerical_data(), progress = progress)
                                on.exit(progress$close())
                                private$.status$update(processName = "modelOptimized", value = TRUE)
                              }#if
                              else{
                                shiny::showNotification(paste("No loq analysis perfomred. Please mutate loq outliers first."),type = "error", duration = 10)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the detected trafo types table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateDetectedTrafoTypes = function(input, output, session){
                              if(self$status$query(processName = "modelOptimized")){
                                output$tbl.trafoDetectTypes <- DT::renderDataTable(
                                  self$optimizer$optTypes %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$bluntFileName("optTrafoTypes"),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.trafoDetectTypes <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the detected trafo parameters table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateDetectedTrafoParameter = function(input, output, session){
                              if(self$status$query(processName = "modelOptimized")){
                                output$tbl.trafoDetectParameters <- DT::renderDataTable(
                                  self$optimizer$optParameter %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$bluntFileName("optTrafoParameter"),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.trafoDetectParameters <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoDetectGui = function(input, output, session){
                              if(self$status$query(processName = "loqMutated")){
                                trafoAlphabet <- self$optimizer$trafoAlphabet
                                shiny::updateCheckboxInput(session,
                                                           "cb.wizardLog",
                                                           value = any(grepl(pattern = "log",
                                                                             x = trafoAlphabet)))
                                shiny::updateCheckboxInput(session,
                                                           "cb.wizardRoot",
                                                           value = any(grepl(pattern = "Root",
                                                                             x = trafoAlphabet)))
                                shiny::updateCheckboxInput(session,
                                                           "cb.wizardArcsine",
                                                           value = any(grepl(pattern = "arcsine",
                                                                             x = trafoAlphabet)))
                                shiny::updateCheckboxInput(session,
                                                           "cb.wizardInverse",
                                                           value = any(grepl(pattern = "inverse",
                                                                             x = trafoAlphabet)))
                                shiny::updateCheckboxInput(session,
                                                           "cb.wizardTLOP",
                                                           value = any(grepl(pattern = "tukeyLOP",
                                                                             x = trafoAlphabet)))
                                shiny::updateCheckboxInput(session,
                                                           "cb.wizardBoxCox",
                                                           value = any(grepl(pattern = "boxCox",
                                                                             x = trafoAlphabet)))
                                shiny::updateCheckboxInput(session,
                                                           "cb.wizardMirror",
                                                           value = self$optimizer$mirror)
                              }#if
                              else{
                                shiny::showNotification(paste("No loq analysis perfomred. Please mutate loq outliers first."),type = "error", duration = 10)
                              }#else
                            }, #function

                            ##############################################
                            # trafo mutate functions (model by gaussian) #
                            ##############################################
                            #' @description
                            #' Updates the si.trafoMutateFeature shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoMutateFeature = function(input, output, session){
                              if(self$status$query(processName = "loqMutated")){
                                shiny::updateSelectInput(session,
                                                         inputId = "si.trafoMutateFeature",
                                                         choices = self$loqMutatedData$numericalAttributeNames,
                                                         selected = self$loqMutatedData$numericalAttributeNames[1])
                              }#if
                            }, #function

                            #' @description
                            #' Updates the si.trafoMutateType shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoMutateType = function(input, output, session){
                              if(self$status$query(processName = "loqMutated")){
                                shiny::updateSelectInput(session,
                                                         inputId = "si.trafoMutateType",
                                                         choices = self$transformator$trafoAlphabet,
                                                         selected = self$transformator$trafoType(feature = input$si.trafoMutateFeature)
                                )
                              }#if
                            }, #function

                            #' @description
                            #' Updates the ni.trafoMutateLambda shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoMutateLambda = function(input, output, session){
                              if(self$status$query(processName = "loqMutated")){
                                shiny::updateNumericInput(session,
                                                         inputId = "ni.trafoMutateLambdaLOP",
                                                         value = self$transformator$lambdaLOP(feature = input$si.trafoMutateFeature))
                              }#if
                            }, #function

                            #' @description
                            #' Updates the cb.trafoMutateMirror shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoMutateMirror = function(input, output, session){
                              if(self$status$query(processName = "loqMutated")){
                                shiny::updateCheckboxInput(session,
                                                           inputId = "cb.trafoMutateMirror",
                                                           value = self$transformator$mirrorLogic(feature = input$si.trafoMutateFeature)
                                )
                              }#if
                            }, #function

                            #' @description
                            #' Updates the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            resetTrafoMutateGui = function(input, output, session){
                              if(self$status$query(processName = "loqMutated")){
                                self$loqMutatedData$numerical_data() %>%
                                  private$.transformator$resetTrafoParameter()
                                self$updateTrafoMutateFeature(input, output, session)
                                self$updateTrafoMutateType(input, output, session)
                                self$updateTrafoMutateLambda(input, output, session)
                                self$updateTrafoMutateMirror(input, output, session)
                              }#if
                            }, #function

                            #' @description
                            #' Updates the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoMutateGui = function(input, output, session){
                              if(self$status$query(processName = "loqMutated")){
                                self$updateTrafoMutateType(input, output, session)
                                self$updateTrafoMutateLambda(input, output, session)
                                self$updateTrafoMutateMirror(input, output, session)
                              }#if
                            }, #function

                            #' @description
                            #' Estimates the optimal transformation parameters.
                            #' Updates the GUI
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            trafoMutateFit = function(input, output, session){
                              if(self$status$query(processName = "loqMutated")){
                                progress <- shiny::Progress$new(session, min = 1, max = length(self$loqMutatedData$numericalAttributeNames))
                                progress$set(message = "Optimizing model parameter", value = 1)
                                self$loqMutatedData$numerical_data() %>%
                                  private$.transformator$resetTrafoParameter()
                                on.exit(progress$close())
                                self$loqMutatedData$numerical_data() %>%
                                  private$.transformator$fit()
                                private$.status$update(processName = "modelOptimized", value = TRUE)
                              }#if
                              else{
                                shiny::showNotification(paste("No loq analysis perfomred. Please mutate loq outliers first."),type = "error", duration = 10)
                              }#else
                            }, #function

                            #' @description
                            #' Calls the transformation routine of the instance variable transformator on the instance variable loqMutatedData.
                            #' Updates the instance class status.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            trafoMutateGlobal = function(input, output, session){
                              if(self$status$query(processName = "modelOptimized")){
                                progress <- shiny::Progress$new(session, min = 1, max = length(self$loqMutatedData$numericalAttributeNames))
                                progress$set(message = "Perfomring transformation", value = 1)
                                on.exit(progress$close())
                                # self$loqMutatedData$numericData() %>%
                                #   private$.transformator$resetTrafoParameter()
                                for (feature in self$loqMutatedData$numericalAttributeNames){
                                  private$.transformator$setTrafoType(feature = feature,
                                                                      type = input$si.trafoMutateType)
                                  private$.transformator$setMirrorLogic(feature = feature,
                                                                        logic = input$cb.trafoMutateMirror)
                                }#for

                                # self$loqMutatedData$numericData() %>%
                                #   private$.transformator$estimateTrafoParameter()
                                self$loqMutatedData$numerical_data() %>%
                                  private$.transformator$mutateData() %>%
                                  private$.model$resetModel(progress)
                                private$.transformedData$setRawData <- self$loqMutatedData$numerical_data() %>%
                                  self$transformator$mutateData() %>%
                                  # self$model$scaleData() %>%
                                  tibble::add_column(self$filteredData$categorical_data()) %>%
                                  dplyr::select(dplyr::all_of(self$filteredData$attributeNames))
                                private$.transformedData$fit()
                                private$.status$update(processName = "modelDefined", value = TRUE)
                              }#if
                              else{
                                shiny::showNotification(paste("Model parameters not optimized. Please optimize first."),type = "error", duration = 10)
                              }#else
                            }, #function

                            #' @description
                            #' Calls the transformation routine of the instance variable transformator on a user defined attribute
                            #' of the instance variable loqMutatedData.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            trafoMutateFeature = function(input, output, session){
                              if(self$status$query(processName = "modelDefined")){
                                private$.transformator$setTrafoType(feature = input$si.trafoMutateFeature,
                                                                    type = input$si.trafoMutateType)
                                private$.transformator$setLambdaLOP(feature = input$si.trafoMutateFeature,
                                                                    lambda = input$ni.trafoMutateLambdaLOP)
                                private$.transformator$setMirrorLogic(feature = input$si.trafoMutateFeature,
                                                                      logic = input$cb.trafoMutateMirror)

                                # self$loqMutatedData$numericData() %>%
                                #   private$.transformator$estimateTrafoParameter()

                                self$loqMutatedData$numerical_data() %>%
                                  self$transformator$mutateData() %>%
                                  dplyr::select(input$si.trafoMutateFeature) %>%
                                  private$.featureModel$resetNormDist()

                                tryCatch({
                                  private$.featureModel$fit()
                                },
                                error = function(e) {
                                  errorString <- sprintf("Error: could not optimize model parameters for  %s transformation of feature %s. Trafo type is reset to 'none'",
                                                         input$si.trafoMutateType,
                                                         input$si.trafoMutateFeature)
                                  shiny::showNotification(paste(errorString),type = "error", duration = 10)

                                  shiny::updateSelectInput(session,
                                                           inputId = "si.trafoMutateType",
                                                           choices = self$transformator$trafoAlphabet,
                                                           selected = "none")

                                  shiny::updateCheckboxInput(session,
                                                             inputId = "cb.trafoMutateMirror",
                                                             value = FALSE)

                                  private$.transformator$setTrafoType(feature = input$si.trafoMutateFeature,
                                                                      type = input$si.trafoMutateType)

                                  private$.transformator$setMirrorLogic(feature = input$si.trafoMutateFeature,
                                                                        logic = input$cb.trafoMutateMirror)

                                  # self$loqMutatedData$numericData() %>%
                                  #   private$.transformator$estimateTrafoParameter()

                                  private$.featureModel$resetNormDist(data = self$loqMutatedData$numerical_data() %>%
                                                                        self$transformator$mutateData() %>%
                                                                        dplyr::select(input$si.trafoMutateFeature)
                                  )

                                  private$.featureModel$fit()
                                })#tryCatch

                                private$.model$setNormDist(data = self$featureModel, feature = input$si.trafoMutateFeature)

                                private$.transformedData$setRawData <- self$loqMutatedData$numerical_data() %>%
                                  self$transformator$mutateData() %>%
                                  # self$model$scaleData() %>%
                                  tibble::add_column(self$filteredData$categorical_data()) %>%
                                  dplyr::select(dplyr::all_of(self$filteredData$attributeNames))
                                private$.transformedData$fit()
                              }#if
                              else{
                                shiny::showNotification(paste("No global model defined. Please defina a global transformation model first."),type = "error", duration = 10)
                              }#else
                            }, #functions

                            #' @description
                            #' Updates the trafo mutate feature graphic.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoMutateFeatureGraphic = function(input, output, session){
                              if(self$status$query(processName = "modelDefined")){
                                output$plt.trafoMutateFeature <- shiny::renderPlot(
                                  self$model$plotModel(feature = input$si.trafoMutateFeature),
                                  bg="transparent"
                                )#output
                              }#if
                              else{
                                output$plt.trafoMutateFeature <- shiny::renderPlot(NULL, bg="transparent")
                              }#else
                            }, #function

                            #' @description
                            #' Updates the trafo mutate feature patameter table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoMutateFeatureParameterTbl = function(input, output, session){
                              if(self$status$query(processName = "modelDefined")){
                                output$tbl.trafoMutateFeatureParameter <- DT::renderDataTable(
                                  self$model$fitResultFeature(feature = input$si.trafoMutateFeature) %>%
                                    pguIMP::transposeTibble() %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("featureTrafoParameter") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.trafoMutateFeatureParameter <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the trafo mutate feature quality table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoMutateFeatureQualityTbl = function(input, output, session){
                              if(self$status$query(processName = "modelDefined")){
                                output$tbl.trafoMutateFeatureQuality <- DT::renderDataTable(
                                  self$model$testResultFeature(feature = input$si.trafoMutateFeature) %>%
                                    pguIMP::transposeTibble() %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("featureTrafoQuality") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatble
                                )#output
                              }#if
                              else{
                                output$tbl.trafoMutateFeatureQuality <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the trafo mutate global parameter table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoMutateGlobalParameterTbl = function(input, output, session){
                              if(self$status$query(processName = "modelDefined")){
                                output$tbl.trafoMutateGlobalParameter <- DT::renderDataTable(
                                  self$transformator$trafoParameter %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("globalTrafoParameter") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.trafoMutateGlobalParameter <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the tbl.trafoMutateGlobalModel table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoMutateGlobalModelTbl = function(input, output, session){
                              if(self$status$query(processName = "modelDefined")){
                                output$tbl.trafoMutateGlobalModel <- DT::renderDataTable(
                                  self$model$modelParameterData() %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("globalModelParameter") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#ouput
                              }#if
                              else{
                                output$tbl.trafoMutateGlobalModel <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the tbl.trafoMutateGlobalQuality table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoMutateGlobalQualityTbl = function(input, output, session){
                              if(self$status$query(processName = "modelDefined")){
                                output$tbl.trafoMutateGlobalQuality <- DT::renderDataTable(
                                  self$model$modelQualityData() %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("globalModelQuality") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.trafoMutateGlobalQuality <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the tbl.trafoMutateGlobalTests table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoMutateGlobalTestsTbl = function(input, output, session){
                              if(self$status$query(processName = "modelDefined")){
                                output$tbl.trafoMutateGlobalTests <- DT::renderDataTable(
                                  self$model$testResultData() %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("globalModelQuality") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.trafoMutateGlobalTests <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the tbl.trafoMutateGlobalData table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoMutateGlobalDataTbl = function(input, output, session){
                              options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
                              if(self$status$query(processName = "modelDefined")){
                                output$tbl.trafoMutateGlobalData <- DT::renderDataTable(
                                  self$transformedData$rawData %>%
                                    dplyr::mutate_if(is.numeric, round, 3) %>%
                                    DT::datatable(
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("transformedData") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.trafoMutateGlobalData <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #############################
                            # trafo normalize functions #
                            #############################
                            #' @description
                            #' Updates the si.trafoNormFeature shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoNormFeature = function(input, output, session){
                              if(self$status$query(processName = "modelDefined")){
                                shiny::updateSelectInput(session,
                                                         "si.trafoNormFeature",
                                                         choices = self$transformedData$numericalAttributeNames,
                                                         selected = self$transformedData$numericalAttributeNames[1])
                              }#if
                            }, #function

                            #' @description
                            #' Updates the si.trafoNormMethod shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoNormMethod = function(input, output, session){
                              if(self$status$query(processName = "modelDefined")){
                                shiny::updateSelectInput(session,
                                                         "si.trafoNormMethod",
                                                         choices = self$normalizer$normAgentAlphabet,
                                                         selected = self$normalizer$normAgent
                                )
                              }#if
                            }, #function

                            #' @description
                            #' Updates the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoNormGui = function(input, output, session){
                              if(self$status$query(processName = "modelDefined")){
                                self$updateTrafoNormMethod(input, output, session)
                                self$updateTrafoNormFeature(input, output, session)
                              }#if
                            }, #function

                            #' @description
                            #' Calls the scale routine of the instance variable normalizer on the instance variable transformedData.
                            #' Updates the instance class status.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            trafoNormMutate = function(input, output, session){
                              if(self$status$query(processName = "modelDefined")){
                                private$.normalizer$setNormAgent <- input$si.trafoNormMethod

                                self$transformedData$numerical_data() %>%
                                  private$.normalizer$detectNormParameter()

                                private$.normalizedData$setRawData <- self$transformedData$numerical_data() %>%
                                  self$normalizer$scale_data() %>%
                                  tibble::add_column(self$filteredData$categorical_data()) %>%
                                  dplyr::select(dplyr::all_of(self$filteredData$attributeNames))
                                private$.normalizedData$fit()
                                private$.status$update(processName = "normalized", value = TRUE)
                              }#if
                              else{
                                shiny::showNotification(paste("No transformation model selected. Please transfrom data first."),type = "error", duration = 10)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the impute norm feature compound graphic.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoNormFeatureGraphic = function(input, output, session){
                              if(self$status$query(processName = "normalized")){
                                output$plt.trafoNormFeature <- shiny::renderPlot(
                                  self$normalizer$featurePlot(data_df = self$normalizedData$rawData, feature = input$si.trafoNormFeature),
                                  height = 425,
                                  bg="transparent"
                                )#output
                              }#if
                              else{
                                output$plt.trafoNormFeature <- shiny::renderPlot(NULL, bg="transparent")
                              }#else
                            }, #function

                            #' @description
                            #' Updates the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            resetTrafoNormGui = function(input, output, session){
                              if(self$status$query(processName = "normalized")){
                                self$updateTrafoNormMethod(input, output, session)
                              }#if
                            }, #function

                            #' @description
                            #' Updates the numerical impute norm analysis table for a user defined feature.
                            #' corresponding to the respective user defined attributes.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoNormFeatureStatisticsTbl = function(input, output, session){
                              if(private$.status$query(processName = "normalized")){
                                output$tbl.trafoNormFeatureStatistics <- DT::renderDataTable({
                                  self$normalizedData$statistics %>%
                                    dplyr::filter(Value == input$si.trafoNormFeature) %>%
                                    dplyr::select_if(is.numeric) %>%
                                    tidyr::pivot_longer(cols = dplyr::everything()) %>%
                                    dplyr::rename(statistics = "name") %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        # scrollX = TRUE,
                                        autowidth = FALSE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        autoWidth = TRUE
                                      )#options
                                    )#DT::datatable
                                })
                              }#if
                              else{
                                output$tbl.trafoNormFeatureStatistics <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the numerical impute norm analysis table.
                            #' corresponding to the respective user defined attributes.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoNormStatisticsTbl = function(input, output, session){
                              if(private$.status$query(processName = "normalized")){
                                output$tbl.trafoNormStatistics <- DT::renderDataTable({
                                  self$normalizedData$statistics %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        autoWidth = TRUE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("normalized_statistics") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                })
                              }#if
                              else{
                                output$tbl.trafoNormStatistics <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the impute norm parameter table.
                            #' corresponding to the respective user defined attributes.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoNormParameterTbl = function(input, output, session){
                              if(private$.status$query(processName = "normalized")){
                                output$tbl.trafoNormParameter <- DT::renderDataTable({
                                  self$normalizer$normParameter %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        # scrollX = TRUE,
                                        autowidth = FALSE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        autoWidth = TRUE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("normalization_parameter") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                })
                              }#if
                              else{
                                output$tbl.trafoNormParameter <- DT::renderDataTable(NULL)
                              }#else
                            },#function

                            #' @description
                            #' Updates the impute norm scaled data table.
                            #' corresponding to the respective user defined attributes.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateTrafoNormDataTbl = function(input, output, session){
                              if(private$.status$query(processName = "normalized")){
                                output$tbl.trafoNormData <- DT::renderDataTable({
                                  self$normalizedData$rawData %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        autoWidth = TRUE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("normalized_data") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                })
                              }#if
                              else{
                                output$tbl.trafoNormData <- DT::renderDataTable(NULL)
                              }#else
                            },#function

                            #############################
                            # impute missings functions #
                            #############################
                            #' @description
                            #' Calls the missing detection routine of the instance variable imputer
                            #' on the instance variable normalizedData.
                            #' Updates the instance class status.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            imputeMissingsAnalyze = function(input, output, session){
                              if(self$status$query(processName = "normalized")){
                                progress <- shiny::Progress$new(session, min = 1, max = length(self$normalizedData$numericalAttributeNames))
                                progress$set(message = "Characterizing missings", value = 1)
                                on.exit(progress$close())
                                self$normalizedData$numerical_data() %>%
                                  private$.missings$resetImputationParameter()
                                self$normalizedData$numerical_data() %>%
                                  private$.missingsCharacterizer$analyze(progress)
                                private$.status$update(processName = "naDetected", value = TRUE)
                              }#if
                              else{
                                shiny::showNotification(paste("No normalized data available. Please normalize data first."),type = "error", duration = 10)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the plt.imputeMissingsSummary graphic.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMissingsGraphic = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                output$plt.imputeMissingsSummary <- shiny::renderPlot(
                                  self$missings$imputationSiteHeatMap(),
                                  bg="transparent"
                                )
                              }#if
                              else{
                                output$plt.imputeMissingsSummary <- shiny::renderPlot(NULL, bg="transparent")
                              }#else
                            }, #function

                            #' @description
                            #' Updates the tbl.imputeMissingsStatistics table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMissingsStatisticsTbl = function(input, output, session){
                              if(private$.status$query(processName = "naDetected")){
                                output$tbl.imputeMissingsStatistics <- DT::renderDataTable(
                                  self$missings$imputationParameter %>%
                                    format.data.frame(scientific = FALSE, digits = 3) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("missingsStatistics") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.imputeMissingsStatistics <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the tbl.imputeMissingsDistribution table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMissingsDistributionTbl = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                output$tbl.imputeMissingsDistribution <- DT::renderDataTable(
                                  self$normalizedData$numerical_data() %>%
                                    self$missings$imputationSiteDistribution() %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("missingsDistribution") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.imputeMissingsDistribution <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the plt.imputeMissingsPairs graphic.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMissingCharacteristicsGraphic = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                output$plt.imputeMissingsPairs <- shiny::renderPlot(
                                  self$normalizedData$numerical_data() %>%
                                    self$missingsCharacterizer$plot_pair_dist(),
                                  bg="transparent"
                                )
                              }#if
                              else{
                                output$plt.imputeMissingsPairs<- shiny::renderPlot(NULL, bg="transparent")
                              }#else
                            }, #fnunction

                            #' @description
                            #' Updates the tbl.imputeMissingsCharacteristics table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMissingsCharacteristicsTbl = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                output$tbl.imputeMissingsCharacteristics <- DT::renderDataTable(
                                  self$missingsCharacterizer$missingsCharacteristics_df %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("missingsMissings") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.imputeMissingsCharacteristics <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the tbl.imputeDetectDetail table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMissingsDetailTbl = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                output$tbl.imputeMissingsDetail <- DT::renderDataTable(
                                  self$normalizedData$rawData %>%
                                    dplyr::filter_all(dplyr::any_vars(is.na(.))) %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("imputationSiteDetectionDetail") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.imputeMissingsDetail <- DT::renderDataTable(NULL)
                              }#else
                            }, #function
#'
#'                             #' @description
#'                             #' Updates the tbl.imputeDetectData table.
#'                             #' @param input
#'                             #' Pointer to shiny input
#'                             #' @param output
#'                             #' Pointer to shiny output
#'                             #' @param session
#'                             #' Pointer to shiny session
#'                             updateImputeMissingsDataTbl = function(input, output, session){
#'                               if(self$status$query(processName = "naDetected")){
#'                                 output$tbl.imputeMissingsData <- DT::renderDataTable(
#'                                   self$filteredMetadata$rawData %>%
#'                                     dplyr::right_join(self$normalizedData$rawData, by = "Sample Name") %>%
#'                                     format.data.frame(scientific = TRUE, digits = 4) %>%
#'                                     DT::datatable(
#'                                       extensions = "Buttons",
#'                                       options = list(
#'                                         scrollX = TRUE,
#'                                         scrollY = '350px',
#'                                         paging = FALSE,
#'                                         dom = "Blfrtip",
#'                                         buttons = list(list(
#'                                           extend = 'csv',
#'                                           filename = self$fileName$predict("imputationSiteDetectionData") %>%
#'                                             tools::file_path_sans_ext(),
#'                                           text = "Download"
#'                                         ))#buttons
#'                                       )#options
#'                                     )#DT::datatable
#'                                 )#output
#'                               }#if
#'                               else{
#'                                 output$tbl.imputeMissingsData <- DT::renderDataTable(NULL)
#'                               }#else
#'                             }, #function

                            #############################
                            # impute outliers functions #
                            #############################
                            #' @description
                            #' Updates the si.imputeOutliersMethod shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeOutliersMethod = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                shiny::updateSelectInput(session,
                                                         "si.imputeOutliersMethod",
                                                         choices = self$outliers$outliersAgentAlphabet,
                                                         selected = self$outliers$outliersAgent
                                )
                              }#if
                            }, #function

                            #' @description
                            #' Updates the si.imputeOutliersFeature shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeOutliersFeature = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                shiny::updateSelectInput(session,
                                                         "si.imputeOutliersFeature",
                                                         choices = self$normalizedData$numericalAttributeNames,
                                                         selected = self$normalizedData$numericalAttributeNames[1])
                              }#if
                            }, #function

                            #' @description
                            #' Updates the ni.imputeOutliersAlpha shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeOutliersAlpha = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                shiny::updateNumericInput(session,
                                                          "ni.imputeOutliersAlpha",
                                                          value = self$outliers$alpha)
                              }#if
                            }, #function

                            #' @description
                            #' Updates the ni.imputeOutliersEpsilon shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeOutliersEpsilon = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                shiny::updateNumericInput(session,
                                                          "ni.imputeOutliersEpsilon",
                                                          value = self$outliers$epsilon)
                              }#if
                            }, #function

                            #' @description
                            #' Updates the ni.imputeOutliersMinSamples shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeOutliersMinSamples = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                shiny::updateNumericInput(session,
                                                          "ni.imputeOutliersMinSamples",
                                                          value = self$outliers$minSamples)
                              }#if
                            }, #function

                            #' @description
                            #' Updates the ni.imputeOutliersGamma shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeOutliersGamma = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                shiny::updateNumericInput(session,
                                                          "ni.imputeOutliersGamma",
                                                          value = self$outliers$gamma)
                              }#if
                            }, #function

                            #' @description
                            #' Updates the ni.imputeOutliersNu shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeOutliersNu = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                shiny::updateNumericInput(session,
                                                          "ni.imputeOutliersNu",
                                                          value = self$outliers$nu)
                              }#if
                            }, #function

                            #' @description
                            #' Updates the ni.imputeOutliersCutoff shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeOutliersCutoff = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                shiny::updateNumericInput(session,
                                                          "ni.imputeOutliersCutoff",
                                                          value = self$outliers$cutoff)
                              }#if
                            }, #function

                            #' @description
                            #' Updates the ni.imputeOutliersK shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeOutliersK = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                shiny::updateNumericInput(session,
                                                          "ni.imputeOutliersK",
                                                          value = self$outliers$k)
                              }#if
                            }, #function

                            #' @description
                            #' Updates the ni.imputeOutliersSeed shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeOutliersSeed = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                shiny::updateNumericInput(session,
                                                          "ni.imputeOutliersSeed",
                                                          value = self$outliers$seed)
                              }#if
                            }, #function

                            #' @description
                            #' Updates the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeOutliersGui = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                self$updateImputeOutliersMethod(input, output, session)
                                self$updateImputeOutliersFeature(input, output, session)
                                self$updateImputeOutliersAlpha(input, output, session)
                                self$updateImputeOutliersEpsilon(input, output, session)
                                self$updateImputeOutliersMinSamples(input, output, session)
                                self$updateImputeOutliersGamma(input, output, session)
                                self$updateImputeOutliersNu(input, output, session)
                                self$updateImputeOutliersCutoff(input, output, session)
                                self$updateImputeOutliersK(input, output, session)
                                self$updateImputeOutliersSeed(input, output, session)
                              }#if
                            }, #function

                            #' @description
                            #' Updates the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            resetImputeOutliersGui = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                self$updateImputeOutliersMethod(input, output, session)
                                self$updateImputeOutliersAlpha(input, output, session)
                                self$updateImputeOutliersEpsilon(input, output, session)
                                self$updateImputeOutliersMinSamples(input, output, session)
                                self$updateImputeOutliersGamma(input, output, session)
                                self$updateImputeOutliersNu(input, output, session)
                                self$updateImputeOutliersCutoff(input, output, session)
                                self$updateImputeOutliersK(input, output, session)
                                self$updateImputeOutliersSeed(input, output, session)
                              }#if
                            },#function

                            #' @description
                            #' Calls the detectOutliers routine of the instance variable outliers
                            #' on the instance variable normalizedData.
                            #' Updates the instance class status.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            imputeOutliersDetect = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                private$.outliers$setOutliersAgent <- input$si.imputeOutliersMethod
                                private$.outliers$setAlpha <- input$ni.imputeOutliersAlpha
                                private$.outliers$setEpsilon <- input$ni.imputeOutliersEpsilon
                                private$.outliers$setMinSamples <- input$ni.imputeOutliersMinSamples
                                private$.outliers$setGamma <- input$ni.imputeOutliersGamma
                                private$.outliers$setNu <- input$ni.imputeOutliersNu
                                private$.outliers$setCutoff <- input$ni.imputeOutliersCutoff
                                private$.outliers$setK <- input$ni.imputeOutliersK
                                private$.outliers$setSeed <- input$ni.imputeOutliersSeed

                                self$resetImputeOutliersGui(input, output, session)

                                progress <- shiny::Progress$new(session, min = 1, max = length(self$loqMutatedData$numericalAttributeNames))
                                progress$set(message = "Searching for anomalies", value = 1)
                                on.exit(progress$close())
                                self$normalizedData$numerical_data() %>%
                                  private$.outliers$detectOutliers(progress)

                                private$.status$update(processName = "outliersDetected", value = TRUE)
                              }#if
                              else{
                                shiny::showNotification(paste("No missings detected. Please detect missings first."),type = "error", duration = 10)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the plt.outliersImputeSummary graphic.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeOutliersGraphic = function(input, output, session){
                              if(self$status$query(processName = "outliersDetected")){
                                output$plt.outliersImputeSummary <- shiny::renderPlot(
                                  self$outliers$plotOutliersDistribution(),
                                  bg="transparent"
                                )
                              }#if
                              else{
                                output$plt.outliersImputeSummary <- shiny::renderPlot(NULL, bg="transparent")
                              }#else
                            }, #function

                            #' @description
                            #' Updates the plt.outliersImputeFeature graphic.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeOutliersFeatureGraphic = function(input, output, session){
                              if(self$status$query(processName = "outliersDetected")){
                                output$plt.outliersImputeFeature <- shiny::renderPlot(
                                    self$outliers$featurePlot(data_df = self$normalizedData$numerical_data(),
                                                              feature = input$si.imputeOutliersFeature),
                                    height = 475,
                                    bg="transparent"
                                )
                              }#if
                              else{
                                output$plt.outliersImputeFeature <- shiny::renderPlot(NULL, bg="transparent")
                              }#else
                            }, #function

                            #' @description
                            #' Updates the numeric outlier feature table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeOutliersFeatureTbl = function(input, output, session)
                            {
                              if(self$status$query(processName = "outliersDetected"))
                              {
                                suffix <- sprintf("%s_outliers", input$si.imputeOutliersFeature)
                                output$tbl.outliersImputeFeature <- DT::renderDataTable(
                                  self$outliers$outliers %>%
                                    dplyr::filter(feature == !!input$si.imputeOutliersFeature) %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict(suffix) %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }else{
                                output$tbl.outliersImputeFeature <- DT::renderDataTable(NULL)
                              }
                              # options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
                              # t <- NULL
                              # if(self$status$query(processName = "outliersDetected")){
                              #   feature <- input$si.imputeOutliersFeature
                              #   dfData <- self$filteredMetadata$rawData %>%
                              #     dplyr::right_join(self$normalizedData$rawData %>%
                              #                         dplyr::select(c("Sample Name", !!feature)),
                              #                       by = "Sample Name")
                              #
                              #   dfOutlier <- self$outliers$featureOutlier(feature = feature)
                              #
                              #   t <- dfData %>%
                              #     dplyr::mutate_if(is.numeric, round, 3) %>%
                              #     DT::datatable(
                              #       options = list(
                              #         scrollX = TRUE,
                              #         scrollY = '350px',
                              #         paging = FALSE,
                              #         dom = "Blfrtip",
                              #         buttons = list(list(
                              #           extend = 'csv',
                              #           filename = self$fileName$predict("OutliersData") %>%
                              #             tools::file_path_sans_ext(),
                              #           text = "Download"
                              #         ))#buttons
                              #       )#options
                              #     )#DT::datatable
                              #
                              #   featureOutlier <- dfOutlier %>%
                              #     dplyr::mutate_if(is.numeric, round, 3)
                              #   if (nrow(featureOutlier)>0){
                              #     t <- DT::formatStyle(t,
                              #                          feature,
                              #                          backgroundColor = styleEqual(dfData %>%
                              #                                                         dplyr::select(!!feature) %>%
                              #                                                         dplyr::slice(featureOutlier[["measurement"]]) %>%
                              #                                                         unlist() %>%
                              #                                                         as.numeric() %>%
                              #                                                         round(digits = 3),
                              #                                                       featureOutlier[["color"]])
                              #
                              #     )#t
                              #   }#if
                              # }#if
                              # output$tbl.outliersImputeFeature <- DT::renderDataTable(t)
                            }, #function

                            #' @description
                            #' Updates the numerical loq statistics analysis table
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeOutliersStatisticsTbl = function(input, output, session){
                              if(private$.status$query(processName = "outliersDetected")){
                                output$tbl.outliersImputeStatistics <- DT::renderDataTable(
                                  self$outliers$outliersStatistics %>%
                                    format.data.frame(scientific = FALSE, digits = 3) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("outliersStatistics") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.outliersImputeStatistics <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the numerical outlier table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeOutliersDetailTbl = function(input, output, session){
                              if(self$status$query(processName = "outliersDetected"))
                              {
                                output$tbl.outliersImputeDetail <- DT::renderDataTable(
                                  self$outliers$outliers %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("outliers") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }else{
                                output$tbl.outliersImputeDetail <- DT::renderDataTable(NULL)
                              }
                            }, #function

                            ###########################
                            # impute mutate functions #
                            ###########################
                            #' @description
                            #' Updates the si.imputeMutateFeature shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMutateFeature = function(input, output, session){
                              if(self$status$query(processName = "outliersDetected")){
                                shiny::updateSelectInput(session,
                                                         "si.imputeMutateFeature",
                                                         choices = self$normalizedData$numericalAttributeNames,
                                                         selected = self$normalizedData$numericalAttributeNames[1]
                                )
                              }#if
                            }, #function

                            #' @description
                            #' Updates the si.imputeMutateMethod shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMutateMethod = function(input, output, session){
                              if(self$status$query(processName = "outliersDetected")){
                                shiny::updateSelectInput(session,
                                                         "si.imputeMutateMethod",
                                                         choices = self$imputer$imputationAgentAlphabet,
                                                         selected = self$imputer$imputationAgent
                                )
                              }#if
                            }, #function

                            #' @description
                            #' Updates the ni.imputeMutateNumberOfNeighbors shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMutateNNeighbors = function(input,  output, session){
                              if(self$status$query(processName = "outliersDetected")){
                                shiny::updateNumericInput(session,
                                                          "ni.imputeMutateNumberOfNeighbors",
                                                          value = self$imputer$nNeighbors)
                              }#if
                            },

                            #' @description
                            #' Updates the ni.imputeMutatePredFrac shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMutatePredFrac = function(input,  output, session){
                              if(self$status$query(processName = "outliersDetected")){
                                shiny::updateNumericInput(session,
                                                          "ni.imputeMutatePredFrac",
                                                          value = self$imputer$pred_frac)
                              }#if
                            },

                            #' @description
                            #' Updates the ni.imputeMutateOutfluxThr shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMutateOutfluxThr = function(input,  output, session){
                              if(self$status$query(processName = "outliersDetected")){
                                shiny::updateNumericInput(session,
                                                          "ni.imputeMutateOutfluxThr",
                                                          value = self$imputer$outflux_thr)
                              }#if
                            },

                            #' @description
                            #' Updates the ni.imputeMutateSeed shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMutateSeed = function(input, output, session){
                              if(self$status$query(processName = "outliersDetected")){
                                shiny::updateNumericInput(session,
                                                          "ni.imputeMutateSeed",
                                                          value = self$imputer$seed)
                              }#if
                            }, #function

                            #' @description
                            #' Updates the ni.imputeMutateIterations shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMutateIterations = function(input, output, session){
                              if(self$status$query(processName = "outliersDetected")){
                                shiny::updateNumericInput(session,
                                                          "ni.imputeMutateIterations",
                                                          value = self$imputer$iterations)
                              }#if
                            }, #function

                            #' @description
                            #' Updates the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMutateGui = function(input, output, session){
                              if(self$status$query(processName = "outliersDetected")){
                                self$resetImputeMutateGui(input, output, session)
                                self$updateImputeMutateFeature(input, output, session)
                              }#if
                            }, #function

                            #' @description
                            #' Resets the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            resetImputeMutateGui = function(input, output, session){
                              if(self$status$query(processName = "outliersDetected")){
                                self$updateImputeMutateMethod(input, output, session)
                                self$updateImputeMutateNNeighbors(input,  output, session)
                                self$updateImputeMutatePredFrac(input, output, session)
                                self$updateImputeMutateOutfluxThr(input, output, session)
                                self$updateImputeMutateSeed(input, output, session)
                                self$updateImputeMutateIterations(input, output, session)
                              }#if
                            }, #function

                            #' @description
                            #' Calls the mutate imputation site routine of the instance variable imputer on the instance variable transformedData.
                            #' Updates the instance class status.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            imputeMutateMutate = function(input, output, session){
                              if(self$status$query(processName = "outliersDetected")){
                                private$.imputer$setImputationAgent <- input$si.imputeMutateMethod
                                private$.imputer$setNNeighbors <-input$ni.imputeMutateNumberOfNeighbors
                                private$.imputer$setPred_frac <- input$ni.imputeMutatePredFrac
                                private$.imputer$setOutflux_thr <- input$ni.imputeMutateOutfluxThr
                                private$.imputer$setSeed <- input$ni.imputeMutateSeed
                                private$.imputer$setIterations <- input$ni.imputeMutateIterations

                                private$.imputer$gatherImputationSites(missings_df = private$.missings$imputationSites,
                                                                       outliers_df = private$.outliers$outliers)

                                private$.imputer$analyzeImputationSites(data_df = self$normalizedData$numerical_data())

                                private$.imputer$detectPredictors(data_df = self$normalizedData$numerical_data())

                                progress <- shiny::Progress$new(session, min = 0, max = 1)
                                progress$set(message = "Mutate imputation sites", value = 0)
                                on.exit(progress$close())

                                private$.imputedData$setRawData <- self$normalizedData$numerical_data() %>%
                                  self$imputer$handleImputationSites(progress) %>%
                                  tibble::add_column(self$filteredData$categorical_data()) %>%
                                  dplyr::select(dplyr::all_of(self$filteredData$attributeNames))
                                private$.imputedData$fit()

                                if(self$imputer$success){
                                  private$.cleanedData$setRawData <- self$imputedData$numerical_data() %>%
                                    self$normalizer$rescale_data() %>%
                                    self$transformator$reverseMutateData() %>%
                                    tibble::add_column(self$filteredData$categorical_data()) %>%
                                    dplyr::select(dplyr::all_of(self$filteredData$attributeNames))
                                  private$.cleanedData$fit()

                                  private$.status$update(processName = "imputed", value = TRUE)
                                }
                                else{
                                  private$.status$update(processName = "imputed", value = FALSE)
                                  shiny::showNotification(paste("Solution of imputation mutate procedure was not satisfactoy. Pleas refine your variables or method and try again. See the Help Webpage of pguIMP for help with the variabes."),type = "error", duration = 20)
                                }

                                # progress <- shiny::Progress$new(session, min = 1, max = length(self$normalizedData$numericFeatureNames))
                                # progress$set(message = "Impute Data", value = 0)
                                # on.exit(progress$close())
                                #
                                # name  <- as.name("Sample Name")
                                # private$.imputedData$setRawData <- self$normalizedData$numericData() %>%
                                #   self$imputer$handleImputationSites(progress = progress) %>%
                                #   # self$model$rescaleData() %>%
                                #   # self$transformator$reverseMutateData() %>%
                                #   tibble::add_column(!! name := self$normalizedData$rawData %>%
                                #                        dplyr::select(!!name) %>%
                                #                        unlist() %>%
                                #                        as.character()) %>%
                                #   dplyr::select(c(!!name, self$normalizedData$numericFeatureNames))
                                # private$.status$update(processName = "imputed", value = TRUE)
                              }#if
                              else{
                                shiny::showNotification(paste("No outliers detected. Please detect outliers first."),type = "error", duration = 10)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the plt.imputeMutateFlux graphic.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeFluxGraphic = function(input, output, session){
                              if(self$status$query(processName = "imputed")){
                                output$plt.imputeMutateFlux <- shiny::renderPlot(
                                  self$imputer$fluxPlot(),
                                  bg="transparent"
                                )
                              }#if
                              else{
                                output$plt.imputeMutateFlux <- shiny::renderPlot(NULL,bg="transparent")
                              }#else
                            }, #function

                            #' @description
                            #' Updates the plt.imputeMutateSummary graphic.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMutateGraphic = function(input, output, session){
                              if(self$status$query(processName = "imputed")){
                                output$plt.imputeMutateSummary <- shiny::renderPlot(
                                  self$imputer$imputationSiteHeatMap(),
                                  bg="transparent"
                                )
                              }#if
                              else{
                                output$plt.imputeMutateSummary <- shiny::renderPlot(NULL, bg="transparent")
                              }#else
                            }, #function

                            #' @description
                            #' Updates the tbl.imputeMutateStatistics table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMutateStatisticsTbl = function(input, output, session){
                              if(private$.status$query(processName = "imputed")){
                                output$tbl.imputeMutateStatistics <- DT::renderDataTable(
                                  self$imputer$imputationStatistics %>%
                                    format.data.frame(scientific = FALSE, digits = 3) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("imputationSiteStatistics") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.imputeMutateStatistics <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the tbl.imputeMutateDistribution table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMutateDistributionTbl = function(input, output, session){
                              if(self$status$query(processName = "imputed")){
                                output$tbl.imputeMutateDistribution <- DT::renderDataTable(
                                  self$imputer$imputationSiteDistribution %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("imputationSiteDistribution") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.imputeMutateDistribution <- DT::renderDataTable(NULL)
                              }#else
                            }, #function


                            #' @description
                            #' Updates the plt.imputeMutateFeatureDetail graphic.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMutateFeatureDetailGraphic = function(input, output, session){
                              if(self$status$query(processName = "imputed")){
                                output$plt.imputeMutateFeatureDetail <- shiny::renderPlot(
                                  self$imputer$featurePlot(data = self$imputedData$numerical_data(),
                                                           feature = input$si.imputeMutateFeature),
                                  height = 475,
                                  bg = "transparent"
                                )#output
                              }#if
                              else{
                                output$plt.imputeMutateFeatureDetail <- shiny::renderPlot(NULL, bg = "transparent")
                              }#else
                            }, #function

                            #' @description
                            #' Updates the tbl.imputeMutateFeatureDetail table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMutateFeatureDetailTbl = function(input, output, session){
                              if (self$status$query(processName = "imputed")){
                                output$tbl.imputeMutateFeatureDetail <- DT::renderDataTable(
                                  self$imputedData$rawData %>%
                                    dplyr::select(dplyr::all_of(c(self$imputedData$categoricalAttributeNames, input$si.imputeMutateFeature))) %>%
                                    dplyr::slice(self$imputer$imputationSiteIdxByFeature(feature = input$si.imputeMutateFeature)) %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("imputationFeatureDetails") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.imputeMutateFeatureDetail <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the tbl.imputeMutateDetail table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMutateDetailTbl = function(input, output, session){
                              if(self$status$query(processName = "imputed")){
                                dfImputationSites <- self$imputer$imputationSites
                                idx <- dfImputationSites[["idx"]][!duplicated(dfImputationSites[["idx"]])]

                                output$tbl.imputeMutateDetail <- DT::renderDataTable(
                                  self$imputedData$rawData %>%
                                    dplyr::slice(idx) %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("imputationDetail") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.imputeMutateDetail <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the tbl.imputeMutateData table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateImputeMutateDataTbl = function(input, output, session){
                              if(self$status$query(processName = "imputed")){
                                output$tbl.imputeMutateData <- DT::renderDataTable(
                                  self$imputedData$rawData %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("imputationData") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        ))#button
                                      )#options
                                    )#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.imputeMutateData <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            ############################
                            # outlier detect functions #
                            ############################



                            ########################
                            # validation functions #
                            ########################
                            #' @description
                            #' Calls the validate routine of the instance variable validator on the instance variables rawData and clenaedData.
                            #' Updates the instance class status.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            validate = function(input, output, session){
                              if(self$status$query(processName = "imputed")){
                                progress <- shiny::Progress$new(session, min = 0, max  = 1.0)
                                progress$set(message = "Validate imputation", value = 0)
                                on.exit(progress$close())
                                private$.validator$validate(org = self$filteredData$numerical_data(),
                                                            imp = self$cleanedData$numerical_data() %>%
                                                              dplyr::select_if(function(x){!all(is.na(x))}),
                                                            progress = progress)
                                private$.status$update(processName = "validated", value = TRUE)
                                private$.corrValidator$fit(org_df = self$normalizedData$numerical_data(),
                                                           imp_df = self$imputedData$numerical_data())
                              }#if
                              else{
                                shiny::showNotification(paste("No imputation performed. Please perform imputation first."),type = "error", duration = 10)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the si.analysisValidationFeature shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateAnalysisValidationGui = function(input, output, session){
                              if(self$status$query(processName = "validated")){
                                shiny::updateSelectInput(session,
                                                         "si.analysisValidationFeature",
                                                         choices = self$validator$features,
                                                         selected = self$validator$features[1]
                                )
                              }#if
                              else{
                                shiny::updateSelectInput(session,
                                                         "si.analysisValidationFeature",
                                                         choices = list(),
                                                         selected = 1
                                )
                              } #else
                            }, #function

                            #' @description
                            #' Updates the plt.analysisValidationFeature shiny widget.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateAnalysisValidationGraphic = function(input, output, session){
                              if(self$status$query(processName = "validated")){
                                output$plt.analysisValidationFeature <- shiny::renderPlot(
                                  self$validator$featurePlot(org_df = self$filteredData$numerical_data(),
                                                             imp_df = self$cleanedData$numerical_data(),
                                                             lloq = self$loq$attribute_lloq(attribute = input$si.analysisValidationFeature),
                                                             uloq = self$loq$attribute_uloq(attribute = input$si.analysisValidationFeature),
                                                             impIdx_df = self$imputer$imputationSites,
                                                             feature = input$si.analysisValidationFeature),
                                  bg="transparent"
                                )#output
                              }#if
                              else{
                                output$plt.plt.analysisValidationFeature <- shiny::renderPlot(NULL, bg="transparent")
                              }#else
                            }, #function

                            #' @description
                            #' Updtates the tbl.analysisValidationTest table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateAnalysisValidationTestTbl = function(input, output, session){
                              if(self$status$query(processName = "validated")){
                                output$tbl.analysisValidationTest <- DT::renderDataTable(
                                  self$validator$testStatistics_df %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        # scrollX = TRUE,
                                        autowidth = FALSE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("validationTests") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        )), #buttons
                                        autoWidth = TRUE,
                                        columnDefs = list(list(width = '50px', targets = "_all"))
                                      )#options
                                    )#DT::datatable
                                )#output
                              } #if
                              else{
                                output$tbl.analysisValidationTest <- DT::renderDataTable(NULL)
                              } #else
                            }, #function

                            #' @description
                            #' Updtates the tbl.centralMomentsOrg table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateCentralMomentsOrgTbl = function(input, output, session){
                              if(self$status$query(processName = "validated")){
                                output$tbl.centralMomentsOrg <- DT::renderDataTable(
                                  self$validator$centralMoments_org %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("rawCentralMoments") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        )), #buttons
                                        autoWidth = TRUE,
                                        columnDefs = list(list(width = '50px', targets = "_all"))
                                      )#options
                                    )#DT::datatable
                                )#output
                              } #if
                              else{
                                output$tbl.centralMomentsOrg <- DT::renderDataTable(NULL)
                              } #else
                            }, #function

                            #' @description
                            #' Updtates the tbl.centralMomentsImp table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateCentralMomentsImpTbl = function(input, output, session){
                              if(self$status$query(processName = "validated")){
                                output$tbl.centralMomentsImp <- DT::renderDataTable(
                                  self$validator$centralMoments_imp %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("impCentralMoments") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        )), #buttons
                                        autoWidth = TRUE,
                                        columnDefs = list(list(width = '50px', targets = "_all"))
                                      )#options
                                    )#DT::datatable
                                )#output
                              } #if
                              else{
                                output$tbl.centralMomentsImp <- DT::renderDataTable(NULL)
                              } #else
                            }, #function

                            #' @description
                            #' Updtates the tbl.centralMomentsDelta table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateCentralMomentsDeltaTbl = function(input, output, session){
                              if(self$status$query(processName = "validated")){
                                output$tbl.centralMomentsDelta <- DT::renderDataTable(
                                  self$validator$centralMoments_delta %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        # scrollX = TRUE,
                                        autowidth = FALSE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("deltaCentralMoments") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        )), #buttons
                                        autoWidth = TRUE,
                                        columnDefs = list(list(width = '50px', targets = "_all"))
                                      )#options
                                    )#DT::datatable
                                )#output
                              } #if
                              else{
                                output$tbl.centralMomentsDelta <- DT::renderDataTable(NULL)
                              } #else
                            }, #function

                            ####################################
                            # correlation validation functions #
                            ####################################



                            #' @description
                            #' Updtates the plt.correlationValidationScatter graphic.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateCorrelationValidationScatterGraphic = function(input, output, session){
                              if(self$status$query(processName = "validated")){
                                output$plt.correlationValidationScatter <- shiny::renderPlot(
                                  self$corrValidator$correlationScatterPlot(),
                                  bg="transparent"
                                )#output
                              }#if
                              else{
                                output$plt.correlationValidationScatter <- shiny::renderPlot(NULL, bg="transparent")
                              }#else
                            }, #function

                            #' @description
                            #' Updtates the plt.correlationValidationBoxPlot graphic.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateCorrelationValidationBoxPlotGraphic = function(input, output, session){
                              if(self$status$query(processName = "validated")){
                                output$plt.correlationValidationBoxPlot <- shiny::renderPlot(
                                  self$corrValidator$correlationCompoundPlot(),
                                  height = 475,
                                  bg="transparent"
                                )#output
                              }#if
                              else{
                                output$plt.correlationValidationBoxPlot <- shiny::renderPlot(NULL, bg="transparent")
                              }#else
                            }, #function

                            #' @description
                            #' Updtates the tbl.correlationValidationDeviation table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateCorrelationValidationDeviationTbl = function(input, output, session){
                              if(self$status$query(processName = "validated")){
                                output$tbl.correlationValidationDeviation <- DT::renderDataTable(
                                  self$corrValidator$summary_df %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        # scrollX = TRUE,
                                        scrollY = '350px',
                                        autowidth = FALSE,
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("correlationValidationDeviation") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        )), #buttons
                                        autoWidth = TRUE,
                                        columnDefs = list(list(width = '50px', targets = "_all"))
                                      )#options
                                    )#DT::datatable
                                )#output
                              } #if
                              else{
                                output$tbl.correlationValidationData <- DT::renderDataTable(NULL)
                              } #else
                            }, #function

                            #' @description
                            #' Updtates the tbl.correlationValidationData table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            updateCorrelationValidationDataTbl = function(input, output, session){
                              if(self$status$query(processName = "validated")){
                                output$tbl.correlationValidationData <- DT::renderDataTable(
                                  self$corrValidator$corr_df %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        # scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$predict("correlationValidationData") %>%
                                            tools::file_path_sans_ext(),
                                          text = "Download"
                                        )), #buttons
                                        autoWidth = TRUE,
                                        columnDefs = list(list(width = '50px', targets = "_all"))
                                      )#options
                                    )#DT::datatable
                                )#output
                              } #if
                              else{
                                output$tbl.correlationValidationData <- DT::renderDataTable(NULL)
                              } #else
                            }, #function


                            #########################
                            # data export functions #
                            #########################
                            #' @description
                            #' Creates and returns an export filename.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @return
                            #' export filename
                            #' (character)
                            exportFileName = function(input, output, session){
                              private$.fileName$predict() %>%
                                return()
                            }, #function

                            #' @description
                            #' Exports the pguIMP analysis results
                            #' @param file
                            #' export filename
                            #' (character)
                            #' @param input
                            #' Pointer to shiny input
                            exportData = function(input, file){
                              if(self$status$query(processName = "validated")){
                                private$.exporter$setFileName <- file
                                gui_parameter <- tibble::tibble(
                                  loq_na_handling = c(self$loq$naHandlingAgent),
                                  lloq_substitute = c(self$loq$lloqSubstituteAgent),
                                  uloq_substitute = c(self$loq$uloqSubstituteAgent),
                                  normalization_type = c(self$normalizer$normAgent),
                                  anomalies_method = c(self$outliers$outliersAgent),
                                  alpha = c(self$outliers$alpha),
                                  epsilon= c(self$outliers$epsilon),
                                  minSamples = c(self$outliers$minSamples),
                                  gamma = c(self$outliers$gamma),
                                  nu = c(self$outliers$nu),
                                  cutoff = c(self$outliers$cutoff),
                                  k = c(self$outliers$k),
                                  anomalies_seed = c(self$outliers$seed),
                                  imputation_method = c(self$imputer$imputationAgent),
                                  number_of_neighbors = c(self$imputer$nNeighbors),
                                  fraction_of_predictors = c(self$imputer$pred_frac),
                                  outflux_threshold = c(self$imputer$outflux_thr),
                                  imputation_seed = c(self$imputer$seed),
                                  iterations = c(self$imputer$iterations)
                                )

                                gui_parameter <- tibble::as_tibble(cbind(parameter = names(gui_parameter), t(gui_parameter)))
                                colnames(gui_parameter) <- c("parameter", "value")

                                list(raw_data = self$cleanedData$rawData,
                                     loq = self$loq$loq,
                                     giu_parameter = gui_parameter,
                                     filter_parameter = tibble::tibble(features = c(self$metadata$featureNames, self$rawData$featureNames[2:length(self$rawData$featureNames)]),
                                                                       filter_parameter = as.vector(input$tbl.filter_search_columns)),

                                     filtered = self$filteredData$rawData,
                                     loq_statistics = self$loq$loqStatistics,
                                     loq_mutated = self$loqMutatedData$rawData,
                                     trafo_parameter = self$transformator$trafoParameter,
                                     transfromed = self$transformedData$rawData,
                                     model_parameter = self$model$modelParameterData(),
                                     model_quality = self$model$modelQualityData(),
                                     model_statistics = self$model$testResultData(),
                                     normalization_parameter = self$normalizer$normParameter,
                                     normalized = self$normalizedData$rawData,
                                     missings = self$missings$imputationSites,
                                     missings_statistics = self$missings$imputationParameter,
                                     missings_characteristics = self$missingsCharacterizer$missingsCharacteristics_df,
                                     outliers = self$outliers$outliers %>%
                                       dplyr::select(-c("color")),
                                     outliers_statistics = self$outliers$outliersStatistics,
                                     predictors = self$imputer$pred_mat %>%
                                       as.data.frame() %>%
                                       tibble::rownames_to_column() %>%
                                       tibble::as_tibble(),
                                     imputation_sites = self$imputer$one_hot_df,
                                     imputation_statistics = self$imputer$imputationStatistics,
                                     imputed = self$imputedData$rawData,
                                     validation = self$validator$testStatistics_df
                                ) %>%
                                  self$exporter$writeDataToExcel()
                              }#if
                            }, #function

                            #############################
                            # analysis report functions #
                            #############################
                            #' @description
                            #' Creates and returns a report filename.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @return
                            #' export filename
                            #' (character)
                            reportFileName = function(input, output, session){
                              private$.fileName$setSuffix <- "pdf"
                              private$.fileName$updateTimeString()
                              # private$.fileName$mergeFileName()
                              private$.fileName$exportFileName() %>%
                                return()
                            }, #function
                            #' @description
                            #' Exports a report on the pguIMP analysis
                            #' in pdf format.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param file
                            #' export filename
                            #' (character)
                            writeReport = function(input, file){
                              print("state")
                              print(str(input$tbl.filter_search_columns))
                              if(self$status$query(processName = "validated")){
                                private$.reporter$setFilename <- file
                                analysis_parameter <- tibble::tibble(
                                  fileName = c(self$fileName$fileName),
                                  numberOfInstances = c(nrow(self$rawData$rawData)),
                                  numberOfFeatures = c(ncol(self$rawData$rawData)),
                                  numberOfMetaFeatures = c(ncol(self$metadata$rawData)),
                                  numberOfNumericFeatures = c(length(self$rawData$numericalAttributeNames)),
                                  numberOfNonNumericFeatures = c(length(self$rawData$categoricalAttributeNames)),
                                  totalNumberOfMissings = c(self$rawData$rawData %>%
                                                              dplyr::select(dplyr::everything()) %>%  # replace to your needs
                                                              dplyr::summarise_all(~ sum(is.na(.x))) %>%
                                                              sum()),
                                  loq_na_handling = c(self$loq$naHandlingAgent),
                                  lloq_substitute = c(self$loq$lloqSubstituteAgent),
                                  uloq_substitute = c(self$loq$uloqSubstituteAgent),
                                  normalization_type = c(self$normalizer$normAgent),
                                  anomalies_method = c(self$outliers$outliersAgent),
                                  alpha = c(self$outliers$alpha),
                                  epsilon= c(self$outliers$epsilon),
                                  minSamples = c(self$outliers$minSamples),
                                  gamma = c(self$outliers$gamma),
                                  nu = c(self$outliers$nu),
                                  cutoff = c(self$outliers$cutoff),
                                  k = c(self$outliers$k),
                                  anomalies_seed = c(self$outliers$seed),
                                  imputation_method = c(self$imputer$imputationAgent),
                                  number_of_neighbors = c(self$imputer$nNeighbors),
                                  fraction_of_predictors = c(self$imputer$pred_frac),
                                  outflux_threshold = c(self$imputer$outflux_thr),
                                  imputation_seed = c(self$imputer$seed),
                                  iterations = c(self$imputer$iterations)
                                )

                                list(filter_parameter = tibble::tibble(features = c(self$metadata$featureNames, self$rawData$featureNames[2:length(self$rawData$featureNames)]),
                                                                       filter_parameter = as.vector(input$tbl.filter_search_columns)) %>%
                                       dplyr::filter(filter_parameter != ""),
                                     selected_features = self$filteredData$numericalAttributeNames,
                                     loq_statistics = self$loq$loqStatistics,
                                     trafo_parameter = self$transformator$trafoParameter,
                                     model_parameter = self$model$modelParameterData(),
                                     model_quality = self$model$modelQualityData(),
                                     model_statistics = self$model$testResultData(),
                                     normalization_parameter = self$normalizer$normParameter,
                                     missings_statistics = self$missings$imputationParameter,
                                     missings_distribution = self$missings$imputationSiteDistribution(self$filteredData$numerical_data()),
                                     outliers_statistics = self$outliers$outliersStatistics,
                                     imputation_statistics = self$imputer$imputationStatistics,
                                     imputation_distribution = self$imputer$imputationSiteDistribution,
                                     validation_test = self$validator$testStatistics_df,
                                     validation_corrSum = self$corrValidator$summary(),
                                     validation_correlation = self$corrValidator$corr_df,
                                     analysis_parameter = analysis_parameter
                                ) %>%
                                  self$reporter$write_report()
                              }#if
                            },#function

                            ###################################
                            # update graphical user interface #
                            ###################################
                            #' @description
                            #' Updates the gui if analysis parameters change.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            hide_outdated_results = function(input, output, session){
                              if(!private$.status$query(processName = "dataImported")){
                                output$tbl.importDataTypes <- DT::renderDataTable(NULL)
                                output$tbl.importDataStatistics <- DT::renderDataTable(NULL)
                                output$tbl.importMissingsStatistics <- DT::renderDataTable(NULL)
                                output$tbl.filterSelect <- DT::renderDataTable(NULL)
                              }#if
                              if(!private$.status$query(processName = "dataFiltered")){
                                output$plt.exploreGraphic <- shiny::renderPlot(NULL, bg="transparent")
                                output$plt.exploreAbscissaGraphic <- shiny::renderPlot(NULL, bg="transparent")
                                output$plt.exploreOrdinateGraphic <- shiny::renderPlot(NULL, bg="transparent")
                                output$tbl.exploreAbscissaStatistics <- DT::renderDataTable(NULL)
                                output$tbl.exploreOrdinateStatistics <- DT::renderDataTable(NULL)
                                output$tbl.filterStatistics <- DT::renderDataTable(NULL)
                                output$tbl.filterMissings <- DT::renderDataTable(NULL)
                              }#if
                              if(!private$.status$query(processName = "loqImported")){
                                output$tbl.loqDefineValues <- DT::renderDataTable(NULL)
                              }#if
                              if(!private$.status$query(processName = "loqDetected")){
                                output$tbl.loqDetectStatistics <- DT::renderDataTable(NULL)
                                output$tbl.loqDetectOutlier <- DT::renderDataTable(NULL)
                                output$tbl.loqDetectData <- DT::renderDataTable(NULL)
                                output$plt.loqDetectStatistics <- shiny::renderPlot(NULL, bg="transparent")
                                output$plt.loqDetectFeature <- shiny::renderPlot(NULL, bg="transparent")
                                output$tbl.loqDetectFeature <- DT::renderDataTable(NULL)
                              }#if
                              if(!private$.status$query(processName = "loqMutated")){
                                output$tbl.loqMutateStatistics <- DT::renderDataTable(NULL)
                                output$tbl.loqMutateOutlier <- DT::renderDataTable(NULL)
                                output$tbl.loqMutateData <- DT::renderDataTable(NULL)
                                output$plt.loqMutateStatistics <- shiny::renderPlot(NULL, bg="transparent")
                                output$tbl.loqMutateFeature <- DT::renderDataTable(NULL)
                                output$plt.loqMutateFeature <- shiny::renderPlot(NULL, bg="transparent")
                              }#if
                              if(!self$status$query(processName = "modelOptimized")){
                                output$tbl.trafoDetectTypes <- DT::renderDataTable(NULL)
                                output$tbl.trafoDetectParameters <- DT::renderDataTable(NULL)
                              }#if
                              if(!self$status$query(processName = "modelDefined")){
                                output$plt.trafoMutateFeature <- shiny::renderPlot(NULL, bg="transparent")
                                output$tbl.trafoMutateFeatureParameter <- DT::renderDataTable(NULL)
                                output$tbl.trafoMutateFeatureQuality <- DT::renderDataTable(NULL)
                                output$tbl.trafoMutateGlobalParameter <- DT::renderDataTable(NULL)
                                output$tbl.trafoMutateGlobalModel <- DT::renderDataTable(NULL)
                                output$tbl.trafoMutateGlobalQuality <- DT::renderDataTable(NULL)
                                output$tbl.trafoMutateGlobalTests <- DT::renderDataTable(NULL)
                                output$tbl.trafoMutateGlobalData <- DT::renderDataTable(NULL)
                              }#if
                              if(!self$status$query(processName = "normalized")){
                                output$plt.trafoNormFeature <- shiny::renderPlot(NULL, bg="transparent")
                                output$tbl.trafoNormFeatureStatistics<- DT::renderDataTable(NULL)
                                output$tbl.trafoNormParameter<- DT::renderDataTable(NULL)
                                output$tbl.trafoNormStatistics<- DT::renderDataTable(NULL)
                                output$tbl.trafoNormData<- DT::renderDataTable(NULL)
                              }#if
                              if(!self$status$query(processName = "naDetected")){
                                output$plt.imputeMissingsSummary <- shiny::renderPlot(NULL, bg="transparent")
                                output$tbl.imputeMissingsStatistics <- DT::renderDataTable(NULL)
                                output$tbl.imputeMissingsDistribution <- DT::renderDataTable(NULL)
                                output$plt.imputeMissingsPairs <- shiny::renderPlot(NULL, bg ="transparent")
                                output$tbl.imputeMissingsCharacteristics <- DT::renderDataTable(NULL)
                                output$tbl.imputeMissingsDetail <- DT::renderDataTable(NULL)
                                output$tbl.imputeMissingsData <- DT::renderDataTable(NULL)
                              }#if
                              if(!self$status$query(processName = "outliersDetected")){
                                output$plt.outliersImputeSummary <- shiny::renderPlot(NULL, bg="transparent")
                                output$plt.outliersImputeFeature <- shiny::renderPlot(NULL, bg="transparent")
                                output$tbl.outliersImputeFeature <- DT::renderDataTable(NULL)
                                output$tbl.outliersImputeStatistics <- DT::renderDataTable(NULL)
                                output$tbl.outliersImputeDetail <- DT::renderDataTable(NULL)
                                output$tbl.outliersImputeData <- DT::renderDataTable(NULL)
                              }#if
                              if(!self$status$query(processName = "imputed")){
                                output$plt.imputeMutateSummary <- shiny::renderPlot(NULL, bg="transparent")
                                output$plt.imputeMutateFeatureDetail <- shiny::renderPlot(NULL, bg="transparent")
                                output$plt.imputeMutateFlux <- shiny::renderPlot(NULL, bg = "transparent")
                                output$tbl.imputeMutateFeatureDetail<- DT::renderDataTable(NULL)
                                output$tbl.imputeMutateStatistics <- DT::renderDataTable(NULL)
                                output$tbl.imputeMutateDistribution <- DT::renderDataTable(NULL)
                                output$tbl.imputeMutateDetail <- DT::renderDataTable(NULL)
                                output$tbl.imputeMutateData <- DT::renderDataTable(NULL)
                              }#if
                              if(!self$status$query(processName = "validated")){
                                output$plt.analysisValidationFeature <- shiny::renderPlot(NULL, bg="transparent")
                                output$tbl.analysisValidationTest <- DT::renderDataTable(NULL)
                                output$tbl.centralMomentsOrg <- DT::renderDataTable(NULL)
                                output$tbl.centralMomentsImp <- DT::renderDataTable(NULL)
                                output$tbl.centralMomentsDelta <- DT::renderDataTable(NULL)
                                output$plt.correlationValidationScatter <- shiny::renderPlot(NULL, bg = "transparent")
                                output$plt.correlationValidationBoxPlot <- shiny::renderPlot(NULL, bg="transparent")
                                output$tbl.correlationValidationDeviation <- DT::renderDataTable(NULL)
                                output$tbl.correlationValidationData <- DT::renderDataTable(NULL)
                              } #if
                              # if(!self$status$query(processName = "correlated")){
                              #   output$tbl.correlationMatrixR <- DT::renderDataTable(NULL)
                              #   output$tbl.correlationMatrixPPearson <- DT::renderDataTable(NULL)
                              #   output$tbl.correlationMatrixTau <- DT::renderDataTable(NULL)
                              #   output$tbl.correlationMatrixPKendall <- DT::renderDataTable(NULL)
                              #   output$tbl.correlationMatrixRho <- DT::renderDataTable(NULL)
                              #   output$tbl.correlationMatrixPSpearman <- DT::renderDataTable(NULL)
                              # }#if
                              # if(!self$status$query(processName = "regression")){
                              #   output$plt.regressionFeature <- shiny::renderPlot(NULL, bg="transparent")
                              #   output$tbl.regressionFeature <- DT::renderDataTable(NULL)
                              #   output$tbl.regressionIntercept <- DT::renderDataTable(NULL)
                              #   output$tbl.regressionPIntercept <- DT::renderDataTable(NULL)
                              #   output$tbl.regressionSlope <- DT::renderDataTable(NULL)
                              #   output$tbl.regressionPSlope <- DT::renderDataTable(NULL)
                              # }#if
                            }, #function

                            #########################
                            # update help interface #
                            #########################
                            #' @description
                            #' Updates the gui if analysis parameters change.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            update_help_html = function(input, output, session)
                            {
                              if(input$switch.help)
                              {
                                wd <- getwd()
                                help_folder <- file.path(wd, "www/_site")
                                output$html.uploadHelp <- shiny::renderUI({
                                  help_folder %>%
                                    file.path("upload.html") %>%
                                    shiny::includeHTML()
                                })
                                output$html.filterHelp <- shiny::renderUI({
                                  help_folder %>%
                                    file.path("filter.html") %>%
                                    shiny::includeHTML()
                                })
                                output$html.exploreHelp <- shiny::renderUI({
                                  help_folder %>%
                                    file.path("explore.html") %>%
                                    shiny::includeHTML()
                                })
                                output$html.uploadLOQHelp <- shiny::renderUI({
                                  help_folder %>%
                                    file.path("upload_loq.html") %>%
                                    shiny::includeHTML()
                                })
                                output$html.defineLOQHelp <- shiny::renderUI({
                                  help_folder %>%
                                    file.path("define_loq.html") %>%
                                    shiny::includeHTML()
                                })
                                output$html.detectLOQHelp <- shiny::renderUI({
                                  help_folder %>%
                                    file.path("detect_loq.html") %>%
                                    shiny::includeHTML()
                                })
                                output$html.substituteLOQHelp <- shiny::renderUI({
                                  help_folder %>%
                                    file.path("substitute_loq.html") %>%
                                    shiny::includeHTML()
                                })
                                output$html.transformHelp <- shiny::renderUI({
                                  help_folder %>%
                                    file.path("transform.html") %>%
                                    shiny::includeHTML()
                                })
                                output$html.normalizeHelp <- shiny::renderUI({
                                  help_folder %>%
                                    file.path("normalize.html") %>%
                                    shiny::includeHTML()
                                })
                                output$html.missingsHelp <- shiny::renderUI({
                                  help_folder %>%
                                    file.path("missings.html") %>%
                                    shiny::includeHTML()
                                })
                                output$html.anomaliesHelp <- shiny::renderUI({
                                  help_folder %>%
                                    file.path("anomalies.html") %>%
                                    shiny::includeHTML()
                                })
                                output$html.substituteImpHelp <- shiny::renderUI({
                                  help_folder %>%
                                    file.path("substitute_imp.html") %>%
                                    shiny::includeHTML()
                                })
                                output$html.validateHelp <- shiny::renderUI({
                                  help_folder %>%
                                    file.path("validate.html") %>%
                                    shiny::includeHTML()
                                })
                              }
                              else
                              {
                                output$html.uploadHelp <- shiny::renderUI({NULL})
                                output$html.filterHelp <- shiny::renderUI({NULL})
                                output$html.exploreHelp <- shiny::renderUI({NULL})
                                output$html.uploadLOQHelp <- shiny::renderUI({NULL})
                                output$html.defineLOQHelp <- shiny::renderUI({NULL})
                                output$html.detectLOQHelp <- shiny::renderUI({NULL})
                                output$html.substituteLOQHelp <- shiny::renderUI({NULL})
                                output$html.transformHelp <- shiny::renderUI({NULL})
                                output$html.normalizeHelp <- shiny::renderUI({NULL})
                                output$html.missingsHelp <- shiny::renderUI({NULL})
                                output$html.anomaliesHelp <- shiny::renderUI({NULL})
                                output$html.substituteImpHelp <- shiny::renderUI({NULL})
                                output$html.validateHelp <- shiny::renderUI({NULL})
                              }
                            }#function
                          )#public
)#class
