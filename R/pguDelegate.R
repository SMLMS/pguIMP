#' @title pgu.delegate
#'
#' @description
#' Manages the communication between the shiny gui layer and the classes of the pguIMP package
#'
#' @details
#' Comprises all needed classes from the pguIMP package and manages the communication between the gui and the analysis.
#'
#' @format [R6::R6Class] object.
#' @section Construction:
#' x <- pguIMP::pgu.delegate$new(data)
#'
#' @import R6
#' @import tidyverse
#' @import shiny
#' @import DT
#'
#' @include pguStatus.R
#' @include pguFile.R
#' @include pguImporter.R
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
                            .importer = "pgu.importer",
                            .rawData = "pgu.data",
                            .loq = "pgu.limitsOfQuantification",
                            .metadata = "pgu.data",
                            .filterSet = "pgu.filter",
                            .filteredData = "pgu.data",
                            .filteredMetadata = "pgu.data",
                            .explorer= "pgu.explorer",
                            .loqMutatedData = "pgu.data",
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
                            .reporter = "pgu.reporter"
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
                            #' @field importer
                            #' Returns the instance variable status
                            #' (pguIMP::pgu.status)
                            importer = function(){
                              return(private$.importer)
                            },
                            #' @field rawData
                            #' Returns the instance variable rawData
                            #' (pguIMP::pgu.data)
                            rawData = function(){
                              return(private$.rawData)
                            },
                            #' @field loq
                            #' Returns the instance variable loq
                            #' (pguIMP::pgu.limitsOfQuantification)
                            loq = function(){
                              return(private$.loq)
                            },
                            #' @field metadata
                            #' Returns the instance variable metadata
                            #' (pguIMP::pgu.data)
                            metadata = function(){
                              return(private$.metadata)
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
                            #' @field filteredMetadata
                            #' Returns the instance variable filteredMetadata
                            #' (pguIMP::pgu.data)
                            filteredMetadata = function(){
                              return(private$.filteredMetadata)
                            },
                            #' @field explorer
                            #' Returns the instance variable explorer
                            #' (pguIMP::pgu.explorer)
                            explorer = function(){
                              return(private$.explorer)
                            },
                            #' @field loqMutatedData
                            #' Returns the instance variable loqMutatedData
                            #' (pguIMP::pgu.data)
                            loqMutatedData = function(){
                              return(private$.loqMutatedData)
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
                            #' @examples
                            #' y <- tibble:tibble()
                            #' x <- pguIMP:pgu.delegate$new(data = y)
                            initialize = function(data = "tbl_df") {
                              print("Instance of pgu.delegate allocated")
                              private$.status <- pgu.status$new()
                              private$.fileName <- pgu.file$new()
                              private$.importer <- pgu.importer$new()
                              private$.rawData <- pgu.data$new()
                              private$.loq <- pgu.limitsOfQuantification$new()
                              private$.metadata <- pgu.data$new()
                              private$.filterSet <- pgu.filter$new()
                              private$.filteredData <- pgu.data$new()
                              private$.filteredMetadata <- pgu.data$new()
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

                            #' @description
                            #' Clears the heap and
                            #' indicates that instance of `pgu.delegate` is removed from heap.
                            finalize = function() {
                              print("Instance of pgu.delegate removed from heap")
                            }, #function

                            ##########################
                            # print instance variables
                            ##########################
                            #' @description
                            #' Prints instance variables of a `pgu.delegate` object.
                            #' @return
                            #' string
                            #' @examples
                            #' x$print()
                            #' print(x)
                            print = function() {
                              sprintf("\npgu.delegate\n\n") %>%
                                cat()
                              print(self$status)
                              print(self$fileName)
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
                            }, #fucntion
                            ####################
                            # import functions #
                            ####################
                            #' @description
                            #' Manages the data upload to the R server.
                            #' Updates the instance class status.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$queryExcel(input, output, session)
                            queryExcel = function(input, output, session){
                              if (length(input$fi.import$datapath) > 0){
                                private$.fileName$setUploadFileName <- input$fi.import$datapath
                                private$.fileName$setFileName <- input$fi.import$name
                                private$.fileName$splitFileName()
                                private$.status$update(processName = "dataUploaded", value = TRUE)
                              } #if
                              else{
                                private$.status$update(processName = "dataUploaded", value = FALSE)
                                shiny::showNotification(paste("Please select a valid file."),type = "error", duration = 10)
                              } #else
                              if((private$.fileName$suffix != "xls") && (private$.fileName$suffix != "xlsx")){
                                private$.status$update(processName = "dataUploaded", value = FALSE)
                                shiny::showNotification(paste("Please select a valid file of type '.xls' or '.xlsx'."),type = "error", duration = 10)
                              } #if
                            }, #function

                            #' @description
                            #' Imports uploaded data from the R server into the instance variable rawData.
                            #' Updates the instance class status.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$importData(input, output, session)
                            importData = function(input, output, session){
                              if (private$.status$query(processName = "dataUploaded")){
                                tryCatch({
                                  private$.rawData$setRawData <- private$.importer$importData(self$fileName)
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
                            }, #function

                            #' @description
                            #' Imports uploaded data from the R server into the instance variable loqData.
                            #' Updates the instance class status.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$importLoq(input, output, session)
                            importLoq = function(input, output, session){
                              if (private$.status$query(processName = "dataImported")){
                                tryCatch({
                                  private$.loq$setLoq <- private$.importer$importLoq(self$fileName)
                                  private$.status$update(processName = "loqImported", value = TRUE)
                                },
                                error = function(e) {
                                  private$.status$update(processName = "loqImported", value = FALSE)
                                  shiny::showNotification(paste(e),type = "error", duration = 10)
                                }#error
                                )#tryCatch
                              }#if
                              else{
                                private$.status$update(processName = "loqImported", value = FALSE)
                                shiny::showNotification(paste("No file uploaded to import. Please upload a valid file first."),type = "error", duration = 10)
                              }#else
                            }, #function

                            #' @description
                            #' Imports uploaded data from the R server into the instance variable metadata.
                            #' Updates the instance class status.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$importMetadata(input, output, session)
                            importMetadata = function(input, output, session){
                              if (private$.status$query(processName = "dataImported")){
                                tryCatch({
                                  private$.metadata$setRawData <- private$.importer$importMetadata(self$fileName)
                                  private$.status$update(processName = "metadataImported", value = TRUE)
                                },
                                error = function(e) {
                                  private$.status$update(processName = "metadataImported", value = FALSE)
                                  shiny::showNotification(paste(e),type = "error", duration = 10)
                                }#error
                                )#tryCatch
                              }#if
                              else{
                                private$.status$update(processName = "metadataImported", value = FALSE)
                                shiny::showNotification(paste("No file uploaded to import. Please upload a valid file first."),type = "error", duration = 10)
                              }#else
                            }, #function

                            ####################
                            # filter functions #
                            ####################
                            #' @description
                            #' Queries the filter parameters selected by the user in the gui
                            #'  and stores them in the instance variable filterSet.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$updateFilter(input, output, session)
                            updateFilter = function(input, output, session){
                              if(private$.status$query(processName = "dataImported")){
                                if (length(input$tbl.filter_rows_all) < 1) {
                                  private$.filterSet$resetRowIdx(data = self$rawData$rawData)
                                }#if
                                else {
                                  private$.filterSet$setRowIdx <- input$tbl.filter_rows_all
                                }#else
                                if (length(input$tbl.filter_columns_selected) < 1) {
                                  private$.filterSet$resetColIdx(data = self$rawData$rawData)
                                }#if
                                else{
                                  colSelection <- input$tbl.filter_columns_selected - ncol(self$metadata$rawData) + 1
                                  colSelection <- colSelection[colSelection > 0] + 1
                                  if(length(colSelection) < 1){
                                    private$.filterSet$resetColIdx(data = self$rawData$rawData)
                                  }#if
                                  else{
                                    colSelection <- colSelection %>%
                                      append(1) %>%
                                      unique() %>%
                                      sort
                                    private$.filterSet$setColIdx <- colSelection
                                  }#else
                                }#else
                              }#if
                              else{
                                shiny::showNotification(paste("No file imported. Please import a valid file first."),type = "error", duration = 10)
                              }#else
                            }, #fucntion

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
                            #' @examples
                            #' x$updateFilterInverse(input, output, session)
                            updateFilterInverse = function(input, output, session){
                              if(private$.status$query(processName = "dataImported")){
                                if (length(input$tbl.filter_rows_all) < 1) {
                                  private$.filterSet$resetRowIdx(data = self$rawData$rawData)
                                }#if
                                else {
                                  private$.filterSet$setRowIdx <- input$tbl.filter_rows_all
                                }#else
                                if (length(input$tbl.filter_columns_selected) < 1) {
                                  private$.filterSet$resetColIdx(data = self$rawData$rawData)
                                }#if
                                else{
                                  colSelection <- input$tbl.filter_columns_selected - ncol(self$metadata$rawData) + 1
                                  colSelection <- colSelection[colSelection > 0] + 1
                                  idx <- seq(1,ncol(self$rawData$rawData), 1)
                                  iverseColSelection <- idx[-c(colSelection)]
                                  if(length(iverseColSelection) < 1){
                                    private$.filterSet$resetColIdx(data = self$rawData$rawData)
                                  }#if
                                  else{
                                    iverseColSelection <- iverseColSelection %>%
                                      append(1) %>%
                                      unique() %>%
                                      sort()
                                    private$.filterSet$setColIdx <- iverseColSelection
                                  }#else
                                }#else
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
                            #' @examples
                            #' x$resetFilter(input, output, session)
                            resetFilter = function(input, output, session){
                              if(private$.status$query(processName = "dataImported")){
                                private$.filterSet$resetRowIdx(data = self$rawData$rawData)
                                private$.filterSet$resetColIdx(data = self$rawData$rawData)
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
                            #' @examples
                            #' x$filterData(input, output, session)
                            filterData = function(input, output, session){
                              if(private$.status$query(processName = "dataImported")){
                                private$.filteredData$setRawData <- self$rawData$rawData %>%
                                  self$filterSet$filter()
                                private$.filteredMetadata$setRawData <- self$metadata$rawData %>%
                                  self$filterSet$filterRows()
                                self$updateExplorationData(input, output, session)
                                private$.status$update(processName = "dataFiltered", value = TRUE)
                              }#if
                              else{
                                shiny::showNotification(paste("No file imported. Please import a valid file first."),type = "error", duration = 10)
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
                            #' @examples
                            #' x$updateExplorationData(input, output, session)
                            updateExplorationData = function(input, output, session){
                              if(private$.status$query(processName = "dataFiltered")){
                                private$.explorer$reset(obj =self$filteredMetadata$rawData %>%
                                                          dplyr::right_join(self$filteredData$rawData, by = "Sample Name"),
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
                            #' @examples
                            #' x$updateExplorationAbscissa(input, output, session)
                            updateExplorationAbscissa = function(input, output, session){
                              private$.explorer$setAbscissa <- input$si.exploreAbs
                            }, #function

                            #' @description
                            #' Transfers the information oabout the selected ordinate attribute to the explorer class.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$updateExplorationOrdinate(input, output, session)
                            updateExplorationOrdinate = function(input, output, session){
                              private$.explorer$setOrdinate <- input$si.exploreOrd
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
                            #' @examples
                            #' x$updateExplorationGraphic(input, output, session)
                            updateExplorationGraphic = function(input, output, session){
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
                            #' @examples
                            #' x$updateExplorationAbscissaGraphic(input, output, session)
                            updateExplorationAbscissaGraphic = function(input, output, session){
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
                            #' @examples
                            #' x$updateExplorationOrdinateGraphic(input, output, session)
                            updateExplorationOrdinateGraphic = function(input, output, session){
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
                            #' @examples
                            #' x$updateExplorationAbscissaTable(input, output, session)
                            updateExplorationAbscissaTable = function(input, output, session){
                              if(private$.status$query(processName = "dataFiltered")){
                                output$tbl.exploreAbscissaStatistics <- DT::renderDataTable({
                                  self$explorer$abscissaStatistic() %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        # scrollY = '75vh',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$bluntFileName("rawData_type"),
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
                            #' @examples
                            #' x$updateExplorationOrdinateTable(input, output, session)
                            updateExplorationOrdinateTable = function(input, output, session){
                              if(private$.status$query(processName = "dataFiltered")){
                                output$tbl.exploreOrdinateStatistics <- DT::renderDataTable({
                                  self$explorer$ordinateStatistic() %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$bluntFileName("rawData_type"),
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
                            # LOQ Detect functions #
                            ########################
                            #' @description
                            #' Updates the gui.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$updateLoqDetectGui(input, output, session)
                            updateLoqDetectGui = function(input, output, session){
                              if(private$.status$query(processName = "dataFiltered")){
                                self$updateLoqNaHandling(input, output, session)
                                shiny::updateSelectInput(session,
                                                         "si.loqDetectFeature",
                                                         choices = self$filteredData$numericFeatureNames,
                                                         selected = self$filteredData$numericFeatureNames[1])

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
                            #' @examples
                            #' x$updateLoqNaHandling(input, output, session)
                            updateLoqNaHandling = function(input, output, session){
                              shiny::updateSelectInput(session,
                                                       "si.loqNaHandling",
                                                       choices = self$loq$naHandlingAlphabet,
                                                       selected = self$loq$naHandlingAgent)
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
                            #' @examples
                            #' x$detectLoq(input, output, session)
                            detectLoq = function(input, output, session){
                              if(private$.status$query(processName = "dataFiltered")){
                                private$.loq$setNaHandlingAgent <- input$si.loqNaHandling
                                private$.loq$findOutliers(obj = self$filteredData$numericData())
                                private$.loq$collectStatistics(obj = self$filteredData$numericData())
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
                            #' @examples
                            #' x$updateLoqDetectStatisticsTbl(input, output, session)
                            updateLoqDetectStatisticsTbl = function(input, output, session){
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
                                          filename = self$fileName$bluntFileName("loqStatistics"),
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
                            #' @examples
                            #' x$updateLoqDetectOutlierTbl(input, output, session)
                            updateLoqDetectOutlierTbl = function(input, output, session){
                              options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
                              t <- NULL
                              if(self$status$query(processName = "loqDetected")){
                                dfData <- self$filteredMetadata$rawData %>%
                                  dplyr::right_join(self$filteredData$rawData, by = "Sample Name")
                                dfOutlier <- self$loq$outliers
                                idx <- dfOutlier[["measurement"]][!duplicated(dfOutlier[["measurement"]])]
                                t <- dfData %>%
                                  dplyr::slice(idx) %>%
                                  dplyr::mutate_if(is.numeric, round, 3) %>%
                                  DT::datatable(
                                    options = list(
                                      scrollX = TRUE,
                                      scrollY = '350px',
                                      paging = FALSE,
                                      dom = "Blfrtip",
                                      buttons = list(list(
                                        extend = 'csv',
                                        filename = self$fileName$bluntFileName("loqOutlier"),
                                        text = "Download"
                                      ))#buttons
                                    )#options
                                  )#DT::datatable
                                for (featureName in self$filteredData$numericFeatureNames){
                                  featureOutlier <- dfOutlier %>%
                                    dplyr::filter(feature == featureName) %>%
                                    dplyr::mutate_if(is.numeric, round, 3)
                                  if (nrow(featureOutlier)>0){
                                    t <- DT::formatStyle(t,
                                                         featureName,
                                                         backgroundColor = styleEqual(dfData %>%
                                                                                        dplyr::select(!!featureName) %>%
                                                                                        dplyr::slice(featureOutlier[["measurement"]]) %>%
                                                                                        unlist() %>%
                                                                                        as.numeric() %>%
                                                                                        round(digits = 3),
                                                                                      featureOutlier[["color"]])

                                    )#t
                                  }#if
                                }#for
                              }#if
                              output$tbl.loqDetectOutlier <- DT::renderDataTable(t)
                            }, #function

                            #' @description
                            #' Updates the numerical loq data table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$updateLoqDetectDataTbl(input, output, session)
                            updateLoqDetectDataTbl = function(input, output, session){
                              options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
                              t <- NULL
                              if(self$status$query(processName = "loqDetected")){
                                dfData <- self$filteredMetadata$rawData %>%
                                  dplyr::right_join(self$filteredData$rawData, by = "Sample Name")
                                dfOutlier <- self$loq$outliers
                                idx <- dfOutlier[["measurement"]][!duplicated(dfOutlier[["measurement"]])]
                                t <- dfData %>%
                                  dplyr::mutate_if(is.numeric, round, 3) %>%
                                  DT::datatable(
                                    options = list(
                                      scrollX = TRUE,
                                      scrollY = '350px',
                                      paging = FALSE,
                                      dom = "Blfrtip",
                                      buttons = list(list(
                                        extend = 'csv',
                                        filename = self$fileName$bluntFileName("loqData"),
                                        text = "Download"
                                      ))#buttons
                                    )#options
                                  )#DT::datatable
                                for (featureName in self$filteredData$numericFeatureNames){
                                  featureOutlier <- dfOutlier %>%
                                    dplyr::filter(feature == featureName) %>%
                                    dplyr::mutate_if(is.numeric, round, 3)
                                  if (nrow(featureOutlier)>0){
                                    t <- DT::formatStyle(t,
                                                         featureName,
                                                         backgroundColor = styleEqual(dfData %>%
                                                                                        dplyr::select(!!featureName) %>%
                                                                                        dplyr::slice(featureOutlier[["measurement"]]) %>%
                                                                                        unlist() %>%
                                                                                        as.numeric() %>%
                                                                                        round(digits = 3),
                                                                                      featureOutlier[["color"]])

                                    )#t
                                  }#if
                                }#for
                              }#if
                              output$tbl.loqDetectData <- DT::renderDataTable(t)
                            }, #function

                            #' @description
                            #' Updates the loq statistics graphic.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$updateLoqDetectStatisticsGraphic(input, output, session)
                            updateLoqDetectStatisticsGraphic = function(input, output, session){
                              if(self$status$query(processName = "loqDetected")){
                                output$plt.loqDetectStatistics <- shiny::renderPlot(
                                  self$loq$plotLoqDistribution(),
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
                            #' @examples
                            #' x$updateLoqDetectFeatureGraphic(input, output, session)
                            updateLoqDetectFeatureGraphic = function(input, output, session){
                              if(self$status$query(processName = "loqDetected")){
                                output$plt.loqDetectFeature <- shiny::renderPlot(
                                  self$loq$featurePlot(obj = self$filteredData$rawData, feature = input$si.loqDetectFeature),
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
                            #' @examples
                            #' x$updateLoqDetectFeatureTbl(input, output, session)
                            updateLoqDetectFeatureTbl = function(input, output, session){
                              options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
                              t <- NULL
                              if(private$.status$query(processName = "loqDetected")){
                                feature <- input$si.loqDetectFeature
                                dfData <- self$filteredMetadata$rawData %>%
                                  dplyr::right_join(self$filteredData$rawData %>%
                                                      dplyr::select(c("Sample Name", !!feature)),
                                                    by = "Sample Name")

                                dfOutlier <- self$loq$featureOutlier(feature = feature)

                                t <- dfData %>%
                                  DT::datatable(options = list(scrollX = TRUE,
                                                               scrollY = '300px',
                                                               paging = FALSE,
                                                               dom = "Blfrtip",
                                                               buttons = list(list(
                                                                 extend = 'csv',
                                                                 filename = self$fileName$bluntFileName("rawData_type"),
                                                                 text = "Download"
                                                               ))
                                  ))#DT::datatable
                                if (nrow(dfOutlier) > 0){
                                  t <- DT::formatStyle(
                                    t,
                                    feature,
                                    backgroundColor = DT::styleEqual(dfData %>%
                                                                       dplyr::select(!!feature) %>%
                                                                       dplyr::slice(dfOutlier[["measurement"]]) %>%
                                                                       unlist() %>%
                                                                       round(digits = 3),
                                                                     dfOutlier[["color"]]))
                                }#if
                              }#if
                              output$tbl.loqDetectFeature <- DT::renderDataTable(t)
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
                            #' @examples
                            #' x$updateLoqMutateGui(input, output, session)
                            updateLoqMutateGui = function(input, output, session){
                              if(private$.status$query(processName = "loqDetected")){
                                self$updateLloqSubstitute(input, output, session)
                                self$updateUloqSubstitute(input, output, session)
                                shiny::updateSelectInput(session,
                                                         "si.loqMutateFeature",
                                                         choices = self$filteredData$numericFeatureNames,
                                                         selected = self$filteredData$numericFeatureNames[1])

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
                            #' @examples
                            #' x$updateLloqSubstitute(input, output, session)
                            updateLloqSubstitute = function(input, output, session){
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
                            #' @examples
                            #' x$updateUloqSubstitute(input, output, session)
                            updateUloqSubstitute = function(input, output, session){
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
                            #' @examples
                            #' x$mutateLoq(input, output, session)
                            mutateLoq = function(input, output, session){
                              if(private$.status$query(processName = "loqDetected")){
                                private$.loq$setLloqSubstituteAgent <- input$si.lloqSubstitute
                                private$.loq$setUloqSubstituteAgent <- input$si.uloqSubstitute
                                name  <- as.name("Sample Name")
                                private$.loqMutatedData$setRawData <- self$filteredData$numericData() %>%
                                  self$loq$mutateLoqOutliers() %>%
                                  tibble::add_column(!! name := self$filteredData$rawData %>%
                                                       dplyr::select(!!name) %>%
                                                       unlist() %>%
                                                       as.character()) %>%
                                  dplyr::select(c(!!name, self$filteredData$numericFeatureNames))
                                private$.status$update(processName = "loqMutated", value = TRUE)
                              }#if
                              else{
                                shiny::showNotification(paste("No loq outliers detected. Please screen for loq outliers first."),type = "error", duration = 10)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the numerical loq statistics table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$updateLoqMutateStatisticsTbl(input, output, session)
                            updateLoqMutateStatisticsTbl = function(input, output, session){
                              if(private$.status$query(processName = "loqMutated")){
                                output$tbl.loqMutateStatistics <- DT::renderDataTable(
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
                                          filename = self$fileName$bluntFileName("loqStatistics"),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.loqMutateStatistics <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the numerical loq outliers table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$updateLoqMutateOutlierTbl(input, output, session)
                            updateLoqMutateOutlierTbl = function(input, output, session){
                              options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
                              t <- NULL
                              if(self$status$query(processName = "loqMutated")){
                                dfData <- self$filteredMetadata$rawData %>%
                                  dplyr::right_join(self$loqMutatedData$rawData, by = "Sample Name")
                                dfOutlier <- self$loq$outliers
                                idx <- dfOutlier[["measurement"]][!duplicated(dfOutlier[["measurement"]])]
                                t <- dfData %>%
                                  dplyr::slice(idx) %>%
                                  dplyr::mutate_if(is.numeric, round, 3) %>%
                                  DT::datatable(
                                    options = list(
                                      scrollX = TRUE,
                                      scrollY = '350px',
                                      paging = FALSE,
                                      dom = "Blfrtip",
                                      buttons = list(list(
                                        extend = 'csv',
                                        filename = self$fileName$bluntFileName("loqOutlier"),
                                        text = "Download"
                                      ))#nuttons
                                    )#options
                                  )#DT::datatable
                                for (featureName in self$loqMutatedData$numericFeatureNames){
                                  featureOutlier <- dfOutlier %>%
                                    dplyr::filter(feature == featureName) %>%
                                    dplyr::mutate_if(is.numeric, round, 3)
                                  if (nrow(featureOutlier)>0){
                                    t <- DT::formatStyle(t,
                                                         featureName,
                                                         backgroundColor = styleEqual(dfData %>%
                                                                                        dplyr::select(!!featureName) %>%
                                                                                        dplyr::slice(featureOutlier[["measurement"]]) %>%
                                                                                        unlist() %>%
                                                                                        as.numeric() %>%
                                                                                        round(digits = 3),
                                                                                      featureOutlier[["color"]])

                                    )#t
                                  }#if
                                }#for
                              }#if
                              output$tbl.loqMutateOutlier <- DT::renderDataTable(t)
                            }, #function

                            #' @description
                            #' Updates the numerical loq mutate outliers table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$updateLoqMutateDataTbl(input, output, session)
                            updateLoqMutateDataTbl = function(input, output, session){
                              options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
                              t <- NULL
                              if(self$status$query(processName = "loqMutated")){
                                dfData <- self$filteredMetadata$rawData %>%
                                  dplyr::right_join(self$loqMutatedData$rawData, by = "Sample Name")
                                dfOutlier <- self$loq$outliers
                                idx <- dfOutlier[["measurement"]][!duplicated(dfOutlier[["measurement"]])]
                                t <- dfData %>%
                                  dplyr::mutate_if(is.numeric, round, 3) %>%
                                  DT::datatable(
                                    options = list(
                                      scrollX = TRUE,
                                      scrollY = '350px',
                                      paging = FALSE,
                                      dom = "Blfrtip",
                                      buttons = list(list(
                                        extend = 'csv',
                                        filename = self$fileName$bluntFileName("loqData"),
                                        text = "Download"
                                      ))#buttons
                                    )#options
                                  )#DT::datatable
                                for (featureName in self$loqMutatedData$numericFeatureNames){
                                  featureOutlier <- dfOutlier %>%
                                    dplyr::filter(feature == featureName) %>%
                                    dplyr::mutate_if(is.numeric, round, 3)
                                  if (nrow(featureOutlier)>0){
                                    t <- DT::formatStyle(t,
                                                         featureName,
                                                         backgroundColor = styleEqual(dfData %>%
                                                                                        dplyr::select(!!featureName) %>%
                                                                                        dplyr::slice(featureOutlier[["measurement"]]) %>%
                                                                                        unlist() %>%
                                                                                        as.numeric() %>%
                                                                                        round(digits = 3),
                                                                                      featureOutlier[["color"]])

                                    )#t
                                  }#if
                                }#for
                              }#if
                              output$tbl.loqMutateData <- DT::renderDataTable(t)
                            }, #function

                            #' @description
                            #' Updates the loq mutate statistics graphic.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$updateLoqMutateStatisticsGraphic(input, output, session)
                            updateLoqMutateStatisticsGraphic = function(input, output, session){
                              if(self$status$query(processName = "loqMutated")){
                                output$plt.loqMutateStatistics <- shiny::renderPlot(
                                  self$loq$plotLoqDistribution(),
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
                            #' @examples
                            #' x$updateLoqMutateFeatureGraphic(input, output, session)
                            updateLoqMutateFeatureGraphic = function(input, output, session){
                              if(self$status$query(processName = "loqMutated")){
                                output$plt.loqMutateFeature <- shiny::renderPlot(
                                  self$loq$featurePlot(obj = self$loqMutatedData$rawData, feature = input$si.loqMutateFeature),
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
                            #' @examples
                            #' x$updateLoqMutateFeatureTbl(input, output, session)
                            updateLoqMutateFeatureTbl = function(input, output, session){
                              options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
                              t <- NULL
                              if(self$status$query(processName = "loqMutated")){
                                feature <- input$si.loqMutateFeature
                                dfData <- self$filteredMetadata$rawData %>%
                                  dplyr::right_join(self$loqMutatedData$rawData %>%
                                                      dplyr::select(c("Sample Name", !!feature)),
                                                    by = "Sample Name")

                                dfOutlier <- self$loq$featureOutlier(feature = feature)

                                t <- dfData %>%
                                  DT::datatable(options = list(scrollX = TRUE,
                                                               scrollY = '300px',
                                                               paging = FALSE,
                                                               dom = "Blfrtip",
                                                               buttons = list(list(
                                                                 extend = 'csv',
                                                                 filename = self$fileName$bluntFileName("rawData_type"),
                                                                 text = "Download"
                                                               ))#buttons
                                  ))#DT::datatable
                                if (nrow(dfOutlier) > 0){
                                  t <- DT::formatStyle(
                                    t,
                                    feature,
                                    backgroundColor = DT::styleEqual(dfData %>%
                                                                       dplyr::select(!!feature) %>%
                                                                       dplyr::slice(dfOutlier[["measurement"]]) %>%
                                                                       unlist() %>%
                                                                       round(digits = 3),
                                                                     dfOutlier[["color"]])
                                    )#t
                                }#if
                              }#if
                              output$tbl.loqMutateFeature <- DT::renderDataTable(t)
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
                            #' @examples
                            #' x$optimizeTrafoParameter(input, output, session)
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
                                private$.optimizer$resetOptimizer(data = self$loqMutatedData$numericData())
                                private$.optimizer$setTrafoAlphabet <- trafoAlphabet
                                private$.optimizer$setMirror <- input$cb.wizardMirror
                                progress <- shiny::Progress$new(session, min = 1, max = length(self$optimizer$trafoAlphabet)*2)
                                progress$set(message = "optimizing transformation parameters ...", value = 1)
                                private$.optimizer$optimize(data = self$loqMutatedData$numericData(), progress = progress)
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
                            #' @examples
                            #' x$updateDetectedTrafoTypes(input, output, session)
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
                            #' @examples
                            #' x$updateDetectedTrafoParameter(input, output, session)
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
                            #' @examples
                            #' x$updateTrafoDetectGui(input, output, session)
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
                            #' @examples
                            #' x$updateTrafoMutateFeature(input, output, session)
                            updateTrafoMutateFeature = function(input, output, session){
                              if(self$status$query(processName = "loqMutated")){
                                shiny::updateSelectInput(session,
                                                         inputId = "si.trafoMutateFeature",
                                                         choices = self$loqMutatedData$numericFeatureNames,
                                                         selected = self$loqMutatedData$numericFeatureNames[1])
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
                            #' @examples
                            #' x$updateTrafoMutateType(input, output, session)
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
                            #' @examples
                            #' x$updateTrafoMutateLambdae(input, output, session)
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
                            #' @examples
                            #' x$updateTrafoMutateMirror(input, output, session)
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
                            #' @examples
                            #' x$updateTrafoMutateGui(input, output, session)
                            updateTrafoMutateGui = function(input, output, session){
                              if(self$status$query(processName = "loqMutated")){
                                self$loqMutatedData$numericData() %>%
                                  self$transformator$resetTrafoParameter()
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
                            #' @examples
                            #' x$resetTrafoMutateGui(input, output, session)
                            resetTrafoMutateGui = function(input, output, session){
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
                            #' @examples
                            #' x$trafoMutateFit(input, output, session)
                            trafoMutateFit = function(input, output, session){
                              if(self$status$query(processName = "modelDefined")){
                                progress <- shiny::Progress$new(session, min = 1, max = length(self$loqMutatedData$numericFeatureNames))
                                progress$set(message = "Optimizing model parameter", value = 1)
                                on.exit(progress$close())
                                self$loqMutatedData$numericData() %>%
                                  private$.transformator$fit()
                                self$resetTrafoMutateGui(input, output, session)
                                self$loqMutatedData$numericData() %>%
                                  private$.transformator$mutateData() %>%
                                  private$.model$resetModel(progress)
                                name  <- as.name("Sample Name")
                                private$.transformedData$setRawData <- self$loqMutatedData$numericData() %>%
                                  self$transformator$mutateData() %>%
                                  tibble::add_column(!! name := self$loqMutatedData$rawData %>%
                                                       dplyr::select(!!name) %>%
                                                       unlist() %>%
                                                       as.character()) %>%
                                  dplyr::select(c(!!name, self$loqMutatedData$numericFeatureNames))
                              }#if
                              else{
                                shiny::showNotification(paste("No global model defined. Please defina a global transformation model first."),type = "error", duration = 10)
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
                            #' @examples
                            #' x$trafoMutateGlobal(input, output, session)
                            trafoMutateGlobal = function(input, output, session){
                              if(self$status$query(processName = "loqMutated")){
                                progress <- shiny::Progress$new(session, min = 1, max = length(self$loqMutatedData$numericFeatureNames))
                                progress$set(message = "Optimizing model parameter", value = 1)
                                on.exit(progress$close())
                                # self$loqMutatedData$numericData() %>%
                                #   private$.transformator$resetTrafoParameter()
                                for (feature in self$loqMutatedData$numericFeatureNames){
                                  private$.transformator$setTrafoType(feature = feature,
                                                                      type = input$si.trafoMutateType)
                                  private$.transformator$setLambdaLOP(feature = feature,
                                                                      lambda = input$ni.trafoMutateLambdaLOP)
                                  private$.transformator$setMirrorLogic(feature = feature,
                                                                        logic = input$cb.trafoMutateMirror)
                                }#for

                                # self$loqMutatedData$numericData() %>%
                                #   private$.transformator$estimateTrafoParameter()
                                self$loqMutatedData$numericData() %>%
                                  private$.transformator$mutateData() %>%
                                  private$.model$resetModel(progress)
                                name  <- as.name("Sample Name")
                                private$.transformedData$setRawData <- self$loqMutatedData$numericData() %>%
                                  self$transformator$mutateData() %>%
                                  # self$model$scaleData() %>%
                                  tibble::add_column(!! name := self$loqMutatedData$rawData %>%
                                                       dplyr::select(!!name) %>%
                                                       unlist() %>%
                                                       as.character()) %>%
                                  dplyr::select(c(!!name, self$loqMutatedData$numericFeatureNames))
                                private$.status$update(processName = "modelDefined", value = TRUE)
                              }#if
                              else{
                                shiny::showNotification(paste("No loq analysis perfomred. Please mutate loq outliers first."),type = "error", duration = 10)
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
                            #' @examples
                            #' x$trafoMutateFeature(input, output, session)
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

                                self$loqMutatedData$numericData() %>%
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

                                  private$.featureModel$resetNormDist(data = self$loqMutatedData$numericData %>%
                                                                        self$transformator$mutateData() %>%
                                                                        dplyr::select(input$si.trafoMutateFeature)
                                  )

                                  private$.featureModel$fit()
                                })#tryCatch

                                private$.model$setNormDist(data = self$featureModel, feature = input$si.trafoMutateFeature)

                                name  <- as.name("Sample Name")
                                private$.transformedData$setRawData <- self$loqMutatedData$numericData() %>%
                                  self$transformator$mutateData() %>%
                                  # self$model$scaleData() %>%
                                  tibble::add_column(!! name := self$loqMutatedData$rawData %>%
                                                       dplyr::select(!!name) %>%
                                                       unlist() %>%
                                                       as.character()) %>%
                                  dplyr::select(c(!!name, self$loqMutatedData$numericFeatureNames))
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
                            #' @examples
                            #' x$updateTrafoMutateFeatureGraphic(input, output, session)
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
                            #' @examples
                            #' x$updateTrafoMutateFeatureParameterTbl(input, output, session)
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
                                          filename = self$fileName$bluntFileName("featureTrafoParameter"),
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
                            #' @examples
                            #' x$updateTrafoMutateFeatureQualityTbl(input, output, session)
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
                                          filename = self$fileName$bluntFileName("featureTrafoQuality"),
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
                            #' @examples
                            #' x$updateTrafoMutateGlobalParameterTbl(input, output, session)
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
                                          filename = self$fileName$bluntFileName("globalTrafoParameter"),
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
                            #' @examples
                            #' x$updateTrafoMutateGlobalModelTbl(input, output, session)
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
                                          filename = self$fileName$bluntFileName("globalModelParameter"),
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
                            #' @examples
                            #' x$updateTrafoMutateGlobalQualityTbl(input, output, session)
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
                                          filename = self$fileName$bluntFileName("globalModelQuality"),
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
                            #' @examples
                            #' x$updateTrafoMutateGlobalTestsTbl(input, output, session)
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
                                          filename = self$fileName$bluntFileName("globalModelQuality"),
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
                            #' @examples
                            #' x$updateTrafoMutateGlobalDataTbl(input, output, session)
                            updateTrafoMutateGlobalDataTbl = function(input, output, session){
                              options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
                              if(self$status$query(processName = "modelDefined")){
                                output$tbl.trafoMutateGlobalData <- DT::renderDataTable(
                                  self$filteredMetadata$rawData %>%
                                    dplyr::right_join(self$transformedData$rawData, by = "Sample Name") %>%
                                    dplyr::mutate_if(is.numeric, round, 3) %>%
                                    DT::datatable(
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '350px',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$bluntFileName("transformedData"),
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
                            #' @examples
                            #' x$updateTrafoNormFeature(input, output, session)
                            updateTrafoNormFeature = function(input, output, session){
                              if(self$status$query(processName = "modelDefined")){
                                shiny::updateSelectInput(session,
                                                         "si.trafoNormFeature",
                                                         choices = self$transformedData$numericFeatureNames,
                                                         selected = self$transformedData$numericFeatureNames[1])
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
                            #' @examples
                            #' x$updateTrafoNormMethod(input, output, session)
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
                            #' @examples
                            #' x$updateTrafoNormGui(input, output, session)
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
                            #' @examples
                            #' x$trafoNormMutate(input, output, session)
                            trafoNormMutate = function(input, output, session){
                              if(self$status$query(processName = "modelDefined")){
                                private$.normalizer$setNormAgent <- input$si.trafoNormMethod

                                self$transformedData$numericData() %>%
                                  private$.normalizer$detectNormParameter()

                                name  <- as.name("Sample Name")
                                private$.normalizedData$setRawData <- self$transformedData$numericData() %>%
                                  self$normalizer$scale_data() %>%
                                  tibble::add_column(!! name := self$transformedData$rawData %>%
                                                       dplyr::select(!!name) %>%
                                                       unlist() %>%
                                                       as.character()) %>%
                                  dplyr::select(c(!!name, self$transformedData$numericFeatureNames))
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
                            #' @examples
                            #' x$updateTrafoNormFeatureGraphic(input, output, session)
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
                            #' @examples
                            #' x$resetTrafoNormGui(input, output, session)
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
                            #' @examples
                            #' x$updateTrafoNormFeatureStatisticsTbl(input, output, session)
                            updateTrafoNormFeatureStatisticsTbl = function(input, output, session){
                              if(private$.status$query(processName = "normalized")){
                                output$tbl.trafoNormFeatureStatistics <- DT::renderDataTable({
                                  self$normalizedData$dataStatistics() %>%
                                    dplyr::filter(Value == input$si.trafoNormFeature) %>%
                                    dplyr::select_if(is.numeric) %>%
                                    tidyr::pivot_longer(cols = dplyr::everything()) %>%
                                    dplyr::rename(statistics = "name") %>%
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
                            #' @examples
                            #' x$updateTrafoNormStatisticsTbl(input, output, session)
                            updateTrafoNormStatisticsTbl = function(input, output, session){
                              if(private$.status$query(processName = "normalized")){
                                output$tbl.trafoNormStatistics <- DT::renderDataTable({
                                  self$normalizedData$dataStatistics() %>%
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
                                          filename = self$fileName$bluntFileName("normalized_statistics"),
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
                            #' @examples
                            #' x$updateTrafoNormParameterTbl(input, output, session)
                            updateTrafoNormParameterTbl = function(input, output, session){
                              if(private$.status$query(processName = "normalized")){
                                output$tbl.trafoNormParameter <- DT::renderDataTable({
                                  self$normalizer$normParameter %>%
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
                                          filename = self$fileName$bluntFileName("normalization_parameter"),
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
                            #' @examples
                            #' x$updateImputeNormDataTbl(input, output, session)
                            updateTrafoNormDataTbl = function(input, output, session){
                              if(private$.status$query(processName = "normalized")){
                                output$tbl.trafoNormData <- DT::renderDataTable({
                                  self$normalizedData$rawData %>%
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
                                          filename = self$fileName$bluntFileName("normalized_data"),
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
                            #' @examples
                            #' x$imputeMissingsAnalyze(input, output, session)
                            imputeMissingsAnalyze = function(input, output, session){
                              if(self$status$query(processName = "normalized")){
                                progress <- shiny::Progress$new(session, min = 1, max = length(self$normalizedData$numericFeatureNames))
                                progress$set(message = "Characterizing missings", value = 1)
                                on.exit(progress$close())
                                self$normalizedData$numericData() %>%
                                  private$.missings$resetImputationParameter()
                                self$normalizedData$numericData() %>%
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
                            #' @examples
                            #' x$updateImputeMissingsGraphic(input, output, session)
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
                            #' @examples
                            #' x$updateImputeMissingsStatisticsTbl(input, output, session)
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
                                          filename = self$fileName$bluntFileName("missingsStatistics"),
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
                            #' @examples
                            #' x$updateImputeMissingsDistributionTbl(input, output, session)
                            updateImputeMissingsDistributionTbl = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                output$tbl.imputeMissingsDistribution <- DT::renderDataTable(
                                  self$normalizedData$numericData() %>%
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
                                          filename = self$fileName$bluntFileName("missingsDistribution"),
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
                            #' @examples
                            #' x$updateImputeMissingCharacteristicsGraphic(input, output, session)
                            updateImputeMissingCharacteristicsGraphic = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                output$plt.imputeMissingsPairs <- shiny::renderPlot(
                                  self$normalizedData$numericData() %>%
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
                            #' @examples
                            #' x$updateImputeMissingsCharacteristicsTbl(input, output, session)
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
                                          filename = self$fileName$bluntFileName("missingsMissings"),
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
                            #' @examples
                            #' x$updateImputeMissingsDetailTbl(input, output, session)
                            updateImputeMissingsDetailTbl = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                output$tbl.imputeMissingsDetail <- DT::renderDataTable(
                                  self$normalizedData$rawData %>%
                                    self$missings$mergeImputationSiteData(metadata_df = self$metadata$rawData) %>%
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
                                          filename = self$fileName$bluntFileName("imputationSiteDetectionDetail"),
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

                            #' @description
                            #' Updates the tbl.imputeDetectData table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$updateImputeMissingsDataTbl(input, output, session)
                            updateImputeMissingsDataTbl = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                output$tbl.imputeMissingsData <- DT::renderDataTable(
                                  self$filteredMetadata$rawData %>%
                                    dplyr::right_join(self$normalizedData$rawData, by = "Sample Name") %>%
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
                                          filename = self$fileName$bluntFileName("imputationSiteDetectionData"),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.imputeMissingsData <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

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
                            #' @examples
                            #' x$updateImputeOutliersMethod(input, output, session)
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
                            #' @examples
                            #' x$updateImputeOutliersFeature(input, output, session)
                            updateImputeOutliersFeature = function(input, output, session){
                              if(self$status$query(processName = "naDetected")){
                                shiny::updateSelectInput(session,
                                                         "si.imputeOutliersFeature",
                                                         choices = self$normalizedData$numericFeatureNames,
                                                         selected = self$normalizedData$numericFeatureNames[1])
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
                            #' @examples
                            #' x$updateImputeOutliersAlpha(input, output, session)
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
                            #' @examples
                            #' x$updateImputeOutliersEpsilon(input, output, session)
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
                            #' @examples
                            #' x$updateImputeOutliersMinSamples(input, output, session)
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
                            #' @examples
                            #' x$updateImputeOutliersGamma(input, output, session)
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
                            #' @examples
                            #' x$updateImputeOutliersNu(input, output, session)
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
                            #' @examples
                            #' x$updateImputeOutliersCutoff(input, output, session)
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
                            #' @examples
                            #' x$updateImputeOutliersK(input, output, session)
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
                            #' @examples
                            #' x$updateImputeOutliersSeed(input, output, session)
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
                            #' @examples
                            #' x$updateImputeOutliersGui(input, output, session)
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
                            #' @examples
                            #' x$resetImputeOutliersGui(input, output, session)
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
                            #' @examples
                            #' x$imputeOutliersDetect(input, output, session)
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

                                progress <- shiny::Progress$new(session, min = 1, max = length(self$loqMutatedData$numericFeatureNames))
                                progress$set(message = "Searching for anomalies", value = 1)
                                on.exit(progress$close())
                                self$normalizedData$numericData() %>%
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
                            #' @examples
                            #' x$updateImputeOutliersGraphic(input, output, session)
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
                            #' @examples
                            #' x$updateImputeOutliersFeatureGraphic(input, output, session)
                            updateImputeOutliersFeatureGraphic = function(input, output, session){
                              if(self$status$query(processName = "outliersDetected")){
                                output$plt.outliersImputeFeature <- shiny::renderPlot(
                                    self$outliers$featurePlot(data_df = self$normalizedData$numericData(),
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
                            #' @examples
                            #' x$updateImputeOutliersFeatureTbl(input, output, session)
                            updateImputeOutliersFeatureTbl = function(input, output, session){
                              options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
                              t <- NULL
                              if(self$status$query(processName = "outliersDetected")){
                                feature <- input$si.imputeOutliersFeature
                                dfData <- self$filteredMetadata$rawData %>%
                                  dplyr::right_join(self$normalizedData$rawData %>%
                                                      dplyr::select(c("Sample Name", !!feature)),
                                                    by = "Sample Name")

                                dfOutlier <- self$outliers$featureOutlier(feature = feature)

                                t <- dfData %>%
                                  dplyr::mutate_if(is.numeric, round, 3) %>%
                                  DT::datatable(
                                    options = list(
                                      scrollX = TRUE,
                                      scrollY = '350px',
                                      paging = FALSE,
                                      dom = "Blfrtip",
                                      buttons = list(list(
                                        extend = 'csv',
                                        filename = self$fileName$bluntFileName("OutliersData"),
                                        text = "Download"
                                      ))#buttons
                                    )#options
                                  )#DT::datatable

                                featureOutlier <- dfOutlier %>%
                                  dplyr::mutate_if(is.numeric, round, 3)
                                if (nrow(featureOutlier)>0){
                                  t <- DT::formatStyle(t,
                                                       feature,
                                                       backgroundColor = styleEqual(dfData %>%
                                                                                      dplyr::select(!!feature) %>%
                                                                                      dplyr::slice(featureOutlier[["measurement"]]) %>%
                                                                                      unlist() %>%
                                                                                      as.numeric() %>%
                                                                                      round(digits = 3),
                                                                                    featureOutlier[["color"]])

                                  )#t
                                }#if
                              }#if
                              output$tbl.outliersImputeFeature <- DT::renderDataTable(t)
                            }, #function

                            #' @description
                            #' Updates the numerical loq statistics analysis table
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$updateImputeOutliersStatisticsTbl(input, output, session)
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
                                          filename = self$fileName$bluntFileName("outliersStatistics"),
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
                            #' @examples
                            #' x$updateImputeOutliersDetailTbl(input, output, session)
                            updateImputeOutliersDetailTbl = function(input, output, session){
                              options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
                              t <- NULL
                              if(self$status$query(processName = "outliersDetected")){
                                dfData <- self$filteredMetadata$rawData %>%
                                  dplyr::right_join(self$normalizedData$rawData, by = "Sample Name")
                                dfOutlier <- self$outliers$outliers
                                idx <- dfOutlier[["measurement"]][!duplicated(dfOutlier[["measurement"]])]
                                t <- dfData %>%
                                  dplyr::slice(idx) %>%
                                  dplyr::mutate_if(is.numeric, round, 3) %>%
                                  DT::datatable(
                                    options = list(
                                      scrollX = TRUE,
                                      scrollY = '350px',
                                      paging = FALSE,
                                      dom = "Blfrtip",
                                      buttons = list(list(
                                        extend = 'csv',
                                        filename = self$fileName$bluntFileName("OutliersDetail"),
                                        text = "Download"
                                      ))#buttons
                                    )#options
                                  )#DT::datatable
                                for (featureName in self$normalizedData$numericFeatureNames){
                                  featureOutlier <- dfOutlier %>%
                                    dplyr::filter(feature == featureName) %>%
                                    dplyr::mutate_if(is.numeric, round, 3)
                                  if (nrow(featureOutlier)>0){
                                    t <- DT::formatStyle(t,
                                                         featureName,
                                                         backgroundColor = styleEqual(dfData %>%
                                                                                        dplyr::select(!!featureName) %>%
                                                                                        dplyr::slice(featureOutlier[["measurement"]]) %>%
                                                                                        unlist() %>%
                                                                                        as.numeric() %>%
                                                                                        round(digits = 3),
                                                                                      featureOutlier[["color"]])

                                    )#t
                                  }#if
                                }#for
                              }#if
                              output$tbl.outliersImputeDetail <- DT::renderDataTable(t)
                            }, #function

                            #' @description
                            #' Updates the numerical outliers data table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$updateImputeOutliersDataTbl(input, output, session)
                            updateImputeOutliersDataTbl = function(input, output, session){
                              options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
                              t <- NULL
                              if(self$status$query(processName = "outliersDetected")){
                                dfData <- self$filteredMetadata$rawData %>%
                                  dplyr::right_join(self$normalizedData$rawData, by = "Sample Name")
                                dfOutlier <- self$outliers$outliers
                                # idx <- dfOutlier[["measurement"]][!duplicated(dfOutlier[["measurement"]])]
                                t <- dfData %>%
                                  dplyr::mutate_if(is.numeric, round, 3) %>%
                                  DT::datatable(
                                    options = list(
                                      scrollX = TRUE,
                                      scrollY = '350px',
                                      paging = FALSE,
                                      dom = "Blfrtip",
                                      buttons = list(list(
                                        extend = 'csv',
                                        filename = self$fileName$bluntFileName("OutliersData"),
                                        text = "Download"
                                      ))#buttons
                                    )#options
                                  )#DT::datatable
                                for (featureName in self$normalizedData$numericFeatureNames){
                                  featureOutlier <- dfOutlier %>%
                                    dplyr::filter(feature == featureName) %>%
                                    dplyr::mutate_if(is.numeric, round, 3)
                                  if (nrow(featureOutlier)>0){
                                    t <- DT::formatStyle(t,
                                                         featureName,
                                                         backgroundColor = styleEqual(dfData %>%
                                                                                        dplyr::select(!!featureName) %>%
                                                                                        dplyr::slice(featureOutlier[["measurement"]]) %>%
                                                                                        unlist() %>%
                                                                                        as.numeric() %>%
                                                                                        round(digits = 3),
                                                                                      featureOutlier[["color"]])

                                    )#t
                                  }#if
                                }#for
                              }#if
                              output$tbl.outliersImputeData <- DT::renderDataTable(t)
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
                            #' @examples
                            #' x$updateImputeMutateFeature(input, output, session)
                            updateImputeMutateFeature = function(input, output, session){
                              if(self$status$query(processName = "outliersDetected")){
                                shiny::updateSelectInput(session,
                                                         "si.imputeMutateFeature",
                                                         choices = self$normalizedData$numericFeatureNames,
                                                         selected = self$normalizedData$numericFeatureNames[1]
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
                            #' @examples
                            #' x$updateImputeMutateMethod(input, output, session)
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
                            #' @examples
                            #' x$updateImputeMutateNNeighbors(input, output, session)
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
                            #' @examples
                            #' x$updateImputeMutatePredFrac(input, output, session)
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
                            #' @examples
                            #' x$updateImputeMutateOutfluxThr(input, output, session)
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
                            #' @examples
                            #' x$updateImputeMutateSeed(input, output, session)
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
                            #' @examples
                            #' x$updateImputeMutateIterations(input, output, session)
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
                            #' @examples
                            #' x$updateImputeMutateGui(input, output, session)
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
                            #' @examples
                            #' x$resetImputeMutateGui(input, output, session)
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
                            #' @examples
                            #' x$imputeMutateMutate(input, output, session)
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

                                private$.imputer$analyzeImputationSites(data_df = self$normalizedData$numericData())

                                private$.imputer$detectPredictors(data_df = self$normalizedData$numericData())

                                progress <- shiny::Progress$new(session, min = 0, max = 1)
                                progress$set(message = "Mutate imputation sites", value = 0)
                                on.exit(progress$close())

                                name  <- as.name("Sample Name")
                                private$.imputedData$setRawData <- self$normalizedData$numericData() %>%
                                  self$imputer$handleImputationSites(progress) %>%
                                  tibble::add_column(!! name := self$normalizedData$rawData %>%
                                                       dplyr::select(!!name) %>%
                                                       unlist() %>%
                                                       as.character()) %>%
                                  dplyr::select(c(!!name, self$normalizedData$numericFeatureNames))

                                if(self$imputer$success){
                                  private$.cleanedData$setRawData <- self$imputedData$numericData() %>%
                                    self$normalizer$rescale_data() %>%
                                    self$transformator$reverseMutateData() %>%
                                    tibble::add_column(!! name := self$imputedData$rawData %>%
                                                         dplyr::select(!!name) %>%
                                                         unlist() %>%
                                                         as.character()) %>%
                                    dplyr::select(c(!!name, self$imputedData$numericFeatureNames))

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
                            #' @examples
                            #' x$updateImputeFluxGraphic(input, output, session)
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
                            #' @examples
                            #' x$updateImputeMutateGraphic(input, output, session)
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
                            #' @examples
                            #' x$updateImputeMutateStatisticsTbl(input, output, session)
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
                                          filename = self$fileName$bluntFileName("imputationSiteStatistics"),
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
                            #' @examples
                            #' x$updateImputeMutateDistributionTbl(input, output, session)
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
                                          filename = self$fileName$bluntFileName("imputationSiteDistribution"),
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
                            #' @examples
                            #' x$updateImputeMutateFeatureDetailGraphic(input, output, session)
                            updateImputeMutateFeatureDetailGraphic = function(input, output, session){
                              if(self$status$query(processName = "imputed")){
                                output$plt.imputeMutateFeatureDetail <- shiny::renderPlot(
                                  self$imputer$featurePlot(data = self$imputedData$numericData(),
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
                            #' @examples
                            #' x$updateImputeMutateFeatureDetailTbl(input, output, session)
                            updateImputeMutateFeatureDetailTbl = function(input, output, session){
                              if (self$status$query(processName = "imputed")){
                                output$tbl.imputeMutateFeatureDetail <- DT::renderDataTable(
                                  self$filteredMetadata$rawData %>%
                                    dplyr::right_join(self$imputedData$rawData %>%
                                                        dplyr::select(c("Sample Name", input$si.imputeMutateFeature)),
                                                      by = "Sample Name") %>%
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
                                          filename = self$fileName$bluntFileName("imputationFeatureDetails"),
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
                            #' @examples
                            #' x$updateImputeMutateDetailTbl(input, output, session)
                            updateImputeMutateDetailTbl = function(input, output, session){
                              if(self$status$query(processName = "imputed")){
                                dfImputationSites <- self$imputer$imputationSites
                                idx <- dfImputationSites[["idx"]][!duplicated(dfImputationSites[["idx"]])]

                                output$tbl.imputeMutateDetail <- DT::renderDataTable(
                                  self$filteredMetadata$rawData %>%
                                    dplyr::right_join(self$imputedData$rawData, by = "Sample Name") %>%
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
                                          filename = self$fileName$bluntFileName("imputationDetail"),
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
                            #' @examples
                            #' x$updateImputeMutateDataTbl(input, output, session)
                            updateImputeMutateDataTbl = function(input, output, session){
                              if(self$status$query(processName = "imputed")){
                                output$tbl.imputeMutateData <- DT::renderDataTable(
                                  self$filteredMetadata$rawData %>%
                                    dplyr::right_join(self$imputedData$rawData, by = "Sample Name") %>%
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
                                          filename = self$fileName$bluntFileName("imputationData"),
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
                            #' #' @description
                            #' #' Calls the detect outliers routine of the instance variable outliers on the instance variable transformedData.
                            #' #' Updates the instance class status.
                            #' #' @param input
                            #' #' Pointer to shiny input
                            #' #' @param output
                            #' #' Pointer to shiny output
                            #' #' @param session
                            #' #' Pointer to shiny session
                            #' #' @examples
                            #' #' x$outliersDetect(input, output, session)
                            #' outliersDetect = function(input, output, session){
                            #'   if(self$status$query(processName = "naDetected")){
                            #'     self$transformedData$numericData() %>%
                            #'       private$.outliers$resetOutliersParameter()
                            #'     private$.status$update(processName = "outliersDetected", value = TRUE)
                            #'   }#if
                            #'   else{
                            #'     shiny::showNotification(paste("No NaNs detected. Please run detect NaN first."),type = "error", duration = 10)
                            #'   }#else
                            #' }, #function
                            #'
                            #' #' @description
                            #' #' Updates the plt.outliersDetectSummary graphic.
                            #' #' @param input
                            #' #' Pointer to shiny input
                            #' #' @param output
                            #' #' Pointer to shiny output
                            #' #' @param session
                            #' #' Pointer to shiny session
                            #' #' @examples
                            #' #' x$updateOutliersDetectGraphic(input, output, session)
                            #' updateOutliersDetectGraphic = function(input, output, session){
                            #'   if(self$status$query(processName = "outliersDetected")){
                            #'     output$plt.outliersDetectSummary <- shiny::renderPlot(
                            #'       self$outliers$plotOutliersDistribution()
                            #'     )#output
                            #'   }#if
                            #'   else{
                            #'     output$plt.outliersDetectSummary <- shiny::renderPlot(NULL)
                            #'   }#else
                            #' }, #function
                            #'
                            #' #' @description
                            #' #' Updates the tbl.outliersDetectStatistics table.
                            #' #' @param input
                            #' #' Pointer to shiny input
                            #' #' @param output
                            #' #' Pointer to shiny output
                            #' #' @param session
                            #' #' Pointer to shiny session
                            #' #' @examples
                            #' #' x$updateOutliersDetectStatisticsTbl(input, output, session)
                            #' updateOutliersDetectStatisticsTbl = function(input, output, session){
                            #'   if(self$status$query(processName = "outliersDetected")){
                            #'     output$tbl.outliersDetectStatistics <- DT::renderDataTable(
                            #'       self$outliers$outliersStatistics %>%
                            #'         DT::datatable(
                            #'           extensions = "Buttons",
                            #'           options = list(
                            #'             scrollX = TRUE,
                            #'             scrollY = '350px',
                            #'             paging = FALSE,
                            #'             dom = "Blfrtip",
                            #'             buttons = list(list(
                            #'               extend = 'csv',
                            #'               filename = self$fileName$bluntFileName("outliersDetectionStatistics"),
                            #'               text = "Download"
                            #'             ))#buttons
                            #'           )#options
                            #'         )#DT::datatable
                            #'     )#output
                            #'   }#if
                            #'   else{
                            #'     output$tbl.outliersDetectStatistics <- DT::renderDataTable(NULL)
                            #'   }#else
                            #' }, #function
                            #'
                            #' #' @description
                            #' #' Updates the tbl.outliersDetectDetail table.
                            #' #' @param input
                            #' #' Pointer to shiny input
                            #' #' @param output
                            #' #' Pointer to shiny output
                            #' #' @param session
                            #' #' Pointer to shiny session
                            #' #' @examples
                            #' #' x$updateOutliersDetectDetailTbl(input, output, session)
                            #' updateOutliersDetectDetailTbl = function(input, output, session){
                            #'   if(self$status$query(processName = "outliersDetected")){
                            #'     dfOutlier <- self$outliers$outliers
                            #'     idx <- dfOutlier[["measurement"]][!duplicated(dfOutlier[["measurement"]])]
                            #'     output$tbl.outliersDetectDetail <- DT::renderDataTable(
                            #'       self$filteredMetadata$rawData %>%
                            #'         dplyr::right_join(self$transformedData$rawData, by = "Sample Name") %>%
                            #'         dplyr::slice(idx) %>%
                            #'         format.data.frame(scientific = TRUE, digits = 4) %>%
                            #'         DT::datatable(
                            #'           extensions = "Buttons",
                            #'           options = list(
                            #'             scrollX = TRUE,
                            #'             scrollY = '350px',
                            #'             paging = FALSE,
                            #'             dom = "Blfrtip",
                            #'             buttons = list(list(
                            #'               extend = 'csv',
                            #'               filename = self$fileName$bluntFileName("outliersDetectionDetail"),
                            #'               text = "Download"
                            #'             ))#buttons
                            #'           )#options
                            #'         )#DT::datatable
                            #'     )#output
                            #'   }#if
                            #'   else{
                            #'     output$tbl.outliersDetectDetail <- DT::renderDataTable(NULL)
                            #'   }#else
                            #' }, #function
                            #'
                            #' #' @description
                            #' #' Updates the tbl.outliersDetectData table.
                            #' #' @param input
                            #' #' Pointer to shiny input
                            #' #' @param output
                            #' #' Pointer to shiny output
                            #' #' @param session
                            #' #' Pointer to shiny session
                            #' #' @examples
                            #' #' x$updateOutliersDetectDataTbl(input, output, session)
                            #' updateOutliersDetectDataTbl = function(input, output, session){
                            #'   if(self$status$query(processName = "outliersDetected")){
                            #'     output$tbl.outliersDetectData <- DT::renderDataTable(
                            #'       self$filteredMetadata$rawData %>%
                            #'         dplyr::right_join(self$transformedData$rawData, by = "Sample Name") %>%
                            #'         format.data.frame(scientific = TRUE, digits = 4) %>%
                            #'         DT::datatable(
                            #'           extensions = "Buttons",
                            #'           options = list(
                            #'             scrollX = TRUE,
                            #'             scrollY = '350px',
                            #'             paging = FALSE,
                            #'             dom = "Blfrtip",
                            #'             buttons = list(list(
                            #'               extend = 'csv',
                            #'               filename = self$fileName$bluntFileName("outliersDetectionData"),
                            #'               text = "Download"
                            #'             ))#buttons
                            #'           )#options
                            #'         )#DT::datatable
                            #'     )#output
                            #'   }#if
                            #'   else{
                            #'     output$tbl.outliersDetectData <- DT::renderDataTable(NULL)
                            #'   }#else
                            #' }, #function

                            #' ############################
                            #' # outlier mutate functions #
                            #' ############################
                            #' #' @description
                            #' #' Updates the si.outliersMutateFeature shiny widget.
                            #' #' @param input
                            #' #' Pointer to shiny input
                            #' #' @param output
                            #' #' Pointer to shiny output
                            #' #' @param session
                            #' #' Pointer to shiny session
                            #' #' @examples
                            #' #' x$updateOutliersMutateFeature(input, output, session)
                            #' updateOutliersMutateFeature = function(input, output, session){
                            #'   if(self$status$query(processName = "outliersDetected")){
                            #'     shiny::updateSelectInput(session,
                            #'                              "si.outliersMutateFeature",
                            #'                              choices = self$outliers$outliersParameter[["features"]],
                            #'                              selected = self$outliers$outliersParameter[["features"]][1]
                            #'     )#input
                            #'   }#if
                            #' }, #function
                            #'
                            #' #' @description
                            #' #' Updates the si.outliersMutateMethod shiny widget.
                            #' #' @param input
                            #' #' Pointer to shiny input
                            #' #' @param output
                            #' #' Pointer to shiny output
                            #' #' @param session
                            #' #' Pointer to shiny session
                            #' #' @examples
                            #' #' x$updateOutliersMutateMethod(input, output, session)
                            #' updateOutliersMutateMethod = function(input, output, session){
                            #'   if(self$status$query(processName = "outliersDetected")){
                            #'     shiny::updateSelectInput(session,
                            #'                              "si.outliersMutateMethod",
                            #'                              choices = self$outliers$cleaningAgentAlphabet,
                            #'                              selected = self$outliers$cleaningAgent
                            #'     )#input
                            #'   }#if
                            #' }, #function
                            #'
                            #' #' @description
                            #' #' Updates the ni.outliersMutateSeed shiny widget.
                            #' #' @param input
                            #' #' Pointer to shiny input
                            #' #' @param output
                            #' #' Pointer to shiny output
                            #' #' @param session
                            #' #' Pointer to shiny session
                            #' #' @examples
                            #' #' x$updateOutliersMutateSeed(input, output, session)
                            #' updateOutliersMutateSeed = function(input, output, session){
                            #'   if(self$status$query(processName = "outliersDetected")){
                            #'     shiny::updateNumericInput(session,
                            #'                               "ni.outliersMutateSeed",
                            #'                               value = self$outliers$seed)
                            #'   }#if
                            #' }, #function
                            #'
                            #' #' @description
                            #' #' Updates the ni.outliersMutateIterations shiny widget.
                            #' #' @param input
                            #' #' Pointer to shiny input
                            #' #' @param output
                            #' #' Pointer to shiny output
                            #' #' @param session
                            #' #' Pointer to shiny session
                            #' #' @examples
                            #' #' x$updateOutliersMutateIterations(input, output, session)
                            #' updateOutliersMutateIterations = function(input, output, session){
                            #'   if(self$status$query(processName = "outliersDetected")){
                            #'     shiny::updateNumericInput(session,
                            #'                               "ni.outliersMutateIterations",
                            #'                               value = self$outliers$iterations)
                            #'   }#if
                            #' }, #function
                            #'
                            #' #' @description
                            #' #' Updates the gui.
                            #' #' @param input
                            #' #' Pointer to shiny input
                            #' #' @param output
                            #' #' Pointer to shiny output
                            #' #' @param session
                            #' #' Pointer to shiny session
                            #' #' @examples
                            #' #' x$updateOutliersMutateGui(input, output, session)
                            #' updateOutliersMutateGui = function(input, output, session){
                            #'   if(self$status$query(processName = "outliersDetected")){
                            #'     self$updateOutliersMutateFeature(input, output, session)
                            #'     self$updateOutliersMutateMethod(input, output, session)
                            #'     self$updateOutliersMutateSeed(input, output, session)
                            #'     self$updateOutliersMutateIterations(input, output, session)
                            #'   }#if
                            #' }, #function
                            #'
                            #' #' @description
                            #' #' Calls the mutate outliers routine of the instance variable outliers on the instance variable transformedData.
                            #' #' Updates the instance class status.
                            #' #' @param input
                            #' #' Pointer to shiny input
                            #' #' @param output
                            #' #' Pointer to shiny output
                            #' #' @param session
                            #' #' Pointer to shiny session
                            #' #' @examples
                            #' #' x$outliersMutate(input, output, session)
                            #' outliersMutate = function(input, output, session){
                            #'   if(self$status$query(processName = "outliersDetected")){
                            #'     private$.outliers$setCleaningAgent <- input$si.outliersMutateMethod
                            #'     private$.outliers$setSeed <- input$ni.outliersMutateSeed
                            #'     private$.outliers$setIterations <- input$ni.outliersMutateIterations
                            #'
                            #'     progress <- shiny::Progress$new(session, min = 1, max = 2 * length(self$imputedData$numericFeatureNames))
                            #'     progress$set(message = "Mutate Outliers", value = 0)
                            #'     on.exit(progress$close())
                            #'
                            #'     name  <- as.name("Sample Name")
                            #'     private$.revisedData$setRawData <- self$imputedData$numericData() %>%
                            #'       self$outliers$handleOutliers(progress = progress) %>%
                            #'       tibble::add_column(!! name := self$imputedData$rawData %>%
                            #'                            dplyr::select(!!name) %>%
                            #'                            unlist() %>%
                            #'                            as.character()) %>%
                            #'       dplyr::select(c(!!name, self$imputedData$numericFeatureNames))
                            #'     private$.cleanedData$setRawData <- self$revisedData$numericData() %>%
                            #'       self$model$rescaleData() %>%
                            #'       self$transformator$reverseMutateData() %>%
                            #'       tibble::add_column(!! name := self$imputedData$rawData %>%
                            #'                            dplyr::select(!!name) %>%
                            #'                            unlist() %>%
                            #'                            as.character()) %>%
                            #'       dplyr::select(c(!!name, self$imputedData$numericFeatureNames))
                            #'     private$.status$update(processName = "outliersMutated", value = TRUE)
                            #'   }#if
                            #'   else{
                            #'     shiny::showNotification(paste("No outliers detected. Please detect outliers first."),type = "error", duration = 10)
                            #'   }#else
                            #' }, #function
                            #'
                            #' #' @description
                            #' #' Updates the plt.outliersMutateFeatureDetail graphic.
                            #' #' @param input
                            #' #' Pointer to shiny input
                            #' #' @param output
                            #' #' Pointer to shiny output
                            #' #' @param session
                            #' #' Pointer to shiny session
                            #' #' @examples
                            #' #' x$updateOutliersMutateFeatureDetailGraphic(input, output, session)
                            #' updateOutliersMutateFeatureDetailGraphic = function(input, output, session){
                            #'   if(self$status$query(processName = "outliersMutated")){
                            #'     output$plt.outliersMutateFeatureDetail <- shiny::renderPlot(
                            #'       self$outliers$featurePlot(data = self$revisedData$numericData(),
                            #'                                 feature = input$si.outliersMutateFeature)
                            #'     )#output
                            #'   }#if
                            #'   else{
                            #'     output$plt.outliersMutateFeatureDetail <- shiny::renderPlot(NULL)
                            #'   }#else
                            #' }, #function
                            #'
                            #' #' @description
                            #' #' Updates the tbl.outliersMutateFeatureDetail table.
                            #' #' @param input
                            #' #' Pointer to shiny input
                            #' #' @param output
                            #' #' Pointer to shiny output
                            #' #' @param session
                            #' #' Pointer to shiny session
                            #' #' @examples
                            #' #' x$updateOutliersMutateFeatureDetailTbl(input, output, session)
                            #' updateOutliersMutateFeatureDetailTbl = function(input, output, session){
                            #'   if (self$status$query(processName = "outliersMutated")){
                            #'     output$tbl.outliersMutateFeatureDetail <- DT::renderDataTable(
                            #'       self$filteredMetadata$rawData %>%
                            #'         dplyr::right_join(self$revisedData$rawData %>%
                            #'                             dplyr::select(c("Sample Name", input$si.outliersMutateFeature))  ,
                            #'                           by = "Sample Name") %>%
                            #'         dplyr::slice(self$outliers$outliersIdxByFeature(feature = input$si.outliersMutateFeature)) %>%
                            #'         format.data.frame(scientific = TRUE, digits = 4) %>%
                            #'         DT::datatable(
                            #'           extensions = "Buttons",
                            #'           options = list(
                            #'             scrollX = TRUE,
                            #'             scrollY = '350px',
                            #'             paging = FALSE,
                            #'             dom = "Blfrtip",
                            #'             buttons = list(list(
                            #'               extend = 'csv',
                            #'               filename = self$fileName$bluntFileName("outliersMutationDetails"),
                            #'               text = "Download"
                            #'             ))#buttons
                            #'           )#options
                            #'         )#DT::datatable
                            #'     )#output
                            #'   }#if
                            #'   else{
                            #'     output$tbl.outliersMutateFeatureDetail <- DT::renderDataTable(NULL)
                            #'   }#else
                            #' }, #function
                            #'
                            #' #' @description
                            #' #' Updates the tbl.outliersMutateDetail table.
                            #' #' @param input
                            #' #' Pointer to shiny input
                            #' #' @param output
                            #' #' Pointer to shiny output
                            #' #' @param session
                            #' #' Pointer to shiny session
                            #' #' @examples
                            #' #' x$updateOutliersMutateDetailTbl(input, output, session)
                            #' updateOutliersMutateDetailTbl = function(input, output, session){
                            #'   if(self$status$query(processName = "outliersMutated")){
                            #'     dfOutlier <- self$outliers$outliers
                            #'     idx <- dfOutlier[["measurement"]][!duplicated(dfOutlier[["measurement"]])]
                            #'     output$tbl.outliersMutateDetail <- DT::renderDataTable(
                            #'       self$filteredMetadata$rawData %>%
                            #'         dplyr::right_join(self$revisedData$rawData, by = "Sample Name") %>%
                            #'         dplyr::slice(idx) %>%
                            #'         format.data.frame(scientific = TRUE, digits = 4) %>%
                            #'         DT::datatable(
                            #'           extensions = "Buttons",
                            #'           options = list(
                            #'             scrollX = TRUE,
                            #'             scrollY = '350px',
                            #'             paging = FALSE,
                            #'             dom = "Blfrtip",
                            #'             buttons = list(list(
                            #'               extend = 'csv',
                            #'               filename = self$fileName$bluntFileName("outliersMutationDetail"),
                            #'               text = "Download"
                            #'             ))#buttons
                            #'           )#options
                            #'         )#DT::datatable
                            #'     )#output
                            #'   }#if
                            #'   else{
                            #'     output$tbl.outliersMutateDetail <- DT::renderDataTable(NULL)
                            #'   }#else
                            #' }, #function
                            #'
                            #' #' @description
                            #' #' Updates the tbl.outliersMutateData table.
                            #' #' @param input
                            #' #' Pointer to shiny input
                            #' #' @param output
                            #' #' Pointer to shiny output
                            #' #' @param session
                            #' #' Pointer to shiny session
                            #' #' @examples
                            #' #' x$updateOutliersMutateDataTbl(input, output, session)
                            #' updateOutliersMutateDataTbl = function(input, output, session){
                            #'   if(self$status$query(processName = "outliersMutated")){
                            #'     output$tbl.outliersMutateData <- DT::renderDataTable(
                            #'       self$filteredMetadata$rawData %>%
                            #'         dplyr::right_join(self$revisedData$rawData, by = "Sample Name") %>%
                            #'         format.data.frame(scientific = TRUE, digits = 4) %>%
                            #'         DT::datatable(
                            #'           extensions = "Buttons",
                            #'           options = list(
                            #'             scrollX = TRUE,
                            #'             scrollY = '350px',
                            #'             paging = FALSE,
                            #'             dom = "Blfrtip",
                            #'             buttons = list(list(
                            #'               extend = 'csv',
                            #'               filename = self$fileName$bluntFileName("outliersMutationData"),
                            #'               text = "Download"
                            #'             ))#output
                            #'           )#options
                            #'         )#DT::datatabe
                            #'     )#output
                            #'   }#if
                            #'   else{
                            #'     output$tbl.outliersMutateData <- DT::renderDataTable(NULL)
                            #'   } #else
                            #' }, #function


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
                            #' @examples
                            #' x$validate(input, output, session)
                            validate = function(input, output, session){
                              if(self$status$query(processName = "imputed")){
                                progress <- shiny::Progress$new(session, min = 0, max  = 1.0)
                                progress$set(message = "Validate imputation", value = 0)
                                on.exit(progress$close())
                                private$.validator$validate(org = self$filteredData$numericData(),
                                                            imp = self$cleanedData$numericData() %>%
                                                              dplyr::select_if(function(x){!all(is.na(x))}),
                                                            progress = progress)
                                private$.status$update(processName = "validated", value = TRUE)
                                private$.corrValidator$validate(org_df = self$normalizedData$numericData(),
                                                                imp_df = self$imputedData$numericData())
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
                            #' @examples
                            #' x$updateAnalysisValidationGui(input, output, session)
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
                            #' @examples
                            #' x$updateAnalysisValidationGraphic(input, output, session)
                            updateAnalysisValidationGraphic = function(input, output, session){
                              if(self$status$query(processName = "validated")){
                                output$plt.analysisValidationFeature <- shiny::renderPlot(
                                  self$validator$featurePlot(org_df = self$filteredData$numericData(),
                                                             imp_df = self$cleanedData$numericData(),
                                                             lloq = self$loq$featureLloq(feature = input$si.analysisValidationFeature),
                                                             uloq = self$loq$featureUloq(feature = input$si.analysisValidationFeature),
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
                            #' @examples
                            #' x$updateAnalysisValidationTestTbl(input, output, session)
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
                                          filename = self$fileName$bluntFileName("validationTests"),
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
                            #' @examples
                            #' x$updateCentralMomentsOrgTbl(input, output, session)
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
                                          filename = self$fileName$bluntFileName("rawCentralMoments"),
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
                            #' @examples
                            #' x$updateCentralMomentsImpTbl(input, output, session)
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
                                          filename = self$fileName$bluntFileName("impCentralMoments"),
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
                            #' @examples
                            #' x$updateCentralMomentsDeltaTbl(input, output, session)
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
                                          filename = self$fileName$bluntFileName("deltaCentralMoments"),
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
                            #' @examples
                            #' x$updateCorrelationValidationScatterGraphic(input, output, session)
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
                            #' @examples
                            #' x$updateCorrelationValidationBoxPlotGraphic(input, output, session)
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
                            #' @examples
                            #' x$updateCorrelationValidationDeviationTbl(input, output, session)
                            updateCorrelationValidationDeviationTbl = function(input, output, session){
                              if(self$status$query(processName = "validated")){
                                output$tbl.correlationValidationDeviation <- DT::renderDataTable(
                                  self$corrValidator$summary() %>%
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
                                          filename = self$fileName$bluntFileName("correlationValidationDeviation"),
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
                            #' @examples
                            #' x$updateCorrelationValidationDataTbl(input, output, session)
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
                                          filename = self$fileName$bluntFileName("correlationValidationData"),
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


#'
#'                             #########################
#'                             # correlation functions #
#'                             #########################
#'                             #' @description
#'                             #' Calls the correlate routine of the instance variable correlator on the instance variable revisedData.
#'                             #' Updates the instance class status.
#'                             #' @param input
#'                             #' Pointer to shiny input
#'                             #' @param output
#'                             #' Pointer to shiny output
#'                             #' @param session
#'                             #' Pointer to shiny session
#'                             #' @examples
#'                             #' x$correlate(input, output, session)
#'                             correlate = function(input, output, session){
#'                               if(self$status$query(processName = "validated")){
#'                                 progress <- shiny::Progress$new(session, min = 1, max  = 3 * length(self$imputedData$numericFeatureNames) ** 2)
#'                                 progress$set(message = "Calculate Correlation", value = 1)
#'                                 on.exit(progress$close())
#'                                 # self$cleanedData$numericData() %>%
#'                                 self$cleanedData$numericData() %>%
#'                                   dplyr::select_if(function(x){!all(is.na(x))}) %>%
#'                                   private$.correlator$resetCorrelator(progress = progress)
#'                                 private$.status$update(processName = "correlated", value = TRUE)
#'                               }#if
#'                               else{
#'                                 shiny::showNotification(paste("No outliers revised. Please revise outliers first."),type = "error", duration = 10)
#'                               }#else
#'                             }, #function
#'
#'                             #' @description
#'                             #' Updates the tbl.correlationMatrixR table.
#'                             #' @param input
#'                             #' Pointer to shiny input
#'                             #' @param output
#'                             #' Pointer to shiny output
#'                             #' @param session
#'                             #' Pointer to shiny session
#'                             #' @examples
#'                             #' x$updateCorrelationMatrixRTbl(input, output, session)
#'                             updateCorrelationMatrixRTbl = function(input, output, session){
#'                               if(self$status$query(processName = "correlated")){
#'                                 output$tbl.correlationMatrixR <- DT::renderDataTable(
#'                                   self$correlator$printRTbl() %>%
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
#'                                           filename = self$fileName$bluntFileName("CorrelationMatrix_R"),
#'                                           text = "Download"
#'                                         )), #buttons
#'                                         autoWidth = TRUE,
#'                                         columnDefs = list(list(width = '50px', targets = "_all"))
#'                                       )#options
#'                                     )#DT::datatable
#'                                 )#output
#'                               }#if
#'                               else{
#'                                 output$tbl.correlationMatrixR <- DT::renderDataTable(NULL)
#'                               }#else
#'                             }, #function
#'
#'                             #' @description
#'                             #' Updates the tbl.correlationMatrixPPearson table.
#'                             #' @param input
#'                             #' Pointer to shiny input
#'                             #' @param output
#'                             #' Pointer to shiny output
#'                             #' @param session
#'                             #' Pointer to shiny session
#'                             #' @examples
#'                             #' x$updateCorrelationMatrixPPearsonTbl(input, output, session)
#'                             updateCorrelationMatrixPPearsonTbl = function(input, output, session){
#'                               if(self$status$query(processName = "correlated")){
#'                                 output$tbl.correlationMatrixPPearson <- DT::renderDataTable(
#'                                   self$correlator$printPPearsonTbl() %>%
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
#'                                           filename = self$fileName$bluntFileName("CorrelationMatrix_P_Pearson"),
#'                                           text = "Download"
#'                                         )),#buttons
#'                                         autoWidth = TRUE,
#'                                         columnDefs = list(list(width = '50px', targets = "_all"))
#'                                       )#options
#'                                     )#DT::Datatable
#'                                 )#output
#'                               }#if
#'                               else{
#'                                 output$tbl.correlationMatrixPPearson <- DT::renderDataTable(NULL)
#'                               }#else
#'                             }, #function
#'
#'                             #' @description
#'                             #' Updates the tbl.correlationMatrixTau table.
#'                             #' @param input
#'                             #' Pointer to shiny input
#'                             #' @param output
#'                             #' Pointer to shiny output
#'                             #' @param session
#'                             #' Pointer to shiny session
#'                             #' @examples
#'                             #' x$updateCorrelationMatrixTauTbl(input, output, session)
#'                             updateCorrelationMatrixTauTbl = function(input, output, session){
#'                               if(self$status$query(processName = "correlated")){
#'                                 output$tbl.correlationMatrixTau <- DT::renderDataTable(
#'                                   self$correlator$printTauTbl() %>%
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
#'                                           filename = self$fileName$bluntFileName("CorrelationMatrix_Tau"),
#'                                           text = "Download"
#'                                         )),#buttons
#'                                         autoWidth = TRUE,
#'                                         columnDefs = list(list(width = '50px', targets = "_all"))
#'                                       )#options
#'                                     )#DT::datatable
#'                                 )#output
#'                               }#if
#'                               else{
#'                                 output$tbl.correlationMatrixTau <- DT::renderDataTable(NULL)
#'                               }#else
#'                             }, #function
#'
#'                             #' @description
#'                             #' Updates the tbl.correlationMatrixPKendall table.
#'                             #' @param input
#'                             #' Pointer to shiny input
#'                             #' @param output
#'                             #' Pointer to shiny output
#'                             #' @param session
#'                             #' Pointer to shiny session
#'                             #' @examples
#'                             #' x$updateCorrelationMatrixPKendallTbl(input, output, session)
#'                             updateCorrelationMatrixPKendallTbl = function(input, output, session){
#'                               if(self$status$query(processName = "correlated")){
#'                                 output$tbl.correlationMatrixPKendall <- DT::renderDataTable(
#'                                   self$correlator$printPKendallTbl() %>%
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
#'                                           filename = self$fileName$bluntFileName("CorrelationMatrix_P_Kendall"),
#'                                           text = "Download"
#'                                         )), #buttons
#'                                         autoWidth = TRUE,
#'                                         columnDefs = list(list(width = '50px', targets = "_all"))
#'                                       )#options
#'                                     )#DT::datatable
#'                                 )#output
#'                               }#if
#'                               else{
#'                                 output$tbl.correlationMatrixPKendall <- DT::renderDataTable(NULL)
#'                               }#else
#'                             }, #function
#'
#'                             #' @description
#'                             #' Updates the tbl.correlationMatrixRho table.
#'                             #' @param input
#'                             #' Pointer to shiny input
#'                             #' @param output
#'                             #' Pointer to shiny output
#'                             #' @param session
#'                             #' Pointer to shiny session
#'                             #' @examples
#'                             #' x$updateCorrelationMatrixRhoTbl(input, output, session)
#'                             updateCorrelationMatrixRhoTbl = function(input, output, session){
#'                               if(self$status$query(processName = "correlated")){
#'                                 output$tbl.correlationMatrixRho <- DT::renderDataTable(
#'                                   self$correlator$printRhoTbl() %>%
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
#'                                           filename = self$fileName$bluntFileName("CorrelationMatrix_Rho"),
#'                                           text = "Download"
#'                                         )), #buttons
#'                                         autoWidth = TRUE,
#'                                         columnDefs = list(list(width = '50px', targets = "_all"))
#'                                       )#options
#'                                     )#DT::Datatable
#'                                 )#output
#'                               }#if
#'                               else{
#'                                 output$tbl.correlationMatrixRho <- DT::renderDataTable(NULL)
#'                               }#else
#'                             }, #function
#'
#'                             #' @description
#'                             #' Updates the tbl.correlationMatrixPSpearman table.
#'                             #' @param input
#'                             #' Pointer to shiny input
#'                             #' @param output
#'                             #' Pointer to shiny output
#'                             #' @param session
#'                             #' Pointer to shiny session
#'                             #' @examples
#'                             #' x$updateCorrelationMatrixPSpearmanTbl(input, output, session)
#'                             updateCorrelationMatrixPSpearmanTbl = function(input, output, session){
#'                               if(self$status$query(processName = "correlated")){
#'                                 output$tbl.correlationMatrixPSpearman <- DT::renderDataTable(
#'                                   self$correlator$printPSpearmanTbl() %>%
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
#'                                           filename = self$fileName$bluntFileName("CorrelationMatrix_P_Spearman"),
#'                                           text = "Download"
#'                                         )), #buttons
#'                                         autoWidth = TRUE,
#'                                         columnDefs = list(list(width = '50px', targets = "_all"))
#'                                       )#options
#'                                     )#DT::datatable
#'                                 )#output
#'                               }#if
#'                               else{
#'                                 output$tbl.correlationMatrixPSpearman <- DT::renderDataTable(NULL)
#'                               }#else
#'                             }, #function
#'
#'                             ########################
#'                             # regression functions #
#'                             ########################
#'                             #' @description
#'                             #' Calls the regression routine of the instance variable regressor on the instance variable revisedData.
#'                             #' Updates the instance class status.
#'                             #' @param input
#'                             #' Pointer to shiny input
#'                             #' @param output
#'                             #' Pointer to shiny output
#'                             #' @param session
#'                             #' Pointer to shiny session
#'                             #' @examples
#'                             #' x$regression(input, output, session)
#'                             regression = function(input, output, session){
#'                               if(self$status$query(processName = "correlated")){
#'                                 progress <- shiny::Progress$new(session, min = 1, max = length(self$cleanedData$numericFeatureNames)**2)
#'                                 progress$set(message = "Calculate Regression", value = 1)
#'                                 on.exit(progress$close())
#'                                 # private$.regressor$resetRegressor(data = self$cleanedData$numericData(), progress = progress)
#'                                 private$.regressor$resetRegressor(data = self$cleanedData$numericData(), progress = progress)
#'                                 private$.status$update(processName = "regression", value = TRUE)
#'                               }#if
#'                               else{
#'                                 shiny::showNotification(paste("No outliers revised. Please revise outliers first."),type = "error", duration = 10)
#'                               }#else
#'                             }, #function
#'
#'                             #' @description
#'                             #' Updates the si.regressionAbs shiny widget.
#'                             #' @param input
#'                             #' Pointer to shiny input
#'                             #' @param output
#'                             #' Pointer to shiny output
#'                             #' @param session
#'                             #' Pointer to shiny session
#'                             #' @examples
#'                             #' x$updateRegressionAbscissa(input, output, session)
#'                             updateRegressionAbscissa = function(input, output, session){
#'                               if(self$status$query(processName = "regression")){
#'                                 shiny::updateSelectInput(session,
#'                                                          "si.regressionAbs",
#'                                                          choices = self$regressor$featureNames,
#'                                                          selected = self$regressor$featureNames[1]
#'                                 )#input
#'                               }#if
#'                             }, #function
#'
#'                             #' @description
#'                             #' Updates the si.regressionOrd shiny widget.
#'                             #' @param input
#'                             #' Pointer to shiny input
#'                             #' @param output
#'                             #' Pointer to shiny output
#'                             #' @param session
#'                             #' Pointer to shiny session
#'                             #' @examples
#'                             #' x$updateRegressionOrdinate(input, output, session)
#'                             updateRegressionOrdinate = function(input, output, session){
#'                               if(self$status$query(processName = "regression")){
#'                                 ordinateFeatureNames <- self$regressor$featureNames[-self$regressor$featureIdx(input$si.regressionAbs)]
#'                                 shiny::updateSelectInput(session,
#'                                                          "si.regressionOrd",
#'                                                          choices = ordinateFeatureNames,
#'                                                          selected = ordinateFeatureNames[1]
#'                                 )#input
#'                               }#if
#'                             }, #function
#'
#'                             #' @description
#'                             #' Updates the gui.
#'                             #' @param input
#'                             #' Pointer to shiny input
#'                             #' @param output
#'                             #' Pointer to shiny output
#'                             #' @param session
#'                             #' Pointer to shiny session
#'                             #' @examples
#'                             #' x$updateRegressionGui(input, output, session)
#'                             updateRegressionGui = function(input, output, session){
#'                               self$updateRegressionAbscissa(input, output, session)
#'                               self$updateRegressionOrdinate(input, output, session)
#'                             }, #function
#'
#'                             #' @description
#'                             #' Updates the plt.regressionFeature graphic.
#'                             #' @param input
#'                             #' Pointer to shiny input
#'                             #' @param output
#'                             #' Pointer to shiny output
#'                             #' @param session
#'                             #' Pointer to shiny session
#'                             #' @examples
#'                             #' x$updateRegressionGraphic(input, output, session)
#'                             updateRegressionGraphic = function(input, output, session){
#'                               if(self$status$query(processName = "regression")){
#'                                 output$plt.regressionFeature <- shiny::renderPlot(
#'                                   self$regressor$plotModel(data = self$cleanedData$numericData(),
#'                                                            abscissa = input$si.regressionAbs,
#'                                                            ordinate = input$si.regressionOrd)
#'                                 )#output
#'                               }#if
#'                               else{
#'                                 output$plt.regressionFeature <- shiny::renderPlot(NULL)
#'                               } #else
#'                             }, #function
#'
#'                             #' @description
#'                             #' Updates the tbl.regressionFeature table.
#'                             #' @param input
#'                             #' Pointer to shiny input
#'                             #' @param output
#'                             #' Pointer to shiny output
#'                             #' @param session
#'                             #' Pointer to shiny session
#'                             #' @examples
#'                             #' x$updateRegressionModelTbl(input, output, session)
#'                             updateRegressionModelTbl = function(input, output, session){
#'                               if(self$status$query(processName = "regression")){
#'                                 output$tbl.regressionFeature <- DT::renderDataTable(
#'                                   self$regressor$printModel(abscissa = input$si.regressionAbs, ordinate = input$si.regressionOrd) %>%
#'                                     # format.data.frame(scientific = TRUE, digits = 4) %>%
#'                                     DT::datatable(
#'                                       extensions = "Buttons",
#'                                       options = list(
#'                                         scrollX = TRUE,
#'                                         scrollY = '350px',
#'                                         paging = FALSE,
#'                                         dom = "Blfrtip",
#'                                         buttons = list(list(
#'                                           extend = 'csv',
#'                                           filename = self$fileName$bluntFileName("RegressionMatrix_Model"),
#'                                           text = "Download"
#'                                         )), #buttons
#'                                         autoWidth = TRUE,
#'                                         columnDefs = list(list(width = '50px', targets = "_all"))
#'                                       )#options
#'                                     )#DT::datatable
#'                                 )#output
#'                               }#if
#'                               else{
#'                                 output$tbl.regressionFeature <- DT::renderDataTable(NULL)
#'                               }#else
#'                             }, #function
#'
#'                             #' @description
#'                             #' Updates the tbl.regressionIntercept table.
#'                             #' @param input
#'                             #' Pointer to shiny input
#'                             #' @param output
#'                             #' Pointer to shiny output
#'                             #' @param session
#'                             #' Pointer to shiny session
#'                             #' @examples
#'                             #' x$updateRegressionInterceptTbl(input, output, session)
#'                             updateRegressionInterceptTbl = function(input, output, session){
#'                               if(self$status$query(processName = "regression")){
#'                                 output$tbl.regressionIntercept <- DT::renderDataTable(
#'                                   self$regressor$printInterceptTbl() %>%
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
#'                                           filename = self$fileName$bluntFileName("RegressionMatrix_Intercept"),
#'                                           text = "Download"
#'                                         )), #buttons
#'                                         autoWidth = TRUE,
#'                                         columnDefs = list(list(width = '50px', targets = "_all"))
#'                                       )#options
#'                                     )#DT::datatable
#'                                 )#output
#'                               }#if
#'                               else{
#'                                 output$tbl.regressionIntercept <- DT::renderDataTable(NULL)
#'                               }#else
#'                             }, #functions
#'
#'                             #' @description
#'                             #' Updates the tbl.regressionPIntercept table.
#'                             #' @param input
#'                             #' Pointer to shiny input
#'                             #' @param output
#'                             #' Pointer to shiny output
#'                             #' @param session
#'                             #' Pointer to shiny session
#'                             #' @examples
#'                             #' x$updateRegressionPInterceptTbl(input, output, session)
#'                             updateRegressionPInterceptTbl = function(input, output, session){
#'                               if(self$status$query(processName = "regression")){
#'                                 output$tbl.regressionPIntercept <- DT::renderDataTable(
#'                                   self$regressor$printPInterceptTbl() %>%
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
#'                                           filename = self$fileName$bluntFileName("RegressionMatrix_P_Intercept"),
#'                                           text = "Download"
#'                                         )), #buttons
#'                                         autoWidth = TRUE,
#'                                         columnDefs = list(list(width = '50px', targets = "_all"))
#'                                       )#options
#'                                     )#DT::datatable
#'                                 )#output
#'                               }#if
#'                               else{
#'                                 output$tbl.regressionPIntercept <- DT::renderDataTable(NULL)
#'                               }#else
#'                             }, #function
#'
#'                             #' @description
#'                             #' Updates the tbl.regressionSlope table.
#'                             #' @param input
#'                             #' Pointer to shiny input
#'                             #' @param output
#'                             #' Pointer to shiny output
#'                             #' @param session
#'                             #' Pointer to shiny session
#'                             #' @examples
#'                             #' x$updateRegressionSlopeTbl(input, output, session)
#'                             updateRegressionSlopeTbl = function(input, output, session){
#'                               if(self$status$query(processName = "regression")){
#'                                 output$tbl.regressionSlope <- DT::renderDataTable(
#'                                   self$regressor$printSlopeTbl() %>%
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
#'                                           filename = self$fileName$bluntFileName("RegressionMatrix_Slope"),
#'                                           text = "Download"
#'                                         )), #buttons
#'                                         autoWidth = TRUE,
#'                                         columnDefs = list(list(width = '50px', targets = "_all"))
#'                                       )#options
#'                                     )#DT::datatable
#'                                 )#output
#'                               }#if
#'                               else{
#'                                 output$tbl.regressionSlope <- DT::renderDataTable(NULL)
#'                               }#else
#'                             }, #function
#'
#'                             #' @description
#'                             #' Updates the tbl.regressionPSlope table.
#'                             #' @param input
#'                             #' Pointer to shiny input
#'                             #' @param output
#'                             #' Pointer to shiny output
#'                             #' @param session
#'                             #' Pointer to shiny session
#'                             #' @examples
#'                             #' x$updateRegressionPSlopeTbl(input, output, session)
#'                             updateRegressionPSlopeTbl = function(input, output, session){
#'                               if(self$status$query(processName = "regression")){
#'                                 output$tbl.regressionPSlope <- DT::renderDataTable(
#'                                   self$regressor$printPSlopeTbl() %>%
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
#'                                           filename = self$fileName$bluntFileName("RegressionMatrix_P_Slope"),
#'                                           text = "Download"
#'                                         )), #buttons
#'                                         autoWidth = TRUE,
#'                                         columnDefs = list(list(width = '50px', targets = "_all"))
#'                                       )#options
#'                                     )#DT::datatable
#'                                 )#output
#'                               }#if
#'                               else{
#'                                 output$tbl.regressionPSlope <- DT::renderDataTable(NULL)
#'                               } #else
#'                             }, #function

                            ##############################
                            # numerical output functions #
                            ##############################
                            #' @description
                            #' Updates the tbl.rawDataInfo table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$updateRawDataInfo(input, output, session)
                            updateRawDataInfo = function(input, output, session){
                              if(private$.status$query(processName = "dataImported")){
                                output$tbl.rawDataInfo <- DT::renderDataTable({self$rawData$dataInformation() %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '75vh',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$bluntFileName("rawData_type"),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::datatable
                                  }#output
                                )#output
                              }#if
                              else{
                                output$tbl.rawDataInfo <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the tbl.loqInfo table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$updateLoqInfo(input, output, session)
                            updateLoqInfo = function(input, output, session){
                              if(private$.status$query(processName = "dataImported")){
                                output$tbl.loqInfo <- DT::renderDataTable({self$loq$dataInformation() %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '75vh',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$bluntFileName("loqData_type"),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )#DT::Datatable
                                })#output
                              }#if
                              else{
                                output$tbl.loqInfo <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the tbl.metadataInfo table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$updateMetadataInfo(input, output, session)
                            updateMetadataInfo = function(input, output, session){
                              if(private$.status$query(processName = "metadataImported")){
                                output$tbl.metadataInfo <- DT::renderDataTable({self$metadata$dataInformation() %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '75vh',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$bluntFileName("metadata_type"),
                                          text = "Download"
                                        )) #buttons
                                      )#options
                                    )#DT::datatable
                                })#output
                              }#if
                              else{
                                output$tbl.metadataInfo <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the tbl.rawDataStatistics table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$updateRawDataStatisticsTbl(input, output, session)
                            updateRawDataStatisticsTbl = function(input, output, session){
                              if(private$.status$query(processName = "dataImported")){
                                output$tbl.rawDataStatistics <- DT::renderDataTable({self$rawData$dataStatistics() %>%
                                    format.data.frame(scientific = TRUE, digits = 4) %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      options = list(
                                        scrollX = TRUE,
                                        scrollY = '25vh',
                                        paging = FALSE,
                                        dom = "Blfrtip",
                                        buttons = list(list(
                                          extend = 'csv',
                                          filename = self$fileName$bluntFileName("rawData_statistics"),
                                          text = "Download"
                                        ))#buttons
                                      )#options
                                    )}#DT::datatable
                                )#output
                              }#if
                              else{
                                output$tbl.rawDataStatistics <- DT::renderDataTable(NULL)
                              }#else
                            }, #function

                            #' @description
                            #' Updates the tbl.filter table.
                            #' @param input
                            #' Pointer to shiny input
                            #' @param output
                            #' Pointer to shiny output
                            #' @param session
                            #' Pointer to shiny session
                            #' @examples
                            #' x$updateFilterTbl(input, output, session)
                            updateFilterTbl = function(input, output, session){
                              if(private$.status$query(processName = "dataImported")){
                                df <- self$metadata$rawData %>%
                                  dplyr::right_join(self$rawData$rawData, by = "Sample Name")
                                output$tbl.filter <- DT::renderDataTable({df %>%
                                    DT::datatable(
                                      extensions = "Buttons",
                                      rownames = FALSE,
                                      selection = list(target = "column"),
                                      filter = "top",
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
                                          filename = self$fileName$bluntFileName("filter_data"),
                                          text = "Download"
                                        ))#buttons
                                      )#optins
                                    )#DT::datatable
                                })#output
                              }#if
                              else{
                                output$tbl.filter <- DT::renderDataTable(NULL)
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
                            #' @examples
                            #' x$updateFilterStatisticsTbl(input, output, session)
                            updateFilterStatisticsTbl = function(input, output, session){
                              if(private$.status$query(processName = "dataFiltered")){
                                output$tbl.filterStatistics <- DT::renderDataTable({self$filteredData$dataStatistics() %>%
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
                                          filename = self$fileName$bluntFileName("filter_statistics"),
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
                            #' @examples
                            #' x$updateFilterMissingsTbl(input, output, session)
                            updateFilterMissingsTbl = function(input, output, session){
                              if(private$.status$query(processName = "dataFiltered")){
                                output$tbl.filterMissings <- DT::renderDataTable({self$filteredData$missings() %>%
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
                                          filename = self$fileName$bluntFileName("filter_missings"),
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
                            #' @examples
                            #' y <- x$exportFileName(input, output, session)
                            exportFileName = function(input, output, session){
                              private$.fileName$setSuffix <- "xlsx"
                              private$.fileName$updateTimeString()
                              # private$.fileName$mergeFileName()
                              private$.fileName$exportFileName() %>%
                                return()
                            }, #function

                            #' @description
                            #' Exports the pguIMP analysis results
                            #' @param file
                            #' export filename
                            #' (character)
                            #' @param input
                            #' Pointer to shiny input
                            #' @examples
                            #' x$exportData(input, file="result.xlsx")
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
                                     metadata = self$filteredMetadata$rawData,
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
                            #' @examples
                            #' y <- x$reportFileName(input, output, session)
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
                            #' @examples
                            #' x$write_report(input, file="report.pdf")
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
                                  numberOfNumericFeatures = c(length(self$rawData$numericFeatureNames)),
                                  numberOfNonNumericFeatures = c(length(self$rawData$nonNumericFeatureNames)),
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
                                     selected_features = self$filteredData$numericFeatureNames,
                                     loq_statistics = self$loq$loqStatistics,
                                     trafo_parameter = self$transformator$trafoParameter,
                                     model_parameter = self$model$modelParameterData(),
                                     model_quality = self$model$modelQualityData(),
                                     model_statistics = self$model$testResultData(),
                                     normalization_parameter = self$normalizer$normParameter,
                                     missings_statistics = self$missings$imputationParameter,
                                     missings_distribution = self$missings$imputationSiteDistribution(self$filteredData$numericData()),
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
                            #' @examples
                            #' x$hideOutdatedResults(input, output, session)
                            hideOutdatedResults = function(input, output, session){
                              if(!private$.status$query(processName = "dataImported")){
                                output$tbl.rawDataInfo <- DT::renderDataTable(NULL)
                                output$tbl.rawDataStatistics <- DT::renderDataTable(NULL)
                                output$tbl.filter <- DT::renderDataTable(NULL)
                              }#if
                              if(!private$.status$query(processName = "loqImported")){
                                output$tbl.loqInfo <- DT::renderDataTable(NULL)
                              }#if
                              if(!private$.status$query(processName = "metadataImported")){
                                output$tbl.metadataInfo <- DT::renderDataTable(NULL)
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
                            }#function
                          )#public
)#class
