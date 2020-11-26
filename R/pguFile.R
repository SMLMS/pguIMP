#' @title pgu.fie
#'
#' @description
#' Handles file names
#'
#' @details
#' Menages the file names and upload specifications for the pguIMP dataset.
#'
#' @format [R6::R6Class] object.
#' @section Construction:
#' x <- pguIMP::pgu.file$new()
#'
#' @import R6
#' @import tools
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'
#'

pgu.file <- R6::R6Class("pgu.file",
                        ####################
                        # instance variables
                        ####################
                        private = list(
                          .uploadFileName = "character",
                          .fileName = "character",
                          .baseName = "character",
                          .folderName = "character",
                          .suffix = "character",
                          .exportType = "character",
                          .timeString = "character",
                          .sheetIndex = "numeric",
                          .dataSheet = "character",
                          .loqSheet = "characetr",
                          .metadataSheet = "character",
                          .separator = "character",
                          .skipRows = "numeric",
                          .header = "logical",
                          .naChar ="character"
                        ),
                        ##################
                        # accessor methods
                        ##################
                        active = list(
                          #' @field uploadFileName
                          #' Returns the instance variable uploadFileName
                          #' (character)
                          uploadFileName = function(){
                            return(private$.uploadFileName)
                          },
                          #' @field setUploadFileName
                          #' Sets the instance variable uploadFileName
                          #' (character)
                          setUploadFileName = function(val = "character"){
                            private$.uploadFileName <- val
                          },
                          #' @field fileName
                          #' Returns the instance variable fileName
                          #' (character)
                          fileName = function(){
                            return(private$.fileName)
                          },
                          #' @field setFileName
                          #' Sets the instance variable fileName
                          #' (character)
                          setFileName = function(val = "character"){
                            private$.fileName <- val
                          },
                          #' @field baseName
                          #' Returns the instance variable baseName
                          #' (character)
                          baseName = function(){
                            return(private$.baseName)
                          },
                          #' @field setBaseName
                          #' Sets the instance variable baseName
                          #' (character)
                          setBaseName = function(val = "chatacter"){
                            private$.baseName <- val
                          },
                          #' @field folderName
                          #' Returns the instance variable folderName
                          #' (character)
                          folderName = function(){
                            return(private$.folderName)
                          },
                          #' @field setFolderName
                          #' Sets the instance variable folderName
                          #' (character)
                          setFolderName = function(val = "character"){
                            private$.folderName <- val
                          },
                          #' @field suffix
                          #' Returns the instance variable suffix
                          #' (character)
                          suffix = function(){
                            return(private$.suffix)
                          },
                          #' @field setSuffix
                          #' Sets the instance variable suffix
                          #' (character)
                          setSuffix = function(val = "character"){
                            private$.suffix <- val
                          },
                          #' @field exportType
                          #' Returns the instance variable exportType
                          #' (character)
                          exportType = function(){
                            return(private$.exportType)
                          },
                          #' @field setExportType
                          #' Sets the instance variable exportType
                          #' (character)
                          setExportType = function(val = "character"){
                            private$.exportType <- val
                          },
                          #' @field timeString
                          #' Returns the instance variable timeString
                          #' (character)
                          timeString = function(){
                            return(private$.timeString)
                          },
                          #' @field sheetIndex
                          #' Returns the instance variable sheetIndex
                          #' (numeric)
                          sheetIndex = function(){
                            return(private$.sheetIndex)
                          },
                          #' @field setSheetIndex
                          #' Sets the instance variable sheetIndex
                          #' (numeric)
                          setSheetIndex = function(val = "numeric"){
                            private$.sheetIndex <- val
                          },
                          #' @field dataSheet
                          #' Returns the instance variable dataSheet
                          #' (character)
                          dataSheet = function(){
                            return(private$.dataSheet)
                          },
                          #' @field setDataSheet
                          #' Sets the instance variable dataSheet
                          #' (character)
                          setDataSheet = function(val = "character"){
                            private$.dataSheet <- val
                          },
                          #' @field loqSheet
                          #' Returns the instance variable loqSheet
                          #' (character)
                          loqSheet = function(){
                            return(private$.loqSheet)
                          },
                          #' @field setLoqSheet
                          #' Sets the instance variable loqSheet
                          #' (character)
                          setLoqSheet = function(val = "character"){
                            private$.loqSheet <- val
                          },
                          #' @field metadataSheet
                          #' Returns the instance variable metadataSheet
                          #' (character)
                          metadataSheet = function(){
                            return(private$.metadataSheet)
                          },
                          #' @field setMetadataSheet
                          #' Sets the instance variable metadataSheet
                          #' (character)
                          setMetadataSheet = function(val = "character"){
                            private$.metadataSheet <- val
                          },
                          #' @field separator
                          #' Returns the instance variable separator
                          #' (character)
                          separator = function(){
                            return(private$.separator)
                          },
                          #' @field setSeparator
                          #' Sets the instance variable separator
                          #' (character)
                          setSeparator = function(val = "character"){
                            private$.separator <- val
                          },
                          #' @field skipRows
                          #' Returns the instance variable skipRows
                          #' (numeric)
                          skipRows = function(){
                            return(private$.skipRows)
                          },
                          #' @field setSkipRows
                          #' Sets the instance variable skipRows
                          #' (numeric)
                          setSkipRows = function(val = "numeric"){
                            private$.skipRows <- val
                          },
                          #' @field header
                          #' Returns the instance variable header
                          #' (logical)
                          header = function(){
                            return(private$.header)
                          },
                          #' @field setHeader
                          #' Sets the instance variable header
                          #' (logical)
                          setHeader = function(val = "logical"){
                            private$.header <- val
                          },
                          #' @field naChar
                          #' Returns the instance variable naChar
                          #' (character)
                          naChar = function(){
                            return(private$.naChar)
                          },
                          #' @field setNaChar
                          #' Sets the instance variable naChar
                          #' (character)
                          setNaChar = function(val = "character"){
                            private$.naChar <- val
                          }
                        ),
                        ###################
                        # memory management
                        ###################
                        public = list(
                          #' @description
                          #' Creates and returns a new `pgu.file` object.
                          #' @return
                          #' A new `pgu.file` object.
                          #' (pguIMP::pgu.importer)
                          #' @examples
                          #' x <- pguIMP:pgu.file$new()
                          initialize = function(){
                            private$.uploadFileName <- character(length(0))
                            private$.fileName <- character(length(0))
                            private$.baseName <- character(length(0))
                            private$.folderName <- character(length(0))
                            private$.suffix <- character(length(0))
                            private$.exportType <- "pguIMP"
                            private$.sheetIndex <- 0
                            private$.dataSheet <- "raw_data"
                            private$.loqSheet <- "loq"
                            private$.metadataSheet <- "metadata"
                            private$.separator <- character(length(0))
                            private$.skipRows <- 0
                            private$.header = TRUE
                            private$.naChar <- "NA"
                            self$updateTimeString()
                          }, #function

                          #' @description
                          #' Clears the heap and
                          #' indicates that instance of `pgu.file` is removed from heap.
                          finalize = function(){
                            print("Instance of pgu.fileDesign removed from heap")
                          }, #function

                          ##########################
                          # print instance variables
                          ##########################
                          #' @description
                          #' Prints instance variables of a `pgu.file` object.
                          #' @return
                          #' string
                          #' @examples
                          #' x$print()
                          #' print(x)
                          print = function(){
                            rString <- sprintf("\npgu.file\n")
                            cat(rString)
                            fString <- sprintf("uploadFileName: %s\nfileName: %s\nbaseName: %s\nsuffix: %s\nexportType: %s\ntimeString: %s\nsheetIndex: %i\nseparator: %s\nskipRows: %i\nheader: %s\nnaChar: %s\n",
                                               self$uploadFileName,
                                               self$fileName,
                                               self$baseName,
                                               self$suffix,
                                               self$exportType,
                                               self$timeString,
                                               self$sheetIndex,
                                               self$separator,
                                               self$skipRows,
                                               self$header,
                                               self$naChar
                            )
                            cat(fString)
                            cat("\n\n")
                            invisible(self)
                          }, #function

                          ####################
                          # public functions #
                          ####################
                          #' @description
                          #' Splits fileName and writes the results in the class' instance variables
                          #' folderName, baseName, suffix.
                          #' @examples
                          #' x$splitFileName()
                          splitFileName = function(){
                            self$setBaseName <- tools::file_path_sans_ext(basename(self$fileName))
                            self$setFolderName <- dirname(self$fileName)
                            self$setSuffix <- tools::file_ext(self$fileName)
                          }, #function

                          #' @description
                          #' Gets the current system time and
                          #' stores it into the class' instance variable timeString.
                          #' @examples
                          #' x$updateTimeString()
                          updateTimeString = function(){
                            private$.timeString <- Sys.time() %>%
                              format("%y%m%d-%H%M%S")
                          }, #function

                          #' @description
                          #' Updates the class' instance variable fileName
                          #' @examples
                          #' x$mergeFileName()
                          mergeFileName = function(){
                            private$.fileName <- sprintf("%s_%s_%s.%s",
                                                         self$baseName,
                                                         self$exportType,
                                                         self$timeString,
                                                         self$suffix)
                          }, #function

                          #' @description
                          #' Returns an updated fileName without suffix.
                          #' @param value
                          #' user defined file name extension
                          #' (character)
                          #' @return
                          #' blunt file name without suffix
                          #' (character)
                          #' @examples
                          #' x$bluntFileName(value = "infected")
                          bluntFileName = function(value = "character"){
                            self$updateTimeString()
                            sprintf("%s_%s_%s",
                                    self$baseName,
                                    value,
                                    self$timeString) %>%
                              return()
                          }, #function

                          #' @description
                          #' Returns an export file name
                          #' @return
                          #' export fileName
                          #' (character)
                          #' @examples
                          #' x$exportFileName(value = "infected")
                          exportFileName = function(){
                            sprintf("%s/%s_%s_%s.%s",
                                    self$folderName,
                                    self$baseName,
                                    self$exportType,
                                    self$timeString,
                                    self$suffix) %>%
                              return()
                          }#function
                        )#public
)#class
