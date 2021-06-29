#' @title pgu.fie
#'
#' @description
#' Handles file names for the pguIMP shiny web interface.
#'
#' @details
#' The class stores filenames and upload specifications for the pguIMP shiny web interface in its instance variables.
#' This object is used by the shiny based gui and is not for use in individual R-scripts!
#'
#' @format [R6::R6Class] object.
#'
#' @importFrom R6 R6Class
#' @importFrom  tools file_path_sans_ext file_ext
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
                          #######################
                          # instance variablies #
                          #######################
                          .uploadFileName = "character",
                          .fileName = "character",
                          .baseName = "character",
                          .folderName = "character",
                          .suffix = "character",
                          .exportSuffix = "character",
                          .timeString = "character",
                          .sheetIndex = "numeric",
                          .separator = "character",
                          .skipRows = "numeric",
                          .columnNames = "logical",
                          .naChar ="character",

                          ####################
                          # private fuctions #
                          ####################
                          #' @description
                          #' Clears the heap and
                          #' indicates that instance of `pgu.file` is removed from heap.
                          finalize = function(){
                            print("Instance of pgu.fileDesign removed from heap")
                          }, #end pgu.file$finalize

                          #' @description
                          #' Splits fileName and writes the results in the class' instance variables
                          #' folderName, baseName, suffix.
                          split_file_name = function(){
                            private$.baseName <- tools::file_path_sans_ext(basename(self$fileName))
                            private$.folderName <- dirname(self$fileName)
                            private$.suffix <- tools::file_ext(self$fileName)
                          }, #end pgu.file$split_file_name

                          #' @description
                          #' Stores the current system time into the instance variable timeString.
                          update_time_string = function(){
                            private$.timeString <- Sys.time() %>%
                              format("%y%m%d-%H%M%S")
                          } #end pgu.file$update_time_string
                        ), #end pgu.file$private

                        ##################
                        # accessor methods
                        ##################
                        active = list(
                          #' @field uploadFileName
                          #' Returns the instance variable uploadFileName
                          #' (character)
                          uploadFileName = function()
                          {
                            return(private$.uploadFileName)
                          },
                          #' @field fileName
                          #' Returns the instance variable fileName
                          #' (character)
                          fileName = function()
                          {
                            return(private$.fileName)
                          },
                          #' @field baseName
                          #' Returns the instance variable baseName
                          #' (character)
                          baseName = function()
                          {
                            return(private$.baseName)
                          },
                          #' @field folderName
                          #' Returns the instance variable folderName
                          #' (character)
                          folderName = function()
                          {
                            return(private$.folderName)
                          },
                          #' @field suffix
                          #' Returns the instance variable suffix
                          #' (character)
                          suffix = function()
                          {
                            return(private$.suffix)
                          },
                          #' @field exportSuffix
                          #' Returns the instance variable exportSuffix
                          #' (character)
                          exportSuffix = function()
                          {
                            return(private$.exportSuffix)
                          },
                          #' @field timeString
                          #' Returns the instance variable timeString
                          #' (character)
                          timeString = function()
                          {
                            return(private$.timeString)
                          },
                          #' @field sheetIndex
                          #' Returns the instance variable sheetIndex
                          #' (numeric)
                          sheetIndex = function()
                          {
                            return(private$.sheetIndex)
                          },
                          #' @field separator
                          #' Returns the instance variable separator
                          #' (character)
                          separator = function()
                          {
                            return(private$.separator)
                          },
                          #' @field skipRows
                          #' Returns the instance variable skipRows
                          #' (numeric)
                          skipRows = function()
                          {
                            return(private$.skipRows)
                          },
                          #' @field columnNames
                          #' Returns the instance variable columnNames
                          #' (logical)
                          columnNames = function()
                          {
                            return(private$.columnNames)
                          },
                          #' @field naChar
                          #' Returns the instance variable naChar
                          #' (character)
                          naChar = function()
                          {
                            return(private$.naChar)
                          }
                        ), #end pgu.file$active

                        ####################
                        # public functions #
                        ####################
                        public = list(
                          #' @description
                          #' Creates and returns a new object of type pgu.file.
                          #' @param uploadFileName
                          #' Name of uploaded file.
                          #' (string)
                          #' @param fileName
                          #' Actual file name.
                          #' (string)
                          #' @param sheetIndex
                          #' Index excel sheet to import.
                          #' (integer)
                          #' @param separator
                          #' Character for column separation.
                          #' (character)
                          #' @param skipRows
                          #' Number of rows to skip.
                          #' (integer)
                          #' @param columnNames
                          #' Indicates if the data source file has a columnNames.
                          #' (logical)
                          #' @param naChar
                          #' Character for missing values.
                          #' (string)
                          #' @return
                          #' A new pgu.file object.
                          #' (pguIMP::pgu.file)
                          initialize = function(uploadFileName = "",
                                                fileName = "",
                                                sheetIndex = 1,
                                                separator = ",",
                                                skipRows = 0,
                                                columnNames = TRUE,
                                                naChar = "NA")
                          {
                            self$reset(uploadFileName = uploadFileName,
                                       fileName = fileName,
                                       sheetIndex = sheetIndex,
                                       separator = separator,
                                       skipRows = skipRows,
                                       columnNames = columnNames,
                                       naChar = naChar)
                          }, #pgu.file$initialize()

                          #' @description
                          #' Prints the instance variables of the object.
                          #' @return
                          #' string
                          print = function()
                          {
                            rString <- sprintf("\npgu.file:\n")
                            rString <- sprintf("%suploadFileName: %s\n", rString, self$uploadFileName)
                            rString <- sprintf("%sfileName: %s\n", rString, self$fileName)
                            rString <- sprintf("%sbaseName: %s\n", rString, self$baseName)
                            rString <- sprintf("%ssuffix: %s\n", rString, self$suffix)
                            rString <- sprintf("%sexportSuffix: %s\n", rString, self$exportSuffix)
                            rString <- sprintf("%stimeString: %s\n", rString, self$timeString)
                            rString <- sprintf("%ssheetIndex: %i\n", rString, self$sheetIndex)
                            rString <- sprintf("%sseparator: %s\n", rString, self$separator)
                            rString <- sprintf("%sskipRows: %i\n", rString, self$skipRows)
                            rString <- sprintf("%scolumnNames: %s\n", rString, self$columnNames)
                            rString <- sprintf("%snaChar: %s\n", rString, self$naChar)
                            rString <- sprintf("%s\n\n", rString)
                            cat(rString)
                            invisible(self)
                          }, #end pgu.file$print()

                          #' @description
                          #' Resets the instance variables of the object.
                          #' @param uploadFileName
                          #' Name of uploaded file.
                          #' (string)
                          #' @param fileName
                          #' Actual file name.
                          #' (string)
                          #' @param sheetIndex
                          #' Index excel sheet to import.
                          #' (integer)
                          #' @param separator
                          #' Character for column separation.
                          #' (character)
                          #' @param skipRows
                          #' Number of rows to skip.
                          #' (integer)
                          #' @param columnNames
                          #' Indicates if the data source file has a columnNames.
                          #' (logical)
                          #' @param naChar
                          #' Character for missing values.
                          #' (string)
                          reset = function(uploadFileName = "",
                                           fileName = "",
                                           sheetIndex = 1,
                                           separator = ",",
                                           skipRows = 0,
                                           columnNames = TRUE,
                                           naChar = "NA")
                          {
                            private$.uploadFileName <- uploadFileName
                            private$.fileName <- fileName
                            private$.exportSuffix <- "xlsx"
                            private$.sheetIndex <- as.integer(sheetIndex)
                            private$.separator <- separator
                            private$.skipRows <- as.integer(skipRows)
                            private$.columnNames <- as.logical(columnNames)
                            private$.naChar <- naChar
                            self$fit()
                          }, #end pgu.file$reset

                          #' @description
                          #' Extracts information about upload specifications from the instance variables.
                          fit = function()
                          {
                            private$update_time_string()
                            private$split_file_name()
                          }, #end pgu.file$fit

                          #' @description
                          #' Predicts an export file name.
                          #' @param affix
                          #' User dedined file name affix.
                          #' (string)
                          #' @return
                          #' A file name.
                          #' (string)
                          predict = function(affix = "analysis")
                          {
                            sprintf("%s_pguIMP_%s_%s.%s",
                                    self$baseName,
                                    affix,
                                    self$timeString,
                                    self$exportSuffix) %>%
                              return()
                          }, #end pgu.file$predict

                          #' @description
                          #' Extracts information about upload specifications from the instance variables
                          #' and predicts an export file name.
                          #' @param affix
                          #' User dedined file name affix.
                          #' (string)
                          #' @return
                          #' A file name.
                          #' (string)
                          fit_predict = function(affix = "analysis")
                          {
                            self$fit()
                            self$predict(affix = affix) %>%
                              return()
                          }
                        ) #end pgu.file$public
)#end pgu.file
