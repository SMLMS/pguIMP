#' @title pgu.exporter
#'
#' @description
#' A class that writes the results of the pguIMP analysis to an Excel file.
#'
#' @details
#' Creates a download file name and saves a list of tibbles to an Excel file.
#' Each tibble is written to a separate sheet.
#' This object is used by the shiny based gui and is not for use in individual R-scripts!
#'
#' @format [R6::R6Class] object.
#'
#' @importFrom R6 R6Class
#' @importFrom tools file_ext
#' @importFrom writexl write_xlsx
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pgu.exporter <- R6::R6Class("pgu.exporter",
                            ####################
                            # instance variables
                            ####################
                            private = list(
                              .fileName = "character",
                              .suffix = "factor",
                              .suffixAlphabet  = "character"
                            ),
                            ##################
                            # accessor methods
                            ##################
                            active = list(
                              #' @field fileName
                              #' Returns the fileName.
                              #' (character)
                              fileName = function(){
                              return(private$.fileName)
                            },
                            #' @field setFileName
                            #' Set the fileName.
                            #' (character)
                            setFileName = function(name = "character"){
                              private$.fileName <- name
                              self$extractSuffix()
                            },
                            #' @field suffix
                            #' Returns the file suffix.
                            #' (character)
                            suffix = function(){
                              return(private$.suffix)
                            }
                            ),
                            ###################
                            # memory management
                            ###################
                            public = list(
                              #' @description
                              #' Creates and returns a new `pgu.exporter` object.
                              #' @return
                              #' A new `pgu.exporter` object.
                              #' (pguIMP::pgu.exporter)
                              initialize = function() {
                                private$.suffixAlphabet <- c("xlsx")
                              },
                              #' @description
                              #' Clears the heap and
                              #' indicates if instance of `pgu.exporter` is removed from heap.
                              finalize = function() {
                                print("Instance of pgu.exporter removed from heap")
                              },
                              ##########################
                              # print instance variables
                              ##########################
                              #' @description
                              #' Prints instance variables of a `pgu.exporter` object.
                              #' @return
                              #' string
                              print = function() {
                                rString <- sprintf("\npgu.exporter\n")
                                cat(rString)
                                eString <- sprintf("\noutFileName: %s\nsuffix: %s\n",
                                                   self$fileName,
                                                   self$suffix
                                )
                                cat(eString)
                                cat("\n\n")
                                invisible(self)
                              },
                              ####################
                              # public functions #
                              ####################
                              #' @description
                              #' extracts the suffix from the fileName
                              extractSuffix = function(){
                                private$.suffix <- tools::file_ext(self$fileName)
                              },
                              #' @description
                              #' writes tibble to an excel file of the name fileName.
                              #' @param obj
                              #' A tibble or list of tibble.
                              #' If obj is a list, each member will be written to a seperate sheet.
                              writeDataToExcel = function(obj = "list"){
                                print("write obj")
                                print(typeof(obj))
                                writexl::write_xlsx(obj,
                                                    path = self$fileName,
                                                    col_names = TRUE,
                                                    format_headers = TRUE
                                )
                                print("done")
                              }
                            )#public
)#R6Class
