#' @title pgu.reporter
#'
#' @description
#' Creates a human readable report file of the pguIMP analysis in odf format via rmarkdown and latex.
#'
#' @details
#' I run at the end of the analysis.
#'
#' @format [R6::R6Class] object.
#' @section Construction:
#' x <- pguIMP::pgu.reporter$new()
#'
#' @import R6
#' @import tidyverse
#' @import shiny
#' @import rmarkdown
#'
#' @include pguStatus.R
#' @include pguImporter.R
#' @include pguFile.R
#' @include pguData.R
#' @include pguLimitsOfQuantification.R
#' @include pguFilter.R
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pgu.reporter <- R6::R6Class("pgu.reporter",
                            ####################
                            # instance variables
                            ####################
                            private = list(
                              .filename = "character"
                            ),
                            ##################
                            # accessor methods
                            ##################
                            active = list(
                              #' @field filename
                              #' Returns the instance variable filename
                              #' (character)
                              filename = function(){
                                return(private$.filename)
                              },
                              #' @field setFilename
                              #' Sets the instance variable filename to name.
                              #' (character)
                              setFilename = function(name = "character"){
                                private$.filename <- name
                              }
                            ),
                            ###################
                            # memory management
                            ###################
                            public = list(
                              #' @description
                              #' Creates and returns a new `pgu.reporter` object.
                              #' @param
                              #' Filename of the report pdf.
                              #' (character)
                              #' @return
                              #' A new `pgu.reporter` object.
                              #' (pguIMP::pgu.report)
                              #' @examples
                              #' y <- "report.pdf"
                              #' x <- pguIMP:pgu.reporter$new(name = y)
                              initialize = function(name = "character"){
                                self$setFilename <- name
                              },#function
                              #' @description
                              #' Clears the heap and
                              #' indicates that instance of `pgu.reporter` is removed from heap.
                              finalize = function() {
                                print("Instance of pgu.reporter removed from heap")
                              }, #function
                              ##########################
                              # print instance variables
                              ##########################
                              #' @description
                              #' Prints instance variables of a `pgu.reporter` object.
                              #' @return
                              #' string
                              #' @examples
                              #' x$print()
                              #' print(x)
                              print = function() {
                                sprintf("\npgu.reporter\n\nReport will be written to: %s", self$filename) %>%
                                  cat()
                                invisible(self)
                              }, #fucntion
                              ##################
                              # render functions
                              ##################
                              write_report = function(){
                                rmarkdown::render(input = "parent.Rmd",
                                                  output_format = "pdf_document",
                                                  output_file = self$filename,
                                                  clean = TRUE)
                              }
                            )
)
