#' @title pgu.reporter
#'
#' @description
#' Creates a human readable report file of the pguIMP analysis in odf format via rmarkdown and latex.
#' This object is used by the shiny based gui and is not for use in individual R-scripts!
#'
#' @details
#' I run at the end of the analysis.
#'
#' @format [R6::R6Class] object.
#'
#' @importFrom R6 R6Class
#' @importFrom rmarkdown render
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
                              #' @param name
                              #' Filename of the report pdf.
                              #' (character)
                              #' @return
                              #' A new `pgu.reporter` object.
                              #' (pguIMP::pgu.report)
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
                              print = function() {
                                sprintf("\npgu.reporter\n\nReport will be written to: %s", self$filename) %>%
                                  cat()
                                invisible(self)
                              }, #fucntion
                              ##################
                              # render functions
                              ##################
                              #' @description
                              #' Writes a report of the pguIMP analysis to a pdf file.
                              #' @param obj
                              #' A list of class objects that are passed to the rmarkdown script.
                              #' @return
                              #' t.b.a.
                              write_report = function(obj){
                                report_dir <- tempdir()

                                report_file <- system.file("inst", "rmarkdown", "report.Rmd", package = "pguIMP")
                                header_file <- system.file("inst", "rmarkdown", "header.tex", package = "pguIMP")
                                impressum_file <- system.file("inst", "rmarkdown", "impressum.tex", package = "pguIMP")
                                title_file <- system.file("inst", "rmarkdown", "title_page.tex", package = "pguIMP")

                                report_file_tmp <- file.path(report_dir, "report.Rmd")
                                header_file_tmp <- file.path(report_dir, "header.tex")
                                impressum_file_tmp <- file.path(report_dir, "impressum.tex")
                                title_file_tmp <- file.path(report_dir, "title_page.tex")

                                file.copy(report_file, report_file_tmp, overwrite = TRUE)
                                file.copy(header_file, header_file_tmp, overwrite = TRUE)
                                file.copy(impressum_file, impressum_file_tmp, overwrite = TRUE)
                                file.copy(title_file, title_file_tmp, overwrite = TRUE)

                                rmarkdown::render(report_file,
                                                  output_format = "pdf_document",
                                                  output_file = self$filename,
                                                  params = obj,
                                                  clean = TRUE,
                                                  run_pandoc = TRUE,
                                                  runtime = "static")
                              }
                            )
)
