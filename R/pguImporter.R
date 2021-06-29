#' @title pgu.importer
#'
#' @description
#' Handles the data import
#'
#' @details
#' Menages the import of the pguIMP dataset
#' This object is used by the shiny based gui and is not for use in individual R-scripts!
#'
#' @format [R6::R6Class] object.
#'
#' @importFrom R6 R6Class
#' @importFrom readxl read_xlsx
#' @importFrom tibble tibble
#'
#' @include pguFile.R
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pgu.importer <- R6::R6Class("pgu.importer",
                                ####################
                                # instance variables
                                ####################
                                private = list(
                                  .suffixes = "character"
                                ),
                                ##################
                                # accessor methods
                                ##################
                                active = list(
                                  #' @field suffixes
                                  #' Returns the instance variable suffixes
                                  #' (character)
                                  suffixes = function(){
                                    return(private$.suffixes)
                                  }
                                ),
                                ###################
                                # memory management
                                ###################
                                public = list(
                                  #' @description
                                  #' Creates and returns a new `pgu.importer` object.
                                  #' @return
                                  #' A new `pgu.importer` object.
                                  #' (pguIMP::pgu.importer)
                                  initialize = function(){
                                    private$.suffixes <- c("xls", "xlsx")
                                  }, #function

                                  #' @description
                                  #' Clears the heap and
                                  #' indicates that instance of `pgu.importer` is removed from heap.
                                  finalize = function(){
                                    print("Instance of pgu.importer removed from heap")
                                  }, #function
                                  ##########################
                                  # print instance variables
                                  ##########################
                                  #' @description
                                  #' Prints instance variables of a `pgu.importer` object.
                                  #' @return
                                  #' string
                                  print = function(){
                                    rString <- sprintf("\npgu.importer\nsuffixes:\n")
                                    for (suffix in self$suffixes){
                                      rString <- sprintf("%s%s\n", rString, suffix)
                                    }#for
                                    cat(rString)
                                    invisible(self)
                                  }, #function

                                  ####################
                                  # public functions #
                                  ####################
                                  #' @description
                                  #' Takes an instance of pgu.file and tests if the suffix is valid.
                                  #' @param obj
                                  #' instance of pgu.file.
                                  #' (pguIMP::pgu.file)
                                  #' @return
                                  #' test result
                                  #' (logical)
                                  suffixIsKnown = function(obj = "pgu.file"){
                                    if(sum(obj$suffix == self$suffixes)>0){
                                      return(TRUE)
                                    }#if
                                    else{
                                      return(FALSE)
                                    }#else
                                  }, #function

                                  #' @description
                                  #' Takes an instance of pgu.file imports a dataset.
                                  #' @param obj
                                  #' instance of pgu.file.
                                  #' (pguIMP::pgu.file)
                                  #' @return
                                  #' data frame
                                  #' (tibble::tibble)
                                  importData = function(obj = "pgu.file"){
                                    if(self$suffixIsKnown(obj)){
                                      switch(obj$suffix,
                                             xls = {return(readxl::read_xlsx(path = obj$uploadFileName,
                                                                             sheet = obj$dataSheet,
                                                                             col_names = obj$header,
                                                                             skip = obj$skipRows,
                                                                             na = obj$naChar))},
                                             xlsx = {return(readxl::read_xlsx(path = obj$uploadFileName,
                                                                              sheet = obj$dataSheet,
                                                                              col_names = obj$header,
                                                                              skip = obj$skipRows,
                                                                              na = obj$naChar))})
                                    }#if
                                    else{
                                      return(NULL)
                                    }#esle
                                  }, #function

                                  #' @description
                                  #' Takes an instance of pgu.file imports a loq  dataset.
                                  #' @param obj
                                  #' instance of pgu.file.
                                  #' (pguIMP::pgu.file)
                                  #' @return
                                  #' data frame
                                  #' (tibble::tibble)
                                  importLoq = function(obj = "pgu.file"){
                                    if(self$suffixIsKnown(obj)){
                                      switch(obj$suffix,
                                             xls = {return(readxl::read_xlsx(path = obj$uploadFileName,
                                                                             sheet = obj$loqSheet,
                                                                             col_names = obj$header,
                                                                             skip = obj$skipRows,
                                                                             na = obj$naChar))},
                                             xlsx = {return(readxl::read_xlsx(path = obj$uploadFileName,
                                                                              sheet = obj$loqSheet,
                                                                              col_names = obj$header,
                                                                              skip = obj$skipRows,
                                                                              na = obj$naChar))})
                                    }#if
                                    else{
                                      return(NULL)
                                    }#else
                                  }, #function

                                  #' @description
                                  #' Takes an instance of pgu.file imports a metadata dataset.
                                  #' @param obj
                                  #' instance of pgu.file.
                                  #' (pguIMP::pgu.file)
                                  #' @return
                                  #' data frame
                                  #' (tibble::tibble)
                                  importMetadata = function(obj = "pgu.file"){
                                    if(self$suffixIsKnown(obj)){
                                      switch(obj$suffix,
                                             xls = {return(readxl::read_xlsx(path = obj$uploadFileName,
                                                                             sheet = obj$metadataSheet,
                                                                             col_names = obj$header,
                                                                             skip = obj$skipRows,
                                                                             na = obj$naChar))},
                                             xlsx = {return(readxl::read_xlsx(path = obj$uploadFileName,
                                                                              sheet = obj$metadataSheet,
                                                                              col_names = obj$header,
                                                                              skip = obj$skipRows,
                                                                              na = obj$naChar))})
                                    }#if
                                    else{
                                      return(NULL)
                                    }#else
                                  }#function
                                )#public
)#class
