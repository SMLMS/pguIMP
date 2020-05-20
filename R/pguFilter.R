#' @title pgu.filter
#'
#' @description
#' Filter the pguIMP dataset.
#'
#' @details
#' The filtering is done by colum and row indices.
#'
#' @format [R6::R6Class] object.
#' @section Construction:
#' x <- pguIMP::pgu.filter$new(data)
#'
#' @import R6
#' @import tidyverse
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pgu.filter <- R6::R6Class("pgu.filter",
                          ####################
                          # instance variables
                          ####################
                          private = list(
                            .colIdx = "numeric",
                            .rowIdx = "numeric"
                          ),
                          ##################
                          # accessor methods
                          ##################
                          active = list(
                            #' @field colIdx
                            #' Returns the instance variable colIdx
                            #' (numeric)
                            colIdx = function(){
                              return(private$.colIdx)
                            },
                            #' @field setColIdx
                            #' Sets the instance variable colIdx
                            #' (numeric)
                            setColIdx = function(idx = "numeric"){
                              private$.colIdx <- idx
                            },
                            #' @field rowIdx
                            #' Returns the instance variable rowIdx
                            #' (numeric)
                            rowIdx = function(){
                              return(private$.rowIdx)
                            },
                            #' @field setRowIdx
                            #' Sets the instance variable rowIdx
                            #' (numeric)
                            setRowIdx = function(idx = "numeric"){
                              private$.rowIdx = idx
                            }
                          ),
                          ###################
                          # memory management
                          ###################
                          public = list(
                            #' @description
                            #' Creates and returns a new `pgu.filter` object.
                            #' @param data
                            #' The data to be analyzed.
                            #' (tibble::tibble)
                            #' @return
                            #' A new `pgu.filter` object.
                            #' (pguIMP::pgu.optimizer)
                            #' @examples
                            #' y <- tibble:tibble()
                            #' x <- pguIMP:pgu.filter$new(data = y)
                            initialize = function(data = "tbl_df"){
                              if(class(data) != "tbl_df"){
                                data <- tibble::tibble(names <- "none",
                                                       values <- c(NA))
                              }#if
                              data %>%
                                self$resetFilter()
                            }, #function

                            #' @description
                            #' Clears the heap and
                            #' indicates that instance of `pgu.filter` is removed from heap.
                            finalize = function(){
                              print("Instance of pgu.filter removed from heap")
                            }, #function

                            ##########################
                            # print instance variables
                            ##########################
                            #' @description
                            #' Prints instance variables of a `pgu.filter` object.
                            #' @return
                            #' string
                            #' @examples
                            #' x$print()
                            #' print(x)
                            print = function(){
                              rString <- sprintf("\npgu.filter\n")
                              cat(rString)
                              cat("colIdx:\n")
                              cat(private$.colIdx)
                              cat("\nrawIdx:\n")
                              cat(private$.rowIdx)
                              cat("\n\n")
                              invisible(self)
                            }, #function

                            #' @description
                            #' Resets the filter parameter colIdx to the full dataframe.
                            #' @param data
                            #' The data to be analyzed.
                            #' (tibble::tibble)
                            #' @examples
                            #' x$resetColIdx(data)
                            resetColIdx = function(data = "tbl_df"){
                              self$setColIdx <- seq(1,ncol(data), 1)
                            }, #function

                            #' @description
                            #' Resets the filter parameter rowIdx to the full dataframe.
                            #' @param data
                            #' The data to be analyzed.
                            #' (tibble::tibble)
                            #' @examples
                            #' x$resetRowIdx(data)
                            resetRowIdx = function(data = "tbl_df"){
                              self$setRowIdx <- seq(1,nrow(data), 1)
                            }, #function

                            #' @description
                            #' Resets the filter parameter colIdx and rowIdx to the full dataframe.
                            #' @param data
                            #' The data to be analyzed.
                            #' (tibble::tibble)
                            #' @examples
                            #' x$resetFilter(data)
                            resetFilter = function(data = "tbl_df"){
                              self$resetColIdx(data)
                              self$resetRowIdx(data)
                            }, #function

                            #' @description
                            #' Filters and returns the given data frame.
                            #' @param data
                            #' The data to be analyzed.
                            #' (tibble::tibble)
                            #' @return
                            #' The filtered data frame
                            #' (tibble::tibble)
                            #' @examples
                            #' y <- x$filter(data)
                            filter = function(data = "tbl_df"){
                              data %>%
                                dplyr::select(self$colIdx) %>%
                                dplyr::slice(self$rowIdx) %>%
                                return()
                            }, #function

                            #' @description
                            #' Filters and returns the given data frame.
                            #' In contrast to the filter function, this function only
                            #' filters the rows.
                            #' @param data
                            #' The data to be analyzed.
                            #' (tibble::tibble)
                            #' @return
                            #' The filtered data frame
                            #' (tibble::tibble)
                            #' @examples
                            #' y <- x$filterRows(data)
                            filterRows = function(data = "tbl_df"){
                              data %>%
                                dplyr::slice(self$rowIdx) %>%
                                return()
                            }#function
                          )#public
)#class
