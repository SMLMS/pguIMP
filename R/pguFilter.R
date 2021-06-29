#' @title pgu.filter
#'
#' @description
#' Filter the pguIMP dataset.
#'
#' @details
#' The filtering is done by column and row indices.
#' This object is used by the shiny based gui and is not for use in individual R-scripts!
#'
#' @format [R6::R6Class] object.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select slice
#' @importFrom R6 R6Class
#' @importFrom tibble is_tibble tibble
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
                            .rowIdx = "numeric",

                            #' @description
                            #' Resets the filter parameter colIdx to the full dataframe.
                            reset_colIdx = function(data_df = "tbl_df")
                            {
                              self$setColIdx <- seq(1,ncol(data_df), 1)
                            }, #end pguIMP::pgi.filter$reset_colIndex()

                            #' @description
                            #' Resets the filter parameter rowIdx to the full dataframe.
                            reset_rowIdx = function(data_df = "tbl_df")
                            {
                              self$setRowIdx <- seq(1,nrow(data_df), 1)
                            }, #end pguIMP::pgu.filter$reset_rowIdx

                            #' @description
                            #' Clears the heap and
                            #' indicates that instance of pguIMP::pgu.filter is removed from heap.
                            finalize = function()
                            {
                              print("Instance of pgu.filter removed from heap")
                            } #end pguIMP::pgu.filter$finalize()
                          ),
                          ##################
                          # accessor methods
                          ##################
                          active = list(
                            #' @field colIdx
                            #' Returns the instance variable colIdx
                            #' (numeric)
                            colIdx = function()
                            {
                              return(private$.colIdx)
                            },
                            #' @field setColIdx
                            #' Sets the instance variable colIdx
                            #' (numeric)
                            setColIdx = function(idx = "numeric")
                            {
                              if(is.numeric(idx))
                              {
                                private$.colIdx <- idx %>%
                                  sort()
                              }#if
                            },
                            #' @field rowIdx
                            #' Returns the instance variable rowIdx
                            #' (numeric)
                            rowIdx = function()
                            {
                              return(private$.rowIdx)
                            },
                            #' @field setRowIdx
                            #' Sets the instance variable rowIdx
                            #' (numeric)
                            setRowIdx = function(idx = "numeric")
                            {
                              if(is.numeric(idx))
                              {
                                private$.rowIdx = idx %>%
                                  sort()
                              }#if
                            }
                          ),
                          ####################
                          # public functions #
                          ####################
                          public = list(
                            #' @description
                            #' Creates and returns a new pguIMP::pgu.filter object.
                            #' @param data_df
                            #' The data to be analyzed.
                            #' (tibble::tibble)
                            #' @return
                            #' A new pguIMP::pgu.filter object.
                            #' (pguIMP::pgu.filter)
                            initialize = function(data_df = "tbl_df")
                            {
                              if(!tibble::is_tibble(data_df)){
                                tibble::tibble(names <- "none",
                                               values <- c(NA)) %>%
                                  self$reset()
                              }else{
                                data_df %>%
                                  self$reset()
                              }#if
                            }, #end pguIMP::pgu.filter$initialize()

                            #' @description
                            #' Prints instance variables of a pguIMP::pgu.filter object.
                            #' @return
                            #' string
                            print = function()
                            {
                              sprintf("\npgu.filter\ncolIdx:\n") %>%
                                cat()
                              print(self$colIdx)
                              sprintf("\nrowIdx:\n") %>%
                                cat()
                              print(self$rowIdx)
                              cat("\n\n")
                              invisible(self)
                            }, #end pguIMP::pgu.filter$print()


                            #' @description
                            #' Resets the filter parameter colIdx and rowIdx to the full dataframe.
                            #' @param data_df
                            #' The data to be analyzed.
                            #' (tibble::tibble)
                            reset = function(data_df = "tbl_df")
                            {
                              private$reset_colIdx(data_df)
                              private$reset_rowIdx(data_df)
                            }, #end pguIMP::pgi.filter$reset()

                            #' @description
                            #' Filters and returns the given data frame.
                            #' @param data_df
                            #' The data to be analyzed.
                            #' (tibble::tibble)
                            #' @return
                            #' The filtered data frame
                            #' (tibble::tibble)
                            predict = function(data_df = "tbl_df")
                            {
                              data_df %>%
                                dplyr::select(self$colIdx) %>%
                                dplyr::slice(self$rowIdx) %>%
                                return()
                            } #end pguIMP::pgi.filter$predict()
                          )#public
)#class
