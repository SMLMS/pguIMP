#' @title pgu.normalizer
#'
#' @description
#' Normalization of data. Part of pguIMP.
#'
#' @details
#' Performs a data normalization in order to achieve a standardized version of the dataframe.
#'
#' @format [R6::R6Class] object.
#' @section Construction:
#' x <- pguIMP::pgu.normalizer$new(data)
#'
#' @import R6
#' @import tidyverse
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pgu.normalizer <- R6::R6Class("pgu.normalizer",
                              ####################
                              # instance variables
                              ####################
                              private = list(
                                .normAgentAlphabet = "character",
                                .normAgent = "factor",
                                .normParameter = "tbl_df"
                              ),
                              ##################
                              # accessor methods
                              ##################
                              active = list(
                                #' @field normAgentAlphabet
                                #' Returns the instance variable normAgentAlphabt.
                                normAgentAlphabet = function(){
                                  return(private$.normAgentAlphabet)
                                },
                                #' @field normAgent
                                #' Returns the instance variable normAgent.
                                #' (character)
                                normAgent = function(){
                                  return(as.character(private$.normAgent))
                                },
                                #' @field setNormAgent
                                #' Sets the instance variable normAgent.
                                #' (character)
                                setNormAgent = function(agent = "character") {
                                  private$.normAgent <- factor(agent, levels = self$normAgentAlphabet)
                                },
                                #' @field normParameter
                                #' Returns the instance variable normParameter.
                                normParameter = function(){
                                  return(private$.normParameter)
                                }
                              ), #active
                              ###################
                              # memory management
                              ###################
                              public = list(
                                #' @description
                                #' Creates and returns a new `pgu.normalizer` object.
                                #' @param data
                                #' The data to be analyzed.
                                #' (tibble::tibble)
                                #' @return
                                #' A new `pgu.normalizer` object.
                                #' (pguIMP::pgu.normalizer)
                                #' @examples
                                #' y <- tibble:tibble()
                                #' x <- pguIMP:pgu.normalizer$new(data = y)
                                initialize = function(data = "tbl_df"){
                                  if(class(data) != "tbl_df"){
                                    data <- tibble::tibble(names <- "none",
                                                           values <- c(NA))
                                  }
                                  private$.trafoAlphabet <-c("none", "min-max", "mean", "z-score")
                                  self$setNormAgent <- "none"
                                  self$detectNormParameter(data)
                                }, #function

                                #' @description
                                #' Clears the heap and
                                #' indicates that instance of `pgu.normalizer` is removed from heap.
                                finalize = function(){
                                  print("Instance of pgu.normalizer removed from heap")
                                }, #function

                                ##########################
                                # print instance variables
                                ##########################
                                #' @description
                                #' Prints instance variables of a `pgu.normalizer` object.
                                #' @return
                                #' string
                                #' @examples
                                #' x$print()
                                #' print(x)
                                print = function(){
                                  rString <- sprintf("\npgu.normalizer\n")
                                  cat(rString)
                                  cat("normAgentAlphabet:\n")
                                  print(private$.normAgentAlphabet)
                                  cat("\nnormParameter:\n")
                                  print(private$.normParameter)
                                  cat("\n\n")
                                  invisible(self)
                                }, #function

                                #' @description
                                #' Resets instance variable `trafoParameter`
                                #' @param data
                                #' Dataframe to be analyzed.
                                #' (tibble::tibble)
                                #' @examples
                                #' x$resetTrafoParameter(data)
                                detectNormParameter = function(data  = "tbl_df"){
                                  features <- data %>%
                                    dplyr::select_if(is.numeric) %>%
                                    colnames()
                                  #dplyr::summarise

                                }, #function

                              )#public
)#class
