#' @title pgu.missings
#'
#' @description
#' Detects and substitutes missing values from data set.
#'
#' @details
#' Detects missing values in the transformed and normalized data set.
#'
#' @format [R6::R6Class] object.
#' @section Construction:
#' x <- pguIMP::pgu.missings$new(data)
#'
#' @import R6
#' @import tidyverse
#' @import gridExtra
#' @import DT
#' @import VIM
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pgu.missings <- R6::R6Class("pgu.missings",
                            ####################
                            # instance variables
                            ####################
                            private = list(
                              .imputationParameter = "tbl_df",
                              .imputationSites = "tbl_df",
                              .amv = "ANY"
                            ),
                            ##################
                            # accessor methods
                            ##################
                            active = list(
                              #' @field imputationParameter
                              #' Returns the instance variable outliersParameter.
                              #' (tibble::tibble)
                              imputationParameter = function(){
                                return(private$.imputationParameter)
                              },
                              #' @field imputationSites
                              #' Returns the instance variable imputationSites.
                              #' (tibble::tibble)
                              imputationSites = function(){
                                return(private$.imputationSites)
                              },
                              #' @field amv
                              #' Returns the instance variable amv.
                              #' (numeric)
                              amv = function(){
                                return(private$.amv)
                              }
                            ),
                            ###################
                            # memory management
                            ###################
                            public = list(
                              #' @description
                              #' Creates and returns a new `pgu.missings` object.
                              #' @param data_df
                              #' The data to be cleaned.
                              #' (tibble::tibble)
                              #' @return
                              #' A new `pgu.missings` object.
                              #' (pguIMP::pgu.missings)
                              #' @examples
                              #' y <- tibble:tibble()
                              #' x <- pguIMP:pgu.missings$new(data_df = y)
                              initialize = function(data_df = "tbl_df"){
                                if(!tibble::is_tibble(data_df)){
                                  data_df <- tibble::tibble(names <- "none",
                                                            values <- c(NA))
                                }
                                self$resetImputationParameter(data_df)
                              }, #function

                              #' @description
                              #' Clears the heap and
                              #' indicates that instance of `pgu.missings` is removed from heap.
                              finalize = function(){
                                print("Instance of pgu.missings removed from heap")
                              }, #function

                              ##########################
                              # print instance variables
                              ##########################
                              #' @description
                              #' Prints instance variables of a `pgu.missings` object.
                              #' @return
                              #' string
                              #' @examples
                              #' x$print()
                              #' print(x)
                              print = function(){
                                rString <- sprintf("\npgu.missings\n")
                                cat(rString)
                                print(self$imputationParameter)
                                print(self$imputationSites)
                                cat("\n\n")
                                invisible(self)
                              }, #function

                              ####################
                              # public functions #
                              ####################
                              #' @description
                              #' Resets instance variables and
                              #' identifies missings in the normalized data set.
                              #' @param data_df
                              #' Dataframe to be analyzed.
                              #' (tibble::tibble)
                              #' @examples
                              #' x$resetImputtaionParameter(data_df)
                              resetImputationParameter = function(data_df = "tbl_df"){
                                numericData <- data_df %>%
                                  dplyr::select_if(is.numeric)
                                features <- numericData %>%
                                  colnames()
                                measurements <- c(rep(0.0, length(features)))
                                existings <- c(rep(0.0, length(features)))
                                missings <- c(rep(0.0, length(features)))
                                fractionOfMissings <- c(rep(0.0, length(features)))
                                private$.imputationParameter <- tibble::tibble(features, measurements, existings, missings, fractionOfMissings)
                                private$.amv <- VIM::aggr(numericData, plot=FALSE)
                                self$gatherImputationStatistics(data_df)
                                self$detectImputationSites(data_df)
                              }, #function

                              ####################
                              # helper functions #
                              ####################
                              #' @description
                              #' Returns the position of an attribute within a data frame.
                              #' @param feature
                              #' The attribute's name.
                              #' (character)
                              #' @return
                              #' The postion of the attribute.
                              #' (numeric)
                              #' @examples
                              #' idx <- x$featureIdx(feature = "infected")
                              featureIdx = function(feature = "character"){
                                idx <- match(feature, self$imputationParameter[["features"]])
                                if(is.na(idx)){
                                  rString <- sprintf("\nWarning in pgu.imputation: feature %s is not known\n",
                                                     feature)
                                  cat(rString)
                                }#if
                                return(idx)
                              }, #function

                              #' @description
                              #' Selects features cotaining missing values from a dataset.
                              #' @param data_df
                              #' Dataframe to be analyzed.
                              #' (tibble::tibble)
                              #' @return
                              #' The filtered data frame.
                              #' (tibble::tibble)
                              #' @examples
                              #' idx <- x$filterFeatures(data_df)
                              filterFeatures = function(data_df = "tbl_df"){
                                data_df %>%
                                  dplyr::select(private$.imputationParameter[["features"]]) %>%
                                  return()
                              }, #function

                              #######################
                              # missings statistics #
                              #######################
                              #' @description
                              #' Calculates the number of values of a vector.
                              #' @param value
                              #' A vector comprising numeric data.
                              #' (numeric)
                              #' @return
                              #' The lenght of the vector.
                              #' (numeric)
                              #' @examples
                              #' idx <- x$gatherMeasurements(value)
                              gatherMeasurements = function(value = "numeric"){
                                return(length(value))
                              }, #function

                              #' @description
                              #' Calculates the number of missing values of a vector.
                              #' @param value
                              #' A vector comprising numeric data.
                              #' (numeric)
                              #' @return
                              #' The number of missing in the vector.
                              #' (numeric)
                              #' @examples
                              #' idx <- x$gatherMissings(value)
                              gatherMissings = function(value = "numeric"){
                                y <- sum(is.na(value))
                                return(y)
                              }, #function

                              #' @description
                              #' Calculates the number of existing values of a vector.
                              #' @param value
                              #' A vector comprising numeric data.
                              #' (numeric)
                              #' @return
                              #' The number of existing values in the vector.
                              #' (numeric)
                              #' @examples
                              #' idx <- x$gatherExistings(value)
                              gatherExistings = function(value = "numeric"){
                                y <- sum(!is.na(value))
                                return(y)
                              }, #function

                              #' @description
                              #' Calculates the fraction of missing values of a vector.
                              #' @param value
                              #' A vector comprising numeric data.
                              #' (numeric)
                              #' @return
                              #' The fraction of missing values in the vector.
                              #' (numeric)
                              #' @examples
                              #' idx <- x$gatherFractionOfMissings(value)
                              gatherFractionOfMissings =  function(value = "numeric"){
                                y <- 100.0*sum(is.na(value))/length(value)
                                return(y)
                              }, #function

                              #' @description
                              #' Gathers statistical information about missing values
                              #' that are provided by the classes public `gather` functions.
                              #' The information is stored within the classes instance variable `imputationParameter`
                              #' @param data_df
                              #' The data frame to be analyzed.
                              #' (tibble::tibble)
                              #' @examples
                              #' x$gatherInformationStatistics(data_df)
                              gatherImputationStatistics = function(data_df = "tbl_df"){
                                filteredData <- data_df %>%
                                  self$filterFeatures()
                                private$.imputationParameter["measurements"] <- filteredData %>%
                                  apply(MARGIN=2, FUN=self$gatherMeasurements)
                                private$.imputationParameter["existings"] <- filteredData %>%
                                  apply(MARGIN=2, FUN=self$gatherExistings)
                                private$.imputationParameter["missings"] <- filteredData %>%
                                  apply(MARGIN=2, FUN=self$gatherMissings)
                                private$.imputationParameter["fractionOfMissings"] <- filteredData %>%
                                  apply(MARGIN=2, FUN=self$gatherFractionOfMissings)
                              }, #function

                              ###########################
                              # detect imputation sites #
                              ###########################
                              #' @description
                              #' Detects missing values within the data frame and
                              #' writes the to the instance variable `imputationsites`.
                              #' @param data_df
                              #' The data frame to be analyzed.
                              #' (tibble::tibble)
                              #' @examples
                              #' y <- x$detectImputationsites(data_df)
                              detectImputationSites = function(data_df = "tbl_df"){
                                private$.imputationSites <- data_df %>%
                                  self$filterFeatures() %>%
                                  is.na() %>%
                                  which(arr.ind=TRUE) %>%
                                  tibble::as_tibble() %>%
                                  dplyr::mutate(features = self$imputationParameter[["features"]][col])
                              }, #function


                              ##########
                              # output #
                              ##########
                              #' @description
                              #' Numeric representation of the distribution of missing values within the data frame.
                              #' @param data_df
                              #' The data frame to be analyzed.
                              #' (tibble::tibble)
                              #' @return
                              #' A data frame
                              #' (tibble::tibble)
                              #' @examples
                              #' data %>%
                              #'  x$imputationSiteDistribution() %>%
                              #'  print()
                              imputationSiteDistribution = function(data_df = "tbl_df"){
                                d <- data_df %>%
                                  self$filterFeatures() %>%
                                  as.data.frame() %>%
                                  mice::md.pattern(plot=FALSE)
                                # colnames(d)[-1] <- "Sites"
                                colnames(d)[length(colnames(d))] <- "Sites"
                                rownames(d)[length(rownames(d))] <- "Sum"
                                return(d)
                              }, #function


                              #' @description
                              #' Merges the numeric attributes of the pguIMP data with its metadata.
                              #' @param data_df
                              #' The data frame to be analyzed.
                              #' (tibble::tibble)
                              #' @param metadata_df
                              #' The corresponding metadata.
                              #' (tibble::tibble)
                              #' @return
                              #' A data frame
                              #' (tibble::tibble)
                              #' @examples
                              #' data %>%
                              #'  x$mergeImputationSiteData() %>%
                              #'  print()
                              mergeImputationSiteData = function(data_df = "tbl_df", metadata_df = "tbl_df"){
                                dfMerge <- data_df
                                if(nrow(data_df) == nrow(metadata_df)){
                                  dfMerge <- dplyr::bind_cols(metadata_df, data_df)
                                }#if
                                dfMerge %>%
                                  dplyr::filter_all(dplyr::any_vars(is.na(.))) %>%
                                  return()
                              }, #function

                              ##################
                              # plot functions #
                              ##################
                              #' @description
                              #' Displays the distribution of missing values in form of a heatmap.
                              #' @return
                              #' A heatmap plot.
                              #' (ggplot2::ggplot)
                              #' @examples
                              #' x$imputationSiteHeatMap() %>%
                              #'  show()
                              imputationSiteHeatMap = function(){
                                p <- plot(self$amv,
                                          col=c('navyblue','red'),
                                          numbers=TRUE,
                                          sortVars=TRUE,
                                          labels=self$imputationParameter[["features"]],
                                          cex.axis=.7,
                                          gap=3,
                                          ylab=c("Histogram of imputation sites","Pattern"))
                                return(p)
                              } #function
                            )#public
)#class
