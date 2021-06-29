#' @title pgu.model
#'
#' @description
#' Comprises a list of models for data manipulation.
#'
#' @details
#' Comprises a list of pgu.normDist objects and model parameters.
#' These can be used to scale data.
#' This object is used by the shiny based gui and is not for use in individual R-scripts!
#'
#' @format [R6::R6Class] object.
#'
#' @importFrom dplyr select select_if slice
#' @importFrom ggplot2 coord_flip ggplot layer_scales scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom gridExtra grid.arrange
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @importFrom shiny Progress
#' @importFrom tibble tibble
#'
#' @include pguNormDist.R
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pgu.model <- R6::R6Class("pgu.model",
                              ####################
                              # instance variables
                              ####################
                               private = list(
                                 .modelList = "pgu.normDist",
                                 .modelParameter = "tbl_df"
                               ),
                              ##################
                              # accessor methods
                              ##################
                               active = list(
                                 #' @field modelList
                                 #' Returns a vector of pgu-normDist objects.
                                 #' (pgu.normDist)
                                 modelList = function(){
                                   return(private$.modelList)
                                 },
                                 #' @field modelParameter
                                 #' Returns a dataframe comrising model parameters.
                                 #' (tibble::tibble)
                                 modelParameter = function(){
                                   return(private$.modelParameter)
                                 }
                                 ),
                              ###################
                              # memory management
                              ###################
                               public = list(
                                 #' @description
                                 #' Creates and returns a new `pgu.model` object.
                                 #' @param data
                                 #' The data to be analyzed.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' A new `pgu.model` object.
                                 #' (pguIMP::pgu.model)
                                 initialize = function(data = "tbl_df"){
                                   if(class(data)[1] != "tbl_df"){
                                     data <- tibble::tibble(names <- "none",
                                                            values <- c(NA))
                                   }
                                   self$resetModel(data)
                                 }, #function

                                 #' @description
                                 #' Clears the heap and
                                 #' indicates that instance of `pgu.model` is removed from heap.
                                 finalize = function(){
                                   print("Instance of pgu.model removed from heap")
                                 }, #function
                                 ############################
                                 # print instance variables #
                                 ############################
                                 #' @description
                                 #' Prints instance variables of a `pgu.model` object.
                                 #' @return
                                 #' string
                                 print = function(){
                                   rString <- sprintf("\npgu.model\n")
                                   cat(rString)
                                   cat("modelParameter:\n")
                                   print(self$modelParameter)
                                   cat("\n\n")
                                   invisible(self)
                                 }, #function

                                 ####################
                                 # public functions #
                                 ####################
                                 #' @description
                                 #' Resets instance variable `modelParameter`
                                 #' @param data
                                 #' Dataframe to be analyzed.
                                 #' (tibble::tibble)
                                 resetModelParameter = function(data  = "tbl_df"){
                                   features <- data %>%
                                     dplyr::select_if(is.numeric) %>%
                                     colnames()

                                   mu <- c(rep(0.0, length(features)))
                                   sigma <- c(rep(0.0, length(features)))
                                   # fit quality
                                   dataPoints <- c(rep(0.0, length(features)))
                                   logLikelihood <-  c(rep(0.0, length(features)))
                                   degOfFreedom <- c(rep(0.0, length(features)))
                                   bic <- c(rep(0.0, length(features)))
                                   aic <- c(rep(0.0, length(features)))
                                   aicc <- c(rep(0.0, length(features)))
                                   rmse <- c(rep(0.0, length(features)))
                                   # shapiro wilk test parameters
                                   w.shapiro <- c(rep(0.0, length(features)))
                                   p.shapiro <- c(rep(0.0, length(features)))
                                   # kolmogorow smirnow test parameters
                                   d.kolmogorow <- c(rep(0.0, length(features)))
                                   p.kolmogorow <- c(rep(0.0, length(features)))
                                   # anderson darling test parameters
                                   a.anderson <- c(rep(0.0, length(features)))
                                   p.anderson <- c(rep(0.0, length(features)))
                                   # create modelParameter
                                   private$.modelParameter <- tibble::tibble(features, mu, sigma, dataPoints, logLikelihood, degOfFreedom, bic, aic, aicc, rmse,
                                                                             w.shapiro, p.shapiro, d.kolmogorow, p.kolmogorow, a.anderson, p.anderson)
                                 }, #function

                                 #' @description
                                 #' Resets instance variable `modelList`
                                 #' @param data
                                 #' Dataframe to be analyzed.
                                 #' (tibble::tibble)
                                 resetModelList = function(data = "tbl_df"){
                                   results=list()
                                   for (feature in self$modelParameter[["features"]]){
                                     idx <- self$featureIdx(feature)
                                     if(!is.na(idx)){
                                       results[feature] <- list(pgu.normDist$new(data[feature]))
                                     }#if
                                   }#for
                                   private$.modelList <- results
                                 }, #function

                                 #' @description
                                 #' Resets instance variable `modelList`.
                                 #' Resets instance variable `modelParameter`.
                                 #' Displays progress if shiny is loaded.
                                 #' @param data
                                 #' Dataframe to be analyzed.
                                 #' (tibble::tibble)
                                 #' @param progress
                                 #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                                 #' (shiny::Progress)
                                 resetModel = function(data = "tbl_df", progress = "Progress"){
                                   self$resetModelParameter(data)
                                   self$resetModelList(data)
                                   self$fitData(progress)
                                 }, #function

                                 #' @description
                                 #' Stores the information of a pgu.norDist object in an
                                 #' entry of the instance variable `modelList`
                                 #' @param data
                                 #' Instance of pgu.normDist
                                 #' (pguIMP::pgu.normDist)
                                 #' @param feature
                                 #' Attribute corresponding to the pgu.normDist object data.
                                 #' (character)
                                 setNormDist = function(data = "pgu.normDist", feature = "character"){
                                   idx <- match(feature, self$modelParameter[["features"]])
                                   if(!is.na(idx)){
                                     private$.modelList[feature] <- list(data)
                                     self$logFitResultsFeature(feature)
                                   }#if
                                 }, #function

                                 ####################
                                 # helper functions #
                                 ####################
                                 #' @description
                                 #' Returns the index of a pgu.normDist object wihtin the instance variable `modelParameter`.
                                 #' @param feature
                                 #' Attribute's name.
                                 #' (character)
                                 #' @return
                                 #' Index of attribute entry in dataframe
                                 #' (numeric)
                                 featureIdx = function(feature = "character"){
                                   idx <- match(feature, self$modelParameter[["features"]])
                                   if(is.na(idx)){
                                     rString <- sprintf("\nWarning in pgu.model: feature %s is not known\n",
                                                        feature)
                                     cat(rString)
                                   }#if
                                   return(idx)
                                 }, #function

                                 #################
                                 # fit functions #
                                 #################
                                 #' @description
                                 #' Runs the fit function of a pgu.normDist object at a
                                 #' user denied position within the instance variable modelList.
                                 #' @param feature
                                 #' Attribute's name.
                                 #' (character)
                                 fitFeature = function(feature = "character"){
                                   idx <- match(feature, self$modelParameter[["features"]])
                                   if(!is.na(idx)){
                                     tryCatch({
                                       private$.modelList[[feature]]$fit()
                                       self$logFitResultsFeature(feature)
                                     },
                                     error = function(e){
                                       self$logFailedFitResultsFeature(feature)
                                       print(e)
                                     }
                                     )#tryCatch
                                   }#if
                                 }, #function

                                 #' @description
                                 #' Loops through all attributes and calls the object's
                                 #' ftiFeature function.
                                 #' Displays progress if shiny is loaded.
                                 #' @param progress
                                 #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                                 #' (shiny::Progress)
                                 fitData = function(progress = "Progress"){
                                   for (feature in self$modelParameter[["features"]]){
                                     if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                       progress$inc(1)
                                     }#if
                                     idx <- self$featureIdx(feature)
                                     if(!is.na(idx)){
                                       self$fitFeature(feature)
                                     }#if
                                   }#for
                                 }, #function

                                 ###############
                                 # log results #
                                 ###############
                                 #' @description
                                 #' Stores results from fitting procedure of a user defined attribute
                                 #' into the corrsponding attribute of instance variable `modelParameter`.
                                 #' @param feature
                                 #' Attribute's name.
                                 #' (character)
                                 logFitResultsFeature = function(feature = "character"){
                                   idx <- self$featureIdx(feature)
                                   if(!is.na(idx)){
                                     private$.modelParameter[idx, "mu"] <- private$.modelList[[feature]]$expMu
                                     private$.modelParameter[idx, "sigma"] <- private$.modelList[[feature]]$expSigma
                                     private$.modelParameter[idx, "logLikelihood"] <- private$.modelList[[feature]]$logLikelihood
                                     private$.modelParameter[idx, "dataPoints"] <- private$.modelList[[feature]]$dataPoints
                                     private$.modelParameter[idx, "degOfFreedom"] <- private$.modelList[[feature]]$degOfFreedom
                                     private$.modelParameter[idx, "bic"] <- private$.modelList[[feature]]$bic
                                     private$.modelParameter[idx, "aic"] <- private$.modelList[[feature]]$aic
                                     private$.modelParameter[idx, "aicc"] <- private$.modelList[[feature]]$aicc
                                     private$.modelParameter[idx, "rmse"] <- private$.modelList[[feature]]$rmse
                                     private$.modelParameter[idx, "w.shapiro"] <- private$.modelList[[feature]]$w.shapiro
                                     private$.modelParameter[idx, "p.shapiro"] <- private$.modelList[[feature]]$p.shapiro
                                     private$.modelParameter[idx, "d.kolmogorow"] <- private$.modelList[[feature]]$d.kolmogorow
                                     private$.modelParameter[idx, "p.kolmogorow"] <- private$.modelList[[feature]]$p.kolmogorow
                                     private$.modelParameter[idx, "a.anderson"] <- private$.modelList[[feature]]$a.anderson
                                     private$.modelParameter[idx, "p.anderson"] <- private$.modelList[[feature]]$p.anderson
                                   }#if
                                 }, #feature

                                 #' @description
                                 #' Stores results from fitting procedure of a user defined attribute
                                 #' into the corrsponding attribute of instance variable `modelParameter`
                                 #' in case of a failed fitting routine.
                                 #' @param feature
                                 #' Attribute's name.
                                 #' (character)
                                 logFailedFitResultsFeature = function(feature = "character"){
                                   idx <- self$featureIdx(feature)
                                   if(!is.na(idx)){
                                     private$.modelParameter[idx, "mu"] <- NA
                                     private$.modelParameter[idx, "sigma"] <- NA
                                     private$.modelParameter[idx, "logLikelihood"] <- NA
                                     private$.modelParameter[idx, "dataPoints"] <- NA
                                     private$.modelParameter[idx, "degOfFreedom"] <- NA
                                     private$.modelParameter[idx, "bic"] <- NA
                                     private$.modelParameter[idx, "aic"] <- NA
                                     private$.modelParameter[idx, "aicc"] <- NA
                                     private$.modelParameter[idx, "rmse"] <- NA
                                     private$.modelParameter[idx, "w.shapiro"] <- NA
                                     private$.modelParameter[idx, "p.shapiro"] <- NA
                                     private$.modelParameter[idx, "d.kolmogorow"] <- NA
                                     private$.modelParameter[idx, "p.kolmogorow"] <- NA
                                     private$.modelParameter[idx, "a.anderson"] <- NA
                                     private$.modelParameter[idx, "p.anderson"] <- NA
                                   }#if
                                 }, #function

                                 ##############
                                 # scale data #
                                 ##############
                                 #' @description
                                 #' Scales numeric data based upon the model of a user defined attribute.
                                 #' @param value
                                 #' Numeric vector
                                 #' (numeric)
                                 #' @param feature
                                 #' Attribute's name.
                                 #' (character)
                                 #' @return
                                 #' scaled version of the given vector
                                 #' (numeric)
                                 scaleNumeric = function(value = "numeric", feature = "character"){
                                   idx <- self$featureIdx(feature)
                                   if(!is.na(idx)){
                                     mu <- private$.modelParameter[[idx, "mu"]]
                                     sigma <- private$.modelParameter[[idx, "sigma"]]
                                     value <- (value - mu)/sigma
                                   }#if
                                   return(value)
                                 }, #feature

                                 #' @description
                                 #' Scales a dataframe based upon  a list of models
                                 #' stored in the instance variable modelList..
                                 #' @param data
                                 #' Dataframe to be analyzed.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' scaled version of the given dataframe
                                 #' (tibble::tibble)
                                 scaleData = function(data = "tbl_df"){
                                   for (feature in self$modelParameter[["features"]]){
                                     idx <- self$featureIdx(feature)
                                     if(!is.na(idx)){
                                       data[feature] <- self$scaleNumeric(data[[feature]], feature)
                                     }#if
                                   }#for
                                   return(data)
                                 }, #function

                                 ################
                                 # rescale data #
                                 ################
                                 #' @description
                                 #' Re-scales numeric data based upon the model of a user defined attribute.
                                 #' @param value
                                 #' Numeric vector
                                 #' (numeric)
                                 #' @param feature
                                 #' Attribute's name.
                                 #' (character)
                                 #' @return
                                 #' Re-scaled version of the given vector
                                 #' (numeric)
                                 rescaleNumeric = function(value = "numeric", feature = "character"){
                                   idx <- self$featureIdx(feature)
                                   if(!is.na(idx)){
                                     mu <- private$.modelParameter[[idx, "mu"]]
                                     sigma <- private$.modelParameter[[idx, "sigma"]]
                                     value <- (value * sigma) + mu
                                   }#if
                                   return(value)
                                 }, #function

                                 #' @description
                                 #' Re-scales a dataframe based upon  a list of models
                                 #' stored in the instance variable modelList..
                                 #' @param data
                                 #' Dataframe to be analyzed.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' Re-scaled version of the given dataframe
                                 #' (tibble::tibble)
                                 rescaleData = function(data = "tbl_df"){
                                   for (feature in self$modelParameter[["features"]]){
                                     idx <- self$featureIdx(feature)
                                     if(!is.na(idx)){
                                       data[feature] <- self$rescaleNumeric(data[[feature]], feature)
                                     }#if
                                   }#for
                                   return(data)
                                 }, #function

                                 ##################
                                 # return results #
                                 ##################
                                 #' @description
                                 #' Returns the model parameter (expectation value, standard deviation).
                                 #' @return
                                 #' Dataframe comprising model parameter.
                                 #' (tibble::tibble)
                                 modelParameterData = function(){
                                   self$modelParameter %>%
                                     dplyr::select(features, mu:sigma) %>%
                                     return()
                                 }, #function

                                 #' @description
                                 #' Returns the model parameter (expectation value, standard deviation)
                                 #' for a user deined attribute.
                                 #' @param feature
                                 #' Attribute's name.
                                 #' (character)
                                 #' @return
                                 #' Dataframe comprising model parameter.
                                 #' (tibble::tibble)
                                 modelParameterFeature = function(feature = "character"){
                                   idx <- self$featureIdx(feature)
                                   self$modelParameter %>%
                                     dplyr::slice(idx) %>%
                                     dplyr::select(features, mu:sigma) %>%
                                     return()
                                 }, #function

                                 #' @description
                                 #' Returns the model parameters connected to model quality.
                                 #' @return
                                 #' Dataframe comprising model parameter.
                                 #' (tibble::tibble)
                                 modelQualityData = function(){
                                   self$modelParameter %>%
                                     dplyr::select(features, dataPoints:rmse) %>%
                                     return()
                                 }, #function

                                 #' @description
                                 #' Returns the model parameters connected to model quality
                                 #' for a user deined attribute.
                                 #' @param feature
                                 #' Attribute's name.
                                 #' (character)
                                 #' @return
                                 #' Dataframe comprising model parameter.
                                 #' (tibble::tibble)
                                 modelQualityFeature = function(feature = "character"){
                                   idx <- self$featureIdx(feature)
                                   self$modelParameter %>%
                                     dplyr::slice(idx) %>%
                                     dplyr::select(features, dataPoints:rmse) %>%
                                     return()
                                 }, #function

                                 #' @description
                                 #' Returns the model fit results.
                                 #' @return
                                 #' Dataframe comprising model fit results.
                                 #' (tibble::tibble)
                                 fitResultData = function(){
                                   self$modelParameter %>%
                                     dplyr::select(features, mu:rmse) %>%
                                     return()
                                 }, #function

                                 #' @description
                                 #' Returns the model fit results
                                 #' for a user deined attribute.
                                 #' @param feature
                                 #' Attribute's name.
                                 #' (character)
                                 #' @return
                                 #' Dataframe comprising model fit results.
                                 #' (tibble::tibble)
                                 fitResultFeature = function(feature = "character"){
                                   idx <- self$featureIdx(feature)
                                   self$modelParameter %>%
                                     dplyr::slice(idx) %>%
                                     dplyr::select(features, mu:rmse) %>%
                                     return()
                                 }, #function

                                 #' @description
                                 #' Returns the hypothesis test results.
                                 #' @return
                                 #' Dataframe comprising the hypothesis test results.
                                 #' (tibble::tibble)
                                 testResultData = function(){
                                   self$modelParameter %>%
                                     dplyr::select(features, w.shapiro:p.anderson) %>%
                                     return()
                                 }, #function

                                 #' @description
                                 #' Returns the hypothesis test results.
                                 #' for a user deined attribute.
                                 #' @param feature
                                 #' Attribute's name.
                                 #' (character)
                                 #' @return
                                 #' Dataframe comprising the hypothesis test results.
                                 #' (tibble::tibble)
                                 testResultFeature = function(feature = "character"){
                                   idx <- self$featureIdx(feature)
                                   self$modelParameter %>%
                                     dplyr::slice(idx) %>%
                                     dplyr::select(features, w.shapiro:p.anderson) %>%
                                     return()
                                 }, #function

                                 ################
                                 # plot results #
                                 ################
                                 #' @description
                                 #' Creates and returns a composite graphical analysis
                                 #' of the modeling procedure of
                                 #' a user defined attribute.
                                 #' @param feature
                                 #' Attribute's name.
                                 #' (character)
                                 #' @return
                                 #' Composite result plot.
                                 #' (ggplot2::ggplot)
                                 plotModel = function(feature = "character"){
                                   idx <- self$featureIdx(feature)
                                   model <- self$modelList[[idx]]
                                   p1 <- model$plotHistogram()
                                   xLimits <- ggplot2::layer_scales(p1)$x$range$range
                                   p2 <- model$plotResiduals() +
                                     ggplot2::scale_x_continuous(position = "bottom", limits=xLimits)
                                   p3 <- model$plotResidualDist() +
                                     ggplot2::coord_flip()
                                   yLimits <- ggplot2::layer_scales(p3)$x$range$range
                                   p2 <- p2 +
                                     ggplot2::scale_y_continuous(limits=yLimits)

                                   p4 <- model$normalQQPlot()
                                   p <- gridExtra::grid.arrange(p1,p2,p3,p4, layout_matrix = rbind(c(1,1,4),c(2,2,3))) %>%
                                     suppressMessages()
                                   return(p)
                                 }#function
                               )#public
)#class
