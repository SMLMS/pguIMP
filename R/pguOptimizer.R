#' @title pgu.optimizer
#'
#' @description
#' Finds the transformation models that result in distributions that come closest to a normal distribution.
#'
#' @details
#' Analysis is performed individually on each attribute.
#' This object is used by the shiny based gui and is not for use in individual R-scripts!
#'
#' @format [R6::R6Class] object.
#'
#' @importFrom dplyr mutate select_if
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @importFrom shiny Progress
#' @importFrom tibble tibble
#' @importFrom tidyr separate unite
#'
#' @include pguTransformator.R
#' @include pguModel.R
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pgu.optimizer <- R6::R6Class("pgu.optimizer",
                              ####################
                              # instance variables
                              ####################
                              private = list(
                                .features = "character",
                                .trafoAlphabet = "character",
                                .mirror = "logical",
                                .optParameter = "tbl_df",
                                .optTypes = "tbl_df"
                              ),
                              ##################
                              # accessor methods
                              ##################
                              active = list(
                                #' @field features
                                #' Returns the instance variable features.
                                #' (character)
                                features = function(){
                                  return(private$.features)
                                },
                                #' @field trafoAlphabet
                                #' Returns the instance variable trafoAlphabet.
                                #' (character)
                                trafoAlphabet = function(){
                                  return(private$.trafoAlphabet)
                                },
                                #' @field setTrafoAlphabet
                                #' Sets the instance variable trafoAlphabet to data.
                                #' (character)
                                setTrafoAlphabet = function(data = "character"){
                                  private$.trafoAlphabet <- data
                                },
                                #' @field mirror
                                #' Returns the instance variable mirror
                                #' (logical)
                                mirror = function(){
                                  return(private$.mirror)
                                },
                                #' @field setMirror
                                #' Sets the instance variable mirror to data
                                #' (logical)
                                setMirror = function(data = "logical"){
                                  private$.mirror <- data
                                  self$resetOptParameter()
                                  self$resetOptTypes()
                                },
                                #' @field optParameter
                                #' Returns the instance variable optParameter
                                #' (tibble::tibble)
                                optParameter = function(){
                                  return(private$.optParameter)
                                },
                                #' @field optTypes
                                #' Returns the instance variable optTypes
                                #' (tibble::tibble)
                                optTypes = function(){
                                  return(private$.optTypes)
                                }
                              ),
                              ###################
                              # memory management
                              ###################
                              public = list(
                                #' @description
                                #' Creates and returns a new `pgu.optimizer` object.
                                #' @param data
                                #' The data to be analyzed.
                                #' (tibble::tibble)
                                #' @return
                                #' A new `pgu.optimizer` object.
                                #' (pguIMP::pgu.optimizer)
                                initialize = function(data = "tbl_df"){
                                  if(class(data) != "tbl_df"){
                                    data <- tibble::tibble(names <- "none",
                                                           values <- c(NA))
                                  }#if
                                  self$resetOptimizer(data)
                                }, #function
                                #' @description
                                #' Clears the heap and
                                #' indicates that instance of `pgu.optimizer` is removed from heap.
                                finalize = function(){
                                  print("Instance of pgu.optimizer removed from heap")
                                }, #function
                                ##########################
                                # print instance variables
                                ##########################
                                #' @description
                                #' Prints instance variables of a `pgu.optimizer` object.
                                #' @return
                                #' string
                                print = function(){
                                  rString <- sprintf("\npgu.optimizer\n")
                                  cat(rString)
                                  cat("\ntransformatins\n")
                                  print(private$.trafoAlphabet)
                                  cat("\nmirror\n")
                                  print(private$.mirror)
                                  cat("\noptParameter\n")
                                  print(private$.optParameter)
                                  cat("\noptTypes\n")
                                  print(private$.optTypes)
                                  cat("\n\n")
                                  invisible(self)
                                }, #function

                                ####################
                                # public functions #
                                ####################
                                #' @description
                                #' Extract the attribute names from the given data frame
                                #' and stores them in the class' instance variable
                                #' features,
                                #' @param data
                                #' The data to be analyzed.
                                #' (tibble::tibble)
                                resetFeatures = function(data = "tbl_df"){
                                  private$.features <- data %>%
                                    dplyr::select_if(is.numeric) %>%
                                    colnames()
                                }, #function

                                #' @description
                                #' Initializes the instance variable optParameter.
                                resetOptParameter = function(){
                                  features <- self$features
                                  mirrorLogic <- c(rep(FALSE, length(features)))
                                  if(self$mirror){
                                    features <- append(features, features)
                                    mirrorLogic <- append(mirrorLogic, c(rep(TRUE, length(self$features))))
                                  }#if
                                  logLikelihood <- as.numeric(c(rep(0, length(features))))
                                  bic <- as.numeric(c(rep(0, length(features))))
                                  aic <- as.numeric(c(rep(0, length(features))))
                                  aicc <- as.numeric(c(rep(0, length(features))))
                                  rmse <- as.numeric(c(rep(0, length(features))))
                                  w.shapiro <- as.numeric(c(rep(0, length(features))))
                                  p.shapiro <- as.numeric(c(rep(0, length(features))))
                                  d.kolmogorow <- as.numeric(c(rep(0, length(features))))
                                  p.kolmogorow <- as.numeric(c(rep(0, length(features))))
                                  a.anderson <- as.numeric(c(rep(0, length(features))))
                                  p.anderson <- as.numeric(c(rep(0, length(features))))
                                  private$.optParameter <- tibble::tibble(features, mirrorLogic, logLikelihood, bic, aic, aicc, rmse,
                                                                          w.shapiro, p.shapiro, d.kolmogorow, p.kolmogorow, a.anderson, p.anderson)
                                }, #function

                                #' @description
                                #' Initializes the instance variable optTypes.
                                resetOptTypes = function(){
                                  features <- self$features
                                  mirrorLogic <- c(rep(FALSE, length(features)))
                                  if(self$mirror){
                                    features <- append(features, features)
                                    mirrorLogic <- append(mirrorLogic, c(rep(TRUE, length(self$features))))
                                  }#if
                                  logLikelihood <- c(rep("none", length(features)))
                                  bic <- c(rep("none", length(features)))
                                  aic <- c(rep("none", length(features)))
                                  aicc <- c(rep("none", length(features)))
                                  rmse <- c(rep("none", length(features)))
                                  w.shapiro <- c(rep("none", length(features)))
                                  p.shapiro <- c(rep("none", length(features)))
                                  d.kolmogorow <- c(rep("none", length(features)))
                                  p.kolmogorow <- c(rep("none", length(features)))
                                  a.anderson <- c(rep("none", length(features)))
                                  p.anderson <- c(rep("none", length(features)))
                                  private$.optTypes <- tibble::tibble(features, mirrorLogic, logLikelihood, bic, aic, aicc, rmse,
                                                                      w.shapiro, p.shapiro, d.kolmogorow, p.kolmogorow, a.anderson, p.anderson)
                                }, #function

                                #' @description
                                #' Initializes the optimizer instance variables.
                                #' Here, initialization defines a consecutive sequence of the class' functions:
                                #' resetFeatures, setTrafoAlphabet, setMirror, resetOptParameter and resetOptTypes.
                                #' @param data
                                #' The data to be analyzed.
                                #' (tibble::tibble)
                                resetOptimizer = function(data = "tbl_df"){
                                  self$resetFeatures(data)
                                  self$setTrafoAlphabet <- c("none", "log2", "logNorm", "log10", "squareRoot", "cubeRoot", "arcsine", "inverse")
                                  self$setMirror <- FALSE
                                  self$resetOptParameter()
                                  self$resetOptTypes()
                                }, #function

                                ####################
                                # helper functions #
                                ####################
                                #' @description
                                #' Determines the numerical index of the column of an attribute based on the attribute name.
                                #' @param feature
                                #' The attribute's name.
                                #' (character)
                                #' @return
                                #' The attributes column index.
                                #' (numeric)
                                featureIdx = function(feature = "character"){
                                  idx <- match(feature, self$features)
                                  if(is.na(idx)){
                                    rString <- sprintf("\nWarning in pgu.optimizer: feature %s is not known\n",
                                                       feature)
                                    cat(rString)
                                  }#if
                                  return(idx)
                                }, #function

                                #' @description
                                #' Compares a model parameter to a reference parameter and tests, if the model parameter is bigger.
                                #' @param modelParameter
                                #' The model parameter
                                #' (numeric)
                                #' @param referenceParameter
                                #' The reference parameter
                                #' (numeric)
                                #' @return
                                #' Test Result
                                #' (logical)
                                modelParameterIsBigger = function(modelParameter = "numeric", referenceParameter = "numeric"){
                                  result <- FALSE
                                  if ((!is.na(modelParameter)) &&
                                      ((referenceParameter < modelParameter) ||
                                       (is.na(referenceParameter)))){
                                    result <- TRUE
                                  }#if
                                  return(result)
                                }, #function

                                #' @description
                                #' Compares a model parameter to a reference parameter and tests, if the model parameter is smaller.
                                #' @param modelParameter
                                #' The model parameter
                                #' (numeric)
                                #' @param referenceParameter
                                #' The reference parameter
                                #' (numeric)
                                #' @return
                                #' Test Result
                                #' (logical)
                                modelParameterIsSmaller = function(modelParameter = "numeric", referenceParameter = "numeric"){
                                  result <- FALSE
                                  if ((!is.na(modelParameter)) &&
                                      ((referenceParameter > modelParameter) ||
                                       (is.na(referenceParameter)))){
                                    result <- TRUE
                                  }#if
                                  return(result)
                                }, #function

                                #######################
                                # iteration functions #
                                #######################
                                #' @description
                                #' Takes an instance of the pgu.transfromator class and
                                #' sets the transformation type to a user defined value.
                                #' @param transformator
                                #' An instance of the pgu.transformator class
                                #' (pguIMP::pgu.transformator)
                                #' @param type
                                #' A transfromation type
                                #' (character)
                                #' @return
                                #' An updated instance of the pgu.transformator class
                                #' (pguIMP::pgu.transformator)
                                updateTrafoType = function(transformator = "pgu.transformator", type = "character"){
                                  for (feature in self$features){
                                    transformator$setTrafoType(feature, type)
                                  }#for
                                  return(transformator)
                                }, #function

                                #' @description
                                #' Takes an instance of the pgu.transfromator class and
                                #' sets the mirrorLogic parameter to a user defined value.
                                #' @param transformator
                                #' An instance of the pgu.transformator class
                                #' (pguIMP::pgu.transformator)
                                #' @param logic
                                #' The mirrorLogic parameter
                                #' (logic)
                                #' @return
                                #' An updated instance of the pgu.transformator class
                                #' (pguIMP::pgu.transformator)
                                updateMirrorLogic = function(transformator = "pgu.transformator", logic = "logical"){
                                  for (feature in self$features){
                                    transformator$setMirrorLogic(feature, logic)
                                  }# for
                                  return(transformator)
                                }, #function

                                ####################
                                # update Functions #
                                ####################
                                #' @description
                                #' Takes an instance of the pgu.model class and analyzes it.
                                #' Keeps track of the optimal model parameters during optimization
                                #' and stores them in the instance variables optTypes and optParameter.
                                #' @param model
                                #' An instance of the pgu.model class
                                #' (pguIMP::pgu.model)
                                #' @param type
                                #' A transfromation type
                                #' (character)
                                #' @param logic
                                #' The mirrorLogic parameter
                                #' (logic)
                                updateOptParameter = function(model = "pgu.model", type = "character", logic = "character"){
                                  modelParameter <- model$modelParameter
                                  referenceParameter <- self$optParameter %>%
                                    tidyr::unite(features, features, mirrorLogic, sep="//")
                                  referenceTypes <- self$optTypes %>%
                                    tidyr::unite(features, features, mirrorLogic, sep="//")
                                  for(feature in model$modelParameter[["features"]]){
                                    referenceFeature <- paste(feature, as.character(logic), sep = "//")
                                    referenceIdx <- match(referenceFeature, referenceTypes[["features"]])
                                    modelIdx = match(feature, modelParameter[["features"]])
                                    for (test in c("logLikelihood", "p.shapiro", "p.kolmogorow", "p.anderson", "w.shapiro")){
                                      if (self$modelParameterIsBigger(modelParameter[modelIdx, test],
                                                                      referenceParameter[referenceIdx, test])){
                                        referenceTypes[referenceIdx, test] <- type
                                        referenceParameter[referenceIdx, test] <- modelParameter[modelIdx, test]
                                      }#if
                                    }#for
                                    for (test in c("bic", "aic", "aicc", "rmse", "d.kolmogorow", "a.anderson")){
                                      if (self$modelParameterIsSmaller(modelParameter[modelIdx, test],
                                                                       referenceParameter[referenceIdx, test])){
                                        referenceTypes[referenceIdx, test] <- type
                                        referenceParameter[referenceIdx, test] <- modelParameter[modelIdx, test]
                                      }#if
                                    }#for
                                  }#for
                                  private$.optParameter <- referenceParameter %>%
                                    tidyr::separate(features, into = c("features", "mirrorLogic"), sep="//") %>%
                                    dplyr::mutate(mirrorLogic = as.logical(mirrorLogic))

                                  private$.optTypes <- referenceTypes %>%
                                    tidyr::separate(features, into = c("features", "mirrorLogic"), sep="//")
                                }, #function

                                ##########################
                                # optimization functions #
                                ##########################
                                #' @description
                                #' Permutates all possible variations of data transfromations and iterates through them.
                                #' Analysis the optimal transformation parameters for each attribute in the data frame and stores them
                                #' in the instance variables optParameter, optTypes.
                                #' @param data
                                #' The data frame to be analyzed.
                                #' (tibble::tibble)
                                #' @param progress
                                #' If shiny is loaded, the analysis' progress is stored within this instance of the shiny Progress class.
                                #' (shiny::Progress)
                                optimize = function(data  = "tbl_df", progress = "Progress"){
                                  transformator <- pgu.transformator$new(data)
                                  mirrorLogic <- c(FALSE)
                                  if (self$mirror){
                                    mirrorLogic <- c(mirrorLogic, TRUE)

                                  }#if
                                  for (logic in mirrorLogic){
                                    transformator <- self$updateMirrorLogic(transformator, logic)
                                    for (type  in self$trafoAlphabet){
                                      if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                        progress$inc(1)
                                      }#iftidyverse,@exam
                                      transformator <- self$updateTrafoType(transformator, type)
                                      transformator$estimateTrafoParameter(data)
                                      model <- pgu.model$new(data %>%
                                                               transformator$mutateData())
                                      model$fitData()
                                      self$updateOptParameter(model, type, logic)
                                    }#for
                                  }#for
                                }, #function

                                #' @description
                                #' Returns information on the optimization progress
                                #' @return
                                #' The data frame comprizing analysis information.
                                #' (tibble::tibble)
                                trafoAlpahbetTblDf = function(){
                                  trafos <- c("none", "log2", "logNorm", "log10", "squareRoot", "cubeRoot", "arcsine", "inverse", "tuckeyLOP", "boxCox")
                                  optimized <- is.element(trafos, self$trafoAlphabet)
                                  mirrored <- c(rep(self$mirror, length(trafos)))
                                  trafoAlphabetTbl <- tibble::tibble(trafos,
                                                                     optimized,
                                                                     mirrored)
                                  return(trafoAlphabetTbl)
                                }#function
                              )#public
)#class
