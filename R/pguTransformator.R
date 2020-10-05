#' @title pgu.transformator
#'
#' @description
#' Transforms the data of pguIMP.
#'
#' @details
#' Performs a data transfromation in order to achieve a normally distributed version of the dataframe.
#'
#' @format [R6::R6Class] object.
#' @section Construction:
#' x <- pguIMP::pgu.transformator$new(data)
#'
#' @import R6
#' @import tidyverse
#' @import rcompanion
#' @import MASS
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pgu.transformator <- R6::R6Class("pgu.transformator",
                              ####################
                              # instance variables
                              ####################
                               private = list(
                                 .trafoAlphabet = "character",
                                 .trafoParameter = "tbl_df"
                               ),
                              ##################
                              # accessor methods
                              ##################
                               active = list(
                                 #' @field trafoAlphabet
                                 #' Returns the instance variable trafoAlphabte.
                                 trafoAlphabet = function(){
                                   return(private$.trafoAlphabet)
                                 },
                                 #' @field trafoParameter
                                 #' Returns the instance variable trafoParameter.
                                 trafoParameter = function(){
                                   return(private$.trafoParameter)
                                 }
                                 ),
                              ###################
                              # memory management
                              ###################
                               public = list(
                                 #' @description
                                 #' Creates and returns a new `pgu.transformator` object.
                                 #' @param data
                                 #' The data to be analyzed.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' A new `pgu.transformator` object.
                                 #' (pguIMP::pgu.transformator)
                                 #' @examples
                                 #' y <- tibble:tibble()
                                 #' x <- pguIMP:pgu.transformator$new(data = y)
                                 initialize = function(data = "tbl_df"){
                                   if(class(data) != "tbl_df"){
                                     data <- tibble::tibble(names <- "none",
                                                            values <- c(NA))
                                   }
                                   private$.trafoAlphabet <-c("none", "log2", "logNorm", "log10", "squareRoot", "cubeRoot", "arcsine", "inverse", "tukeyLOP", "boxCox")
                                   self$resetTrafoParameter(data)
                                 }, #function

                                 #' @description
                                 #' Clears the heap and
                                 #' indicates that instance of `pgu.transformator` is removed from heap.
                                 finalize = function(){
                                   print("Instance of pgu.transformator removed from heap")
                                 },
                                 ##########################
                                 # print instance variables
                                 ##########################
                                 #' @description
                                 #' Prints instance variables of a `pgu.transformator` object.
                                 #' @return
                                 #' string
                                 #' @examples
                                 #' x$print()
                                 #' print(x)
                                 print = function(){
                                   rString <- sprintf("\npgu.transformator\n")
                                   cat(rString)
                                   cat("trafoAlphabet:\n")
                                   print(private$.trafoAlphabet)
                                   cat("\ntrafoParameter:\n")
                                   print(private$.trafoParameter)
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
                                 resetTrafoParameter = function(data  = "tbl_df"){
                                   features <- data %>%
                                     dplyr::select_if(is.numeric) %>%
                                     colnames()
                                   trafoType <- c(rep(self$trafoAlphabet[1], length(features))) %>%
                                     factor(level = self$trafoAlphabet)
                                   mirrorLogic <- c(rep(FALSE, length(features )))
                                   addConst <- c(rep(0.0, length(features )))
                                   lambda <- c(rep(1.0, length(features )))
                                   private$.trafoParameter <- tibble::tibble(features = as.character(features),
                                                                             trafoType = as.factor(trafoType),
                                                                             addConst = as.numeric(addConst),
                                                                             mirrorLogic = as.logical(mirrorLogic),
                                                                             lambda = as.numeric(lambda))
                                 }, #function

                                 ############################
                                 # trafoParameter accessors #
                                 ############################
                                 #' @description
                                 #' Returns entry of `trafoType`
                                 #' for user defined attribute.
                                 #' @param feature
                                 #' Attribute's name.
                                 #' (character)
                                 #' @return
                                 #' Value of entry.
                                 #' (character)
                                 #' @examples
                                 #' y <- x$trafoType(feature = "infected")
                                 trafoType = function(feature = "character"){
                                   t <- "none"
                                   idx <- self$featureIdx(feature)
                                   if(!is.na(idx)){
                                     t <- self$trafoParameter[[idx, "trafoType"]]
                                   }#if
                                   return(t)
                                 }, #function

                                 #' @description
                                 #' Sets entry of `trafoType`
                                 #' for user defined attribute.
                                 #' @param feature
                                 #' Attribute's name.
                                 #' (character)
                                 #' @param type
                                 #' Trafo type parameter. Valid choices are:
                                 #' "none", "log2", "logNorm", "log10", "squareRoot", "cubeRoot", "arcsine", "inverse", "tukeyLOP", "boxCox".
                                 #' (character)
                                 #' @examples
                                 #' x$setTrafoType(feature = "infected", type = "logNorm")
                                 setTrafoType = function(feature = "character", type = "character"){
                                   idx <- self$featureIdx(feature)
                                   t <- factor(type, levels = self$trafoAlphabet)
                                   if(is.na(t)){
                                     rString <- sprintf("\nWarning in pgu.transformator$setTrafoType: type %s is not known\n",
                                                        type)
                                     cat(rString)
                                   }#if
                                   else if (!is.na(idx)){
                                     private$.trafoParameter[idx, "trafoType"] <- t
                                   }#else if
                                 }, #function

                                 #' @description
                                 #' Returns entry of `addConst`
                                 #' for user defined attribute.
                                 #' @param feature
                                 #' Attribute's name.
                                 #' (character)
                                 #' @return
                                 #' Value of entry.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$addConstant(feature = "infected")
                                 addConstant = function(feature = "character"){
                                   c <- 0.0
                                   idx <- self$featureIdx(feature)
                                   if(!is.na(idx)){
                                     c <- self$trafoParameter[idx, "addConst"]
                                   }#if
                                   return(c)
                                 }, #function

                                 #' @description
                                 #' Returns entry of `mirrorLogic`
                                 #' for user defined attribute.
                                 #' @param feature
                                 #' Attribute's name.
                                 #' (character)
                                 #' @return
                                 #' Value of entry.
                                 #' (logical)
                                 #' @examples
                                 #' y <- x$mirrorLogic(feature = "infected")
                                 mirrorLogic = function(feature = "character"){
                                   l <- FALSE
                                   idx <- self$featureIdx(feature)
                                   if(!is.na(idx)){
                                     l <- self$trafoParameter[[idx, "mirrorLogic"]]
                                   }#if
                                   return(l)
                                 }, #unction

                                 #' @description
                                 #' Sets entry of `mirrorLogic`
                                 #' for user defined attribute.
                                 #' @param feature
                                 #' Attribute's name.
                                 #' (character)
                                 #' @param logic
                                 #' Specifies whether the data should be mirrored at the coordinate origin.
                                 #' (logical)
                                 #' @examples
                                 #' x$setMirrorLogic(feature = "infected", logic = FALSE)
                                 setMirrorLogic = function(feature = "character", logic = "logical"){
                                   idx <- self$featureIdx(feature)
                                   if(!is.na(idx)){
                                     private$.trafoParameter[idx, "mirrorLogic"] <- logic
                                   }#if
                                 }, #function

                                 #' @description
                                 #' Returns entry of `lambda`
                                 #' for user defined attribute.
                                 #' Lambda is a specific optimization parameter
                                 #' that is derived from the Box-Cox or Tukey-LOP
                                 #' transfromation procedure.
                                 #' @param feature
                                 #' Attribute's name.
                                 #' (character)
                                 #' @return
                                 #' Value of entry.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$lambda(feature = "infected")
                                 lambda = function(feature = "character"){
                                   l <- 0.0
                                   idx <- self$featureIdx(feature)
                                   if(!is.na(idx)){
                                     l <- self$trafoParameter[idx, "lambda"]
                                   }#if
                                   return(l)
                                 }, #function

                                 ####################
                                 # helper functions #
                                 ####################
                                 #' @description
                                 #' Returns the index of a pgu.normDist object wihtin the instance variable `trafoParameter`.
                                 #' @param feature
                                 #' Attribute's name.
                                 #' (character)
                                 #' @return
                                 #' Index of attribute entry in dataframe
                                 #' (numeric)
                                 #' @examples
                                 #' idx <- x$featureIdx(feature = "infected")
                                 featureIdx =  function(feature = "character"){
                                   idx <- match(feature, self$trafoParameter[["features"]])
                                   if(is.na(idx)){
                                     rString <- sprintf("\nWarning in pgu.transformator: feature %s is not known\n",
                                                        feature)
                                     cat(rString)
                                   }#if
                                   return(idx)
                                 }, #function

                                 #' @description
                                 #' Calculates and returns the addConst.
                                 #' A constant that prevents the occurrence of negative values as well as zero,
                                 #' if added to an attribute.
                                 #' @param value
                                 #' The smallest of the attribute's values.
                                 #' (numeric)
                                 #' @return
                                 #' The addConst for the attribute
                                 #' (numeric)
                                 #' @examples
                                 #' idx <- x$addConstGenerator(value = -0.5)
                                 addConstGenerator =  function(value = "numeric"){
                                   c <- 0.0
                                   # if (value <= 0.0){
                                   #   c <- (-1.0 * value) + .Machine$double.xmin
                                   if (value < 0.0){
                                     c <- (-1.0 * value) + 1
                                   }#if
                                   else if(dplyr::near(0.0, value)){
                                     c <- value + 1
                                   }#else if
                                   return(c)
                                 }, #function
                                 ####################
                                 # mirror functions #
                                 ####################
                                 #' @description
                                 #' Mirrors the assigned values at the coordinate origin.
                                 #' @param value
                                 #' Value or vector of values.
                                 #' (numeric)
                                 #' @return
                                 #' Value or vector of values.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$mirrorNumeric(value = c(-0.5, 0.0, 0.5, 1.0))
                                 mirrorNumeric = function(value = "numeric"){
                                   return(-1.0 * value)
                                 }, #function

                                 #' @description
                                 #' Calls the class' mirrorNumeric function on all numeric attributes of a data frame.
                                 #' @param data
                                 #' A data frame.
                                 #' (tibble:tibble)
                                 #' @return
                                 #' A data frame
                                 #' (tibble::tibble)
                                 #' @examples
                                 #' y <- x$mirrorData(data)
                                 mirrorData = function(data = "tbl_df"){
                                   for (feature in self$trafoParameter[["features"]]){
                                     idx <- self$featureIdx(feature)
                                     if((!is.na(idx)) & (as.logical(self$trafoParameter[idx,"mirrorLogic"]))){
                                       data[feature] <- self$mirrorNumeric(data[[feature]])
                                     }#if
                                   }#for
                                   return(data)
                                 }, #function

                                 #########################
                                 # translation functions #
                                 #########################
                                 #' @description
                                 #' Calculates the addConst value for each attribute of the assigned data frame,
                                 #' by calling the class' addConstGenerator function.
                                 #' The results are stored in addConst attribute of the trafoParameter instance variable.
                                 #' @param data
                                 #' A data frame.
                                 #' (tibble:tibble)
                                 #' @examples
                                 #' y <- x$calculateAddConst(data)
                                 calculateAddConst = function(data = "tbl_df"){
                                   private$.trafoParameter["addConst"] <- data %>%
                                     dplyr::select(self$trafoParameter[["features"]]) %>%
                                     self$mirrorData() %>%
                                     dplyr::summarise_all(min, na.rm=TRUE) %>%
                                     dplyr::mutate_all(self$addConstGenerator) %>%
                                     t() %>%
                                     as.numeric()
                                 }, #function

                                 #' @description
                                 #' Translates the assigned values by a constant.
                                 #' @param value
                                 #' A numeric or a vector of numerics to be translated.
                                 #' (numeric)
                                 #' @param const
                                 #' A constant value.
                                 #' (numeric)
                                 #' @return
                                 #' A numeric or a vector of numerics.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$translateNumeric(value = c(0.1, 0.2, 0.3), const = 4.0)
                                 translateNumeric =  function(value = "numeric", const = "numeric"){
                                   return(value + const)
                                 }, #function

                                 #' @description
                                 #' Translates each attribute of the assigned data frame,
                                 #' by calling the class' translateNumeric function.
                                 #' The respective addConst values of the individual attributes of the data frame
                                 #' serve as const variables.
                                 #' @param data
                                 #' A data frame.
                                 #' (tibble:tibble)
                                 #' @return
                                 #' A data frame.
                                 #' (tibble:tibble)
                                 #' @examples
                                 #' y <- x$translateData(data)
                                 translateData = function(data = "tbl_df"){
                                   for (feature in self$trafoParameter[["features"]]){
                                     idx <- self$featureIdx(feature)
                                     if(!is.na(idx)){
                                       data[feature] <- self$translateNumeric(data[[feature]],
                                                                              as.numeric(self$trafoParameter[idx, "addConst"]))
                                     }#if
                                   }#for
                                   return(data)
                                 }, #function

                                 #' @description
                                 #' Back-translates the assigned values by a constant.
                                 #' @param value
                                 #' A numeric or a vector of numerics to be back-translated.
                                 #' (numeric)
                                 #' @param const
                                 #' A constant value.
                                 #' (numeric)
                                 #' @return
                                 #' A numeric or a vector of numerics.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$backTranslateNumeric(value = c(4.1, 4.2, 4.3), const = 4.0)
                                 backTranslateNumeric = function(value = "numeric", const = "numeric"){
                                   return(value - const)
                                 }, #function

                                 #' @description
                                 #' Back-translates each attribute of the assigned data frame,
                                 #' by calling the class' backTranslateNumeric function.
                                 #' The respective addConst values of the individual attributes of the data frame
                                 #' serve as const variables.
                                 #' @param data
                                 #' A data frame.
                                 #' (tibble:tibble)
                                 #' @return
                                 #' A data frame.
                                 #' (tibble:tibble)
                                 #' @examples
                                 #' y <- x$backTranslateData(data)
                                 backTranslateData = function(data = "tbl_df"){
                                   for (feature in self$trafoParameter[["features"]]){
                                     idx <- self$featureIdx(feature)
                                     if(!is.na(idx)){
                                       data[feature] <- self$backTranslateNumeric(data[[feature]],
                                                                                  as.numeric(self$trafoParameter[idx, "addConst"]))
                                     }#if
                                   }#for
                                   return(data)
                                 }, #function

                                 #####################
                                 # lambda estimation #
                                 #####################
                                 #' @description
                                 #' Estimates the lambda factor for the given values,
                                 #' that are assigned to a user defined attribute..
                                 #' @param value
                                 #' A numeric or a vector of numerics to be analyzed.
                                 #' (numeric)
                                 #' @param feature
                                 #' The attribute which the given values are assigned to.
                                 #' (character)
                                 #' @return
                                 #' The specific lambda factor.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$lambdaEstimator(value = c(4, 5, 6), feature = "infected")
                                 lambdaEstimator = function(value = "numeric", feature = "character"){
                                   lambda <- 1.0
                                   idx <- self$featureIdx(feature)
                                   if(!is.na(idx)){
                                     method <- self$trafoParameter[[idx, "trafoType"]]
                                     switch(as.character(method),
                                            none = {lambda <- 1.0},
                                            log2 = {lambda <- 1.0},
                                            logNorm = {lambda <- 1-0},
                                            log10 = {lambda <- 1.0},
                                            squareRoot = {lambda <- 1.0},
                                            cubeRoot = {lambda <- 1.0},
                                            arcsine = {lambda <- self$normalizeArcSine(value)},
                                            inverse = {lambda <- 1.0},
                                            tukeyLOP = {lambda <- self$optimizeTukeyLadderOfPowers(value)},
                                            boxCox = {lambda <- self$optimizeBoxCox(value)},
                                            {rString <- sprintf("\nWarning in pgu.transformator: trafoType %s is not known\n",as.character(method))
                                            cat(rString)}
                                     )#switch
                                   }#if
                                   return(lambda)
                                 }, #function

                                 #' @description
                                 #' Estimates the lambda factor for each attribute of the assigned data frame,
                                 #' by calling the class' lambdaEstimator function.
                                 #' The respective lambda values of the individual attributes of the data frame
                                 #' are stored in the lambda attribute of the instance variable trafoParameter.
                                 #' @param data
                                 #' A data frame.
                                 #' (tibble:tibble)
                                 #' @examples
                                 #' y <- x$estimateLambda(data)
                                 estimateLambda = function(data = "tbl_df"){
                                   tempData <- data %>%
                                     dplyr::select(self$trafoParameter[["features"]]) %>%
                                     self$mirrorData() %>%
                                     self$translateData()
                                   private$.trafoParameter["lambda"] <- lapply(self$trafoParameter[["features"]], function(x){self$lambdaEstimator(value = tempData[[x]], feature = x)}) %>%
                                     t() %>%
                                     as.numeric()
                                 }, #function

                                 #' @description
                                 #' Estimates the lambda factor for an arcsine transformation for the given values,
                                 #' @param value
                                 #' A numeric or a vector of numerics to be analyzed.
                                 #' (numeric)
                                 #' @return
                                 #' The specific lambda factor.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$normalizeArcSine(value)
                                 normalizeArcSine = function(value  = "numeric"){
                                   return(max(value, na.rm=TRUE))
                                 }, #function

                                 #' @description
                                 #' Estimates the lambda factor for a tukeyLOP transformation for the given values,
                                 #' @param value
                                 #' A numeric or a vector of numerics to be analyzed.
                                 #' (numeric)
                                 #' @return
                                 #' The specific lambda factor.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$optimizeTukeyLadderOfPowers(value)
                                 optimizeTukeyLadderOfPowers = function(value = "numeric"){
                                   lambda <- 1.0
                                   tryCatch({
                                     lambda <- rcompanion::transformTukey(value,
                                                                          start = -10,
                                                                          end = 10,
                                                                          int = 0.025,
                                                                          plotit = FALSE,
                                                                          verbose = FALSE,
                                                                          quiet = TRUE,
                                                                          statistic = 1,
                                                                          returnLambda = TRUE)
                                     return(lambda)
                                   },
                                   warning = function(w) {
                                     warningMessage <- sprintf("Warning: Could not optimize Tukey Ladder Of Powers Lambda is set to 1.0:\n%s", w)
                                     warning(warningMessage)
                                     return(lambda)
                                   }, #warning
                                   error = function(e) {
                                     warningMessage <- sprintf("Warning: Could not optimize Tukey Ladder Of Powers Lambda is set to 1.0:\n%s", e)
                                     warning(warningMessage)
                                     return(lambda)
                                   }#error
                                   )#tryCatch
                                 }, #function

                                 #' @description
                                 #' Estimates the lambda factor for a boxcox transformation for the given values,
                                 #' @param value
                                 #' A numeric or a vector of numerics to be analyzed.
                                 #' (numeric)
                                 #' @return
                                 #' The specific lambda factor.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$optimizeBoxCox(value)
                                 optimizeBoxCox = function(value = "numeric"){
                                   lambda <- 1.0
                                   tryCatch({
                                     logLikelihoodVector <- MASS::boxcox(value ~ 1,
                                                                         lambda = seq(-10, 10, 0.1),
                                                                         plotit = FALSE)
                                     logLikelihood <- tibble::tibble(logLikelihoodVector$x, logLikelihoodVector$y)
                                     colnames(logLikelihood) <- c("lambda", "logLikelihood")
                                     maxLikelihoodIdx <- which(logLikelihood[, "logLikelihood"] == max(logLikelihood[, "logLikelihood"], na.rm = TRUE), arr.ind = TRUE)
                                     lambda <- logLikelihood[[maxLikelihoodIdx[1], "lambda"]]
                                     return(lambda)
                                   },
                                   warning = function(w) {
                                     errorMessage <- sprintf("Warning: Could not optimize Box Cox Lambda is set to 1.0:\n%s", w)
                                     warning(errorMessage)
                                     return(lambda)
                                   }, #warning
                                   error = function(e) {
                                     errorMessage <- sprintf("Warning: Could not optimize Box Cox Lambda is set to 1.0:\n%s", e)
                                     warning(errorMessage)
                                     return(lambda)
                                   }#error
                                   )#tryCatch
                                 }, #function

                                 ############################
                                 # transformation functions #
                                 ############################
                                 #' @description
                                 #' Transforms the given numeric values,
                                 #' that are assigned to a user defined attribute.
                                 #' @param value
                                 #' A numeric or a vector of numerics to be tranformed.
                                 #' (numeric)
                                 #' @param feature
                                 #' The attribute which the given values are assigned to.
                                 #' (character)
                                 #' @return
                                 #' A transfromed version of the given numeric or vector of numerics.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$transformNumeric(value = c(4, 5, 6), feature = "infected")
                                 transformNumeric = function(value = "numeric", feature = "character"){
                                   tf <- numeric()
                                   idx <- self$featureIdx(feature)
                                   if(!is.na(idx)){
                                     method <- self$trafoParameter[[idx, "trafoType"]]
                                     switch(as.character(method),
                                            none = {tf <- value},
                                            log2 = {tf <- self$transformLogModulus(value, base=2)},
                                            logNorm = {tf <- self$transformLogModulus(value, base=exp(1))},
                                            log10 = {tf <- self$transformLogModulus(value, base=10)},
                                            squareRoot = {tf <- self$transformSquareRoot(value)},
                                            cubeRoot = {tf <- self$transformCubeRoot(value)},
                                            arcsine = {tf <- self$transformArcsine(value, lambda=self$trafoParameter[[idx,"lambda"]])},
                                            inverse = {tf <- self$transformInverse(value)},
                                            tukeyLOP = {tf <- self$transformTukeyLadderOfPowers(value, lambda=self$trafoParameter[[idx,"lambda"]])},
                                            boxCox = {tf <- self$transformBoxCox(value, lambda=self$trafoParameter[[idx,"lambda"]])},
                                            {private$.trafoParameter[idx, "trafoType"] <- "none"
                                            tf <- value
                                            rString <- sprintf("\nWarning in pgu.transformator: trafoType %s is not known\n",as.character(method))
                                            cat(rString)}
                                     )#switch
                                   }#if
                                   return (tf)
                                 }, #function

                                 #' @description
                                 #' Transforms each attribute of the assigned data frame,
                                 #' by calling the class' tranformNumeric function.
                                 #' The respective lambda values of the individual attributes of the data frame
                                 #' are read from the lambda attribute of the instance variable trafoParameter.
                                 #' @param data
                                 #' A data frame.
                                 #' (tibble:tibble)
                                 #' @examples
                                 #' y <- x$transformData(data)
                                 transformData = function(data = "tbl_df"){
                                   for (feature in self$trafoParameter[["features"]]){
                                     idx <- self$featureIdx(feature)
                                     if(!is.na(idx)){
                                       data[feature] <- self$transformNumeric(data[[feature]], feature)
                                     }#if
                                   }#for
                                   return(data)
                                 }, #function

                                 #' @description
                                 #' Performes a log transformation for the given values,
                                 #' based on a user defined base value.
                                 #' @param value
                                 #' A numeric or a vector of numerics to be analyzed.
                                 #' (numeric)
                                 #' @param base
                                 #' Logarithmic base.
                                 #' (numeric)
                                 #' @return
                                 #' The transformed values.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$transformLogModulus(value, base = 2)
                                 transformLogModulus = function(value = "numeric", base="numeric"){
                                   return(log(value, base=base))
                                 }, #function

                                 #' @description
                                 #' Performes a square root transformation for the given values.
                                 #' @param value
                                 #' A numeric or a vector of numerics to be analyzed.
                                 #' (numeric)
                                 #' @return
                                 #' The transformed values.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$transformSquareRoot(value)
                                 transformSquareRoot = function(value = "numeric"){
                                   return(sqrt(value))
                                 }, #function

                                 #' @description
                                 #' Performes a cube root transformation for the given values.
                                 #' @param value
                                 #' A numeric or a vector of numerics to be analyzed.
                                 #' (numeric)
                                 #' @return
                                 #' The transformed values.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$transformCubeRoot(value)
                                 transformCubeRoot = function(value = "numeric"){
                                   return((value)^(1/3))
                                 }, #function


                                 #' @description
                                 #' Performes an arcsine transformation for the given values.
                                 #' @param value
                                 #' A numeric or a vector of numerics to be analyzed.
                                 #' (numeric)
                                 #' @param lambda
                                 #' Normalization factor.
                                 #' (numeric)
                                 #' @return
                                 #' The transformed values.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$transformArcsine(value, lambda = max(value))
                                 transformArcsine = function(value = "numeric", lambda="numeric"){
                                   return(asin(sqrt((value)/lambda)))
                                 }, #function

                                 #' @description
                                 #' Performes an inverse transformation for the given values.
                                 #' @param value
                                 #' A numeric or a vector of numerics to be analyzed.
                                 #' (numeric)
                                 #' @return
                                 #' The transformed values.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$transformInverse(value)
                                 transformInverse = function(value = "numeric"){
                                   return(1.0/(value))
                                 }, #function

                                 #' @description
                                 #' Performes a tukeyLOP transformation for the given values.
                                 #' @param value
                                 #' A numeric or a vector of numerics to be analyzed.
                                 #' (numeric)
                                 #' @param lambda
                                 #' Lambda factor.
                                 #' (numeric)
                                 #' @return
                                 #' The transformed values.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$transformTukeyLadderOfPowers(value, lambda = 0)
                                 transformTukeyLadderOfPowers = function(value = "numeric", lambda="numeric"){
                                   if(lambda > 0){
                                     return((value)^lambda)
                                   }#if
                                   else if(lambda == 0){
                                     return(self$transformLogModulus(value, base=exp(1)))
                                   }#else if
                                   else {
                                     return(-1.0*((value)^lambda))
                                   }#else
                                 }, #function

                                 #' @description
                                 #' Performes a boxcox transformation for the given values.
                                 #' @param value
                                 #' A numeric or a vector of numerics to be analyzed.
                                 #' (numeric)
                                 #' @param lambda
                                 #' Lambda factor.
                                 #' (numeric)
                                 #' @return
                                 #' The transformed values.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$transformBoxCox(value, lambda = 0)
                                 transformBoxCox = function(value = "numeric", lambda="numeric"){
                                   if (lambda == 0){
                                     return(self$transformLogModulus(value, base=exp(1)))
                                   }#if
                                   else{
                                     return(((value)^lambda -1) / lambda)
                                   }#else
                                 }, #function

                                 ####################################
                                 # inverse transfromation functions #
                                 ####################################
                                 #' @description
                                 #' Inverse transforms the given numeric values,
                                 #' that are assigned to a user defined attribute.
                                 #' @param value
                                 #' A numeric or a vector of numerics to be tranformed.
                                 #' (numeric)
                                 #' @param feature
                                 #' The attribute which the given values are assigned to.
                                 #' (character)
                                 #' @return
                                 #' An inverse transfromed version of the given numeric or vector of numerics.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$inverseTransformNumeric(value = c(4, 5, 6), feature = "infected")
                                 inverseTransformNumeric = function(value = "numeric", feature = "character"){
                                   tf <- numeric()
                                   idx <- self$featureIdx(feature)
                                   if(!is.na(idx)){
                                     method <- self$trafoParameter[[idx, "trafoType"]]
                                     switch(as.character(method),
                                            none = {tf <- value},
                                            log2 = {tf <- self$inverseTransformLogModulus(value, base=2)},
                                            logNorm = {tf <- self$inverseTransformLogModulus(value, base=exp(1))},
                                            log10 = {tf <- self$inverseTransformLogModulus(value, base=10)},
                                            squareRoot = {tf <- self$inverseTransformSquareRoot(value)},
                                            cubeRoot = {tf <- self$inverseTransformCubeRoot(value)},
                                            arcsine = {tf <- self$inverseTransformArcsine(value, lambda=self$trafoParameter[[idx,"lambda"]])},
                                            inverse = {tf <- self$inverseTransformInverse(value)},
                                            tukeyLOP = {tf <- self$inverseTransformTukeyLadderOfPowers(value, lambda=self$trafoParameter[[idx,"lambda"]])},
                                            boxCox = {tf <- self$inverseTransformBoxCox(value, lambda=self$trafoParameter[[idx,"lambda"]])},
                                            {private$.trafoParameter[idx, "trafoType"] <- "none"
                                            tf <- value
                                            rString <- sprintf("\nWarning in pgu.transformator: trafoType %s is not known\n",as.character(method))
                                            cat(rString)}
                                     )#switch
                                   }#if
                                   return (tf)
                                 }, #function

                                 #' @description
                                 #' Inverse transforms each attribute of the assigned data frame,
                                 #' by calling the class' tranformNumeric function.
                                 #' The respective lambda values of the individual attributes of the data frame
                                 #' are read from the lambda attribute of the instance variable trafoParameter.
                                 #' @param data
                                 #' A data frame.
                                 #' (tibble:tibble)
                                 #' @examples
                                 #' y <- x$inverseTransformData(data)
                                 inverseTransformData = function(data = "tbl_df"){
                                   for (feature in self$trafoParameter[["features"]]){
                                     idx <- self$featureIdx(feature)
                                     if(!is.na(idx)){
                                       data[feature] <- self$inverseTransformNumeric(data[[feature]], feature)
                                     }#if
                                   }#for
                                   return(data)
                                 }, #function

                                 #' @description
                                 #' Performes an inverse log transformation for the given values,
                                 #' based on a user defined base value.
                                 #' @param value
                                 #' A numeric or a vector of numerics to be analyzed.
                                 #' (numeric)
                                 #' @param base
                                 #' Logarithmic base.
                                 #' (numeric)
                                 #' @return
                                 #' The transformed values.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$inverseTransformLogModulus(value, base = 2)
                                 inverseTransformLogModulus = function(value = "numeric", base="numeric"){
                                   return (base^value)
                                 }, #function

                                 #' @description
                                 #' Performes an inverse square root transformation for the given values.
                                 #' @param value
                                 #' A numeric or a vector of numerics to be analyzed.
                                 #' (numeric)
                                 #' @return
                                 #' The transformed values.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$inverseTransformSquareRoot(value)
                                 inverseTransformSquareRoot = function(value = "numeric"){
                                   return(value^2)
                                 }, #function

                                 #' @description
                                 #' Performes an inverse cube root transformation for the given values.
                                 #' @param value
                                 #' A numeric or a vector of numerics to be analyzed.
                                 #' (numeric)
                                 #' @return
                                 #' The transformed values.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$inverseTransformCubeRoot(value)
                                 inverseTransformCubeRoot = function(value = "numeric"){
                                   return(value^3)
                                 }, #function

                                 #' @description
                                 #' Performes an inverse arcsine transformation for the given values.
                                 #' @param value
                                 #' A numeric or a vector of numerics to be analyzed.
                                 #' (numeric)
                                 #' @param lambda
                                 #' Normalization factor.
                                 #' (numeric)
                                 #' @return
                                 #' The transformed values.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$inverseTransformArcsine(value, lambda = max(value))
                                 inverseTransformArcsine = function(value = "numeric", lambda="numeric"){
                                   return(((sin(value))^2.0)*lambda)
                                 }, #function

                                 #' @description
                                 #' Performes an inverse inverse-transformation for the given values.
                                 #' @param value
                                 #' A numeric or a vector of numerics to be analyzed.
                                 #' (numeric)
                                 #' @return
                                 #' The transformed values.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$inverseTransformInverse(value)
                                 inverseTransformInverse = function(value = "numeric"){
                                   return(1.0/value)
                                 }, #function

                                 #' @description
                                 #' Performes an inverse tukeyLOP transformation for the given values.
                                 #' @param value
                                 #' A numeric or a vector of numerics to be analyzed.
                                 #' (numeric)
                                 #' @param lambda
                                 #' Lambda factor.
                                 #' (numeric)
                                 #' @return
                                 #' The transformed values.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$inverseTransformTukeyLadderOfPowers(value, lambda = 0)
                                 inverseTransformTukeyLadderOfPowers = function(value = "numeric", lambda="numeric"){
                                   if(lambda > 0){
                                     return(value^(1/lambda))
                                   }#if
                                   else if(lambda == 0){
                                     return(self$inverseTransformLogModulus(value, base=exp(1)))
                                   }#else if
                                   else {
                                     return(((-1.0*value)^(1/lambda)))
                                   }#else
                                 }, #function

                                 #' @description
                                 #' Performes an inverse boxcox transformation for the given values.
                                 #' @param value
                                 #' A numeric or a vector of numerics to be analyzed.
                                 #' (numeric)
                                 #' @param lambda
                                 #' Lambda factor.
                                 #' (numeric)
                                 #' @return
                                 #' The transformed values.
                                 #' (numeric)
                                 #' @examples
                                 #' y <- x$inverseTransformBoxCox(value, lambda = 0)
                                 inverseTransformBoxCox = function(value = "numeric", lambda="numeric"){
                                   if (lambda == 0){
                                     return(self$inverseTransformLogModulus(value, base=exp(1)))
                                   }#if
                                   else{
                                     return(((value*lambda)+1)^(1/lambda))
                                   }#else
                                 }, #function

                                 ####################
                                 # compound methods #
                                 ####################
                                 #' @description
                                 #' Estimate all transformation parameters(lambda, addConst,...)
                                 #' for each attribute of a given data frame.
                                 #' The function calls the class' functions calculateAddConst and estimateLambda.
                                 #' The results are stored in the respective attributes of the instance variable trafoParameter.
                                 #' @param data
                                 #' A data frame.
                                 #' (tibble:tibble)
                                 #' @examples
                                 #' x$estimateTrafoParameter(data)
                                 estimateTrafoParameter = function(data = "tbl_df"){
                                   data %>%
                                     self$calculateAddConst()
                                   data %>%
                                     self$estimateLambda()
                                 }, #function

                                 #' @description
                                 #' Mutates the values of each attribute of a given data frame.
                                 #' Here, mutation is defined as the cesecutive sequence of the class'
                                 #' functions mirrorData, tranlsateData and transfromData.
                                 #' @param data
                                 #' A data frame.
                                 #' (tibble:tibble)
                                 #' @return
                                 #' A mutated data frame.
                                 #' (tibble::tibble)
                                 #' @examples
                                 #' y <- x$mutateData(data)
                                 mutateData = function(data = "tbl_df"){
                                   data %>%
                                     dplyr::select(self$trafoParameter[["features"]]) %>%
                                     self$mirrorData() %>%
                                     self$translateData() %>%
                                     self$transformData() %>%
                                     return()
                                 }, #function

                                 #' @description
                                 #' Re-mutates the values of each attribute of a given data frame.
                                 #' Here, re-mutation is defined as the cesecutive sequence of the class'
                                 #' functions inverseTransformData, backTranslateData, mirrorData
                                 #' @param data
                                 #' A data frame.
                                 #' (tibble:tibble)
                                 #' @return
                                 #' A mutated data frame.
                                 #' (tibble::tibble)
                                 #' @examples
                                 #' y <- x$reverseMutateData(data)
                                 reverseMutateData = function(data = "tbl_df"){
                                   data %>%
                                     dplyr::select(self$trafoParameter[["features"]]) %>%
                                     self$inverseTransformData() %>%
                                     self$backTranslateData() %>%
                                     self$mirrorData() %>%
                                     return()
                                 }#function
                               )#public
)#class
