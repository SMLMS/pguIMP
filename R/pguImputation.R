#' @title pgu.imputation
#'
#' @description
#' Detects and substitutes missing values from data set.
#'
#' @details
#' Detects missing values in the normalized and Z-score transfromed data set.
#' Replace missing values with substitutes by classical and AI-powerd substitution algorithms.
#'
#' @format [R6::R6Class] object.
#' @section Construction:
#' x <- pguIMP::pgu.imputation$new(data)
#'
#' @import R6
#' @import tidyverse
#' @import gridExtra
#' @import outliers
#' @import DT
#' @import DMwR
#' @import mice
#' @import RWeka
#' @import mice
#' @import Amelia
#' @import psych
#' @import VIM
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pgu.imputation <- R6::R6Class("pgu.imputation",
                              ####################
                              # instance variables
                              ####################
                               private = list(
                                 .imputationParameter = "tbl_df",
                                 .imputationSites = "tbl_df",
                                 .imputationAgentAlphabet = "character",
                                 .imputationAgent = "factor",
                                 .seed = "numeric",
                                 .iterations = "numeric",
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
                                 #' @field imputationAgentAlphabet
                                 #' Returns the instance variable imputationagentAlphabet.
                                 #' (character)
                                 imputationAgentAlphabet = function(){
                                   return(private$.imputationAgentAlphabet)
                                 },
                                 #' @field imputationAgent
                                 #' Returns the instance variable imputationAgent.
                                 #' (character)
                                 imputationAgent = function(){
                                   return(as.character(private$.imputationAgent))
                                 },
                                 #' @field setImputationAgent
                                 #' Sets the instance variable imputationAgent.
                                 #' (character)
                                 setImputationAgent = function(agent = "character") {
                                   private$.imputationAgent <- factor(agent, levels = self$imputationAgentAlphabet)
                                 },
                                 #' @field seed
                                 #' Returns the instance variable seed.
                                 #' (numeric)
                                 seed = function(){
                                   return(private$.seed)
                                 },
                                 #' @field setSeed
                                 #' Sets the instance variable seed.
                                 #' (numeric)
                                 setSeed = function(value = "numeric"){
                                   private$.seed <- value
                                 },
                                 #' @field iterations
                                 #' Returns the instance variable iterations.
                                 #' (numeric)
                                 iterations = function(){
                                   return(private$.iterations)
                                 },
                                 #' @field setIterations
                                 #' Sets the instance variable iterations.
                                 #' (numeric)
                                 setIterations = function(value = "numeric"){
                                   private$.iterations <- value
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
                                 #' Creates and returns a new `pgu.imputation` object.
                                 #' @param data
                                 #' The data to be cleaned.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' A new `pgu.imputation` object.
                                 #' (pguIMP::pgu.imputation)
                                 #' @examples
                                 #' y <- tibble:tibble()
                                 #' x <- pguIMP:pgu.imputation$new(data = y)
                                 initialize = function(data = "tbl_df"){
                                   private$.imputationAgentAlphabet <- c("none", "median", "mean", "mu", "mc", "knn", "pmm", "cart", "rf", "M5P", "amelia", "amelia_bound")
                                   self$setSeed <- 42
                                   self$setIterations <- 4
                                   self$setImputationAgent <- self$imputationAgentAlphabet[1]
                                   if(class(data)[1] != "tbl_df"){
                                     data <- tibble::tibble(names <- "none",
                                                            values <- c(NA))
                                   }
                                   self$resetImputationParameter(data)
                                 },#function
                                 #' @description
                                 #' Clears the heap and
                                 #' indicates that instance of `pgu.imputation` is removed from heap.
                                 finalize = function(){
                                   print("Instance of pgu.imputation removed from heap")
                                 },#function
                                 ##########################
                                 # print instance variables
                                 ##########################
                                 #' @description
                                 #' Prints instance variables of a `pgu.imputation` object.
                                 #' @return
                                 #' string
                                 #' @examples
                                 #' x$print()
                                 #' print(x)
                                 print = function(){
                                   rString <- sprintf("\npgu.imputation\n")
                                   cat(rString)
                                   uString <- sprintf("\nseed: %i\niterations: %i\nimputationAgent: %s\nimputationSites:\n", self$seed, self$iterations, as.character(self$imputationAgent))
                                   cat(uString)
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
                                 #' identifies missings in the normalized and Z-score transfromed data set.
                                 #' @param data
                                 #' Dataframe to be analyzed.
                                 #' (tibble::tibble)
                                 #' @examples
                                 #' x$resetImputtaionParameter(data)
                                 resetImputationParameter = function(data = "tbl_df"){
                                   numericData <- data %>%
                                     dplyr::select_if(is.numeric)
                                   features <- numericData %>%
                                     colnames()
                                   measurements <- c(rep(0.0, length(features)))
                                   existings <- c(rep(0.0, length(features)))
                                   missings <- c(rep(0.0, length(features)))
                                   fractionOfMissings <- c(rep(0.0, length(features)))
                                   private$.imputationParameter <- tibble::tibble(features, measurements, existings, missings, fractionOfMissings)
                                   private$.amv <- VIM::aggr(numericData, plot=FALSE)
                                   self$gatherImputationStatistics(data)
                                   self$detectImputationSites(data)
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
                                 #' @param data
                                 #' Dataframe to be analyzed.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' The filtered data frame.
                                 #' (tibble::tibble)
                                 #' @examples
                                 #' idx <- x$filterFeatures(data)
                                 filterFeatures = function(data = "tbl_df"){
                                   data %>%
                                     dplyr::select(private$.imputationParameter[["features"]]) %>%
                                     return()
                                 }, #function

                                 #' @description
                                 #' Returns the position of an attribute's missing values within a data frame.
                                 #' @param featureName
                                 #' The attribute's name.
                                 #' (character)
                                 #' @return
                                 #' The postion of the missing values.
                                 #' (numeric)
                                 #' @examples
                                 #' idx <- x$imputationIdxByFeature(featureName = "infected")
                                 imputationSiteIdxByFeature =  function(featureName = "character"){
                                   self$imputationSites %>%
                                     dplyr::filter(features == featureName) %>%
                                     dplyr::pull(row) %>%
                                     as.integer() %>%
                                     return()
                                 }, #function

                                 #' @description
                                 #' Characterizes each row of the data frame as either `complete`
                                 #' or indicates which attribute are missing within the row.
                                 #' If multiple attributes' row entries are missing, the row is characterized by `multiple`.
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' Vector of row characteristics.
                                 #' (character)
                                 #' @examples
                                 #' idx <- x$nanFeatureList(data)
                                 nanFeatureList = function(data = "tbl_df"){
                                   nanFeature <- c(rep("complete", nrow(data)))
                                   if (nrow(self$imputationSites) > 0) {
                                     for(i in seq(from = 1,to = nrow(self$imputationSites), by =1)){
                                       if (grepl("complete", nanFeature[self$imputationSites[[i,"row"]]])){
                                         nanFeature[self$imputationSites[[i,"row"]]] <- self$imputationSites[[i, "features"]]
                                       }#if
                                       else{
                                         nanFeature[self$imputationSites[[i,"row"]]] <- "multiple"
                                       }#else
                                     }#for
                                   }#if
                                   return(nanFeature)
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
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @examples
                                 #' x$gatherInformationStatistics(value)
                                 gatherImputationStatistics = function(data = "tbl_df"){
                                   filteredData <- data %>%
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
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @param  feature
                                 #' The attribute within the data frame to be analyzed.
                                 #' @examples
                                 #' y <- x$detectImputationsites(data, feature = "infected")
                                 detectImputationSites = function(data = "tbl_df"){
                                   private$.imputationSites <- data %>%
                                     self$filterFeatures() %>%
                                     is.na() %>%
                                     which(arr.ind=TRUE) %>%
                                     tibble::as_tibble() %>%
                                     dplyr::mutate(features = self$imputationParameter[["features"]][col])
                                 }, #function

                                 ###########################
                                 # handle imputation sites #
                                 ###########################
                                 #' @description
                                 #' Chooseses a cleaning method based upon the instance variable `imputationAgent`
                                 #' and handles the imputation sites in the dataframe.
                                 #' Returns a cleaned data set.
                                 #' Display the progress if shiny is loaded.
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @param progress
                                 #' If shiny is loaded, the analysis' progress is stored within this instance of the shiny Progress class.
                                 #' (shiny::Progress)
                                 #' @return
                                 #' Cleaned dataframe.
                                 #' (tibble:tibble)
                                 #' @examples
                                 #' x$handleImputationSites(data, progress)
                                 handleImputationSites = function(data = "tbl_df", progress = "Progress"){
                                   if(is.na(self$imputationAgent)){
                                     print("Warning: Error in pgu.imputation imputationAgent is not valid. Will be set to none.")
                                     self$setimputationAgent <- "none"
                                   }#if
                                   cleanedData <- switch((self$imputationAgent),
                                                         "none" = data,
                                                         "median" = self$imputeByMedian(data, progress),
                                                         "mean" = self$imputeByMean(data, progress),
                                                         "mu" = self$imputeByExpectationValue(data, progress),
                                                         "mc" = self$imputeByMC(data, progress),
                                                         "knn" = self$imputeByKnn(data, progress),
                                                         "pmm" = self$imputeByMice(data,"pmm", progress),
                                                         "cart" = self$imputeByMice(data, "cart", progress),
                                                         "rf" = self$imputeByMice(data, "rf", progress),
                                                         "M5P" = self$imputeByM5P(data, progress),
                                                         "amelia" = self$imputeByAmelia(data, progress),
                                                         "amelia_bound" = self$imputeByAmeliaBound(data, progress)
                                   )
                                   return(cleanedData)
                                 }, #function

                                 #' @description
                                 #' Substitutes imputation sites by the median of the respective attribute.
                                 #' Returns the cleaned dataframe.
                                 #' Display the progress if shiny is loaded.
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @param progress
                                 #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                                 #' (shiny::Progress)
                                 #' @return
                                 #' Cleaned dataframe.
                                 #' (tibble:tibble)
                                 #' @examples
                                 #' x$imputeByMedian(data, progress)
                                 imputeByMedian = function(data = "tbl_df", progress = "Progress"){
                                   for (feature in self$imputationParameter[["features"]]){
                                     if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                       progress$inc(1)
                                     }#if
                                     indices <- self$imputationSiteIdxByFeature(feature)
                                     data <- data %>%
                                       dplyr::mutate(!!feature := replace(!!as.name(feature),
                                                                          indices,
                                                                          stats::median(!!as.name(feature), na.rm = TRUE)))
                                   }#for
                                   return(data)
                                 }, #function

                                 #' @description
                                 #' Substitutes imputation sites by the aritmertic mean of the respective attribute.
                                 #' Returns the cleaned dataframe.
                                 #' Display the progress if shiny is loaded.
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @param progress
                                 #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                                 #' (shiny::Progress)
                                 #' @return
                                 #' Cleaned dataframe.
                                 #' (tibble:tibble)
                                 #' @examples
                                 #' x$imputeByMean(data, progress)
                                 imputeByMean = function(data = "tbl_df", progress = "Progress"){
                                   for (feature in self$imputationParameter[["features"]]){
                                     if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                       progress$inc(1)
                                     }#if
                                     indices <- self$imputationSiteIdxByFeature(feature)
                                     data <- data %>%
                                       dplyr::mutate(!!feature := replace(!!as.name(feature),
                                                                          indices,
                                                                          mean(!!as.name(feature), na.rm = TRUE)))
                                   }#for
                                   return(data)
                                 }, #function

                                 #' @description
                                 #' Substitutes imputation sites by the expectation value of the respective attribute.
                                 #' Returns the cleaned dataframe.
                                 #' Display the progress if shiny is loaded.
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @param progress
                                 #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                                 #' (shiny::Progress)
                                 #' @return
                                 #' Cleaned dataframe.
                                 #' (tibble:tibble)
                                 #' @examples
                                 #' x$imputeByExpectationValue(data, progress)
                                 imputeByExpectationValue = function(data = "tbl_df", progress = "Progress"){
                                   for (feature in self$imputationParameter[["features"]]){
                                     if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                       progress$inc(1)
                                     }#if
                                     indices <- self$imputationSiteIdxByFeature(feature)
                                     data <- data %>%
                                       dplyr::mutate(!!feature := replace(!!as.name(feature),
                                                                          indices,
                                                                          0.0))
                                   }#for
                                   return(data)
                                 }, #function

                                 #' @description
                                 #' Substitutes imputation sites by values generated by a monte carlo simulation.
                                 #' The procedure runs several times as defined by the instance variable `iterations`.
                                 #' The run with the best result is identified and used for substitution.
                                 #' Returns the cleaned dataframe.
                                 #' Display the progress if shiny is loaded.
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @param progress
                                 #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                                 #' (shiny::Progress)
                                 #' @return
                                 #' Cleaned dataframe.
                                 #' (tibble:tibble)
                                 #' @examples
                                 #' x$imputeByMC(data, progress)
                                 imputeByMC = function(data = "tbl_df", progress = "Progress"){
                                   imputed_df <- data
                                   for (feature in self$imputationParameter[["features"]]){
                                     if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                       progress$inc(1)
                                     }#if
                                     stats0 <- data %>%
                                       dplyr::select(feature) %>%
                                       unlist() %>%
                                       psych::describe()
                                     stats <- matrix(NA, ncol= self$iterations, nrow = 13)
                                     for (j in 1:self$iterations) {
                                       set.seed(self$seed + j - 1)
                                       indices <- self$imputationSiteIdxByFeature(feature)
                                       mcVal <- stats::rnorm(n = length(indices),
                                                             mean = 0.0,
                                                             sd = 1.0)
                                       complete_Data <- data %>%
                                         dplyr::mutate(!!feature := replace(!!as.name(feature),
                                                                            indices,
                                                                            mcVal))
                                       stats[,j] <-complete_Data %>%
                                         dplyr::select(feature) %>%
                                         unlist() %>%
                                         psych::describe() %>%
                                         t()%>%
                                         unlist()
                                     }#for
                                     diffMat <- stats %>%
                                       sweep(MARGIN = 1, STATS = unlist(stats0), FUN = "-") %>%
                                       abs()
                                     ranks <- apply(X = diffMat, MARGIN = 1, FUN = function(x)rank(x, ties.method = "max"))
                                     set.seed(self$seed+which.min(rowSums(ranks[,3:13]))-1)
                                     indices <- self$imputationSiteIdxByFeature(feature)
                                     mcVal <- stats::rnorm(n = length(indices),
                                                           mean = 0.0,
                                                           sd = 1.0)
                                     complete_Data <- data %>%
                                       dplyr::mutate(!!feature := replace(!!as.name(feature),
                                                                          indices,
                                                                          mcVal))

                                     imputed_df <- imputed_df %>%
                                       dplyr::mutate(!!feature := complete_Data %>%
                                                       dplyr::select(feature) %>%
                                                       unlist())
                                   }#for
                                   return(imputed_df)
                                 }, #function

                                 #' @description
                                 #' Substitutes imputation sites by predictions of a KNN analysis of the whole dataframe.
                                 #' Returns the cleaned dataframe.
                                 #' Display the progress if shiny is loaded.
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @param progress
                                 #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                                 #' (shiny::Progress)
                                 #' @return
                                 #' Cleaned dataframe.
                                 #' (tibble:tibble)
                                 #' @examples
                                 #' x$imputeByKnn(data, progress)
                                 imputeByKnn = function(data = "tbl_df", progress = "Progress"){
                                   if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                     progress$inc(length(self$imputationParameter[["features"]]))
                                   }#if
                                   if (ncol(data) < 4){
                                     return(data)
                                   }#if
                                   else{
                                     data %>%
                                       as.data.frame() %>%
                                       DMwR::knnImputation(k=3,
                                                           scale = TRUE,
                                                           meth = "weighAvg",
                                                           distData = NULL) %>%
                                       tibble::as_tibble() %>%
                                       return()
                                   }#else
                                 }, #function

                                 #' @description
                                 #' Substitutes imputation sites by values generated by a different methods of the mice package.
                                 #' The procedure runs several times as defined by the instance variable `iterations`.
                                 #' The run with the best result is identified and used for substitution.
                                 #' Returns the cleaned dataframe.
                                 #' Display the progress if shiny is loaded.
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @param method
                                 #' One of the methods supported by the mice package.
                                 #' (character)
                                 #' @param progress
                                 #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                                 #' (shiny::Progress)
                                 #' @return
                                 #' Cleaned dataframe.
                                 #' (tibble:tibble)
                                 #' @examples
                                 #' x$imputeByMice(data, progress)
                                 imputeByMice = function(data, method = "character", progress = "Progress") {
                                   if(ncol(data) < 2){
                                     return(data)
                                   }#if
                                   else{
                                     data_col_names <- colnames(data)
                                     colnames(data) <- paste0("F", seq(1:ncol(data))) %>%
                                       as.character()
                                     imputed_df <- data
                                     for (col_name in colnames(data)) {
                                       if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                         progress$inc(1)
                                       }#if
                                       stats0 <- data %>%
                                         dplyr::select(col_name) %>%
                                         unlist() %>%
                                         psych::describe()
                                       stats <- matrix(NA, ncol= self$iterations, nrow = 13)
                                       for (j in 1:self$iterations) {
                                         imputed_Data <- data %>%
                                           mice::mice(method = method, seed = self$seed+j-1, printFlag = FALSE)
                                         complete_Data <- mice::complete(imputed_Data,1)
                                         stats[,j] <-complete_Data %>%
                                           dplyr::select(col_name) %>%
                                           unlist() %>%
                                           psych::describe() %>%
                                           t()%>%
                                           unlist()
                                       }#for
                                       diffMat <- stats %>%
                                         sweep(MARGIN = 1, STATS = unlist(stats0), FUN = "-") %>%
                                         abs()
                                       ranks <- apply(X = diffMat, MARGIN = 1, FUN = function(x)rank(x, ties.method = "max"))
                                       imputed_Data <- data %>%
                                         mice::mice(method = method,
                                                    seed = self$seed+which.min(rowSums(ranks[,3:13]))-1,
                                                    printFlag = FALSE)
                                       complete_Data <- mice::complete(imputed_Data,1)
                                       imputed_df <- imputed_df %>%
                                         dplyr::mutate(!!col_name := complete_Data %>%
                                                         dplyr::select(col_name) %>%
                                                         unlist())
                                     }#for
                                     colnames(imputed_df) <- data_col_names
                                     return(imputed_df)
                                   }#else
                                 }, #function

                                 #' @description
                                 #' Substitutes imputation sites by predictions of a M5P tree trained on the whole dataframe.
                                 #' Returns the cleaned dataframe.
                                 #' Display the progress if shiny is loaded.
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @param progress
                                 #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                                 #' (shiny::Progress)
                                 #' @return
                                 #' Cleaned dataframe.
                                 #' (tibble:tibble)
                                 #' @examples
                                 #' x$imputeByM5P(data, progress)
                                 imputeByM5P = function(data = "tl_df", progress = "Progress"){
                                   if(ncol(data) < 2){
                                     return(data)
                                   }#if
                                   else{
                                     data_col_names <- colnames(data)
                                     colnames(data) <- paste0("F", seq(1:ncol(data))) %>%
                                       as.character()
                                     imputed_df <- data
                                     for (i in 1:length(colnames(data))) {
                                       if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                         progress$inc(1)
                                       }#if

                                       na_idx <- self$imputationSiteIdxByFeature(featureName = data_col_names[i])

                                       if((length(na_idx)<1) | length(na_idx) == nrow(data)){
                                         next
                                       }#if
                                       train_df <- data %>%
                                         dplyr::slice(-na_idx)

                                       na_df <- data %>%
                                         dplyr::slice(na_idx)

                                       m5 <- colnames(data)[i] %>%
                                         paste("~.") %>%
                                         as.formula() %>%
                                         RWeka::M5P(data = train_df)

                                       na_values <- predict(m5, newdata = na_df)

                                       for (j in 1:length(na_idx)){
                                         imputed_df[[na_idx[j], colnames(data)[i]]] <- na_values[j]
                                       }#for
                                     }#for
                                     colnames(imputed_df) <- data_col_names
                                     return(imputed_df)
                                   }#else
                                 }, #functions


                                 #' @description
                                 #' Substitutes imputation sites by values generated by a different methods of the amelia package.
                                 #' The procedure is run without bound information.
                                 #' Returns the cleaned dataframe.
                                 #' Display the progress if shiny is loaded.
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @param progress
                                 #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                                 #' (shiny::Progress)
                                 #' @return
                                 #' Cleaned dataframe.
                                 #' (tibble:tibble)
                                 #' @examples
                                 #' x$imputeByAmelia(data, progress)
                                 imputeByAmelia = function(data = "tbl_df", progress = "Progress"){
                                   if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                     progress$inc(length(self$imputationParameter[["features"]]))
                                   }#if
                                   ameliaOutput <- data %>%
                                     Amelia::amelia()#(m = 1, parallel = "multicore", ncpus = 8)
                                   ameliaOutput$imputations[[1]] %>%
                                     tibble::as_tibble() %>%
                                     return()
                                 }, #function

                                 #' @description
                                 #' Substitutes imputation sites by values generated by a different methods of the amelia package.
                                 #' The procedure is run with bound information.
                                 #' Returns the cleaned dataframe.
                                 #' Display the progress if shiny is loaded.
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @param progress
                                 #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                                 #' (shiny::Progress)
                                 #' @return
                                 #' Cleaned dataframe.
                                 #' (tibble:tibble)
                                 #' @examples
                                 #' x$imputeByAmeliaBound(data, progress)
                                 imputeByAmeliaBound = function(data = "tbl_df", progress = "Progress"){
                                   if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                     progress$inc(length(self$imputationParameter[["features"]]))
                                   }#if
                                   tblBounds = data.frame(column=c(1:ncol(data)),
                                                          lower=c(0),
                                                          upper=sapply(data,stats::quantile,probs = c(0.25),names=FALSE, na.rm = TRUE)) %>%
                                     as.matrix()
                                   ameliaOutput <- data %>%
                                     Amelia::amelia(bounds=tblBounds)#(m = 1, parallel = "multicore", ncpus = 8)
                                   ameliaOutput$imputations[[1]] %>%
                                     tibble::as_tibble() %>%
                                     return()
                                 }, #function

                                 ##########
                                 # output #
                                 ##########
                                 #' @description
                                 #' Numeric representation of the distribution of missing values within the data frame.
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' A data frame
                                 #' (tibble::tibble)
                                 #' @examples
                                 #' data %>%
                                 #'  x$imputationSiteDistribution() %>%
                                 #'  print()
                                 imputationSiteDistribution = function(data = "tbl_df"){
                                   d <- data %>%
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
                                 #' @param dfData
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @param dfMetadata
                                 #' The corresponding metadata.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' A data frame
                                 #' (tibble::tibble)
                                 #' @examples
                                 #' data %>%
                                 #'  x$mergeImputationSiteData() %>%
                                 #'  print()
                                 mergeImputationSiteData = function(dfData = "tbl_df", dfMetadata = "tbl_df"){
                                   dfMerge <- dfData
                                   if(nrow(dfData) == nrow(dfMetadata)){
                                     dfMerge <- dplyr::bind_cols(dfMetadata, dfData)
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
                                 }, #function

                                 #' @description
                                 #' Displays the distribution of an attribute values as histogram.
                                 #' @param data
                                 #' dataframe to be analyzed.
                                 #' (tibble::tibble)
                                 #' @param feature
                                 #' attribute to be shown.
                                 #' (character)
                                 #' @return
                                 #' A histogram.
                                 #' (ggplot2::ggplot)
                                 #' @examples
                                 #' x$featureBarPlot() %>%
                                 #'  show()
                                 featureBarPlot = function(data = "tbl_df", feature = "character"){
                                   feature <- sym(feature)
                                   p <- data %>%
                                     ggplot2::ggplot(mapping = ggplot2::aes_string(x=feature), na.rm=TRUE) +
                                     ggplot2::geom_bar(stat = "bin")
                                   return(p)
                                 }, #function

                                 #' @description
                                 #' Displays the distribution of an attribute's values as box plot.
                                 #' @param data
                                 #' dataframe to be analyzed.
                                 #' (tibble::tibble)
                                 #' @param feature
                                 #' attribute to be shown.
                                 #' (character)
                                 #' @return
                                 #' A box plot.
                                 #' (ggplot2::ggplot)
                                 #' @examples
                                 #' x$featureBoxPlotWithSubset() %>%
                                 #'  show()
                                 featureBoxPlotWithSubset = function(data = "tbl_df", feature = "character"){
                                   nanFeature <- self$nanFeatureList(data)
                                   p <- data %>%
                                     dplyr::select(feature) %>%
                                     dplyr::mutate(nanFeature = nanFeature) %>%
                                     tidyr::gather_(key="feature", value="measurement", feature) %>%
                                     ggplot2::ggplot(mapping=ggplot2::aes_string(x="feature",y="measurement"), na.rm=TRUE)+
                                     ggplot2::geom_boxplot(na.rm=TRUE)+
                                     ggplot2::geom_jitter(ggplot2::aes(colour=nanFeature), na.rm=TRUE)
                                   return(p)
                                 }, #function

                                 #' @description
                                 #' Displays the distribution of an attribute's values as a composition of a box plot and a histogram.
                                 #' @param data
                                 #' dataframe to be analyzed.
                                 #' (tibble::tibble)
                                 #' @param feature
                                 #' attribute to be shown.
                                 #' (character)
                                 #' @return
                                 #' A composite plot.
                                 #' (ggplot2::ggplot)
                                 #' @examples
                                 #' x$featurePlot() %>%
                                 #'  show()
                                 featurePlot = function(data = "tbl_df", feature = "character"){
                                   p1 <- self$featureBoxPlotWithSubset(data, feature) +
                                     ggplot2::theme(legend.position = c(0.9, 0.9),
                                                    legend.key = ggplot2::element_blank(),
                                                    legend.background = ggplot2::element_blank())

                                   limits1 <- layer_scales(p1)$y$range$range

                                   p2 <- self$featureBarPlot(data, feature)

                                   limits2 <- ggplot2::layer_scales(p2)$x$range$range

                                   limits <- c(min(c(limits1[1], limits2[1])),
                                               max(c(limits1[2], limits2[2]))
                                   )

                                   p1 <- p1 +
                                     ggplot2::scale_y_continuous(limits=limits)

                                   p2 <- p2 +
                                     ggplot2::scale_x_continuous(position = "top", limits=limits) +
                                     ggplot2::coord_flip()

                                   # p <- gridExtra::grid.arrange(p1,p2, layout_matrix = rbind(c(1,2),c(1,2)))

                                   p <- gridExtra::grid.arrange(p1,p2, layout_matrix = rbind(c(1,1,2),c(1,1,2)))
                                   return(p)
                                 }#function

                               )#public
)#class
