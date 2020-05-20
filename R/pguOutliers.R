#' @title pgu.outliers
#'
#' @description
#' Detects and replaces possible outliers from data set.
#'
#' @details
#' Performes Grubb's test for outliers to detect outliers in the normalized and Z-score transfromed data set.
#' Replace missing values with substitutes by classical and AI-powerd substitution algorithms.
#' For this purpose outliers are handled as imputation sites.
#'
#' @format [R6::R6Class] object.
#' @section Construction:
#' x <- pguIMP::pgu.outliers$new(data)
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
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pgu.outliers <- R6::R6Class("pgu.outliers",
                              ####################
                              # instance variables
                              ####################
                               private = list(
                                 .outliersParameter = "tbl_df",
                                 .outliers = "tbl_df",
                                 .cleaningAgentAlphabet = "character",
                                 .cleaningAgent = "factor",
                                 .seed = "numeric",
                                 .iterations = "numeric",
                                 .featureData = "numeric",
                                 .alpha = "numeric",
                                 .minSamples = "numeric",
                                 .outliersStatistics = "tbl_df"
                               ),
                              ##################
                              # accessor methods
                              ##################
                               active = list(
                                 #' @field outliersParameter
                                 #' Returns the instance variable outliersParameter.
                                 #' (tibble::tibble)
                                 outliersParameter = function(){
                                   return(private$.outliersParameter)
                                 },
                                 #' @field outliers
                                 #' Returns the instance variable outliers.
                                 #' (tibble::tibble)
                                 outliers = function(){
                                   return(private$.outliers)
                                 },
                                 #' @field cleaningAgentAlphabet
                                 #' Returns the instance variable cleaningAgentAlphabet.
                                 #' (character)
                                 cleaningAgentAlphabet = function(){
                                   return(private$.cleaningAgentAlphabet)
                                 },
                                 #' @field cleaningAgent
                                 #' Returns the instance variable cleaningAgent.
                                 #' (character)
                                 cleaningAgent = function(){
                                   return(as.character(private$.cleaningAgent))
                                 },
                                 #' @field setCleaningAgent
                                 #' Sets the instance variable cleaningAgent.
                                 #' (character)
                                 setCleaningAgent = function(agent = "character"){
                                   private$.cleaningAgent <- factor(agent, levels = self$cleaningAgentAlphabet)
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
                                 #' @field featureData
                                 #' Returns the instance variable featureData.
                                 #' (numeric)
                                 featureData = function(){
                                   return(private$.featureData)
                                 },
                                 #' @field alpha
                                 #' Returns the instance variable alpha.
                                 #' (numeric)
                                 alpha = function(){
                                   return(private$.alpha)
                                 },
                                 #' @field setAlpha
                                 #' Set the instance variable alpha.
                                 #' (numeric)
                                 setAlpha = function(value = "numeric"){
                                   private$.alpha <- value
                                 },
                                 #' @field minSamples
                                 #' Returns the instance variable minSamples.
                                 #' (numeric)
                                 minSamples = function(){
                                   return(private$.minSamples)
                                 },
                                 #' @field outliersStatistics
                                 #' Returns the instance variable outliersStatistics.
                                 #' (tibble::tibble)
                                 outliersStatistics = function(){
                                   return(private$.outliersStatistics)
                                 }
                                 ),#active
                              ###################
                              # memory management
                              ###################
                               public = list(
                                 #' @description
                                 #' Creates and returns a new `pgu.outliers` object.
                                 #' @param data
                                 #' The data to be cleaned.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' A new `pgu.outliers` object.
                                 #' (pguIMP::pgu.outliers)
                                 #' @examples
                                 #' y <- tibble:tibble()
                                 #' x <- pguIMP:pgu.outliers$new(data = y)
                                 initialize = function(data = "tbl_df"){
                                   self$setAlpha <- 0.05
                                   private$.minSamples <- 6
                                   private$.cleaningAgentAlphabet <- c("none", "median", "mean", "mu", "mc", "knn", "pmm", "cart", "rf", "M5P", "amelia", "amelia_bound")
                                   self$setSeed <- 42.0
                                   self$setIterations <- 4
                                   self$setCleaningAgent <- self$cleaningAgentAlphabet[1]
                                   if(class(data)[1] != "tbl_df"){
                                     data <- tibble::tibble(names <- "none",
                                                            values <- c(NA))
                                   }
                                   self$resetOutliersParameter(data)
                                 },#function
                                 #' @description
                                 #' Clears the heap and
                                 #' indicates that instance of `pgu.outliers` is removed from heap.
                                 finalize = function(){
                                   print("Instance of pgu.outliers removed from heap")
                                 },#function

                                 ##########################
                                 # print instance variables
                                 ##########################
                                 #' @description
                                 #' Prints instance variables of a `pgu.outliers` object.
                                 #' @return
                                 #' string
                                 #' @examples
                                 #' x$print()
                                 #' print(x)
                                 print = function(){
                                   rString <- sprintf("\npgu.outliers\n")
                                   cat(rString)
                                   uString <- sprintf("\nseed: %.3e\niterations: $i\ncleaningAgent: %s\noutliers:\n", self$seed, self$iterations, self$cleaningAgent)
                                   cat(uString)
                                   print(self$outliers)
                                   print(self$outliersParameter)
                                   print(self$outliersStatistics)
                                   cat("\n\n")
                                   invisible(self)
                                 }, #function

                                 ####################
                                 # public functions #
                                 ####################
                                 #' @description
                                 #' Resets instance variables and
                                 #' performes Grubb's test for outliers to detect outliers in the normalized and Z-score transfromed data set.
                                 #' Progresse is indicated by the progress object passed to the function.
                                 #' @param data
                                 #' Dataframe to be analyzed.
                                 #' (tibble::tibble)
                                 #' @param progress
                                 #' Keeps track of the analysis progress.
                                 #' (shiny::Progress)
                                 #' @examples
                                 #' x$resetOutliersParameter(data, progress)
                                 resetOutliersParameter = function(data = "tbl_df", progress = "Progress"){
                                   numericData <- data %>%
                                     dplyr::select_if(is.numeric)
                                   features <- numericData %>%
                                     colnames()
                                   measurements <- c(rep(0.0, length(features)))
                                   existings <- c(rep(0.0, length(features)))
                                   outliers <- c(rep(0.0, length(features)))
                                   fractionOfOutliers <- c(rep(0.0, length(features)))
                                   private$.outliersParameter <- tibble::tibble(features, measurements, existings, outliers, fractionOfOutliers)
                                   private$.outliers <- tibble::tibble(measurement = numeric(0),
                                                                       feature = character(0),
                                                                       values = numeric(0),
                                                                       type = character(0),
                                                                       color = character(0)) %>%
                                     dplyr::mutate_if(is.numeric, round, 8)
                                   self$findOutliers(data, progress)
                                 }, #function

                                 ####################
                                 # helper functions #
                                 ####################
                                 #' @description
                                 #' Filters attributes from the given dataframe that are known to the class.
                                 #' @param data
                                 #' Dataframe to be filtered.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' A filterd dataframe.
                                 #' (tibble::tibble)
                                 #' @examples
                                 #' matrix <- x$filterFeatures(data)
                                 filterFeatures = function(data = "tbl_df"){
                                   data %>%
                                     dplyr::select(private$.outliersParameter[["features"]]) %>%
                                     return()
                                 }, #function

                                 #' @description
                                 #' Returns the position of an attribute's outliers within a data frame.
                                 #' @param featureName
                                 #' The attribute's name.
                                 #' (character)
                                 #' @return
                                 #' The postion of entries identified as outliers.
                                 #' (numeric)
                                 #' @examples
                                 #' idx <- x$outliersIdxByFeature(featureName = "infected")
                                 outliersIdxByFeature = function(featureName = "character"){
                                   self$outliers %>%
                                     dplyr::filter(feature == featureName) %>%
                                     dplyr::pull(measurement) %>%
                                     as.integer() %>%
                                     return()
                                 }, #function

                                 #' @description
                                 #' Characterizes each row of the data frame as either `complete`
                                 #' or indicates which attribute has been identified as an outlier within the row.
                                 #' If multiple attributes' row entries were identified as outliers, the row is characterized by `multiple`.
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' Vector of row characteristics.
                                 #' (character)
                                 #' @examples
                                 #' idx <- x$outliersFeatureList(data)
                                 outliersFeatureList = function(data = "tbl_df"){
                                   outFeature <- c(rep("complete", nrow(data)))
                                   if (nrow(self$outliers) > 0) {
                                     for(i in seq(from = 1,to = nrow(self$outliers), by =1)){
                                       if (grepl("complete", outFeature[self$outliers[[i,"measurement"]]])){
                                         outFeature[self$outliers[[i,"measurement"]]] <- self$outliers[[i, "feature"]]
                                       }#if
                                       else{
                                         outFeature[self$outliers[[i,"measurement"]]] <- "multiple"
                                       }#else
                                     }#for
                                   }#if
                                   return(outFeature)
                                 },#function

                                 ###################
                                 # detect outliers #
                                 ###################
                                 #' @description
                                 #' Performes Grubb's test for outliers to detect a single outlier in the provided attributes data.
                                 #' If an outlier is found, it is written to the instance variable `outliers`.
                                 #' The function indicates a find by a positive feedback.
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @param  feature
                                 #' The attribute within the data frame to be analyzed.
                                 #' @return
                                 #' Feedback if an outlier was found.
                                 #' (logical)
                                 #' @examples
                                 #' y <- x$runGrubbs(data, feature = "infected")
                                 runGrubbs = function(data = "tbl_df", feature = "character"){
                                   result <- outliers::grubbs.test(self$featureData, opposite = FALSE)
                                   if(result$p.value < self$alpha){
                                     if (grepl("lowest", result$alternative)){
                                       col <- "blue"
                                       t <- "min"
                                       idx <- which.min(self$featureData)
                                     }#if
                                     else{
                                       col <- "firebrick"
                                       t <- "max"
                                       idx <- which.max(self$featureData)
                                     }#else
                                     value <- data[[idx, feature]]
                                     private$.outliers <- tibble::add_row(self$outliers,
                                                                          measurement = as.numeric(idx),
                                                                          feature = as.character(feature),
                                                                          values = as.numeric(value),
                                                                          type = as.character(t),
                                                                          color = as.character(col))

                                     private$.featureData[idx] <- NA
                                     return (TRUE)
                                   }#if
                                   else{
                                     return (FALSE)
                                   }#else
                                 },#function

                                 #' @description
                                 #' Performes Grubb's test for outliers ontil all outliers in the dataframe are identified.
                                 #' Displays the progress if shiny is loaded.
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @param progress
                                 #' If shiny is loaded, the analysis' progress is stored within this instance of the shiny Progress class.
                                 #' (shiny::Progress)
                                 #' @examples
                                 #' x$findOutliers(data, progress)
                                 findOutliers = function(data = "tbl_df", progress = "Progress"){
                                   for (feature in self$outliersParameter[["features"]]){
                                     if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                       progress$inc(1)
                                     }#if
                                     private$.featureData <- data[[feature]]
                                     foundOutlier <- TRUE
                                     while (foundOutlier) {
                                       foundOutlier <- self$runGrubbs(data, feature)
                                     }#while
                                   }#for
                                   self$outlierStatistics(data)
                                 },#function

                                 ######################
                                 # outlier statistics #
                                 ######################
                                 #' @description
                                 #' Calculates the statistics on the previously performed outlier detection analysis
                                 #' and stores the results in the instance variable `outliersStatistcs`.
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @examples
                                 #' x$outlierStatistics(data)
                                 outlierStatistics = function(data = "tbl_df"){
                                   absCount <- data %>%
                                     self$filterFeatures() %>%
                                     dplyr::summarise_all(~sum(!is.na(.)))
                                   private$.outliersStatistics <- tibble::tibble(features = colnames(absCount),
                                                                                 absCount = absCount %>%
                                                                                   unlist() %>%
                                                                                   as.numeric())
                                   outlierCount <- c(rep(0.0, length(self$outliersParameter[["features"]])))
                                   minCount <- c(rep(0.0, length(self$outliersParameter[["features"]])))
                                   maxCount <- c(rep(0.0, length(self$outliersParameter[["features"]])))
                                   if(nrow(self$outliers) > 0){
                                     for (i in seq(from = 1,to = length(self$outliersParameter[["features"]]), by =1)) {
                                       outlierCount[i] <- sum(self$outliers["feature"] == self$outliersParameter[[i, "features"]])
                                       minCount[i] <- self$outliers %>%
                                         dplyr::filter(type == "min" & feature == self$outliersParameter[[i, "features"]]) %>%
                                         dplyr::select("feature") %>%
                                         dplyr::count() %>%
                                         unlist() %>%
                                         as.numeric()
                                       maxCount[i] <- self$outliers %>%
                                         dplyr::filter(type == "max" & feature == self$outliersParameter[[i, "features"]]) %>%
                                         dplyr::select("feature") %>%
                                         dplyr::count() %>%
                                         unlist() %>%
                                         as.numeric()

                                     }#for
                                   }#if
                                   private$.outliersStatistics <- self$outliersStatistics %>%
                                     dplyr::mutate(outlierCount = outlierCount,
                                                   low = minCount,
                                                   high = maxCount,
                                                   outlierFraction = 100* outlierCount/absCount) %>%
                                     dplyr::mutate(minFraction = 100 * low/absCount,
                                                   maxFraction = 100 * high/absCount)
                                 },#function

                                 ###################
                                 # handle outliers #
                                 ###################
                                 #' @description
                                 #' Includes imputation sites into the data frame.
                                 #' Outliers are replaces by NA.
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' Returns a data frame with outliers replaced by NA.
                                 #' @examples
                                 #' x$insertImputationSites(data)
                                 insertImputationSites = function(data = "tbl_df"){
                                   for(feature in self$outliersParameter[["features"]]){
                                     indices <- self$outliers %>%
                                       dplyr::filter(!!feature == feature) %>%
                                       dplyr::pull(measurement) %>%
                                       as.integer()
                                     data <- data %>%
                                       dplyr::mutate(!!feature := replace(x = !!as.name(feature),
                                                                          list = indices,
                                                                          values = NA))
                                   }#for
                                   return(data)
                                 },#function

                                 #' @description
                                 #' Chooseses a cleaning method based upon the instance variable `cleaningAgent` and handles the imputation sites in the dataframe.
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
                                 #' x$handleOutliers(data, progress)
                                 handleOutliers = function(data = "tbl_df", progress = "Progress"){
                                   cleanedData <- switch(self$cleaningAgent,
                                                         "none" = {data},
                                                         "median" = {data %>%
                                                             self$filterFeatures() %>%
                                                             self$insertImputationSites() %>%
                                                             self$cleanByMedian(progress = progress)},
                                                         "mean" = {data %>%
                                                             self$filterFeatures() %>%
                                                             self$insertImputationSites() %>%
                                                             self$cleanByMean(progress = progress)},
                                                         "mu" = {data %>%
                                                             self$filterFeatures() %>%
                                                             self$insertImputationSites() %>%
                                                             self$cleanByExpectationValue(progress = progress)},
                                                         "mc" = {data %>%
                                                             self$filterFeatures() %>%
                                                             self$insertImputationSites() %>%
                                                             self$cleanByMC(progress = progress)},
                                                         "knn" = {data %>%
                                                             self$filterFeatures() %>%
                                                             self$insertImputationSites() %>%
                                                             self$cleanByKnn(progress = progress)},
                                                         "pmm" = {data %>%
                                                             self$filterFeatures() %>%
                                                             self$insertImputationSites() %>%
                                                             self$cleanByMice("pmm", progress = progress)},
                                                         "cart" = {data %>%
                                                             self$filterFeatures() %>%
                                                             self$insertImputationSites() %>%
                                                             self$cleanByMice("cart", progress = progress)},
                                                         "rf" = {data %>%
                                                             self$filterFeatures() %>%
                                                             self$insertImputationSites() %>%
                                                             self$cleanByMice("rf", progress = progress)},
                                                         "M5P" = {data %>%
                                                             self$filterFeatures() %>%
                                                             self$insertImputationSites() %>%
                                                             self$cleanByM5P(progress = progress)},
                                                         "amelia" = {data %>%
                                                             self$filterFeatures() %>%
                                                             self$insertImputationSites() %>%
                                                             self$cleanByAmelia(progress = progress)},
                                                         "amelia_bound" = {data %>%
                                                             self$filterFeatures() %>%
                                                             self$insertImputationSites() %>%
                                                             self$cleanByAmeliaBound(progress = progress)}
                                   )#switch
                                   data %>%
                                     dplyr::select(-dplyr::one_of(self$outliersParameter[["features"]])) %>%
                                     dplyr::bind_cols(cleanedData) %>%
                                     dplyr::select(colnames(data)) %>%
                                     return()
                                 },#function

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
                                 #' x$cleanByMedian(data, progress)
                                 cleanByMedian = function(data = "tbl_df", progress = "Progress"){
                                   for (feature in self$outliersParameter[["features"]]){
                                     if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                       progress$inc(1)
                                     }#if
                                     indices <- self$outliersIdxByFeature(feature)
                                     data <- data %>%
                                       dplyr::mutate(!!feature := replace(!!as.name(feature),
                                                                          indices,
                                                                          stats::median(!!as.name(feature), na.rm = TRUE)))
                                   }#for
                                   return(data)
                                 },#function

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
                                 #' x$cleanByMean(data, progress)
                                 cleanByMean = function(data = "tbl_df", progress = "Progress"){
                                   for (feature in self$outliersParameter[["features"]]){
                                     if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                       progress$inc(1)
                                     }#if
                                     indices <- self$outliersIdxByFeature(feature)
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
                                 #' x$cleanByExpectationValue(data, progress)
                                 cleanByExpectationValue = function(data = "tbl_df", progress = "Progress"){
                                   for (feature in self$outliersParameter[["features"]]){
                                     if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                       progress$inc(1)
                                     }#if
                                     indices <- self$outliersIdxByFeature(feature)
                                     data <- data %>%
                                       dplyr::mutate(!!feature := replace(!!as.name(feature),
                                                                          indices,
                                                                          0.0))
                                   }#for
                                   return(data)
                                 },#function

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
                                 #' x$cleanByMC(data, progress)
                                 cleanByMC = function(data = "tbl_df", progress = "Progress"){
                                   imputed_df <- data
                                   for (feature in self$outliersParameter[["features"]]){
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
                                       indices <- self$outliersIdxByFeature(feature)
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
                                     indices <- self$outliersIdxByFeature(feature)
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
                                 #' x$cleanByKnn(data, progress)
                                 cleanByKnn =  function(data = "tbl_df", progress = "Progress"){
                                   cleanedData <- data %>%
                                     as.data.frame() %>%
                                     DMwR::knnImputation(k=3,
                                                         scale = TRUE,
                                                         meth = "weighAvg",
                                                         distData = NULL) %>%
                                     tibble::as_tibble()

                                   for (feature in self$outliersParameter[["features"]]){
                                     if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                       progress$inc(1)
                                     }#if
                                     indices <- self$outliersIdxByFeature(feature)
                                     data %>%
                                       dplyr::slice(indices) %>%
                                       dplyr::pull(feature)


                                     data <- data %>%
                                       dplyr::mutate(!!feature := replace(!!as.name(feature),
                                                                          indices,
                                                                          cleanedData %>%
                                                                            dplyr::slice(indices) %>%
                                                                            dplyr::pull(feature)))
                                   }#for
                                   return(data)
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
                                 #' x$cleanByMice(data, progress)
                                 cleanByMice = function(data, method = "character", progress = "Progress") {
                                   if(ncol(data) < 2){
                                     return(data)
                                   }#if
                                   else{
                                     cleaned_df <- data
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
                                     for (col_name in data_col_names) {
                                       if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                         progress$inc(1)
                                       }#if
                                       indices <- self$outliersIdxByFeature(col_name)

                                       cleaned_df <- cleaned_df %>%
                                         dplyr::mutate(!!col_name := replace(!!as.name(col_name),
                                                                             indices,
                                                                             imputed_df %>%
                                                                               dplyr::slice(indices) %>%
                                                                               dplyr::pull(col_name)))
                                     }#for

                                     return(cleaned_df)
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
                                 #' x$cleanByM5P(data, progress)
                                 cleanByM5P =  function(data = "tl_df", progress = "Progress"){
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

                                       na_idx <- self$outliersIdxByFeature(featureName = data_col_names[i])

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
                                 },#function

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
                                 #' x$cleanByAmelia(data, progress)
                                 cleanByAmelia = function(data = "tbl_df", progress = "Progress"){
                                   ameliaOutput <- data %>%
                                     Amelia::amelia(m = 1, parallel = "multicore", ncpus = 8)
                                   cleanedData <- ameliaOutput$imputations[[1]] %>%
                                     tibble::as_tibble()

                                   for (feature in self$outliersParameter[["features"]]){
                                     if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                       progress$inc(1)
                                     }#if
                                     indices <- self$outliersIdxByFeature(feature)
                                     data %>%
                                       dplyr::slice(indices) %>%
                                       dplyr::pull(feature)

                                     data <- data %>%
                                       dplyr::mutate(!!feature := replace(!!as.name(feature),
                                                                          indices,
                                                                          cleanedData %>%
                                                                            dplyr::slice(indices) %>%
                                                                            dplyr::pull(feature)))
                                   }#for
                                   return(data)
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
                                 #' x$cleanByAmeliaBound(data, progress)
                                 cleanByAmeliaBound = function(data = "tbl_df", progress = "Progress"){
                                   if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                     progress$inc(length(self$imputationParameter[["features"]]))
                                   }#if
                                   tblBounds = data.frame(column=c(1:ncol(data)),
                                                          lower=c(0),
                                                          upper=sapply(data,stats::quantile,probs = c(0.25),names=FALSE, na.rm = TRUE)) %>%
                                     as.matrix()
                                   ameliaOutput <- data %>%
                                     Amelia::amelia(m = 1,bounds=tblBounds, parallel = "multicore", ncpus = 8)
                                   cleanedData <- ameliaOutput$imputations[[1]] %>%
                                     tibble::as_tibble()

                                   for (feature in self$outliersParameter[["features"]]){
                                     if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                       progress$inc(1)
                                     }#if
                                     indices <- self$outliersIdxByFeature(feature)
                                     data %>%
                                       dplyr::slice(indices) %>%
                                       dplyr::pull(feature)

                                     data <- data %>%
                                       dplyr::mutate(!!feature := replace(!!as.name(feature),
                                                                          indices,
                                                                          cleanedData %>%
                                                                            dplyr::slice(indices) %>%
                                                                            dplyr::pull(feature)))
                                   }#for
                                   return(data)
                                 }, #function

                                 ####################
                                 # output functions #
                                 ####################
                                 #' @description
                                 #' Creates a datatable with outliers highlightes by colored background.
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' Al colored datatable
                                 #' (DT::datatable)
                                 #' @examples
                                 #' data %>%
                                 #'  x$dataTable() %>%
                                 #'  print()
                                 dataTable = function(data = "tbl_df"){
                                   t <- data %>%
                                     dplyr::mutate_if(is.numeric, round, 3) %>%
                                     DT::datatable(
                                       options = list(scrollX = TRUE,
                                                      scrollY = '350px',
                                                      paging = FALSE)
                                     )
                                   for (featureName in self$outliersParameter[["features"]]){
                                     featureOutlier <- self$outliers %>%
                                       dplyr::filter(grepl(featureName, feature)) %>%
                                       dplyr::mutate_if(is.numeric, round, 3)
                                     if (nrow(featureOutlier)>0){
                                       t <- DT::formatStyle(t,
                                                            featureName,
                                                            backgroundColor = styleEqual(data %>%
                                                                                           dplyr::select(!!featureName) %>%
                                                                                           dplyr::slice(featureOutlier[["measurement"]]) %>%
                                                                                           unlist() %>%
                                                                                           as.numeric() %>%
                                                                                           round(digits = 3),
                                                                                         featureOutlier[["color"]])
                                       )
                                     }#if
                                   }#for
                                   return(t)
                                 },#function

                                 #' @description
                                 #' Creates a datatable with substituted outliers highlightes by colored background.
                                 #' @param data
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' A colored datatable
                                 #' (DT::datatable)
                                 #' @examples
                                 #' data %>%
                                 #'  x$outlierTable() %>%
                                 #'  print()
                                 outlierTable = function(data = "tbl_df"){
                                   idx <- self$outliers[["measurement"]][!duplicated(self$outliers[["measurement"]])]
                                   t <- data %>%
                                     dplyr::slice(idx) %>%
                                     dplyr::mutate_if(is.numeric, round, 3) %>%
                                     DT::datatable(
                                       options = list(
                                         scrollX = TRUE,
                                         scrollY = '350px',
                                         paging = FALSE)
                                     )
                                   for (featureName in self$outliersParameter[["features"]]){
                                     featureOutlier <- self$outliers %>%
                                       dplyr::filter(grepl(featureName, feature)) %>%
                                       dplyr::mutate_if(is.numeric, round, 3)
                                     if (nrow(featureOutlier)>0){
                                       t <- DT::formatStyle(t,
                                                            featureName,
                                                            backgroundColor = styleEqual(data %>%
                                                                                           dplyr::select(!!featureName) %>%
                                                                                           dplyr::slice(featureOutlier[["measurement"]]) %>%
                                                                                           unlist() %>%
                                                                                           as.numeric() %>%
                                                                                           round(digits = 3),
                                                                                         featureOutlier[["color"]])
                                       )
                                     }#if
                                   }#for
                                   return(t)
                                 },#function

                                 ##################
                                 # plot functions #
                                 ##################
                                 #' @description
                                 #' Displays the occurrence of outlier candidates per attribute as bar plot.
                                 #' @return
                                 #' A bar plot.
                                 #' (ggplot2::ggplot)
                                 #' @examples
                                 #' x$plotOutliersDistribution() %>%
                                 #'  show()
                                 plotOutliersDistribution = function(){
                                   p <- self$outliersStatistics %>%
                                     tidyr::gather('low', 'high', key = "type", value="typeCount") %>%
                                     dplyr::mutate(fraction = 100 * typeCount/absCount) %>%
                                     ggplot2::ggplot(mapping = ggplot2::aes_string(x = "features", y = "fraction", fill = "type"), na.rm=TRUE)+
                                     ggplot2::geom_col()+
                                     ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
                                   return(p)
                                 },#function

                                 #' @description
                                 #' Displays the distribution of an attribute's values as histogram.
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
                                 },#function


                                 #' @description
                                 #' Displays the distribution of an attribute's vlues as box plot.
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
                                   outFeature <- self$outliersFeatureList(data)
                                   p <- data %>%
                                     dplyr::select(feature) %>%
                                     dplyr::mutate(outFeature = outFeature) %>%
                                     tidyr::gather_(key="feature", value="measurement", feature) %>%
                                     ggplot2::ggplot(mapping=ggplot2::aes_string(x="feature",y="measurement"), na.rm=TRUE)+
                                     ggplot2::geom_boxplot(na.rm=TRUE)+
                                     ggplot2::geom_jitter(ggplot2::aes(colour=outFeature), na.rm=TRUE)
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
                                   p2 <- self$featureBarPlot(data, feature) +
                                     ggplot2::scale_x_discrete(position = "top") +
                                     ggplot2::coord_flip()
                                   p <- gridExtra::grid.arrange(p1,p2, layout_matrix = rbind(c(1,1,2),c(1,1,2)))
                                   return(p)
                                 }#function

                               )#public
)#class
