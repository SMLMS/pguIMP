#' @title pgu.imputation
#'
#' @description
#' Analyses and substitutes imputation sites in a data set.
#'
#' @details
#' Analyses imputation sites in a data set.
#' Replaces imputation sites by nissing values and substitutes NAs by classical and AI-powerd substitution algorithms.
#'
#' @format [R6::R6Class] object.
#' @section Construction:
#' x <- pguIMP::pgu.imputation$new()
#'
#' @import R6
#' @import tidyverse
#' @import gridExtra
#' @import outliers
#' @import MASS
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
                                 .imputationStatistics = "tbl_df",
                                 .imputationSites = "tbl_df",
                                 .imputationSiteDistribution = "matrix",
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
                                 #' @field imputationStatistics
                                 #' Returns the instance variable imputationStatistics.
                                 #' (tibble::tibble)
                                 imputationStatistics = function(){
                                   return(private$.imputationStatistics)
                                 },
                                 #' @field imputationSites
                                 #' Returns the instance variable imputationSites.
                                 #' (tibble::tibble)
                                 imputationSites = function(){
                                   return(private$.imputationSites)
                                 },
                                 #' @field imputationSiteDistribution
                                 #' Returns the instance variable imputationSiteDistribution.
                                 #' (matrix)
                                 imputationSiteDistribution = function(){
                                   return(private$.imputationSiteDistribution)
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
                                 #' @param seed
                                 #' Initially sets the instance variable seed.
                                 #' Default is 42.
                                 #' (integer)
                                 #' @param iterations
                                 #' Initially sets the instance variable iterations.
                                 #' Default is 4.
                                 #' (integer)
                                 #' @param imputationAgent
                                 #' Initially sets the instance variable imputationAgent.
                                 #' Default is "none".
                                 #' Options are: ""none", "median", "mean", "expValue", "monteCarlo", "knn", "pmm", "cart", "randomForest", "M5P".
                                 #' (string)
                                 #' @return
                                 #' A new `pgu.imputation` object.
                                 #' (pguIMP::pgu.imputation)
                                 #' @examples
                                 #' x <- pguIMP:pgu.imputation$new()
                                 initialize = function(seed = 42, iterations = 4, imputationAgent = "none"){
                                   private$.imputationAgentAlphabet <- c("none", "median", "mean", "expValue", "monteCarlo", "knn", "pmm", "cart", "randomForest", "M5P")
                                   self$setSeed <- seed
                                   self$setIterations <- iterations
                                   self$setImputationAgent <- imputationAgent
                                   self$gatherImputationSites()
                                   self$gatherImputationSiteStatistics()
                                   self$gatherImputationSiteDistribution()
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
                                   uString <- sprintf("\nseed: %i\niterations: %i\nimputationAgent: %s\nimputationStatistics:\n", self$seed, self$iterations, as.character(self$imputationAgent))
                                   cat(uString)
                                   print(self$imputationStatistics)
                                   print("imputationsites:")
                                   print(self$imputationSites)
                                   cat("\n\n")
                                   invisible(self)
                                 }, #function

                                 ####################
                                 # public functions #
                                 ####################
                                 #' @description
                                 #' Gathers imputation sites from pguIMP's missings and outliers class.
                                 #' @param  missings_df
                                 #' Dataframe comprising information about the imputation sites of pguIMP's missings class.
                                 #' (tibble::tibble)
                                 #' @param  outliers_df
                                 #' Dataframe comprising information about the imputation sites of pguIMP's outliers class.
                                 #' (tibble::tibble)
                                 #' @examples
                                 #' x$gatherImputtaionSites(data)
                                 gatherImputationSites = function(missings_df = "tbl_df", outliers_df = "tbl_df"){
                                   input_correct <- TRUE
                                   if(!tibble::is_tibble(missings_df)){
                                     input_correct <- FALSE
                                   }
                                   if(!tibble::is_tibble(outliers_df)){
                                     input_correct <- FALSE
                                   }
                                   if(input_correct){
                                     missings_df <- missings_df %>%
                                       dplyr::rename(idx = row) %>%
                                       dplyr::rename(feature = features) %>%
                                       dplyr::select(c("idx", "feature"))

                                     outliers_df <- outliers_df %>%
                                       dplyr::rename(idx = measurement) %>%
                                       dplyr::select(c("idx", "feature"))

                                     private$.imputationSites <- missings_df %>%
                                       dplyr::bind_rows(outliers_df) %>%
                                       dplyr::arrange(feature,idx)
                                   } else{
                                     print("Warning, pguImputation$gatherImputationsites got wrong inut format.")
                                     private$.imputationSites <- tibble::tibble(idx = integer(0),
                                                                                feature = character(0))
                                   }
                                 }, #function

                                 #' @description
                                 #' Gathers statistical information about imputation sites
                                 #' The information is stored within the classes instance variable `imputationStatistics`
                                 #' @param data_df
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @examples
                                 #' x$gatherInformationSiteStatistics(data_df)
                                 gatherImputationSiteStatistics = function(data_df = "tbl_df"){
                                   if(tibble::is_tibble(data_df)){
                                     private$.imputationStatistics <- private$.imputationSites %>%
                                       dplyr::group_by(feature) %>%
                                       dplyr::summarise(imputation_sites = dplyr::n_distinct(idx)) %>%
                                       dplyr::arrange(imputation_sites)

                                     for (name in colnames(data_df)){
                                       if(!any(grepl(name, private$.imputationStatistics$feature))){
                                         private$.imputationStatistics <- private$.imputationStatistics %>%
                                           tibble::add_row(feature = !!name, imputation_sites = 0)
                                       }#if
                                     }#for
                                     private$.imputationStatistics <- private$.imputationStatistics %>%
                                       dplyr::mutate(measurements = rep(nrow(data_df), nrow(private$.imputationStatistics))) %>%
                                       dplyr::mutate(trusted = measurements - imputation_sites) %>%
                                       dplyr::mutate(fraction_of_sites = 100.0 * imputation_sites / measurements) %>%
                                       dplyr::select(c("feature", "measurements", "trusted", "imputation_sites", "fraction_of_sites"))
                                   } else{
                                     private$.imputationStatistics <- tibble::tibble(feature = character(0),
                                                                                     measurements = integer(0),
                                                                                     trusted = integer(0),
                                                                                     imputation_sites = integer(0),
                                                                                     fraction_of_sites = numeric(0))
                                   }
                                 }, #function

                                 #' @description
                                 #' Gathers the distribution of imputation sites within the data frame.
                                 #' The information is stored within the classes instance variable `imputationSiteDistribution`
                                 #' @param data_df
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' A data frame
                                 #' (tibble::tibble)
                                 #' @examples
                                 #' x$gatherImputationSiteDistribution(data_df)
                                 gatherImputationSiteDistribution = function(data_df = "tbl_df"){
                                   if(tibble::is_tibble(data_df)){
                                   d <- data_df %>%
                                     as.data.frame() %>%
                                     mice::md.pattern(plot=FALSE)
                                   # colnames(d)[-1] <- "Sites"
                                   colnames(d)[length(colnames(d))] <- "Sites"
                                   rownames(d)[length(rownames(d))] <- "Sum"
                                   private$.imputationSiteDistribution <- d
                                   } else{
                                     private$.imputationSiteDistribution <- matrix(0)
                                   }
                                 }, #function

                                 #' @description
                                 #' Takes a dataframe, replaces the imputation sites indicated by the instance variable `imputationsites` by NA,
                                 #' and returns the mutated dataframe.
                                 #' @param data_df
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' A mutated version of data_df.
                                 #' (tibble::tibble)
                                 #' @examples
                                 #' x$insertImputationSites(data_df)
                                 insertImputationSites = function(data_df = "tbl_df"){
                                   for(name in colnames(data_df)){
                                     temp_idx <- self$imputationSites %>%
                                       dplyr::filter(feature == name) %>%
                                       dplyr::select(idx) %>%
                                       unlist() %>%
                                       as.integer()

                                     temp_values <- data_df %>%
                                       dplyr::select(name) %>%
                                       unlist() %>%
                                       as.numeric()

                                     temp_values[temp_idx] <- NA
                                     data_df <- data_df %>%
                                       dplyr::mutate(!!name := temp_values)
                                   }#for
                                   return(data_df)
                                 }, #function

                                 #' @description
                                 #' Takes a dataframe and analyses the imputation sites.
                                 #' @param data_df
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @examples
                                 #' x$analyzeImputationSites(data_df)
                                 analyzeImputationSites = function(data_df = "tbl_df"){
                                   numeric_df <- data_df %>%
                                     dplyr::select_if(is.numeric)
                                   self$gatherImputationSiteStatistics(numeric_df)
                                   numeric_df <- self$insertImputationSites(numeric_df)
                                   self$gatherImputationSiteDistribution(numeric_df)
                                   private$.amv <- VIM::aggr(numeric_df, plot=FALSE)
                                 }, #function

                                 #' ####################
                                 #' # public functions #
                                 #' ####################
                                 #' #' @description
                                 #' #' Resets instance variables and
                                 #' #' identifies missings in the normalized and Z-score transfromed data set.
                                 #' #' @param data
                                 #' #' Dataframe to be analyzed.
                                 #' #' (tibble::tibble)
                                 #' #' @examples
                                 #' #' x$resetImputtaionParameter(data)
                                 #' resetImputationStatistics = function(data_df = "tbl_df"){
                                 #'   numericData <- data_df %>%
                                 #'     dplyr::select_if(is.numeric)
                                 #'   features <- numericData %>%
                                 #'     colnames()
                                 #'   measurements <- c(rep(0.0, length(features)))
                                 #'   existings <- c(rep(0.0, length(features)))
                                 #'   missings <- c(rep(0.0, length(features)))
                                 #'   fractionOfMissings <- c(rep(0.0, length(features)))
                                 #'   private$.imputationStatistics <- tibble::tibble(features, measurements, existings, missings, fractionOfMissings)
                                 #'   private$.amv <- VIM::aggr(numericData, plot=FALSE)
                                 #'   self$gatherImputationStatistics(data_df)
                                 #'   self$detectImputationSites(data_df)
                                 #' } #function

                                 # reset imputation sites
                                 # reset pgumputation (both)
                                 # handle imputation sites

                                 #' ####################
                                 #' # helper functions #
                                 #' ####################
                                 #' #' @description
                                 #' #' Returns the position of an attribute within a data frame.
                                 #' #' @param feature
                                 #' #' The attribute's name.
                                 #' #' (character)
                                 #' #' @return
                                 #' #' The postion of the attribute.
                                 #' #' (numeric)
                                 #' #' @examples
                                 #' #' idx <- x$featureIdx(feature = "infected")
                                 #' featureIdx = function(feature = "character"){
                                 #'   idx <- match(feature, self$imputationStatistics[["features"]])
                                 #'   if(is.na(idx)){
                                 #'     rString <- sprintf("\nWarning in pgu.imputation: feature %s is not known\n",
                                 #'                        feature)
                                 #'     cat(rString)
                                 #'   }#if
                                 #'   return(idx)
                                 #' }, #function
                                 #'
                                 #' #' @description
                                 #' #' Selects features cotaining imputation sites from a dataset.
                                 #' #' @param data_df
                                 #' #' Dataframe to be analyzed.
                                 #' #' (tibble::tibble)
                                 #' #' @return
                                 #' #' The filtered data frame.
                                 #' #' (tibble::tibble)
                                 #' #' @examples
                                 #' #' idx <- x$filterFeatures(data)
                                 #' filterFeatures = function(data_df = "tbl_df"){
                                 #'   data_df %>%
                                 #'     dplyr::select(private$.imputationStatistics[["features"]]) %>%
                                 #'     return()
                                 #' }, #function
                                 #'
                                 #' @description
                                 #' Returns the position of an attribute's imputation sites within a data frame.
                                 #' @param featureName
                                 #' The attribute's name.
                                 #' (character)
                                 #' @return
                                 #' The postion of the imputation sites.
                                 #' (numeric)
                                 #' @examples
                                 #' idx <- x$imputationIdxByFeature(featureName = "infected")
                                 imputationSiteIdxByFeature =  function(featureName = "character"){
                                   self$imputationSites %>%
                                     dplyr::filter(feature == featureName) %>%
                                     dplyr::pull(idx) %>%
                                     as.integer() %>%
                                     return()
                                 }, #function

                                 #'
                                 #' @description
                                 #' Characterizes each row of the data frame as either `complete`
                                 #' or indicates which attribute are missing within the row.
                                 #' If multiple attributes' row entries are missing, the row is characterized by `multiple`.
                                 #' @param data_df
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' Vector of row characteristics.
                                 #' (character)
                                 #' @examples
                                 #' idx <- x$nanFeatureList(data)
                                 nanFeatureList = function(data_df = "tbl_df"){
                                   nanFeature <- c(rep("complete", nrow(data_df)))
                                   if (nrow(self$imputationSites) > 0) {
                                     for(i in seq(from = 1,to = nrow(self$imputationSites), by =1)){
                                       if (grepl("complete", nanFeature[self$imputationSites[[i,"idx"]]])){
                                         nanFeature[self$imputationSites[[i,"idx"]]] <- self$imputationSites[[i, "feature"]]
                                       }#if
                                       else{
                                         nanFeature[self$imputationSites[[i,"idx"]]] <- "multiple"
                                       }#else
                                     }#for
                                   }#if
                                   return(nanFeature)
                                 }, #function
                                 #'
                                 #' #######################
                                 #' # missings statistics #
                                 #' #######################
                                 #' #' @description
                                 #' #' Calculates the number of values of a vector.
                                 #' #' @param value
                                 #' #' A vector comprising numeric data.
                                 #' #' (numeric)
                                 #' #' @return
                                 #' #' The lenght of the vector.
                                 #' #' (numeric)
                                 #' #' @examples
                                 #' #' idx <- x$gatherMeasurements(value)
                                 #' gatherMeasurements = function(value = "numeric"){
                                 #'   return(length(value))
                                 #' }, #function
                                 #'
                                 #' #' @description
                                 #' #' Calculates the number of missing values of a vector.
                                 #' #' @param value
                                 #' #' A vector comprising numeric data.
                                 #' #' (numeric)
                                 #' #' @return
                                 #' #' The number of missing in the vector.
                                 #' #' (numeric)
                                 #' #' @examples
                                 #' #' idx <- x$gatherMissings(value)
                                 #' gatherMissings = function(value = "numeric"){
                                 #'   y <- sum(is.na(value))
                                 #'   return(y)
                                 #' }, #function
                                 #'
                                 #' #' @description
                                 #' #' Calculates the number of existing values of a vector.
                                 #' #' @param value
                                 #' #' A vector comprising numeric data.
                                 #' #' (numeric)
                                 #' #' @return
                                 #' #' The number of existing values in the vector.
                                 #' #' (numeric)
                                 #' #' @examples
                                 #' #' idx <- x$gatherExistings(value)
                                 #' gatherExistings = function(value = "numeric"){
                                 #'   y <- sum(!is.na(value))
                                 #'   return(y)
                                 #' }, #function
                                 #'
                                 #' #' @description
                                 #' #' Calculates the fraction of missing values of a vector.
                                 #' #' @param value
                                 #' #' A vector comprising numeric data.
                                 #' #' (numeric)
                                 #' #' @return
                                 #' #' The fraction of missing values in the vector.
                                 #' #' (numeric)
                                 #' #' @examples
                                 #' #' idx <- x$gatherFractionOfMissings(value)
                                 #' gatherFractionOfMissings =  function(value = "numeric"){
                                 #'   y <- 100.0*sum(is.na(value))/length(value)
                                 #'   return(y)
                                 #' }, #function
                                 #'
                                 #' #' @description
                                 #' #' Gathers statistical information about missing values
                                 #' #' that are provided by the classes public `gather` functions.
                                 #' #' The information is stored within the classes instance variable `imputationParameter`
                                 #' #' @param data
                                 #' #' The data frame to be analyzed.
                                 #' #' (tibble::tibble)
                                 #' #' @examples
                                 #' #' x$gatherInformationStatistics(value)
                                 #' gatherImputationStatistics = function(data = "tbl_df"){
                                 #'   filteredData <- data %>%
                                 #'     self$filterFeatures()
                                 #'   private$.imputationParameter["measurements"] <- filteredData %>%
                                 #'     apply(MARGIN=2, FUN=self$gatherMeasurements)
                                 #'   private$.imputationParameter["existings"] <- filteredData %>%
                                 #'     apply(MARGIN=2, FUN=self$gatherExistings)
                                 #'   private$.imputationParameter["missings"] <- filteredData %>%
                                 #'     apply(MARGIN=2, FUN=self$gatherMissings)
                                 #'   private$.imputationParameter["fractionOfMissings"] <- filteredData %>%
                                 #'     apply(MARGIN=2, FUN=self$gatherFractionOfMissings)
                                 #' }, #function
                                 #'
                                 #' ###########################
                                 #' # detect imputation sites #
                                 #' ###########################
                                 #' #' @description
                                 #' #' Detects missing values within the data frame and
                                 #' #' writes the to the instance variable `imputationsites`.
                                 #' #' @param data
                                 #' #' The data frame to be analyzed.
                                 #' #' (tibble::tibble)
                                 #' #' @param  feature
                                 #' #' The attribute within the data frame to be analyzed.
                                 #' #' @examples
                                 #' #' y <- x$detectImputationsites(data, feature = "infected")
                                 #' detectImputationSites = function(data = "tbl_df"){
                                 #'   private$.imputationSites <- data %>%
                                 #'     self$filterFeatures() %>%
                                 #'     is.na() %>%
                                 #'     which(arr.ind=TRUE) %>%
                                 #'     tibble::as_tibble() %>%
                                 #'     dplyr::mutate(features = self$imputationParameter[["features"]][col])
                                 #' }, #function
                                 #'
                                 ###########################
                                 # handle imputation sites #
                                 ###########################
                                 #' @description
                                 #' Chooses a cleaning method based upon the instance variable `imputationAgent`
                                 #' and handles the imputation sites in the dataframe.
                                 #' Returns a cleaned data set.
                                 #' Display the progress if shiny is loaded.
                                 #' @param data_df
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
                                 handleImputationSites = function(data_df = "tbl_df", progress = "Progress"){
                                   if(is.na(self$imputationAgent)){
                                     print("Warning: Error in pgu.imputation imputationAgent is not valid. Will be set to none.")
                                     self$setimputationAgent <- "none"
                                   }#if
                                   cleanedData <- switch((self$imputationAgent),
                                                         "none" = data_df,
                                                         "median" = self$imputeByMedian(data_df, progress),
                                                         "mean" = self$imputeByMean(data_df, progress),
                                                         "expValue" = self$imputeByExpectationValue(data_df, progress),
                                                         "monteCarlo" = self$imputeByMC(data_df, progress),
                                                         "knn" = self$imputeByKnn(data_df, progress),
                                                         "pmm" = self$imputeByMice(data_df,"pmm", progress),
                                                         "cart" = self$imputeByMice(data_df, "cart", progress),
                                                         "randomForest" = self$imputeByMice(data_df, "rf", progress),
                                                         "M5P" = self$imputeByM5P(data_df, progress)
                                   )
                                   return(cleanedData)
                                 }, #function

                                 #' @description
                                 #' Substitutes imputation sites by the median of the respective attribute.
                                 #' Returns the cleaned dataframe.
                                 #' Display the progress if shiny is loaded.
                                 #' @param data_df
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
                                 imputeByMedian = function(data_df = "tbl_df", progress = "Progress"){
                                   for (feature in self$imputationStatistics[["feature"]]){
                                     if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                       progress$inc(1.0/ncol(data_df))
                                     }#if
                                     indices <- self$imputationSiteIdxByFeature(feature)
                                     data_df <- data_df %>%
                                       dplyr::mutate(!!feature := replace(!!as.name(feature),
                                                                          indices,
                                                                          stats::median(!!as.name(feature), na.rm = TRUE)))
                                   }#for
                                   return(data_df)
                                 }, #function

                                 #' @description
                                 #' Substitutes imputation sites by the aritmertic mean of the respective attribute.
                                 #' Returns the cleaned dataframe.
                                 #' Display the progress if shiny is loaded.
                                 #' @param data_df
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
                                 imputeByMean = function(data_df = "tbl_df", progress = "Progress"){
                                   for (feature in self$imputationStatistics[["feature"]]){
                                     if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                       progress$inc(1.0/ncol(data_df))
                                     }#if
                                     indices <- self$imputationSiteIdxByFeature(feature)
                                     data_df <- data_df %>%
                                       dplyr::mutate(!!feature := replace(!!as.name(feature),
                                                                          indices,
                                                                          mean(!!as.name(feature), na.rm = TRUE)))
                                   }#for
                                   return(data_df)
                                 }, #function

                                 #' @description
                                 #' Substitutes imputation sites by the expectation value of the respective attribute.
                                 #' Returns the cleaned dataframe.
                                 #' Display the progress if shiny is loaded.
                                 #' @param data_df
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
                                 imputeByExpectationValue = function(data_df = "tbl_df", progress = "Progress"){
                                   for (feature in self$imputationStatistics[["feature"]]){
                                     if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                       progress$inc(1.0/ncol(data_df))
                                     }#if

                                     tryCatch({
                                       fit_obj <- data_df %>%
                                         dplyr::select(feature) %>%
                                         tidyr::drop_na() %>%
                                         dplyr::pull(feature) %>%
                                         as.double() %>%
                                         MASS::fitdistr("normal")

                                       mu <- fit_obj$estimate["mean"] %>%
                                         as.numeric()
                                     }, error = function(e) {
                                       error_string <- sprintf("\nWarning in pgu.imputation$imputeByExpectationValue: Could not determine expectation value of feature %s. Used mean value instead.\n", feature)
                                       cat(error_string)
                                       mu <- data_df %>%
                                         dplyr::select(feature) %>%
                                         tidyr::drop_na() %>%
                                         dplyr::pull(feature) %>%
                                         as.double() %>%
                                         mean()
                                     }
                                     )

                                     indices <- self$imputationSiteIdxByFeature(feature)
                                     data_df <- data_df %>%
                                       dplyr::mutate(!!feature := replace(!!as.name(feature),
                                                                          indices,
                                                                          mu))
                                   }#for
                                   return(data_df)
                                 }, #function

                                 #' @description
                                 #' Substitutes imputation sites by values generated by a monte carlo simulation.
                                 #' The procedure runs several times as defined by the instance variable `iterations`.
                                 #' The run with the best result is identified and used for substitution.
                                 #' Returns the cleaned dataframe.
                                 #' Display the progress if shiny is loaded.
                                 #' @param data_df
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
                                 imputeByMC = function(data_df = "tbl_df", progress = "Progress"){
                                   imputed_df <- data_df
                                   for (feature in self$imputationStatistics[["feature"]]){
                                     if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                       progress$inc(1.0/ncol(data_df))
                                     }#if
                                     stats0 <- data_df %>%
                                       dplyr::select(feature) %>%
                                       unlist() %>%
                                       psych::describe()

                                     mu <- data_df %>%
                                       dplyr::select(feature) %>%
                                       tidyr::drop_na() %>%
                                       dplyr::pull(feature) %>%
                                       as.double() %>%
                                       mean()

                                     sigma <- data_df %>%
                                       dplyr::select(feature) %>%
                                       tidyr::drop_na() %>%
                                       dplyr::pull(feature) %>%
                                       as.double() %>%
                                       sd()

                                     stats <- matrix(NA, ncol= self$iterations, nrow = 13)
                                     for (j in 1:self$iterations) {
                                       set.seed(self$seed + j - 1)
                                       indices <- self$imputationSiteIdxByFeature(feature)
                                       mcVal <- stats::rnorm(n = length(indices),
                                                             mean = mu,
                                                             sd = sigma)
                                       complete_Data <- data_df %>%
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
                                                           mean = mu,
                                                           sd = sigma)
                                     complete_Data <- data_df %>%
                                       dplyr::mutate(!!feature := replace(!!as.name(feature),
                                                                          indices,
                                                                          mcVal))

                                     imputed_df <- imputed_df %>%
                                       dplyr::mutate(!!feature := complete_Data %>%
                                                       dplyr::pull(feature))
                                   }#for
                                   return(imputed_df)
                                 }, #function

                                 #' @description
                                 #' Substitutes imputation sites by predictions of a KNN analysis of the whole dataframe.
                                 #' Returns the cleaned dataframe.
                                 #' Display the progress if shiny is loaded.
                                 #' @param data_df
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
                                 imputeByKnn = function(data_df = "tbl_df", progress = "Progress"){
                                   if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                     progress$inc(1)
                                   }#if
                                   if (ncol(data_df) < 4){
                                     return(data_df)
                                   }#if
                                   else{
                                     data_df %>%
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
                                 #' @param data_df
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
                                 imputeByMice = function(data_df, method = "character", progress = "Progress") {
                                   if(ncol(data_df) < 2){
                                     return(data_df)
                                   }#if
                                   else{
                                     data_col_names <- colnames(data_df)
                                     colnames(data_df) <- paste0("F", seq(1:ncol(data_df))) %>%
                                       as.character()
                                     imputed_df <- data_df
                                     for (col_name in colnames(data_df)) {
                                       stats0 <- data_df %>%
                                         dplyr::select(col_name) %>%
                                         unlist() %>%
                                         psych::describe()
                                       stats <- matrix(NA, ncol= self$iterations, nrow = 13)
                                       for (j in 1:self$iterations) {
                                         if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                           progress$inc(1.0/(self$iterations*ncol(data_df)))
                                         }#if
                                         imputed_Data <- data_df %>%
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
                                       imputed_Data <- data_df %>%
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
                                 #' @param data_df
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
                                 imputeByM5P = function(data_df = "tl_df", progress = "Progress"){
                                   if(ncol(data_df) < 2){
                                     return(data_df)
                                   }#if
                                   else{
                                     data_col_names <- colnames(data_df)
                                     colnames(data_df) <- paste0("F", seq(1:ncol(data_df))) %>%
                                       as.character()
                                     imputed_df <- data_df
                                     for (i in 1:length(colnames(data_df))) {
                                       if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                         progress$inc(1.0/ncol(data_df))
                                       }#if

                                       na_idx <- self$imputationSiteIdxByFeature(featureName = data_col_names[i])

                                       if((length(na_idx)<1) | length(na_idx) == nrow(data_df)){
                                         next
                                       }#if
                                       train_df <- data_df %>%
                                         dplyr::slice(-na_idx)

                                       na_df <- data_df %>%
                                         dplyr::slice(na_idx)

                                       m5 <- colnames(data_df)[i] %>%
                                         paste("~.") %>%
                                         as.formula() %>%
                                         RWeka::M5P(data = train_df)

                                       na_values <- predict(m5, newdata = na_df)

                                       for (j in 1:length(na_idx)){
                                         imputed_df[[na_idx[j], colnames(data_df)[i]]] <- na_values[j]
                                       }#for
                                     }#for
                                     colnames(imputed_df) <- data_col_names
                                     return(imputed_df)
                                   }#else
                                 }, #functions


                                 #' #' @description
                                 #' #' Substitutes imputation sites by values generated by a different methods of the amelia package.
                                 #' #' The procedure is run without bound information.
                                 #' #' Returns the cleaned dataframe.
                                 #' #' Display the progress if shiny is loaded.
                                 #' #' @param data_df
                                 #' #' The data frame to be analyzed.
                                 #' #' (tibble::tibble)
                                 #' #' @param progress
                                 #' #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                                 #' #' (shiny::Progress)
                                 #' #' @return
                                 #' #' Cleaned dataframe.
                                 #' #' (tibble:tibble)
                                 #' #' @examples
                                 #' #' x$imputeByAmelia(data, progress)
                                 #' imputeByAmelia = function(data_df = "tbl_df", progress = "Progress"){
                                 #'   if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                 #'     progress$inc(1)
                                 #'   }#if
                                 #'   ameliaOutput <- data_df %>%
                                 #'     Amelia::amelia()#(m = 1, parallel = "multicore", ncpus = 8)
                                 #'   ameliaOutput$imputations[[1]] %>%
                                 #'     tibble::as_tibble() %>%
                                 #'     return()
                                 #' }, #function

                                 #' #' @description
                                 #' #' Substitutes imputation sites by values generated by a different methods of the amelia package.
                                 #' #' The procedure is run with bound information.
                                 #' #' Returns the cleaned dataframe.
                                 #' #' Display the progress if shiny is loaded.
                                 #' #' @param data_df
                                 #' #' The data frame to be analyzed.
                                 #' #' (tibble::tibble)
                                 #' #' @param progress
                                 #' #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                                 #' #' (shiny::Progress)
                                 #' #' @return
                                 #' #' Cleaned dataframe.
                                 #' #' (tibble:tibble)
                                 #' #' @examples
                                 #' #' x$imputeByAmeliaBound(data, progress)
                                 #' imputeByAmeliaBound = function(data_df = "tbl_df", progress = "Progress"){
                                 #'   if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                 #'     progress$inc(length(self$imputationStatistics[["feature"]]))
                                 #'   }#if
                                 #'   tblBounds = data.frame(column=c(1:ncol(data_df)),
                                 #'                          lower=c(0),
                                 #'                          upper=sapply(data_df,stats::quantile,probs = c(0.25),names=FALSE, na.rm = TRUE)) %>%
                                 #'     as.matrix()
                                 #'   ameliaOutput <- data_df %>%
                                 #'     Amelia::amelia(bounds=tblBounds)#(m = 1, parallel = "multicore", ncpus = 8)
                                 #'   ameliaOutput$imputations[[1]] %>%
                                 #'     tibble::as_tibble() %>%
                                 #'     return()
                                 #' }, #function
                                 #'
                                 #' ##########
                                 #' # output #
                                 #' ##########
                                 #' #' @description
                                 #' #' Numeric representation of the distribution of missing values within the data frame.
                                 #' #' @param data
                                 #' #' The data frame to be analyzed.
                                 #' #' (tibble::tibble)
                                 #' #' @return
                                 #' #' A data frame
                                 #' #' (tibble::tibble)
                                 #' #' @examples
                                 #' #' data %>%
                                 #' #'  x$imputationSiteDistribution() %>%
                                 #' #'  print()
                                 #' imputationSiteDistribution = function(data = "tbl_df"){
                                 #'   d <- data %>%
                                 #'     self$filterFeatures() %>%
                                 #'     as.data.frame() %>%
                                 #'     mice::md.pattern(plot=FALSE)
                                 #'   # colnames(d)[-1] <- "Sites"
                                 #'   colnames(d)[length(colnames(d))] <- "Sites"
                                 #'   rownames(d)[length(rownames(d))] <- "Sum"
                                 #'   return(d)
                                 #' }, #function
                                 #'
                                 #'
                                 #' #' @description
                                 #' #' Merges the numeric attributes of the pguIMP data with its metadata.
                                 #' #' @param dfData
                                 #' #' The data frame to be analyzed.
                                 #' #' (tibble::tibble)
                                 #' #' @param dfMetadata
                                 #' #' The corresponding metadata.
                                 #' #' (tibble::tibble)
                                 #' #' @return
                                 #' #' A data frame
                                 #' #' (tibble::tibble)
                                 #' #' @examples
                                 #' #' data %>%
                                 #' #'  x$mergeImputationSiteData() %>%
                                 #' #'  print()
                                 #' mergeImputationSiteData = function(dfData = "tbl_df", dfMetadata = "tbl_df"){
                                 #'   dfMerge <- dfData
                                 #'   if(nrow(dfData) == nrow(dfMetadata)){
                                 #'     dfMerge <- dplyr::bind_cols(dfMetadata, dfData)
                                 #'   }#if
                                 #'   dfMerge %>%
                                 #'     dplyr::filter_all(dplyr::any_vars(is.na(.))) %>%
                                 #'     return()
                                 #' }, #function
                                 #'

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
                                             # labels=self$imputationStatistics[["feature"]],
                                             cex.axis=.7,
                                             gap=3,
                                             ylab=c("Histogram of imputation sites","Pattern"))
                                   return(p)
                                 }, #function

                                 #' @description
                                 #' Displays the distribution of an attribute values as histogram.
                                 #' @param data_df
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
                                 featureBarPlot = function(data_df = "tbl_df", feature = "character"){
                                   feature <- dplyr::sym(feature)
                                   p <- data_df %>%
                                     ggplot2::ggplot(mapping = ggplot2::aes_string(x=feature), na.rm=TRUE) +
                                     ggplot2::geom_bar(stat = "bin")
                                   return(p)
                                 }, #function

                                 #' @description
                                 #' Displays the distribution of an attribute's values as box plot.
                                 #' @param data_df
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
                                 featureBoxPlotWithSubset = function(data_df = "tbl_df", feature = "character"){
                                   nanFeature <- self$nanFeatureList(data_df)
                                   p <- data_df %>%
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
                                 #' @param data_df
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
                                 featurePlot = function(data_df = "tbl_df", feature = "character"){
                                   p1 <- self$featureBoxPlotWithSubset(data_df, feature) +
                                     ggplot2::theme(legend.position = c(0.9, 0.9),
                                                    legend.key = ggplot2::element_blank(),
                                                    legend.background = ggplot2::element_blank())

                                   limits1 <- ggplot2::layer_scales(p1)$y$range$range

                                   p2 <- self$featureBarPlot(data_df, feature)

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
