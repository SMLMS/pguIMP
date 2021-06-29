#' @title pgu.imputation
#'
#' @description
#' Analyses and substitutes imputation sites in a data set.
#'
#' @details
#' Analyses imputation sites in a data set.
#' Replaces imputation sites by missing values and substitutes NAs
#' by classical and ML-powered substitution algorithms.
#' This object is used by the shiny based gui and is not for use in individual R-scripts!
#'
#' @format [R6::R6Class] object.
#'
#' @importFrom dplyr all_of arrange bind_rows filter group_by mutate
#' @importFrom dplyr n_distinct pull rename rowwise select select_if
#' @importFrom dplyr slice summarise sym transmute_all ungroup
#' @importFrom ggplot2 aes aes_string coord_flip element_blank
#' @importFrom ggplot2 element_rect geom_bar geom_boxplot geom_hline
#' @importFrom ggplot2 geom_jitter geom_point geom_text ggplot
#' @importFrom ggplot2 ggtitle layer_scales scale_linetype_manual
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous theme theme_linedraw
#' @importFrom ggplot2 xlab xlim ylab ylim
#' @importFrom ggthemes geom_rangeframe theme_tufte
#' @importFrom gridExtra grid.arrange
#' @importFrom magrittr %>%
#' @importFrom MASS fitdistr
#' @importFrom mice complete flux md.pattern mice quickpred
#' @importFrom psych describe
#' @importFrom R6 R6Class
#' @importFrom RWeka M5P
#' @importFrom shiny Progress
#' @importFrom stats as.formula median rnorm
#' @importFrom tibble add_row as_tibble is_tibble rownames_to_column tibble
#' @importFrom tidyr drop_na expand_grid gather_
#' @importFrom VIM aggr
#'
#' @include pguDMwR.R
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
                                 .one_hot_df = "tbl_df",
                                 .imputationSiteDistribution = "matrix",
                                 .imputationAgentAlphabet = "character",
                                 .imputationAgent = "factor",
                                 .nNeighbors = "integer",
                                 .flux_df = "tbl_df",
                                 .outflux_thr = "numeric",
                                 .pred_frac = "numeric",
                                 .pred_mat = "matrix",
                                 .exclude_vec = "character",
                                 .seed = "numeric",
                                 .iterations = "numeric",
                                 .amv = "ANY",
                                 .success = "logical"
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
                                 #' @field one_hot_df
                                 #' Returns the positions of missings in one_hot encoding
                                 #' (tibble::tibble)
                                 one_hot_df = function(){
                                   return(private$.one_hot_df)
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
                                 #' @field nNeighbors
                                 #' Returns the instance variable nNeighbors.
                                 #' (integer)
                                 nNeighbors = function(){
                                   return(private$.nNeighbors)
                                 },
                                 #' @field setNNeighbors
                                 #' Sets the instance variable nNeighbors.
                                 #' (integer)
                                 setNNeighbors = function(value = "integer"){
                                   private$.nNeighbors <- abs(as.integer(value))
                                 },
                                 #' @field flux_df
                                 #' Returns the instance variable flux_df
                                 #' (tibble::tibble)
                                 flux_df = function(){
                                   return(private$.flux_df)
                                 },
                                 #' @field outflux_thr
                                 #' Returns the instance variable outflux_thr.
                                 #' (numeric)
                                 outflux_thr = function(){
                                   return(private$.outflux_thr)
                                 },
                                 #' @field setOutflux_thr
                                 #' Sets the instance variable outflux_thr.
                                 #' (numeric)
                                 setOutflux_thr = function(value = numeric){
                                   private$.outflux_thr <- as.numeric(value)
                                 },
                                 #' @field pred_frac
                                 #' Returns the instance variable pred_frac.
                                 #' (numeric)
                                 pred_frac = function(){
                                   return(private$.pred_frac)
                                 },
                                 #' @field setPred_frac
                                 #' Sets the instance variable pred_frac.
                                 #' (numeric)
                                 setPred_frac = function(value = "numeric"){
                                   private$.pred_frac <- abs(as.numeric(value))
                                 },
                                 #' @field pred_mat
                                 #' Returns the instance variable pred_mat.
                                 #' (matrix)
                                 pred_mat = function(){
                                   return(private$.pred_mat)
                                 },
                                 #' @field exclude_vec
                                 #' Returns the instance variable exclude_vec
                                 #' (character)
                                 exclude_vec = function(){
                                   return(private$.exclude_vec)
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
                                   if(value < 1){
                                     private$.seed <- 1
                                   }
                                   else if (value > 100){
                                     private$.seed <- 100
                                   }
                                   else{
                                     private$.seed <- value
                                   }
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
                                   if(value < 1){
                                     private$.iterations <- 1
                                   }
                                   else if (value > 100){
                                     private$.iterations<- 100
                                   }
                                   else{
                                     private$.iterations <- value
                                   }
                                 },
                                 #' @field amv
                                 #' Returns the instance variable amv.
                                 #' (numeric)
                                 amv = function(){
                                   return(private$.amv)
                                 },
                                 #' @field success
                                 #' Returns the instance variable success.
                                 #' (logical)
                                 success = function(){
                                   return(private$.success)
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
                                 #' @param nNeighbors
                                 #' Initially sets the instance variable nNeighbors.
                                 #' (integer)
                                 #' @param pred_frac
                                 #' Initially sets the instance variable pred_frac.
                                 #' (numeric)
                                 #' @param outflux_thr
                                 #' Initially sets the instance fariable outflux_thr
                                 #' @return
                                 #' A new `pgu.imputation` object.
                                 #' (pguIMP::pgu.imputation)
                                 initialize = function(seed = 42, iterations = 4, imputationAgent = "none", nNeighbors = 3, pred_frac = 1.0, outflux_thr = 0.5){
                                   private$.imputationAgentAlphabet <- c("none", "median", "mean", "mu", "mc", "knn", "pmm", "cart", "rf", "M5P")
                                   self$setSeed <- seed
                                   self$setIterations <- iterations
                                   self$setImputationAgent <- imputationAgent
                                   self$setNNeighbors <- nNeighbors
                                   self$setPred_frac <- pred_frac
                                   self$setOutflux_thr <- outflux_thr
                                   private$.success <- FALSE
                                   private$.pred_mat <- matrix()
                                   private$.flux_df <- tibble::tibble()
                                   private$.exclude_vec <- character(0)
                                   private$.one_hot_df <- tibble::tibble()
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
                                 gatherImputationSites = function(missings_df = "tbl_df", outliers_df = "tbl_df"){
                                   input_correct <- TRUE
                                   if(!tibble::is_tibble(missings_df)){
                                     input_correct <- FALSE
                                     missings_df <- tibble::tibble(row = integer(0),
                                                                   features = character(0))
                                     input_correct <- TRUE
                                   }
                                   if(!tibble::is_tibble(outliers_df)){
                                     input_correct <- FALSE
                                     outliers_df <- tibble::tibble(measurement = integer(0),
                                                                   feature = character(0))
                                     input_correct <- TRUE
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
                                 #' The information is stored within the classes instance variable imputationSiteDistribution.
                                 #' @param data_df
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' A data frame
                                 #' (tibble::tibble)
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
                                 #' Gathers statistical information about missing values
                                 #' in one hot format.
                                 #' The result is stored in the instance variable one_hot_df.
                                 #' @param data_df
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 one_hot = function(data_df = "tbl_df"){
                                   if(!tibble::is_tibble(data_df)){
                                     print("Warning: data_df needs to by of type tibble.")
                                     private$.one_hot_df <- tibble::tibble()
                                   }
                                   private$.one_hot_df <- data_df %>%
                                     dplyr::select_if(is.numeric) %>%
                                     dplyr::transmute_all(list(miss = ~ as.integer(is.na(.))))
                                 }, #function

                                 #' @description
                                 #' Takes a dataframe and analyses the imputation sites.
                                 #' @param data_df
                                 #' The data frame to be analyzed.
                                 #' (tibble::tibble)
                                 analyzeImputationSites = function(data_df = "tbl_df"){
                                   numeric_df <- data_df %>%
                                     dplyr::select_if(is.numeric)
                                   self$gatherImputationSiteStatistics(numeric_df)
                                   numeric_df <- self$insertImputationSites(numeric_df)
                                   self$one_hot(numeric_df)
                                   self$gatherImputationSiteDistribution(numeric_df)
                                   private$.amv <- VIM::aggr(numeric_df, plot=FALSE)
                                 }, #function


                                 #' @description
                                 #' Returns the position of an attribute's imputation sites within a data frame.
                                 #' @param featureName
                                 #' The attribute's name.
                                 #' (character)
                                 #' @return
                                 #' The postion of the imputation sites.
                                 #' (numeric)
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

                                 #####################
                                 # detect predictors #
                                 #####################

                                 #' @description
                                 #' Calculates the average number of predictors for a given dataframe and minpuc and mincor variables
                                 #' using the mice::quickpred routine.
                                 #' @param data_df
                                 #' The dataframe to be analyzed
                                 #' (tibble::tibble)
                                 #' @param minpuc
                                 #' Specifies the minimum threshold for the proportion of usable cases.
                                 #' (numeric)
                                 #' @param mincor
                                 #' Specifies the minimum threshold against which the absolute correlation in the dataframe is compared.
                                 #' (numeric)
                                 #' @return
                                 #' Average_number_of_predictors.
                                 #' (numeric)
                                 average_number_of_predictors = function(data_df = "tbl_df", minpuc = 0, mincor = 0.1){
                                   pred_dist <- data_df %>%
                                     # dplyr::select(-dplyr::all_of(self$exclude_vec)) %>%
                                     mice::quickpred(minpuc = minpuc, mincor = mincor, exclude = self$exclude_vec) %>%
                                     rowSums() %>%
                                     table()

                                   sum(as.numeric(names(pred_dist)) * as.numeric(pred_dist)) / sum(as.numeric(pred_dist)[2:length(pred_dist)]) %>%
                                     return()
                                 }, #function

                                 #' @description
                                 #' Identifies possible predictors for each feature.
                                 #' Analysis results are written to the instance variable pred_mat.
                                 #' Intermediate analysis results are an influx/outflux dataframe
                                 #' that is written to the instance variable flux_df and
                                 #' detect predictors and a list of features that is excluded from
                                 #' the search for possible predictors that is written to the
                                 #' instance variable exclude_vec.
                                 #' @param data_df
                                 #' The dataframe to be analyzed.
                                 #' (tibble::tibble)
                                 detectPredictors = function(data_df = "tbl_df"){
                                   private$.flux_df <- data_df %>%
                                     mice::flux() %>%
                                     tibble::rownames_to_column() %>%
                                     tibble::as_tibble()

                                   private$.exclude_vec <- self$flux_df %>%
                                     dplyr::filter(outflux < self$outflux_thr) %>%
                                     dplyr::select(c("rowname")) %>%
                                     dplyr::pull()

                                   quickpred_df <- tidyr::expand_grid(minpuc = seq(from=0.0, to=0.9, by=0.1),
                                                                      mincor = seq(from=0.0, to=0.9, by=0.1)) %>%
                                     dplyr::rowwise() %>%
                                     dplyr::mutate(mean = self$average_number_of_predictors(data_df, minpuc, mincor)) %>%
                                     dplyr::ungroup()

                                   quickpred_para <- quickpred_df %>%
                                     tidyr::drop_na() %>%
                                     dplyr::slice(which.min(abs(quickpred_df$mean - trunc(self$pred_frac * ncol(data_df)) )))

                                   minpuc <- quickpred_para$minpuc
                                   mincor <- quickpred_para$mincor

                                   private$.pred_mat <- data_df %>%
                                     mice::quickpred(minpuc = minpuc, mincor = mincor, exclude = self$exclude_vec)
                                 }, #function

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
                                 handleImputationSites = function(data_df = "tbl_df", progress = "Progress"){
                                   if(is.na(self$imputationAgent)){
                                     print("Warning: Error in pgu.imputation imputationAgent is not valid. Will be set to none.")
                                     self$setimputationAgent <- "none"
                                   }#if
                                   private$.success <- FALSE
                                   data_df <- self$insertImputationSites(data_df)
                                   cleanedData <- data_df
                                   tryCatch({
                                     cleanedData <- switch((self$imputationAgent),
                                                           "none" = data_df,
                                                           "median" = self$imputeByMedian(data_df, progress),
                                                           "mean" = self$imputeByMean(data_df, progress),
                                                           "mu" = self$imputeByExpectationValue(data_df, progress),
                                                           "mc" = self$imputeByMC(data_df, progress),
                                                           "knn" = self$imputeByKnn(data_df, progress),
                                                           "pmm" = self$imputeByMice(data_df, progress),
                                                           "cart" = self$imputeByMice(data_df, progress),
                                                           "rf" = self$imputeByMice(data_df, progress),
                                                           "M5P" = self$imputeByM5P(data_df, progress)
                                     )
                                     private$.success <- TRUE
                                   },
                                   error = function(e) {
                                     private$.success <- FALSE
                                     errorMesage <- sprintf("\nError in pgu.imputation during handleImputationSites routine:\n%s", e)
                                     cat(errorMesage)
                                   }#error
                                   )#tryCatch
                                   colnames(cleanedData) <- colnames(data_df)
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
                                 imputeByMC = function(data_df = "tbl_df", progress = "Progress"){
                                   imputed_df <- data_df
                                   # Calculate Errors
                                   stats0_mat <- data_df %>%
                                     psych::describe(na.rm=TRUE) %>%
                                     as.matrix()

                                   stats_mat_list = list()
                                   diff_mat_list = list()
                                   for (i in seq(from=1, to=self$iterations, by=1)){
                                     if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                       progress$inc(1.0/self$iterations)
                                     }#if

                                     imputed_df <- data_df
                                     set.seed(self$seed + i)
                                     for(feature in colnames(data_df)){
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

                                       indices <- self$imputationSiteIdxByFeature(feature)
                                       mcVal <- stats::rnorm(n = length(indices),
                                                             mean = mu,
                                                             sd = sigma)
                                       imputed_df <- imputed_df %>%
                                         dplyr::mutate(!!feature := replace(!!as.name(feature),
                                                                            indices,
                                                                            mcVal))
                                     }#for
                                     stats_mat_list[[i]] <- imputed_df %>%
                                       psych::describe(na.rm=TRUE) %>%
                                       as.matrix()

                                     diff_mat_list[[i]] <- stats0_mat - stats_mat_list[[i]]
                                   }# for

                                   # Calculate Ranks
                                   cumulative_diff_df <- tibble::tibble(statistics = colnames(diff_mat_list[[1]]))
                                   for(i in seq(1, self$iterations)){
                                     diff_sum <- rep(0.0, times=13)
                                     for(j in seq(1, ncol(data_df))){
                                       diff_sum <- diff_sum + (diff_mat_list[[i]][j,])^2
                                     }
                                     feature_name <- sprintf("iter_%i", i)
                                     cumulative_diff_df <- cumulative_diff_df %>%
                                       dplyr::mutate(!!feature_name := sqrt(diff_sum))
                                   }

                                   ranks_mat <- cumulative_diff_df %>%
                                     dplyr::select(-c("statistics")) %>%
                                     apply(MARGIN = 1, FUN = function(x)rank(x, ties.method = "min"))

                                   # determine optimal seed
                                   seed_additive <- ranks_mat %>%
                                     rowSums() %>%
                                     which.min() %>%
                                     as.integer()

                                   # calculate optimized imputation
                                   imputed_df <- data_df
                                   set.seed(self$seed + seed_additive)
                                   for(feature in colnames(data_df)){
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

                                     indices <- self$imputationSiteIdxByFeature(feature)
                                     mcVal <- stats::rnorm(n = length(indices),
                                                           mean = mu,
                                                           sd = sigma)
                                     imputed_df <- imputed_df %>%
                                       dplyr::mutate(!!feature := replace(!!as.name(feature),
                                                                          indices,
                                                                          mcVal))
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
                                 imputeByKnn = function(data_df = "tbl_df", progress = "Progress"){
                                   if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                     progress$inc(0.5)
                                   }#if
                                   if(!ncol(data_df) > 2){
                                     e <- simpleError("The number of features needs to be larger than 2.")
                                     stop(e)
                                   }#if
                                   if (nrow(data_df) < self$nNeighbors + 1){
                                     self$setNNeighbors <- nrow(data_df) - 1
                                     sprintf("\nWarning in pgu.imputation$imputeByKnn: nNeighbors set to: %i\n", self$nNeighbors) %>%
                                       cat()
                                   }#if
                                   data_df %>%
                                     as.data.frame() %>%
                                     print()
                                   data_df %>%
                                     as.data.frame() %>%
                                     pguIMP::knnImputation(k=self$nNeighbors,
                                                           scale = TRUE,
                                                           meth = "weighAvg",
                                                           distData = NULL
                                     ) %>%
                                     tibble::as_tibble() %>%
                                     return()
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
                                 #' @param progress
                                 #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                                 #' (shiny::Progress)
                                 #' @return
                                 #' Cleaned dataframe.
                                 #' (tibble:tibble)
                                 imputeByMice = function(data_df, progress = "Progress") {
                                   nPred <- trunc(ncol(data_df) * self$pred_frac)
                                   if (ncol(data_df) < nPred){
                                     e <- simpleError("nPred needs to be smaller that the number of features.")
                                     stop(e)
                                   }#if
                                   if(ncol(data_df) < 2){
                                     e <- simpleError("The number of features needs to be larger than 2.")
                                     stop(e)
                                   }#if

                                   # determine predictor matrix
                                   self$detectPredictors(data_df)

                                   # Calculate Errors for iterations
                                   stats0_mat <- data_df %>%
                                     psych::describe(na.rm=TRUE) %>%
                                     as.matrix()

                                   stats_mat_list = list()
                                   diff_mat_list = list()
                                   for (i in seq(1, self$iterations)){
                                     if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                       progress$inc(1.0/(self$iterations))
                                     }#if
                                     mice_model <- data_df %>%
                                       mice::mice(method = self$imputationAgent,
                                                  pred = self$pred_mat,
                                                  seed = self$seed + i,
                                                  printFlag = FALSE)

                                     stats_mat_list[[i]] <- mice_model %>%
                                       mice::complete() %>%
                                       psych::describe(na.rm=TRUE) %>%
                                       as.matrix()

                                     diff_mat_list[[i]] <- stats0_mat - stats_mat_list[[i]]
                                   }

                                   # Calculate Ranks
                                   cumulative_diff_df <- tibble::tibble(statistics = colnames(diff_mat_list[[1]]))
                                   for(i in seq(1, self$iterations)){
                                     diff_sum <- rep(0.0, times=13)
                                     for(j in seq(1, ncol(data_df))){
                                       diff_sum <- diff_sum + (diff_mat_list[[i]][j,])^2
                                     }
                                     feature_name <- sprintf("iter_%i", i)
                                     cumulative_diff_df <- cumulative_diff_df %>%
                                       dplyr::mutate(!!feature_name := sqrt(diff_sum))
                                   }

                                   ranks_mat <- cumulative_diff_df %>%
                                     dplyr::select(-c("statistics")) %>%
                                     apply(MARGIN = 1, FUN = function(x)rank(x, ties.method = "min"))

                                   # determine optimal seed
                                   seed_additive <- ranks_mat %>%
                                     rowSums() %>%
                                     which.min() %>%
                                     as.integer()

                                   # impute with optimal seed and return imputed data
                                   mice_model <- data_df %>%
                                     mice::mice(method = self$imputationAgent,
                                                pred = self$pred_mat,
                                                seed = self$seed + seed_additive,
                                                printFlag = FALSE)

                                   mice_model %>%
                                     mice::complete() %>%
                                     tibble::as_tibble() %>%
                                     return()
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
                                 imputeByM5P = function(data_df = "tbl_df", progress = "Progress"){
                                   nPred <- trunc(ncol(data_df) * self$pred_frac)
                                   if (ncol(data_df) < nPred){
                                     e <- simpleError("nPred needs to be smaller that the number of features.")
                                     stop(e)
                                   }#if
                                   if(ncol(data_df) < 2){
                                     e <- simpleError("The number of features needs to be larger than 2.")
                                     stop(e)
                                   }#if
                                   imputed_df <- data_df

                                   # determine predictor matrix
                                   self$detectPredictors(data_df)

                                   for(feature in colnames(data_df)){
                                     if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                       progress$inc(1.0/ncol(data_df))
                                     }

                                     # get imputation candidates
                                     na_idx <- data_df %>%
                                       dplyr::pull(feature) %>%
                                       as.numeric() %>%
                                       is.na() %>%
                                       which()

                                     if((length(na_idx)<1) | length(na_idx) == nrow(data_df)){
                                       next
                                     }#if

                                     # select valid predictors
                                     predictor_idx <- self$pred_mat[feature,] %>%
                                       as.logical()

                                     predictor_names <- colnames(self$pred_mat)[predictor_idx]

                                     #split in train and prediction data
                                     train_df <- data_df %>%
                                       dplyr::select(dplyr::all_of(c(feature, predictor_names))) %>%
                                       dplyr::slice(-na_idx)

                                     na_df <- data_df %>%
                                       dplyr::select(dplyr::all_of(c(feature, predictor_names))) %>%
                                       dplyr::slice(na_idx)

                                     # if predictor selection fails, take all features as predictors
                                     if(ncol(na_df) <2){
                                       train_df <- data_df %>%
                                         dplyr::slice(-na_idx)

                                       na_df <- data_df %>%
                                         dplyr::slice(na_idx)
                                     }

                                     m5p_model <- sprintf("%s ~ .", feature) %>%
                                       stats::as.formula() %>%
                                       RWeka::M5P(data=train_df)

                                     imputed_values <- predict(m5p_model, newdata = na_df)
                                     for (i in 1:length(na_idx)){
                                       imputed_df[[na_idx[i], feature]] <- imputed_values[i]
                                     }#for
                                   }#for
                                   return(imputed_df)

                                   # data_col_names <- colnames(data_df)
                                   # colnames(data_df) <- paste0("F", seq(1:ncol(data_df))) %>%
                                   #   as.character()
                                   # imputed_df <- data_df
                                   # for (i in 1:length(colnames(data_df))) {
                                   #   if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                   #     progress$inc(1.0/ncol(data_df))
                                   #   }#if
                                   #
                                   #   na_idx <- self$imputationSiteIdxByFeature(featureName = data_col_names[i])
                                   #
                                   #   if((length(na_idx)<1) | length(na_idx) == nrow(data_df)){
                                   #     next
                                   #   }#if
                                   #   train_df <- data_df %>%
                                   #     dplyr::slice(-na_idx)
                                   #
                                   #   na_df <- data_df %>%
                                   #     dplyr::slice(na_idx)
                                   #
                                   #   m5 <- colnames(data_df)[i] %>%
                                   #     paste("~.") %>%
                                   #     as.formula() %>%
                                   #     RWeka::M5P(data = train_df)
                                   #
                                   #   na_values <- predict(m5, newdata = na_df)
                                   #
                                   #   for (j in 1:length(na_idx)){
                                   #     imputed_df[[na_idx[j], colnames(data_df)[i]]] <- na_values[j]
                                   #   }#for
                                   # }#for
                                   # colnames(imputed_df) <- data_col_names
                                   # return(imputed_df)
                                 }, #functions

                                 ##################
                                 # plot functions #
                                 ##################
                                 #' @description
                                 #' Displays the distribution of missing values in form of a heatmap.
                                 #' @return
                                 #' A heatmap plot.
                                 #' (ggplot2::ggplot)
                                 imputationSiteHeatMap = function(){
                                   p <- plot(self$amv,
                                             col=c('navyblue','red'),
                                             numbers=TRUE,
                                             sortVars=TRUE,
                                             # labels=self$imputationStatistics[["feature"]],
                                             cex.axis=.7,
                                             gap=3,
                                             main = "Histogram of imputation sites",
                                             ylab=c("fraction","fraction"))
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
                                 featureBarPlot = function(data_df = "tbl_df", feature = "character"){
                                   feature <- dplyr::sym(feature)
                                   p <- data_df %>%
                                     ggplot2::ggplot(mapping = ggplot2::aes_string(x=feature), na.rm=TRUE) +
                                     ggplot2::geom_bar(stat = "bin", bins = 30) +
                                     ggplot2::ylab("counts") +
                                     ggplot2::xlab("value") +
                                     ggplot2::theme_linedraw() +
                                     ggplot2::theme(
                                       panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                                       plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                                       legend.background = ggplot2::element_rect(fill = "transparent"),
                                       legend.key = ggplot2::element_rect(fill = "transparent")
                                     )
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
                                 featureBoxPlotWithSubset = function(data_df = "tbl_df", feature = "character"){
                                   imputation_idx <- self$imputationSites %>%
                                     dplyr::filter(feature == !!feature) %>%
                                     dplyr::select(idx) %>%
                                     dplyr::pull()
                                   data_type <- rep("regular", nrow(data_df))
                                   data_type[imputation_idx] <- "imputed"
                                   # nanFeature <- self$nanFeatureList(data_df)
                                   p <- data_df %>%
                                     dplyr::select(feature) %>%
                                     dplyr::mutate(type = data_type ) %>%
                                     # dplyr::mutate(nanFeature = nanFeature) %>%
                                     tidyr::gather_(key="feature", value="measurement", feature) %>%
                                     ggplot2::ggplot(mapping=ggplot2::aes_string(x="feature",y="measurement"), na.rm=TRUE)+
                                     ggplot2::geom_boxplot(na.rm=TRUE, outlier.shape = NA)+
                                     ggplot2::geom_jitter(ggplot2::aes(colour=type), na.rm=TRUE) +
                                     ggplot2::ylab("value") +
                                     ggplot2::xlab("feature") +
                                     ggplot2::theme_linedraw() +
                                     ggplot2::theme(
                                       panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                                       plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                                       legend.background = ggplot2::element_rect(fill = "transparent"),
                                       legend.key = ggplot2::element_rect(fill = "transparent")
                                     )
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
                                 featurePlot = function(data_df = "tbl_df", feature = "character"){
                                   p1 <- self$featureBoxPlotWithSubset(data_df, feature) +
                                     ggplot2::theme(legend.position = c(0.9, 0.9),
                                                    legend.key = ggplot2::element_blank(),
                                                    legend.background = ggplot2::element_blank())

                                   # limits1 <- ggplot2::layer_scales(p1)$y$range$range

                                   p2 <- self$featureBarPlot(data_df, feature)

                                   limits <- ggplot2::layer_scales(p2)$x$range$range

                                   # limits <- c(min(c(limits1[1], limits2[1])),
                                   #             max(c(limits1[2], limits2[2]))
                                   # )

                                   p1 <- p1 +
                                     ggplot2::scale_y_continuous(limits=limits)

                                   p2 <- p2 +
                                     ggplot2::scale_x_continuous(position = "top") +
                                     ggplot2::coord_flip()

                                   # p <- gridExtra::grid.arrange(p1,p2, layout_matrix = rbind(c(1,2),c(1,2)))

                                   p <- gridExtra::grid.arrange(p1,p2, layout_matrix = rbind(c(1,1,2),c(1,1,2)),
                                                                top = textGrob(label = sprintf("Distribution of %s", feature)))
                                   return(p)
                                 },#function

                                 #' @description
                                 #' Displays an influx/outflux plot
                                 #' @return
                                 #' A composite plot.
                                 #' (ggplot2::ggplot)
                                 fluxPlot = function(){
                                   p <- self$flux_df %>%
                                     ggplot2::ggplot(mapping = ggplot2::aes_string(x="influx", y="outflux", label="rowname")) +
                                     ggplot2::geom_point()+
                                     ggplot2::geom_text(mapping = ggplot2::aes(label=ifelse(outflux<self$outflux_thr,as.character(rowname),'')),hjust=0,vjust=0) +
                                     ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = self$outflux_thr, linetype = "threshold")) +
                                     ggplot2::scale_linetype_manual(values = c("threshold" = "dashed")) +
                                     ggplot2::xlim(0,1) +
                                     ggplot2::ylim(0,1) +
                                     ggplot2::ggtitle("Flux plot") +
                                     ggplot2::xlab("influx") +
                                     ggplot2::ylab("outflux") +
                                     ggplot2::theme_linedraw() +
                                     ggplot2::theme(
                                       panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                                       plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                                       legend.background = ggplot2::element_rect(fill = "transparent"),
                                       legend.key = ggplot2::element_rect(fill = "transparent")
                                     )
                                     # ggthemes::geom_rangeframe(size = 1, x=ggplot2::xlim(0,1), y=c(0, 1)) +
                                     # ggthemes::theme_tufte()
                                     # ggplot2::theme(legend.position=c(.9,.75))
                                   return(p)
                                 }#function
                               )#public
)#class
