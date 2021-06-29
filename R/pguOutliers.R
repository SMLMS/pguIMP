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
#'
#' @importFrom dbscan dbscan
#' @importFrom dplyr add_row count filter mutate mutate_if pull select select_if
#' @importFrom dplyr slice summarise_all sym transmute_all
#' @importFrom DT datatable formatStyle
#' @importFrom e1071 svm
#' @importFrom ggplot2 aes aes_string coord_flip element_blank element_rect element_text
#' @importFrom ggplot2 geom_bar geom_boxplot geom_col geom_jitter ggplot
#' @importFrom ggplot2 ggtitle layer_scales scale_x_continuous scale_y_continuous theme theme_linedraw
#' @importFrom ggplot2 xlab ylab
#' @importFrom gridExtra grid.arrange
#' @importFrom magrittr %>%
#' @importFrom outliers grubbs.test
#' @importFrom R6 R6Class
#' @importFrom shiny Progress
#' @importFrom tibble add_row is_tibble tibble
#' @importFrom tidyr drop_na gather
#' @importFrom tidyselect all_of
#'
#' @include pguOutlierDetection.R
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'
#'

pgu.outliers <- R6::R6Class("pgu.outliers",
                            ####################
                            # instance variables
                            ####################
                            private = list(
                              .outliersParameter = "tbl_df",
                              .outliers = "tbl_df",
                              .one_hot_df = "tbl_df",
                              .outliersStatistics = "tbl_df",
                              .outliersAgentAlphabet = "character",
                              .outliersAgent = "factor",
                              .featureData = "numeric",
                              .alpha = "numeric",
                              .epsilon = "numeric",
                              .minSamples = "integer",
                              .gamma = "numeric",
                              .nu = "numeric",
                              .k = "integer",
                              .cutoff = "numeric",
                              .seed = "integer"
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
                              #' @field one_hot_df
                              #' Returns the positions of missings in one_hot encoding
                              #' (tibble::tibble)
                              one_hot_df = function(){
                                return(private$.one_hot_df)
                              },
                              #' @field outliersStatistics
                              #' Returns the instance variable outliersStatistics.
                              #' (tibble::tibble)
                              outliersStatistics = function(){
                                return(private$.outliersStatistics)
                              },
                              #' @field  outliersAgentAlphabet
                              #' Returns the instance variable of outliersAgentAlphabet
                              #' (character)
                              outliersAgentAlphabet = function(){
                                return(private$.outliersAgentAlphabet)
                              },
                              #' @field outliersAgent
                              #' Returns the instance variable outliersAgent.
                              #' (character)
                              outliersAgent = function(){
                                return(as.character(private$.outliersAgent))
                              },
                              #' @field setOutliersAgent
                              #' Sets the instance variable outliersAgent.
                              #' (character)
                              setOutliersAgent = function(agent = "character") {
                                private$.outliersAgent <- factor(agent, levels = self$outliersAgentAlphabet)
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
                                if(value < 0){
                                  private$.alpha <- 0.001
                                }
                                else if(value > 0.999){
                                  private$.alpha <- 0.999
                                }
                                else{
                                  private$.alpha <- value
                                }
                              },
                              #' @field epsilon
                              #' Returns the instance variable epsilon.
                              #' (numeric)
                              epsilon = function(){
                                return(private$.epsilon)
                              },
                              #' @field setEpsilon
                              #' Set the instance variable epsilon.
                              #' (numeric)
                              setEpsilon = function(value = "numeric"){
                                if(value < 0.001){
                                  private$.epsilon <- 0.001
                                }
                                else{
                                  private$.epsilon <- value
                                }
                              },
                              #' @field minSamples
                              #' Returns the instance variable minSamples.
                              #' (integer)
                              minSamples = function(){
                                return(private$.minSamples)
                              },
                              #' @field setMinSamples
                              #' Set the instance variable minSamples.
                              #' (integer)
                              setMinSamples = function(value = "integer"){
                                if(value < 1){
                                  private$.minSamples <- 1
                                }
                                else{
                                  private$.minSamples <- value
                                }
                              },
                              #' @field gamma
                              #' Returns the instance variable gamma.
                              #' (numeric)
                              gamma = function(){
                                return(private$.gamma)
                              },
                              #' @field setGamma
                              #' Set the instance variable gamma.
                              #' (numeric)
                              setGamma = function(value = "numeric"){
                                if(value < 0.001){
                                  private$.gamma <- 0.001
                                }
                                else if(value > 0.999){
                                  private$.gamma <- 0.999
                                }
                                else{
                                  private$.gamma <- value
                                }
                              },
                              #' @field nu
                              #' Returns the instance variable nu.
                              #' (numeric)
                              nu = function(){
                                return(private$.nu)
                              },
                              #' @field setNu
                              #' Set the instance variable nu.
                              #' (numeric)
                              setNu = function(value = "numeric"){
                                if(value < 0.001){
                                  private$.nu <- 0.001
                                }
                                else if(value > 0.999){
                                  private$.nu <- 0.999
                                }
                                else{
                                  private$.nu <- value
                                }
                              },
                              #' @field k
                              #' Returns the instance variable k
                              #' (integer)
                              k = function(){
                                return(private$.k)
                              },
                              #' @field setK
                              #' Sets the instance variable k.
                              #' (integer)
                              setK = function(value = "integer"){
                                if(value < 1){
                                  private$.k <- 1
                                }
                                else{
                                  private$.k <- value
                                }
                              },
                              #' @field cutoff
                              #' Returns the instance variable cutoff.
                              #' (numeric)
                              cutoff = function(){
                                return(private$.cutoff)
                              },
                              #' @field setCutoff
                              #' Sets the instance variable cutoff.
                              #' (numeric)
                              setCutoff = function(value = "numeric"){
                                if(value < 0.01){
                                  private$.cutoff <- 0.01
                                }
                                else if(value > 0.99){
                                  private$.cutoff <- 0.99
                                }
                                else{
                                  private$.cutoff <- value
                                }
                              },
                              #' @field seed
                              #' Returns the instance variable seed.
                              #' (integer)
                              seed = function(){
                                return(private$.seed)
                              },
                              #' @field setSeed
                              #' Set the instance variable seed.
                              #' (integer)
                              setSeed = function(value = "integer"){
                                if(value < 1){
                                  private$.seed <- 1
                                }
                                else if (value > 100){
                                  private$.seed <- 100
                                }
                                else{
                                  private$.seed <- value
                                }
                              }
                            ),#active
                            ###################
                            # memory management
                            ###################
                            public = list(
                              #' @description
                              #' Creates and returns a new `pgu.outliers` object.
                              #' @param data_df
                              #' The data to be cleaned.
                              #' (tibble::tibble)
                              #' @param alpha
                              #' Initial definition of the instance variable alpha.
                              #' (numeric)
                              #' @param epsilon
                              #' Initial definition of the instance variable epsilon.
                              #' (numeric)
                              #' @param minSamples
                              #' Initial definition of the instance variable minSamples.
                              #' (integer)
                              #' @param gamma
                              #' Initial definition of the instance variable gamma.
                              #' (numeric)
                              #' @param nu
                              #' Initial definition of the instance variable nu.
                              #' (numeric)
                              #' @param cutoff
                              #' Initial definition of the instance variable cutoff.
                              #' (numeric)
                              #' @param k
                              #' Initial definition of the instance variable k.
                              #' (integer)
                              #' @param seed
                              #' Initial definition of the instance variable seed.
                              #' (integer)
                              #' @return
                              #' A new `pgu.outliers` object.
                              #' (pguIMP::pgu.outliers)
                              initialize = function(data_df = "tbl_df", alpha = 0.05, epsilon = 0.1, minSamples = 4, gamma = 0.05, nu=0.1, k=4, cutoff=0.99, seed = 42){
                                self$setAlpha <- alpha
                                self$setEpsilon <- epsilon
                                self$setMinSamples <- minSamples
                                self$setGamma <- gamma
                                self$setNu <- nu
                                self$setK <- k
                                self$setCutoff <- cutoff
                                self$setSeed <- seed
                                private$.outliersAgentAlphabet <-c("none", "Grubbs", "DBSCAN", "SVM", "knn")
                                self$setOutliersAgent <- "none"
                                if(!tibble::is_tibble(data_df)){
                                  data_df <- tibble::tibble(names <- "none",
                                                            values <- c(NA))
                                }
                                self$resetOutliers(data_df)
                              }, #function

                              #' @description
                              #' Clears the heap and
                              #' indicates that instance of `pgu.outliers` is removed from heap.
                              finalize = function(){
                                print("Instance of pgu.outliers removed from heap")
                              }, #function

                              ##########################
                              # print instance variables
                              ##########################
                              #' @description
                              #' Prints instance variables of a `pgu.outliers` object.
                              #' @return
                              #' string
                              print = function(){
                                rString <- sprintf("\npgu.outliers\n")
                                cat(rString)
                                uString <- sprintf("outliersAgent: %s\nseed: %i\n", self$outliersAgent,self$seed)
                                uString <- sprintf("%s\nGrubbs parameter:\nalpha: %.3e\n", uString, self$alpha)
                                uString <- sprintf("%s\nDBSCAN parameter\nepsilon: %.3e\nminSamples: %i\n", uString, self$epsilon, self$minSamples)
                                uString <- sprintf("%s\nSVM parameter\ngamma: %.3e\nnu: %.3e\n", uString, self$gamma, self$nu)
                                uString <- sprintf("%s\nKNN parameter\ncutoff: %.3e\nk: %i\n", uString, self$cutoff, self$k)
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
                              #' @param data_df
                              #' Dataframe to be analyzed.
                              #' (tibble::tibble)
                              resetOutliers = function(data_df = "tbl_df"){
                                numericData <- data_df %>%
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
                                self$one_hot(data_df)
                              }, #function

                              ####################
                              # helper functions #
                              ####################
                              #' @description
                              #' Filters attributes from the given dataframe that are known to the class.
                              #' @param data_df
                              #' Dataframe to be filtered.
                              #' (tibble::tibble)
                              #' @return
                              #' A filterd dataframe.
                              #' (tibble::tibble)
                              filterFeatures = function(data_df = "tbl_df"){
                                data_df %>%
                                  dplyr::select(tidyselect::all_of(private$.outliersParameter[["features"]])) %>%
                                  return()
                              }, #function

                              #' @description
                              #' Checks if the feature consists of a sufficient number of instances.
                              #' @param data_df
                              #' Dataframe to be analyzed
                              #' (tibble::tibble)
                              #' @param feature
                              #' The attribute to be analyzed.
                              #' (character)
                              checkFeatureValidity = function(data_df = "tbl_df", feature = "character"){
                                result <- FALSE
                                n <- data_df %>%
                                  dplyr::select(feature) %>%
                                  tidyr::drop_na() %>%
                                  nrow()
                                if(n > 4){
                                  result <- TRUE
                                }
                                return(result)
                              }, #function

                              #' @description
                              #' determines the outliers parameter by analyzing the
                              #' tibble data_df and the instance variable outliers.
                              #' Results are stored to instance variable outliersParameter.
                              #' @param data_df
                              #' Dataframe to be analyzed.
                              #' (tibble::tibble)
                              detectOutliersParameter = function(data_df="tbl_df"){
                                measurements <- rep(0, nrow(self$outliersParameter))
                                n_outliers <- rep(0, nrow(self$outliersParameter))
                                for (i in seq(nrow(self$outliersParameter))){
                                  measurements[i] <- data_df %>%
                                    dplyr::select(tidyselect::all_of(self$outliersParameter[["features"]][i])) %>%
                                    tidyr::drop_na() %>%
                                    dplyr::count() %>%
                                    unlist() %>%
                                    as.integer()

                                  n_outliers[i] <- self$outliers %>%
                                    dplyr::select(feature) %>%
                                    dplyr::filter(grepl(self$outliersParameter[["features"]][i],feature)) %>%
                                    dplyr::count() %>%
                                    unlist() %>%
                                    as.integer()
                                }
                                private$.outliersParameter <- tibble::tibble(features = self$outliersParameter[["features"]],
                                                                             measurements = measurements,
                                                                             outliers = n_outliers) %>%
                                  dplyr::mutate(existings = measurements - outliers) %>%
                                  dplyr::mutate(fractionOfOutliers = outliers/measurements) %>%
                                  dplyr::select(tidyselect::all_of(c("features", "measurements", "existings", "outliers", "fractionOfOutliers")))
                              },

                              #' @description
                              #' Characterizes each row of the data frame as either `complete`
                              #' or indicates which attribute has been identified as an outlier within the row.
                              #' If multiple attributes' row entries were identified as outliers, the row is characterized by `multiple`.
                              #' @param data_df
                              #' The data frame to be analyzed.
                              #' (tibble::tibble)
                              #' @return
                              #' Vector of row characteristics.
                              #' (character)
                              outliersFeatureList = function(data_df = "tbl_df"){
                                outFeature <- c(rep("complete", nrow(data_df)))
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

                              #' @description
                              #' Returns the detected outliers of a given attribute.
                              #' @param feature
                              #' The attribute to be analyzed
                              #' (character)
                              #' @return
                              #' The attribute's outliers
                              #' (tibble::tibble)
                              featureOutlier = function(feature = "character"){
                                t <- NULL
                                tryCatch({
                                  t <- self$outliers %>%
                                    dplyr::filter(feature == !!feature)
                                },
                                error = function(e) {
                                  print("error")
                                  print(e)
                                }, #error
                                warning = function(w) {
                                  print("warning")
                                  print(w)
                                } #warning
                                )#tryCatch
                                return(t)
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

                              ###################
                              # detect outliers #
                              ###################

                              #' @description
                              #' Chooses a method for identification of anomalies based upon the instance variable `outliersAgent`
                              #' Detects anomalies in a data frame by one-dimensional analysis of each feature.
                              #' @param data_df
                              #' Data frame to be analyzed.
                              #' (tibble::tibble)
                              #' @param progress
                              #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                              #' (shiny::Progress)
                              detectOutliers = function(data_df = "tbl_df", progress = "Progress"){
                                set.seed(self$seed)
                                switch(self$outliersAgent,
                                       "none"=self$resetOutliers(data_df),
                                       "Grubbs"=self$detectByGrubbs(data_df, progress),
                                       "DBSCAN"= self$detectByDbscan(data_df, progress),
                                       "SVM"= self$detectBySvm(data_df, progress),
                                       "knn" = self$detectByKnn(data_df, progress),
                                       stop("unkown outliersAgent"))
                                self$detectOutliersParameter(data_df)
                                self$calcOutliersStatistics(data_df)
                              },

                              #' @description
                              #' Identifies anomalies in the data frame based on Grubb's test.
                              #' Iterates over the whole data frame. Calls the object's public function
                              #' `grubbs_numeric` until no more anomalies are identified.
                              #' The threshold for anomaly detection is defined in the instance variable `alpha`.
                              #' Display the progress if shiny is loaded.
                              #' @param data_df
                              #' Data frame to be analyzed.
                              #' (tibble::tibble)
                              #' @param progress
                              #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                              #' (shiny::Progress)
                              detectByGrubbs = function(data_df = "tbl_df", progress = "Progress"){
                                self$resetOutliers(data_df)
                                for (feature in self$outliersParameter[["features"]]){
                                  if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                    progress$inc(1)
                                  }#if
                                  if (self$checkFeatureValidity(data_df, feature)){
                                    private$.featureData <- data_df[[feature]]
                                    foundOutlier <- TRUE
                                    while (foundOutlier) {
                                      foundOutlier <- self$grubbs_numeric(data_df, feature)
                                    }#while
                                  } #if
                                }#for
                              },#function

                              #' @description
                              #' Performs Grubb's test for anomalies to detect a single outlier in the provided attributes data.
                              #' If an outlier is found, it is added to the instance variable `outliers`.
                              #' The threshold for anomaly detection is difined in the instance variable `alpha`.
                              #' The function indicates a find by a positive feedback.
                              #' @param data_df
                              #' The data frame to be analyzed.
                              #' (tibble::tibble)
                              #' @param  feature
                              #' The attribute within the data frame to be analyzed.
                              #' @return
                              #' Feedback if an outlier was found.
                              #' (logical)
                              grubbs_numeric = function(data_df = "tbl_df", feature = "character"){
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
                                  value <- data_df[[idx, feature]]
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
                              #' Identifies anomalies in the data frame based on DBSCAN.
                              #' Iterates over the whole data frame. Calls the object's public function
                              #' `dbscan_numeric` until all features are analyzed.
                              #' The cluster hyper parameter are defined in the instance variables `epsilon` and `minSamples`.
                              #' The results of the `dbscan_numeric` routine are added to the instance variable `outliers`.
                              #' Display the progress if shiny is loaded.
                              #' @param data_df
                              #' Data frame to be analyzed.
                              #' (tibble::tibble)
                              #' @param progress
                              #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                              #' (shiny::Progress)
                              detectByDbscan = function(data_df = "tbl_df", progress = "Progress"){
                                self$resetOutliers(data_df)
                                for (feature in self$outliersParameter[["features"]]){
                                  if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                    progress$inc(1)
                                  }#if
                                  if (self$checkFeatureValidity(data_df, feature)){
                                    outliers_df <- self$dbscan_numeric(data_df, feature)
                                    if(nrow(outliers_df)>0){
                                      private$.outliers <- self$outliers %>%
                                        dplyr::add_row(outliers_df)
                                    } #if
                                  } #if


                                } #for
                              }, #function

                              #' @description
                              #' Identifies anomalies in a single feature of a data frame based on DBSCAN.
                              #' The cluster hyperparameter are defined in the instance variables `epsilon` and `minSamples`.
                              #' Display the progress if shiny is loaded.
                              #' @param data_df
                              #' Data frame to be analyzed.
                              #' (tibble::tibble)
                              #' @param feature
                              #' Feature to be analyzed
                              #' (character)
                              #' @param progress
                              #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                              #' (shiny::Progress)
                              #' @return
                              #' A data frame comprising the information about detected anomalies of the feature.
                              #' (tibble::tibble)
                              dbscan_numeric = function(data_df = "tbl_df", feature = "character"){
                                feature_df <- data_df %>%
                                  dplyr::select(feature) %>%
                                  tidyr::drop_na()

                                mean_val <- feature_df %>%
                                  unlist() %>%
                                  as.numeric() %>%
                                  mean()

                                dbscan_model <- feature_df %>%
                                  dbscan::dbscan(eps = self$epsilon, minPts = self$minSamples)

                                outliers_df <- tibble::tibble(measurement = numeric(0),
                                                              feature = character(0),
                                                              values = numeric(0),
                                                              type = character(0),
                                                              color = character(0))

                                if(sum(dbscan_model$cluster==0)>0){
                                  values <-  feature_df[[feature]][dbscan_model$cluster==0]
                                  idx <- which(data_df[[feature]] %in% values)
                                  feature <- rep(feature, length(values))
                                  outliers_df <- tibble::tibble(measurement = as.numeric(idx),
                                                                feature = as.character(feature),
                                                                values = as.numeric(values)) %>%
                                    dplyr::mutate(type = ifelse(values > mean_val, "max", "min")) %>%
                                    dplyr::mutate(color = ifelse(values > mean_val, "firebrick", "blue"))

                                }#if
                                return(outliers_df)
                              },

                              #' @description
                              #' Identifies anomalies in the data frame based on one class SVM.
                              #' Iterates over the whole data frame. Calls the object's public function
                              #' `svm_numeric` until all features are analyzed.
                              #' The cluster hyper parameter are defined in the instance variables `gamma` and `nu`.
                              #' The results of the `svm_numeric` routine are added to the instance variable `outliers`.
                              #' Display the progress if shiny is loaded.
                              #' @param data_df
                              #' Data frame to be analyzed.
                              #' (tibble::tibble)
                              #' @param progress
                              #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                              #' (shiny::Progress)
                              detectBySvm = function(data_df = "tbl_df", progress = "Process"){
                                self$resetOutliers(data_df)
                                for (feature in self$outliersParameter[["features"]]){
                                  if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                    progress$inc(1)
                                  }#if
                                  if (self$checkFeatureValidity(data_df, feature)){
                                    outliers_df <- self$svm_numeric(data_df, feature)
                                    if(nrow(outliers_df)>0){
                                      private$.outliers <- self$outliers %>%
                                        dplyr::add_row(outliers_df)
                                    } #if
                                  } #if
                                }#for
                              },

                              #' @description
                              #' Identifies anomalies in a single feature of a data frame based on one class SVM.
                              #' The cluster hyperparameter are defined in the instance variables `gamma` and `nu`.
                              #' Display the progress if shiny is loaded.
                              #' @param data_df
                              #' Data frame to be analyzed.
                              #' (tibble::tibble)
                              #' @param feature
                              #' Feature to be analyzed
                              #' (character)
                              #' @param progress
                              #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                              #' (shiny::Progress)
                              #' @return
                              #' A data frame comprising the information about detected anomalies of the feature.
                              #' (tibble::tibble)
                              svm_numeric = function(data_df = "tbl_df", feature = "character"){
                                feature_df <- data_df %>%
                                  dplyr::select(feature) %>%
                                  tidyr::drop_na()

                                mean_val <- feature_df %>%
                                  unlist() %>%
                                  as.numeric() %>%
                                  mean()

                                outliers_df <- tibble::tibble(measurement = numeric(0),
                                                              feature = character(0),
                                                              values = numeric(0),
                                                              type = character(0),
                                                              color = character(0))

                                feature_idx <- integer(0)
                                tryCatch({
                                  svm_model <- feature_df %>%
                                    e1071::svm(type='one-classification', kernel = "radial", gamma=self$gamma, nu=self$nu)

                                  result <- predict(svm_model, data_df[feature]) %>%
                                    as.logical()
                                  feature_idx <- which(!result)
                                },
                                error = function(e) {
                                  sprintf("Warning in pguOutlier's function svm_numeric while analyzing %s:\n %s" , feature, e) %>%
                                    cat()
                                }
                                )

                                if(length(feature_idx)>0){
                                  values <- feature_df[[feature]][feature_idx]
                                  idx <- which(data_df[[feature]] %in% values)
                                  feature <- rep(feature, length(values))
                                  outliers_df <- tibble::tibble(measurement = as.numeric(idx),
                                                                feature = as.character(feature),
                                                                values = as.numeric(values) ) %>%
                                    dplyr::mutate(type = ifelse(values > mean_val, "max", "min")) %>%
                                    dplyr::mutate(color = ifelse(values > mean_val, "firebrick", "blue"))
                                }#if
                                return(outliers_df)
                              },

                              #' @description
                              #' Identifies anomalies in the data frame based on knnO.
                              #' Iterates over the whole data frame. Calls the object's public function
                              #' `svm_numeric` until all features are analyzed.
                              #' The cluster hyper parameter are defined in the instance variables `alpha` and `minSamples`.
                              #' The results of the `knn_numeric` routine are added to the instance variable `outliers`.
                              #' Display the progress if shiny is loaded.
                              #' @param data_df
                              #' Data frame to be analyzed.
                              #' (tibble::tibble)
                              #' @param progress
                              #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                              #' (shiny::Progress)
                              detectByKnn = function(data_df = "tbl_df", progress = "Process"){
                                self$resetOutliers(data_df)
                                for (feature in self$outliersParameter[["features"]]){
                                  if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                    progress$inc(1)
                                  }#if
                                  if (self$checkFeatureValidity(data_df, feature)){
                                    outliers_df <- self$knn_numeric(data_df, feature)
                                    if(nrow(outliers_df)>0){
                                      private$.outliers <- self$outliers %>%
                                        dplyr::add_row(outliers_df)
                                    } #if
                                  }#if
                                }#for
                              },

                              #' @description
                              #' Identifies anomalies in a single feature of a data frame based on knnO.
                              #' The cluster hyperparameter are defined in the instance variables `alpha` and `minSmaples`.
                              #' Display the progress if shiny is loaded.
                              #' @param data_df
                              #' Data frame to be analyzed.
                              #' (tibble::tibble)
                              #' @param feature
                              #' Feature to be analyzed
                              #' (character)
                              #' @param progress
                              #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                              #' (shiny::Progress)
                              #' @return
                              #' A data frame comprising the information about detected anomalies of the feature.
                              #' (tibble::tibble)
                              knn_numeric = function(data_df = "tbl_df", feature = "character"){
                                feature_df <- data_df %>%
                                  dplyr::select(feature) %>%
                                  tidyr::drop_na()

                                mean_val <- feature_df %>%
                                  unlist() %>%
                                  as.numeric() %>%
                                  mean()

                                feature_idx <- integer(0)
                                outliers_df <- tibble::tibble(measurement = numeric(0),
                                                              feature = character(0),
                                                              values = numeric(0),
                                                              type = character(0),
                                                              color = character(0))
                                tryCatch({
                                  knn_model <- feature_df %>%
                                    pguIMP::nnk(k=self$k, cutoff = self$cutoff)
                                  feature_idx <- knn_model$`Location of Outlier`
                                },
                                error = function(e) {
                                  sprintf("Warning in pguOutlier's function knn_numeric while analyzing %s:\n %s" , feature, e) %>%
                                    cat()
                                }
                                )

                                if(length(feature_idx)>0){
                                  values <- feature_df[[feature]][feature_idx]
                                  idx <- which(data_df[[feature]] %in% values)
                                  feature <- rep(feature, length(values))
                                  outliers_df <- tibble::tibble(measurement = as.numeric(idx),
                                                                feature = as.character(feature),
                                                                values = as.numeric(values) ) %>%
                                    dplyr::mutate(type = ifelse(values > mean_val, "max", "min")) %>%
                                    dplyr::mutate(color = ifelse(values > mean_val, "firebrick", "blue"))
                                }#if
                                return(outliers_df)
                              },

                              ########################
                              # set imputation sites #
                              ########################

                              #' @description
                              #' Replaces the detected anomalies of a user provided data frame with `NA` for further imputation routines.
                              #' @param data_df
                              #' Data frame to be mutated.
                              #' (tibble::tibble)
                              #' @return
                              #' A data frame with anomalies replaced by `NA`.
                              #' (tibble::tibble)
                              setImputationSites = function(data_df = "tbl_df"){
                                for (i in seq(nrow(self$outliersParameter))){
                                  temp_feature <- self$outliersParameter[["features"]][i]
                                  idx <- self$outliers %>%
                                    dplyr::select(tidyselect::all_of(c("measurement", "feature"))) %>%
                                    dplyr::filter(grepl(temp_feature, feature)) %>%
                                    dplyr::select("measurement") %>%
                                    unlist() %>%
                                    as.integer()

                                  data_df <- data_df %>%
                                    dplyr::mutate(!!temp_feature := data_df %>%
                                                    dplyr::select(temp_feature) %>%
                                                    unlist() %>%
                                                    as.numeric() %>%
                                                    replace(idx, NA))
                                }#for
                                return(data_df)
                              }, #function

                              ######################
                              # outlier statistics #
                              ######################
                              #' @description
                              #' Calculates the statistics on the previously performed outlier detection analysis
                              #' and stores the results in the instance variable `outliersStatistcs`.
                              #' @param data_df
                              #' The data frame to be analyzed.
                              #' (tibble::tibble)
                              calcOutliersStatistics = function(data_df = "tbl_df"){
                                absCount <- data_df %>%
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

                              ####################
                              # output functions #
                              ####################

                              #' @description
                              #' Creates a datatable with substituted outliers highlightes by colored background.
                              #' @param data_df
                              #' The data frame to be analyzed.
                              #' (tibble::tibble)
                              #' @return
                              #' A colored datatable
                              #' (DT::datatable)
                              outlierTable = function(data_df = "tbl_df"){
                                idx <- self$outliers[["measurement"]][!duplicated(self$outliers[["measurement"]])]
                                t <- data_df %>%
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
                                                         backgroundColor = styleEqual(data_df %>%
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
                              plotOutliersDistribution = function(){
                                p <- self$outliersStatistics %>%
                                  tidyr::gather('low', 'high', key = "type", value="typeCount") %>%
                                  dplyr::mutate(fraction = typeCount/absCount) %>%
                                  ggplot2::ggplot(mapping = ggplot2::aes_string(x = "features", y = "fraction", fill = "type"), na.rm=TRUE)+
                                  ggplot2::geom_col()+
                                  ggplot2::ggtitle("Outliers Distribution") +
                                  ggplot2::ylab("fraction") +
                                  ggplot2::xlab("feature") +
                                  ggplot2::theme_linedraw() +
                                  ggplot2::theme(
                                    panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                                    plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                                    legend.background = ggplot2::element_rect(fill = "transparent"),
                                    legend.key = ggplot2::element_rect(fill = "transparent"),
                                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
                                  )
                                return(p)
                              },#function

                              #' @description
                              #' Displays the distribution of an attribute's values as histogram.
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
                                  ggplot2::xlab("value") +
                                  ggplot2::ylab("counts") +
                                  ggplot2::theme_linedraw() +
                                  ggplot2::theme(
                                    panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                                    plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                                    legend.background = ggplot2::element_rect(fill = "transparent"),
                                    legend.key = ggplot2::element_rect(fill = "transparent")
                                  )
                                return(p)
                              },#function


                              #' @description
                              #' Displays the distribution of an attribute's vlues as box plot.
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
                                outlier_idx <- self$outliers %>%
                                  dplyr::filter(feature == !!feature) %>%
                                  dplyr::select(measurement) %>%
                                  dplyr::pull()
                                data_type <- rep("regular", nrow(data_df))
                                data_type[outlier_idx] <- "outlier"
                                # outFeature <- self$outliersFeatureList(data_df)
                                p <- data_df %>%
                                  dplyr::select(feature) %>%
                                  dplyr::mutate(type = data_type ) %>%
                                  tidyr::gather(key = "feature", value = "value", -type) %>%
                                  ggplot2::ggplot(mapping=ggplot2::aes_string(x = "feature", y="value"), na.rm=TRUE)+
                                  ggplot2::geom_boxplot(outlier.shape = NA, na.rm=TRUE) +
                                  ggplot2::geom_jitter(ggplot2::aes(colour=type), na.rm=TRUE) +
                                  ggplot2::xlab("feature") +
                                  ggplot2::ylab("value") +
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

                                p2 <- self$featureBarPlot(data_df, feature)

                                limits <- ggplot2::layer_scales(p2)$x$range$range

                                p1 <- p1 +
                                  ggplot2::scale_y_continuous(limits=limits)

                                p2 <- p2 +
                                  ggplot2::scale_x_continuous(position = "top") +
                                  ggplot2::coord_flip()

                                p <- gridExtra::grid.arrange(p1,p2, layout_matrix = rbind(c(1,1,2),c(1,1,2)),
                                                             top = textGrob(label = sprintf("Distribution of %s", feature)))
                                return(p)
                              }#function
                            )#public
)#class
