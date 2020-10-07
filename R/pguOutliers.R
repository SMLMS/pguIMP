#' #' @title pgu.outliers
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
#' @import dbscan
#' @import e1071
#' @import DT
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
                              .outliersStatistics = "tbl_df",
                              .outliersAgentAlphabet = "character",
                              .outliersAgent = "factor",
                              .featureData = "numeric",
                              .alpha = "numeric",
                              .epsilon = "numeric",
                              .minSamples = "integer",
                              .gamma = "numeric",
                              .nu = "numeric",
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
                                private$.alpha <- value
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
                                private$.epsilon <- value
                              },
                              #' @field minSamples
                              #' Returns the instance variable minSamples.
                              #' (numeric)
                              minSamples = function(){
                                return(private$.minSamples)
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
                                private$.gamma <- value
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
                                private$.nu <- value
                              },
                              #' @field seed
                              #' Returns the instance variable seed.
                              #' (numeric)
                              seed = function(){
                                return(private$.seed)
                              },
                              #' @field setSeed
                              #' Set the instance variable seed.
                              #' (numeric)
                              setSeed = function(value = "numeric"){
                                private$.seed <- value
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
                              #' @return
                              #' A new `pgu.outliers` object.
                              #' (pguIMP::pgu.outliers)
                              #' @examples
                              #' y <- tibble:tibble()
                              #' x <- pguIMP:pgu.outliers$new(data_df = y)
                              initialize = function(data_df = "tbl_df", alpha = 0.05, epsilon = 0.1, minSamples = 4, gamma = 0.05, nu=0.1, seed = 42){
                                private$.alpha <- alpha
                                private$.epsilon <- epsilon
                                private$.minSamples <- minSamples
                                private$.gamma <- gamma
                                private$.nu <- nu
                                private$.seed <- seed
                                private$.outliersAgentAlphabet <-c("none", "Grubbs", "DBSCAN", "SVM")
                                self$setOutliersAgent <- "none"
                                if(!tibble::is_tibble(data_df)){
                                  data <- tibble::tibble(names <- "none",
                                                         values <- c(NA))
                                }
                                self$resetOutliers(data_df)
                              }, #function

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
                                uString <- sprintf("outliersAgent: %s\nseed: %i\n", self$outliersAgent,self$seed)
                                uString <- sprintf("%s\nGrubbs parameter:\nalpha: %.3e\n", uString, self$alpha)
                                uString <- sprintf("%s\nDBSCAN parameter\nepsilon: %.3e\nminsamples: %i\n", uString, self$epsilon, self$minSamples)
                                uString <- sprintf("%s\nSVM parameter\ngamma: %.3e\nnu: %.3e\n", uString, self$gamma, self$nu)
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
                              #' @examples
                              #' x$resetOutliers(data, progress)
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
                                #self$findOutliers(data_df, progress)
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

                              detectOutliersParameter = function(data_df="tbl_df"){
                                measurements <- rep(0, nrow(self$outliersParameter))
                                n_outliers <- rep(0, nrow(self$outliersParameter))
                                for (i in seq(nrow(self$outliersParameter))){
                                  measurements[i] <- data_df %>%
                                    dplyr::select(self$outliersParameter[["features"]][i]) %>%
                                    tidyr::drop_na() %>%
                                    dplyr::count() %>%
                                    unlist() %>%
                                    as.integer()

                                  n_outliers[i] <- self$outliers %>%
                                    dplyr::select(feature) %>%
                                    dplyr::filter(grepl(self$outliersParameter[["features"]][i],feature)) %>%
                                    count() %>%
                                    unlist() %>%
                                    as.integer()
                                }
                                private$.outliersParameter <- tibble::tibble(features = self$outliersParameter[["features"]],
                                                                             measurements = measurements,
                                                                             outliers = n_outliers) %>%
                                  dplyr::mutate(existings = measurements - outliers) %>%
                                  dplyr::mutate(fractionOfOutliers = outliers/measurements) %>%
                                  dplyr::select(c("features", "measurements", "existings", "outliers", "fractionOfOutliers"))
                              },

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
                              detectOutliers = function(data_df = "tbl_df", progress = "Progress"){
                                set.seed(self$seed)
                                switch(self$outliersAgent,
                                       "none"=self$resetOutliers(data_df),
                                       "Grubbs"=self$detectByGrubbs(data_df, progress),
                                       "DBSCAN"= self$detectByDbscan(data_df, progress),
                                       "SVM"= self$detectBySvm(data_df, progress),
                                       stop("unkown outliersAgent"))
                                self$detectOutliersParameter(data_df)
                                self$calcOutliersStatistics(data_df)
                              },

                              detectByGrubbs = function(data_df = "tbl_df", progress = "Progress"){
                                self$resetOutliers(data_df)
                                for (feature in self$outliersParameter[["features"]]){
                                  if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                    progress$inc(1)
                                  }#if
                                  private$.featureData <- data_df[[feature]]
                                  foundOutlier <- TRUE
                                  while (foundOutlier) {
                                    foundOutlier <- self$grubbs_numeric(data_df, feature)
                                  }#while
                                }#for
                              },#function

                              #' @description
                              #' Performes Grubb's test for outliers to detect a single outlier in the provided attributes data.
                              #' If an outlier is found, it is written to the instance variable `outliers`.
                              #' The function indicates a find by a positive feedback.
                              #' @param data_df
                              #' The data frame to be analyzed.
                              #' (tibble::tibble)
                              #' @param  feature
                              #' The attribute within the data frame to be analyzed.
                              #' @return
                              #' Feedback if an outlier was found.
                              #' (logical)
                              #' @examples
                              #' y <- x$runGrubbs(data, feature = "infected")
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

                              detectByDbscan = function(data_df = "tbl_df", progress = "Progress"){
                                self$resetOutliers(data_df)
                                for (feature in self$outliersParameter[["features"]]){
                                  if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                    progress$inc(1)
                                  }#if
                                  outliers_df <- self$dbscan_numeric(data_df, feature)
                                  if(nrow(outliers_df)>0){
                                    private$.outliers <- self$outliers %>%
                                      dplyr::add_row(outliers_df)
                                  }


                                } #for
                              }, #function

                              dbscan_numeric = function(data_df = "tbl_df", feature = "character"){
                                mean_val <- data_df[[feature]] %>%
                                  mean()

                                dbscan_model <- data_df %>%
                                  dplyr::select(feature) %>%
                                  dbscan::dbscan(eps = self$epsilon, minPts = self$minSamples)

                                outliers_df <- tibble::tibble(measurement = numeric(0),
                                                              feature = character(0),
                                                              values = numeric(0),
                                                              type = character(0),
                                                              color = character(0))

                                idx <- which(dbscan_model$cluster==0)
                                if(length(idx) > 0){
                                  values <-  data_df[[feature]][dbscan_model$cluster==0]
                                  feature <- rep(feature, length(values))
                                  outliers_df <- tibble::tibble(measurement = as.numeric(idx),
                                                                feature = as.character(feature),
                                                                values = as.numeric(values) ) %>%
                                    dplyr::mutate(type = ifelse(values > mean_val, "max", "min")) %>%
                                    dplyr::mutate(color = ifelse(values > mean_val, "firebrick", "blue"))

                                }#if
                                return(outliers_df)
                              },

                              detectBySvm = function(data_df = "tbl_df", progress = "Process"){
                                self$resetOutliers(data_df)
                                for (feature in self$outliersParameter[["features"]]){
                                  if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                    progress$inc(1)
                                  }#if
                                  outliers_df <- self$svm_numeric(data_df, feature)
                                  if(nrow(outliers_df)>0){
                                    private$.outliers <- self$outliers %>%
                                      dplyr::add_row(outliers_df)
                                  }
                                }#for
                              },

                              svm_numeric = function(data_df = "tbl_df", feature = "character"){
                                print(feature)
                                mean_val <- data_df[[feature]] %>%
                                  mean()

                                svm_model <- data_df %>%
                                  dplyr::select(feature) %>%
                                  e1071::svm(type='one-classification', kernel = "radial", gamma=self$gamma, nu=self$nu)


                                outliers_df <- tibble::tibble(measurement = numeric(0),
                                                              feature = character(0),
                                                              values = numeric(0),
                                                              type = character(0),
                                                              color = character(0))

                                result <- predict(model_oneclasssvm,data_df[feature]) %>%
                                  as.logical()
                                idx <- which(!result)

                                if(length(idx)>0){
                                  values <- data_df[[feature]][idx]
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
                              setImputationSites = function(data_df = "tbl_df"){
                                for (i in seq(nrow(self$outliersParameter))){
                                  temp_feature <- self$outliersParameter[["features"]][i]
                                  idx <- self$outliers %>%
                                    dplyr::select(c("measurement", "feature")) %>%
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
                                  # <- data_df[[self$outliersParameter[["features"]][i]]]%>%
                                  #   replace(idx, NA)
                                }
                                return(data_df)
                              }, #function

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
                              #' x$calcOutliersStatistics(data)
                              calcOutliersStatistics = function(data = "tbl_df"){
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

                              ####################
                              # output functions #
                              ####################

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
                                feature <- dplyr::sym(feature)
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
