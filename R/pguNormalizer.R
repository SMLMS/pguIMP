#' @title pgu.normalizer
#'
#' @description
#' Normalization of data. Part of pguIMP.
#'
#' @details
#' Performs a data normalization in order to achieve a standardized version of the dataframe.
#'
#' @format [R6::R6Class] object.
#' @section Construction:
#' x <- pguIMP::pgu.normalizer$new(data)
#'
#' @import R6
#' @import tidyverse
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pgu.normalizer <- R6::R6Class("pgu.normalizer",
                              ####################
                              # instance variables
                              ####################
                              private = list(
                                .normAgentAlphabet = "character",
                                .normAgent = "factor",
                                .normParameter = "tbl_df"
                              ),
                              ##################
                              # accessor methods
                              ##################
                              active = list(
                                #' @field normAgentAlphabet
                                #' Returns the instance variable normAgentAlphabt.
                                normAgentAlphabet = function(){
                                  return(private$.normAgentAlphabet)
                                },
                                #' @field normAgent
                                #' Returns the instance variable normAgent.
                                #' (character)
                                normAgent = function(){
                                  return(as.character(private$.normAgent))
                                },
                                #' @field setNormAgent
                                #' Sets the instance variable normAgent.
                                #' (character)
                                setNormAgent = function(agent = "character") {
                                  private$.normAgent <- factor(agent, levels = self$normAgentAlphabet)
                                },
                                #' @field normParameter
                                #' Returns the instance variable normParameter.
                                normParameter = function(){
                                  return(private$.normParameter)
                                }
                              ), #active
                              ###################
                              # memory management
                              ###################
                              public = list(
                                #' @description
                                #' Creates and returns a new `pgu.normalizer` object.
                                #' @param data_df
                                #' The data to be analyzed.
                                #' (tibble::tibble)
                                #' @return
                                #' A new `pgu.normalizer` object.
                                #' (pguIMP::pgu.normalizer)
                                #' @examples
                                #' y <- tibble:tibble()
                                #' x <- pguIMP:pgu.normalizer$new(data = y)
                                initialize = function(data_df = "tbl_df"){
                                  if(!tibble::is_tibble(data_df)){
                                    data_df <- tibble::tibble(names <- "none",
                                                           values <- c(NA))
                                    private$.normParameter <- tibble::tibble(deatures = "none",
                                                                             min = NA,
                                                                             max = NA,
                                                                             mean = NA,
                                                                             sigma = NA)
                                  } else{
                                    self$detectNormParameter(data_df)
                                  }
                                  private$.normAgentAlphabet <-c("none", "min-max", "mean", "z-score")
                                  self$setNormAgent <- "none"
                                }, #function

                                #' @description
                                #' Clears the heap and
                                #' indicates that instance of `pgu.normalizer` is removed from heap.
                                finalize = function(){
                                  print("Instance of pgu.normalizer removed from heap")
                                }, #function

                                ##########################
                                # print instance variables
                                ##########################
                                #' @description
                                #' Prints instance variables of a `pgu.normalizer` object.
                                #' @return
                                #' string
                                #' @examples
                                #' x$print()
                                #' print(x)
                                print = function(){
                                  rString <- sprintf("\npgu.normalizer\n")
                                  cat(rString)
                                  cat("normAgentAlphabet:\n")
                                  print(private$.normAgentAlphabet)
                                  cat("\nnormAgent:\n")
                                  print(private$.normAgent)
                                  cat("\nnormParameter:\n")
                                  print(private$.normParameter)
                                  cat("\n\n")
                                  invisible(self)
                                }, #function

                                #' @description
                                #' Resets instance variable `normParameter`
                                #' @param data_df
                                #' Dataframe to be analyzed.
                                #' (tibble::tibble)
                                #' @examples
                                #' x$detectNormParameter(data_df)
                                detectNormParameter = function(data_df  = "tbl_df"){
                                  private$.normParameter <- data_df %>%
                                    dplyr::select_if(is.numeric) %>%
                                    dplyr::summarise_all(c(min=min, max=max, mean=mean, sigma=sd), na.rm = TRUE) %>%
                                    tidyr::pivot_longer(cols = dplyr::everything(),
                                                        names_sep = "_",
                                                        names_to  = c("features", ".value"))

                                }, #function

                                #' @description
                                #' Scales a tibble using the method defined by the instance variable normAgent
                                #' @param data_df
                                #' Dataframe to be scaled
                                #' (tible::tibble)
                                #' @return
                                #' A normalized version of the dataframe.
                                #' (tibble::tibble)
                                #' @examples
                                #' x$scale(data_df)
                                scale_data = function(data_df = "tbl_df"){
                                  switch(self$normAgent,
                                         "none"=data_df,
                                         "min-max"=self$scale_minMax(data_df),
                                         "mean"=self$scale_mean(data_df),
                                         "z-score"=self$scale_zScore(data_df),
                                         stop("unkown normAgent")) %>%
                                    return()
                                }, #function

                                #' @description
                                #' Scales a tibble using min-max normalization
                                #' @param data_df
                                #' Dataframe to be scaled
                                #' (tibble::tibble)
                                #' @return
                                #' A min-max normalized version of the dataframe
                                #' @examples
                                #' x$scale_minMax(data_df)
                                scale_minMax = function(data_df = "tbl_df"){
                                  for (feature in self$normParameter[["features"]]){
                                    if(is.numeric(data_df[[feature]])){
                                      data_df[feature] <- self$scale_minMax_numeric(data_df[[feature]], feature)
                                    } #if
                                  } #for
                                  return(data_df)
                                }, #function

                                #' @description
                                #' Scales a numeric object using min-max normalization
                                #' @param values
                                #' Values to be scaled. Either a number or a vector
                                #' (numeric)
                                #' @param feature
                                #' Character to idtentify the proper normalization parameters.
                                #' (character)
                                #' @return
                                #' A min-max normalized version of the numeric object
                                #' @examples
                                #' x$scale_minMax_numeric(values)
                                scale_minMax_numeric = function(values = "numeric", feature = character){
                                  min_val <- self$normParameter %>%
                                    dplyr::filter(features == feature) %>%
                                    dplyr::select(min) %>%
                                    as.numeric()

                                  max_val <- self$normParameter %>%
                                    dplyr::filter(features == feature) %>%
                                    dplyr::select(max) %>%
                                    as.numeric()

                                  scaled_values <- (values - min_val) / (max_val - min_val)

                                  return(scaled_values)
                                }, #function

                                #' @description
                                #' Scales a tibble using mean normalization
                                #' @param data_df
                                #' Dataframe to be scaled.
                                #' (tibble::tibble)
                                #' @return
                                #' A mean normalized version of the dataframe
                                #' @examples
                                #' x$scale_mean(data_df)
                                scale_mean = function(data_df = "tbl_df"){
                                  for (feature in self$normParameter[["features"]]){
                                    if(is.numeric(data_df[[feature]])){
                                      data_df[feature] <- self$scale_mean_numeric(data_df[[feature]], feature)
                                    } #if
                                  } #for
                                  return(data_df)
                                }, #function

                                #' @description
                                #' Scales a numeric object using mean normalization
                                #' @param values
                                #' Values to be scaled. Either a number or a vector
                                #' (numeric)
                                #' @param feature
                                #' Character to idtentify the proper normalization parameters.
                                #' (character)
                                #' @return
                                #' A mean normalized version of the numeric object
                                #' @examples
                                #' x$scale_mean_numeric(values)
                                scale_mean_numeric = function(values = "numeric", feature = character){
                                  min_val <- self$normParameter %>%
                                    dplyr::filter(features == feature) %>%
                                    dplyr::select(min) %>%
                                    as.numeric()

                                  max_val <- self$normParameter %>%
                                    dplyr::filter(features == feature) %>%
                                    dplyr::select(max) %>%
                                    as.numeric()

                                  mean_val <- self$normParameter %>%
                                    dplyr::filter(features == feature) %>%
                                    dplyr::select(mean) %>%
                                    as.numeric()

                                  scaled_values <- (values - mean_val) / (max_val - min_val)

                                  return(scaled_values)
                                }, #function

                                #' @description
                                #' Scales a tibble using z-score normalization
                                #' @param data_df
                                #' Dataframe to be scaled
                                #' (tibble::tibble)
                                #' @return
                                #' A z-score normalized version of the dataframe
                                #' @examples
                                #' x$scale_zScore(data_df)
                                scale_zScore = function(data_df = "tbl_df"){
                                  for (feature in self$normParameter[["features"]]){
                                    if(is.numeric(data_df[[feature]])){
                                      data_df[feature] <- self$scale_zScore_numeric(data_df[[feature]], feature)
                                    } #if
                                  } #for
                                  return(data_df)
                                }, #function

                                #' @description
                                #' Scales a numeric object using z-score normalization
                                #' @param values
                                #' Values to be scaled. Either a number or a vector
                                #' (numeric)
                                #' @param feature
                                #' Character to idtentify the proper normalization parameters.
                                #' (character)
                                #' @return
                                #' A z-score normalized version of the numeric object
                                #' @examples
                                #' x$scale_zScore_numeric(values)
                                scale_zScore_numeric = function(values = "numeric", feature = character){
                                  mean_val <- self$normParameter %>%
                                    dplyr::filter(features == feature) %>%
                                    dplyr::select(mean) %>%
                                    as.numeric()

                                  sigma_val <- self$normParameter %>%
                                    dplyr::filter(features == feature) %>%
                                    dplyr::select(sigma) %>%
                                    as.numeric()

                                  scaled_values <- (values - mean_val) / (sigma_val)

                                  return(scaled_values)
                                }, #function

                                #' @description
                                #' Rescales a tibble using the method defined by the instance variable normAgent
                                #' @param data_df
                                #' Normalized dataframe to be rescaled
                                #' (tible::tibble)
                                #' @return
                                #' A rescaled version of the normalized dataframe.
                                #' (tibble::tibble)
                                #' @examples
                                #' x$rescale(data_df)
                                rescale_data = function(data_df = "tbl_df"){
                                  switch(self$normAgent,
                                         "none"=data_df,
                                         "min-max"=self$rescale_minMax(data_df),
                                         "mean"=self$rescale_mean(data_df),
                                         "z-score"=self$rescale_zScore(data_df),
                                         stop("unkown normAgent")) %>%
                                    return()
                                }, #function

                                #' @description
                                #' Rescales a tibble using min-max normalization
                                #' @param data_df
                                #' Normalized dataframe to be rescaled
                                #' (tibble::tibble)
                                #' @return
                                #' A rescaled version of a min-max normalized dataframe
                                #' @examples
                                #' x$rescale_minMax(data_df)
                                rescale_minMax = function(data_df = "tbl_df"){
                                  for (feature in self$normParameter[["features"]]){
                                    if(is.numeric(data_df[[feature]])){
                                      data_df[feature] <- self$rescale_minMax_numeric(data_df[[feature]], feature)
                                    } #if
                                  } #for
                                  return(data_df)
                                }, #function

                                #' @description
                                #' Rescales a numeric object using min-max normalization
                                #' @param values
                                #' Normalized values to be rescaled. Either a number or a vector
                                #' (numeric)
                                #' @param feature
                                #' Character to idtentify the proper normalization parameters.
                                #' (character)
                                #' @return
                                #' Rescaled version of min-max normalized numeric object
                                #' @examples
                                #' x$rescale_minMax_numeric(values)
                                rescale_minMax_numeric = function(values = "numeric", feature = character){
                                  min_val <- self$normParameter %>%
                                    dplyr::filter(features == feature) %>%
                                    dplyr::select(min) %>%
                                    as.numeric()

                                  max_val <- self$normParameter %>%
                                    dplyr::filter(features == feature) %>%
                                    dplyr::select(max) %>%
                                    as.numeric()

                                  rescaled_values <- (values * (max_val - min_val)) + min_val

                                  return(rescaled_values)
                                }, #function

                                #' @description
                                #' Rescales a tibble using mean normalization
                                #' @param data_df
                                #' Normalized dataframe to be rescaled
                                #' (tibble::tibble)
                                #' @return
                                #' A rescaled version of a mean normalized dataframe
                                #' @examples
                                #' x$rescale_mean(data_df)
                                rescale_mean = function(data_df = "tbl_df"){
                                  for (feature in self$normParameter[["features"]]){
                                    if(is.numeric(data_df[[feature]])){
                                      data_df[feature] <- self$rescale_mean_numeric(data_df[[feature]], feature)
                                    } #if
                                  } #for
                                  return(data_df)
                                }, #function

                                #' @description
                                #' Rescales a numeric object using mean normalization
                                #' @param values
                                #' Normalized values to be rescaled. Either a number or a vector
                                #' (numeric)
                                #' @param feature
                                #' Character to idtentify the proper normalization parameters.
                                #' (character)
                                #' @return
                                #' Rescaled version of mean normalized numeric object
                                #' @examples
                                #' x$rescale_mean_numeric(values)
                                rescale_mean_numeric = function(values = "numeric", feature = character){
                                  min_val <- self$normParameter %>%
                                    dplyr::filter(features == feature) %>%
                                    dplyr::select(min) %>%
                                    as.numeric()

                                  max_val <- self$normParameter %>%
                                    dplyr::filter(features == feature) %>%
                                    dplyr::select(max) %>%
                                    as.numeric()

                                  mean_val <- self$normParameter %>%
                                    dplyr::filter(features == feature) %>%
                                    dplyr::select(mean) %>%
                                    as.numeric()

                                  rescaled_values <- (values * (max_val - min_val)) + mean_val

                                  return(rescaled_values)
                                }, #function

                                #' @description
                                #' Rescales a tibble using z-score normalization
                                #' @param data_df
                                #' Normalized dataframe to be rescaled
                                #' (tibble::tibble)
                                #' @return
                                #' A rescaled version of a z-score normalized dataframe
                                #' @examples
                                #' x$rescale_zScore(data_df)
                                rescale_zScore = function(data_df = "tbl_df"){
                                  for (feature in self$normParameter[["features"]]){
                                    if(is.numeric(data_df[[feature]])){
                                      data_df[feature] <- self$rescale_zScore_numeric(data_df[[feature]], feature)
                                    } #if
                                  } #for
                                  return(data_df)
                                }, #function

                                #' @description
                                #' Rescales a numeric object using z-score normalization
                                #' @param values
                                #' Normalized values to be rescaled. Either a number or a vector
                                #' (numeric)
                                #' @param feature
                                #' Character to idtentify the proper normalization parameters.
                                #' (character)
                                #' @return
                                #' Rescaled version of z-score normalized numeric object
                                #' @examples
                                #' x$rescale_zScore_numeric(values)
                                rescale_zScore_numeric = function(values = "numeric", feature = character){
                                  mean_val <- self$normParameter %>%
                                    dplyr::filter(features == feature) %>%
                                    dplyr::select(mean) %>%
                                    as.numeric()

                                  sigma_val <- self$normParameter %>%
                                    dplyr::filter(features == feature) %>%
                                    dplyr::select(sigma) %>%
                                    as.numeric()

                                  rescaled_values <- (values * sigma_val) + mean_val

                                  return(rescaled_values)
                                }, #function

                                ##############################
                                # Graphical output functions #
                                ##############################

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
                                #' x$featureBarPlot(data_df, ) %>%
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
                                  p <- data_df %>%
                                    dplyr::select(feature) %>%
                                    tidyr::gather_(key="feature", value="measurement", feature) %>%
                                    ggplot2::ggplot(mapping=ggplot2::aes_string(x="feature",y="measurement"), na.rm=TRUE)+
                                    ggplot2::geom_boxplot(na.rm=TRUE)+
                                    ggplot2::geom_jitter(color = "darkblue", na.rm=TRUE)
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