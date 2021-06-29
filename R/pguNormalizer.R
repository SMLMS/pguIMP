#' @title pgu.normalizer
#'
#' @description
#' Normalization of data. Part of pguIMP.
#'
#' @details
#' Performs a data normalization in order to achieve a standardized version of the dataframe.
#' This object is used by the shiny based gui and is not for use in individual R-scripts!
#'
#' @format [R6::R6Class] object.
#'
#' @importFrom dplyr filter group_by mutate pull select select_if summarise_all sym
#' @importFrom ggplot2 aes_string coord_flip element_blank element_rect geom_bar geom_boxplot
#' @importFrom ggplot2 geom_jitter ggplot layer_scales scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 theme theme_linedraw xlab ylab
#' @importFrom gridExtra grid.arrange
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @importFrom tibble is_tibble tibble
#' @importFrom tidyr gather gather_ pivot_longer spread
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
                                .features = "character",
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
                                #' @field features
                                #' Returns instance variable features.
                                #' (character)
                                features = function(){
                                  return(private$.features)
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
                                initialize = function(data_df = "tbl_df"){
                                  if(!tibble::is_tibble(data_df)){
                                    data_df <- tibble::tibble(names <- "none",
                                                           values <- c(NA))
                                    private$.normParameter <- tibble::tibble(feature = character(0),
                                                                             max = numeric(0),
                                                                             mean = numeric(0),
                                                                             min = numeric(0),
                                                                             sigma = numeric(0))
                                    private$.features <- character(0)
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
                                detectNormParameter = function(data_df  = "tbl_df"){
                                  private$.normParameter <- data_df %>%
                                    dplyr::select_if(is.numeric) %>%
                                    tidyr::gather(key = "feature", value = "value") %>%
                                    dplyr::group_by(feature) %>%
                                    dplyr::summarise_all(c(min=min, max=max, mean=mean, sigma=sd), na.rm = TRUE) %>%
                                    tidyr::pivot_longer(cols = -c("feature")) %>%
                                    tidyr::spread(key = name, value = value)

                                  private$.features <- self$normParameter %>%
                                    dplyr::select(feature) %>%
                                    dplyr::pull() %>%
                                    unique()
                                }, #function

                                #' @description
                                #' Scales a tibble using the method defined by the instance variable normAgent
                                #' @param data_df
                                #' Dataframe to be scaled
                                #' (tible::tibble)
                                #' @return
                                #' A normalized version of the dataframe.
                                #' (tibble::tibble)
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
                                scale_minMax = function(data_df = "tbl_df"){
                                  for (feature in self$features){
                                    if(is.numeric(data_df[[feature]])){
                                      data_df <- data_df %>%
                                        dplyr::mutate(!!feature := self$scale_minMax_numeric(data_df[[feature]], feature))
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
                                scale_minMax_numeric = function(values = "numeric", feature = "character"){
                                  min_val <- self$normParameter %>%
                                    dplyr::filter(feature == !!feature) %>%
                                    dplyr::select("min") %>%
                                    dplyr::pull() %>%
                                    as.numeric()

                                  max_val <- self$normParameter %>%
                                    dplyr::filter(feature == !!feature) %>%
                                    dplyr::select("max") %>%
                                    dplyr::pull() %>%
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
                                scale_mean = function(data_df = "tbl_df"){
                                  for (feature in self$features){
                                    if(is.numeric(data_df[[feature]])){
                                      data_df <- data_df %>%
                                        dplyr::mutate(!!feature := self$scale_mean_numeric(data_df[[feature]], feature))
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
                                scale_mean_numeric = function(values = "numeric", feature = character){
                                  min_val <- self$normParameter %>%
                                    dplyr::filter(feature == !!feature) %>%
                                    dplyr::select("min") %>%
                                    dplyr::pull() %>%
                                    as.numeric()

                                  max_val <- self$normParameter %>%
                                    dplyr::filter(feature == !!feature) %>%
                                    dplyr::select("max") %>%
                                    dplyr::pull() %>%
                                    as.numeric()

                                  mean_val <- self$normParameter %>%
                                    dplyr::filter(feature == !!feature) %>%
                                    dplyr::select("mean") %>%
                                    dplyr::pull() %>%
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
                                scale_zScore = function(data_df = "tbl_df"){
                                  for (feature in self$features){
                                    if(is.numeric(data_df[[feature]])){
                                      data_df <- data_df %>%
                                        dplyr::mutate(!!feature := self$scale_zScore_numeric(data_df[[feature]], feature))
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
                                scale_zScore_numeric = function(values = "numeric", feature = character){
                                  mean_val <- self$normParameter %>%
                                    dplyr::filter(feature == !!feature) %>%
                                    dplyr::select("mean") %>%
                                    dplyr::pull() %>%
                                    as.numeric()

                                  sigma_val <- self$normParameter %>%
                                    dplyr::filter(feature == !!feature) %>%
                                    dplyr::select("sigma") %>%
                                    dplyr::pull() %>%
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
                                rescale_minMax = function(data_df = "tbl_df"){
                                  for (feature in self$features){
                                    if(is.numeric(data_df[[feature]])){
                                      data_df <- data_df %>%
                                        dplyr::mutate(!!feature := self$rescale_minMax_numeric(data_df[[feature]], feature))
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
                                rescale_minMax_numeric = function(values = "numeric", feature = character){
                                  min_val <- self$normParameter %>%
                                    dplyr::filter(feature == !!feature) %>%
                                    dplyr::select("min") %>%
                                    dplyr::pull() %>%
                                    as.numeric()

                                  max_val <- self$normParameter %>%
                                    dplyr::filter(feature == !!feature) %>%
                                    dplyr::select("max") %>%
                                    dplyr::pull() %>%
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
                                rescale_mean = function(data_df = "tbl_df"){
                                  for (feature in self$features){
                                    if(is.numeric(data_df[[feature]])){
                                      data_df <- data_df %>%
                                        dplyr::mutate(!!feature := self$rescale_mean_numeric(data_df[[feature]], feature))
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
                                rescale_mean_numeric = function(values = "numeric", feature = character){
                                  min_val <- self$normParameter %>%
                                    dplyr::filter(feature == !!feature) %>%
                                    dplyr::select("min") %>%
                                    dplyr::pull() %>%
                                    as.numeric()

                                  max_val <- self$normParameter %>%
                                    dplyr::filter(feature == !!feature) %>%
                                    dplyr::select("max") %>%
                                    dplyr::pull() %>%
                                    as.numeric()

                                  mean_val <- self$normParameter %>%
                                    dplyr::filter(feature == !!feature) %>%
                                    dplyr::select("mean") %>%
                                    dplyr::pull() %>%
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
                                rescale_zScore = function(data_df = "tbl_df"){
                                  for (feature in self$features){
                                    if(is.numeric(data_df[[feature]])){
                                      data_df <- data_df %>%
                                        dplyr::mutate(!!feature := self$rescale_zScore_numeric(data_df[[feature]], feature))
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
                                rescale_zScore_numeric = function(values = "numeric", feature = character){
                                  mean_val <- self$normParameter %>%
                                    dplyr::filter(feature == !!feature) %>%
                                    dplyr::select("mean") %>%
                                    dplyr::pull() %>%
                                    as.numeric()

                                  sigma_val <- self$normParameter %>%
                                    dplyr::filter(feature == !!feature) %>%
                                    dplyr::select("sigma") %>%
                                    dplyr::pull() %>%
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
                                featureBarPlot = function(data_df = "tbl_df", feature = "character"){
                                  feature <- dplyr::sym(feature)
                                  p <- data_df %>%
                                    ggplot2::ggplot(mapping = ggplot2::aes_string(x=feature), na.rm=TRUE) +
                                    ggplot2::geom_bar(stat = "bin", bins=30) +
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
                                  p <- data_df %>%
                                    dplyr::select(feature) %>%
                                    tidyr::gather_(key="feature", value="measurement", feature) %>%
                                    ggplot2::ggplot(mapping=ggplot2::aes_string(x="feature",y="measurement"), na.rm=TRUE)+
                                    ggplot2::geom_boxplot(na.rm=TRUE, outlier.shape = NA)+
                                    ggplot2::geom_jitter(color = "darkblue", na.rm=TRUE) +
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
