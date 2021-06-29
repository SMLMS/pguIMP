#' @title pgu.explorer
#'
#' @description
#' Visual exploration of the pguIMP dataset.
#'
#' @details
#' Pariwise anlysis of attributes from the pguIMP dataset.
#' This object is used by the shiny based gui and is not for use in individual R-scripts!
#'
#' @format [R6::R6Class] object.
#'
#' @importFrom dplyr select
#' @importFrom ggplot2 aes_string coord_flip element_rect geom_bar geom_boxplot geom_jitter
#' @importFrom ggplot2 geom_point ggplot ggtitle layer_scales scale_x_continuous
#' @importFrom ggplot2 theme theme_linedraw xlab ylab
#' @importFrom gridExtra grid.arrange
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom R6 R6Class
#' @importFrom tibble as_tibble is_tibble rownames_to_column tibble
#' @importFrom tidyr gather_
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pgu.explorer <- R6::R6Class("pgu.explorer",
                        ####################
                        # instance variables
                        ####################
                        private = list(
                          .rawData = "tbl_df",
                          .abscissa = "character",
                          .ordinate = "character",
                          .abscissaStatistics = "tbl_df",
                          .ordinateStatistics = "tbl_df",

                          #' @description
                          #' Tests if the abscissa attribute is of type numeric.
                          abscissa_is_numeric = function()
                            {
                            self$rawData %>%
                              dplyr::select(self$abscissa) %>%
                              purrr::map(is.numeric) %>%
                              unlist() %>%
                              return()
                          }, #pguIMP::pgu.explorer$abscissa_is_numeric

                          #' @description
                          #' Tests if the ordinate attribute is of type numeric.
                          ordinate_is_numeric = function()
                          {
                            self$rawData %>%
                              dplyr::select(self$ordinate) %>%
                              purrr::map(is.numeric) %>%
                              unlist() %>%
                              return()
                          }, #pguIMP::pgu.explorer$ordinate_is_numeric

                          #' @description
                          #' Summarizes the numeric values of a vector.
                          summarize_numeric = function(val = "numeric")
                          {
                            if(!any(is.na(val))){
                              res <- c(summary(val),"NA's"=0)
                            }#if
                            else{
                              res <- summary(val)
                            }#else
                            return(res)
                          }, #pguIMP::pgu.explorer$summarize_numeric

                          #' @description
                          #' Calculates the statistics of the abscissa values.
                          #' Stores the result in the instance variable abscissaStatistics.
                          calculate_abscissa_statistics = function()
                          {
                            if(private$abscissa_is_numeric())
                            {
                              private$.abscissaStatistics <- self$rawData %>%
                                dplyr::select(self$abscissa) %>%
                                apply(MARGIN=2, FUN=private$summarize_numeric) %>%
                                as.data.frame() %>%
                                tibble::rownames_to_column("Value") %>%
                                tibble::as_tibble()
                            }else{
                              private$.abscissaStatistics <- NULL
                            }
                          }, #pguIMP::pgu.explorer$calculate_abscissa_statistics

                          #' @description
                          #' Calculates the statistics of the ordinate values.
                          #' Stores the result in the instance variable ordinateStatistics.
                          calculate_ordinate_statistics = function()
                          {
                            if(private$ordinate_is_numeric())
                            {
                              private$.ordinateStatistics <- self$rawData %>%
                                dplyr::select(self$ordinate) %>%
                                apply(MARGIN=2, FUN=private$summarize_numeric) %>%
                                as.data.frame() %>%
                                tibble::rownames_to_column("Value") %>%
                                tibble::as_tibble()
                            }else{
                              private$.ordinateStatistics <- NULL
                            }
                          }, #pguIMP::pgu.explorer$calculate_ordinate_statistics

                          #' @description
                          #' Clears the heap and
                          #' indicates that instance of `pgu.explorer` is removed from heap.
                          finalize = function() {
                            print("Instance of pgu.explorer removed from heap")
                          } #pguIMP::pgu.explorer$finalize
                        ),
                        ##################
                        # accessor methods
                        ##################
                        active = list(
                          #' @field rawData
                          #' Returns the instance variable rawData
                          #' (tibble::tibble)
                          rawData = function(){
                            return(private$.rawData)
                          },
                          #' @field setRawData
                          #' Sets the instance variable rawData
                          #' (tibble::tibble)
                          setRawData = function(data_df = "tbl_df"){
                            if(tibble::is_tibble(data_df))
                            {
                              private$.rawData <- data_df
                            }
                          },
                          #' @field abscissa
                          #' Returns the instance variable abscissa
                          #' (character)
                          abscissa = function(){
                            return(private$.abscissa)
                          },
                          #' @field setAbscissa
                          #' Sets the instance variable abscissa
                          #' (character)
                          setAbscissa = function(value = "character"){
                            private$.abscissa <- value
                          },
                          #' @field ordinate
                          #' Returns the instance variable ordinate
                          #' (character)
                          ordinate = function(){
                            return(private$.ordinate)
                          },
                          #' @field setOrdinate
                          #' Sets the instance variable ordinate
                          #' (character)
                          setOrdinate = function(value = "character"){
                            private$.ordinate <- value
                          },
                          #' @field abscissaStatistics
                          #' Returns the instance variable abscissaStatistics
                          #' (character)
                          abscissaStatistics = function(){
                            return(private$.abscissaStatistics)
                          },
                          #' @field ordinateStatistics
                          #' Returns the instance variable ordinateStatistics
                          #' (character)
                          ordinateStatistics = function(){
                            return(private$.ordinateStatistics)
                          }
                        ),

                        ####################
                        # public functions #
                        ####################
                        public = list(
                          #' @description
                          #' Creates and returns a new `pgu.explorer` object.
                          #' @param data_df
                          #' The data to be analyzed.
                          #' (tibble::tibble)
                          #' @return
                          #' A new `pgu.explorer` object.
                          #' (pguIMP::pgu.optimizer)
                          initialize = function(data_df = "tbl_df")
                          {
                            if(!tibble::is_tibble(data_df)){
                              data_df <- tibble::tibble(`Sample Name` <- c("none"),
                                                        values <- c(NA)
                              )
                            }#if
                            self$setRawData <- data_df
                            self$setAbscissa <- "Sample Name"
                            self$setOrdinate <- "Sample Name"
                          }, #pguIMP::pgu.explorer$initialize



                          ##########################
                          # print instance variables
                          ##########################
                          #' @description
                          #' Prints instance variables of a `pgu.explorer` object.
                          #' @return
                          #' string
                          print = function()
                          {
                            rString <- sprintf("\npgu.explorer\n")
                            cat(rString)
                            aString <- sprintf("\nabscissa: %s\nordindate: %s\n", self$abscissa, self$ordinate)
                            cat(aString)
                            print(self$rawData)
                            cat("\n\nAbscissa statistics:\n")
                            print(self$abscissaStatistics)
                            cat("\n\nOrdinate statistics:\n")
                            print(self$ordinateStatistics)
                            cat("\n\n")
                            invisible(self)
                          }, #pguIMP::pgu.explorer$print

                          ####################
                          # public functions #
                          ####################
                          #' @description
                          #' Resets the instance of the pgu.explorer class
                          #' @param data_df
                          #' The data to be analyzed.
                          #' (tibble::tibble)
                          #' @param abs
                          #' The abscissa attribute
                          #' (character)
                          #' @param ord
                          #' The ordinate attribute
                          #' (character)
                          reset = function(data_df = "tbl_df", abs = "character", ord = "character")
                          {
                            self$setRawData <- data_df
                            self$setAbscissa <- abs
                            self$setOrdinate <- ord
                          }, #pguIMP::pgu.explorer$reset

                          #' @description
                          #' Calculates the abscissa and ordinate statistics
                          fit = function()
                          {
                            private$calculate_abscissa_statistics()
                            private$calculate_ordinate_statistics()
                          }, #pguIMP::pgu.explorer$fit


                          ####################
                          # graphical output #
                          ####################
                          #' @description
                          #' Creates and returns a scatter plot abscissa and ordinate value pairs.
                          #' @return
                          #' Scatter plot
                          #' (ggplot2::ggplot)
                          scatterPlot = function(){
                            p <- self$rawData %>%
                              ggplot2::ggplot(mapping = ggplot2::aes_string(x = as.name(self$abscissa), y = as.name(self$ordinate)), na.rm = TRUE) +
                              ggplot2::geom_point() +
                              ggplot2::ggtitle("Scatter Plot") +
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
                          #' Creates and returns a histogram from the abscissa values.
                          #' @return
                          #' Bar plot
                          #' (ggplot2::ggplot)
                          abscissaBarPlot = function(){
                            p <- NULL
                            if(private$abscissa_is_numeric()){
                              p <- self$rawData %>%
                                ggplot2::ggplot(mapping = ggplot2::aes_string(x = as.name(self$abscissa)), na.rm=TRUE) +
                                ggplot2::geom_bar(stat = "bin", bins =30, na.rm=TRUE) +
                                ggplot2::ylab("counts") +
                                ggplot2::xlab("value") +
                                ggplot2::theme_linedraw() +
                                ggplot2::theme(
                                  panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                                  plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                                  legend.background = ggplot2::element_rect(fill = "transparent"),
                                  legend.key = ggplot2::element_rect(fill = "transparent")
                                )
                            }#if
                            return(p)
                          }, #function

                          #' @description
                          #' Creates and returns a box plot from the abscissa values.
                          #' @return
                          #' Box plot
                          #' (ggplot2::ggplot)
                          abscissaBoxPlot = function(){
                            p <- NULL
                            if(private$abscissa_is_numeric()){
                              p <- self$rawData %>%
                                dplyr::select(self$abscissa) %>%
                                tidyr::gather_(key="feature", value="measurement", as.name(self$abscissa)) %>%
                                ggplot2::ggplot(mapping=ggplot2::aes_string(x="feature",y="measurement"), na.rm=TRUE)+
                                ggplot2::geom_boxplot(na.rm=TRUE, outlier.shape = NA)+
                                ggplot2::geom_jitter() +
                                ggplot2::ylab("value") +
                                ggplot2::xlab("feature") +
                                ggplot2::theme_linedraw() +
                                ggplot2::theme(
                                  panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                                  plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                                  legend.background = ggplot2::element_rect(fill = "transparent"),
                                  legend.key = ggplot2::element_rect(fill = "transparent")
                                )
                            }#if
                            return(p)
                          }, #function

                          #' @description
                          #' Creates and returns a compund graphical analysis of the abscissa values.
                          #' @return
                          #' Compound plot
                          #' (gridExtra::grid.arrange)
                          abscissaPlot = function(){
                            p <- NULL
                            if(private$abscissa_is_numeric()){
                              p1 <- self$abscissaBoxPlot()
                              limits <- ggplot2::layer_scales(p1)$y$range$range
                              p2 <- self$abscissaBarPlot() +
                                ggplot2::scale_x_continuous(position = "top", limits=limits) +
                                ggplot2::coord_flip()
                              p <- gridExtra::grid.arrange(p1,p2, layout_matrix = rbind(c(1,2),c(1,2)),
                                                           top = textGrob(label = sprintf("Distribution of %s", self$abscissa)))
                            }# if
                            return(p)
                          }, #function

                          #' @description
                          #' Creates and returns a histogram from the ordinate values.
                          #' @return
                          #' Bar plot
                          #' (ggplot2::ggplot)
                          ordinateBarPlot = function(){
                            p <- NULL
                            if(private$ordinate_is_numeric()){
                              p <- self$rawData %>%
                                ggplot2::ggplot(mapping = ggplot2::aes_string(x = as.name(self$ordinate)), na.rm=TRUE) +
                                ggplot2::geom_bar(stat = "bin", bins =30, na.rm = TRUE) +
                                ggplot2::ylab("counts") +
                                ggplot2::xlab("value") +
                                ggplot2::theme_linedraw() +
                                ggplot2::theme(
                                  panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                                  plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                                  legend.background = ggplot2::element_rect(fill = "transparent"),
                                  legend.key = ggplot2::element_rect(fill = "transparent")
                                )
                            }# if
                            return(p)
                          }, #function

                          #' @description
                          #' Creates and returns a box plot from the ordinate values.
                          #' @return
                          #' Box plot
                          #' (ggplot2::ggplot)
                          ordinateBoxPlot = function(){
                            p <- NULL
                            if(private$ordinate_is_numeric()){
                              p <- self$rawData %>%
                                dplyr::select(self$ordinate) %>%
                                tidyr::gather_(key="feature", value="measurement", as.name(self$ordinate)) %>%
                                ggplot2::ggplot(mapping=ggplot2::aes_string(x="feature",y="measurement"), na.rm=TRUE)+
                                ggplot2::geom_boxplot(na.rm=TRUE, outlier.shape = NA)+
                                ggplot2::geom_jitter() +
                                ggplot2::ylab("value") +
                                ggplot2::xlab("feature") +
                                ggplot2::theme_linedraw() +
                                ggplot2::theme(
                                  panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                                  plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                                  legend.background = ggplot2::element_rect(fill = "transparent"),
                                  legend.key = ggplot2::element_rect(fill = "transparent")
                                )
                            }#if
                            return(p)
                          }, #function

                          #' @description
                          #' Creates and returns a compund graphical analysis of the ordinate values.
                          #' @return
                          #' Compound plot
                          #' (gridExtra::grid.arrange)
                          ordinatePlot = function(){
                            p <- NULL
                            if(private$ordinate_is_numeric()){
                              p1 <- self$ordinateBoxPlot()
                              limits <- ggplot2::layer_scales(p1)$y$range$range
                              p2 <- self$ordinateBarPlot() +
                                ggplot2::scale_x_continuous(position = "top", limits=limits) +
                                ggplot2::coord_flip()
                              p <- gridExtra::grid.arrange(p1,p2, layout_matrix = rbind(c(1,2),c(1,2)),
                                                           top = textGrob(label = sprintf("Distribution of %s", self$ordinate)))
                            }# if
                            return(p)
                          }
                        )#public
)#class
