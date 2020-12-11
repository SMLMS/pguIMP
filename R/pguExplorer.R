#' @title pgu.explorer
#'
#' @description
#' Visual exploration of the pguIMP dataset.
#'
#' @details
#' Pariwise anlysis of attributes from the pguIMP dataset.
#'
#' @format [R6::R6Class] object.
#' @section Construction:
#' x <- pguIMP::pgu.explorer$new(data)
#'
#' @import R6
#' @import tidyverse
#' @import gridExtra
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
                          .ordinate = "character"
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
                          setRawData = function(obj = "tbl_df"){
                            private$.rawData <- obj
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
                          }
                        ),
                        ###################
                        # memory management
                        ###################
                        public = list(
                          #' @description
                          #' Creates and returns a new `pgu.explorer` object.
                          #' @param obj
                          #' The data to be analyzed.
                          #' (tibble::tibble)
                          #' @return
                          #' A new `pgu.explorer` object.
                          #' (pguIMP::pgu.optimizer)
                          #' @examples
                          #' y <- tibble:tibble()
                          #' x <- pguIMP:pgu.explorer$new(data = y)
                          initialize = function(obj = "tbl_df") {
                            if(class(obj) != "tbl_df"){
                              # obj <- tibble::tibble(!!("Sample Name") <- c("none"),
                              #                        values <- c(NA)
                              #                        )
                              obj <- tibble::tibble(`Sample Name` <- c("none"),
                                                    values <- c(NA)
                              )
                            }#if
                            self$setRawData <- obj
                            self$setAbscissa <- "Sample Name"
                            self$setOrdinate <- "Sample Name"
                          }, #function

                          #' @description
                          #' Clears the heap and
                          #' indicates that instance of `pgu.explorer` is removed from heap.
                          finalize = function() {
                            print("Instance of pgu.explorer removed from heap")
                          }, #function

                          ##########################
                          # print instance variables
                          ##########################
                          #' @description
                          #' Prints instance variables of a `pgu.explorer` object.
                          #' @return
                          #' string
                          #' @examples
                          #' x$print()
                          #' print(x)
                          print = function() {
                            rString <- sprintf("\npgu.explorer\n")
                            cat(rString)
                            aString <- sprintf("\nabscissa: %s\nordindate: %s\n", self$abscissa, self$ordinate)
                            cat(aString)
                            print(self$rawData)
                            cat("\n\n")
                            invisible(self)
                          }, #function

                          ####################
                          # public functions #
                          ####################
                          #' @description
                          #' Resets the instance of the pgu.explorer class
                          #' @param obj
                          #' The data to be analyzed.
                          #' (tibble::tibble)
                          #' @param abs
                          #' The abscissa attribute
                          #' (character)
                          #' @param ord
                          #' The ordinate attribute
                          #' (character)
                          #' @examples
                          #' x$reset(abj, abs = "time", ord = "infected")
                          reset = function(obj = "tbl_df", abs = "character", ord = "character"){
                            self$setRawData <- obj
                            self$setAbscissa <- abs
                            self$setOrdinate <- ord
                          }, #function

                          #' @description
                          #' Tests if the abscissa attribute is of type numeric.
                          #' @return
                          #' Test result
                          #' (logical)
                          #' @examples
                          #' y <- x$abscissaIsNumeric()
                          abscissaIsNumeric = function(){
                            self$rawData %>%
                              dplyr::select(self$abscissa) %>%
                              purrr::map(is.numeric) %>%
                              unlist() %>%
                              return()
                          }, #function

                          #' @description
                          #' Tests if the ordinate attribute is of type numeric.
                          #' @return
                          #' Test result
                          #' (logical)
                          #' @examples
                          #' y <- x$ordinateIsNumeric()
                          ordinateIsNumeric = function(){
                            self$rawData %>%
                              dplyr::select(self$ordinate) %>%
                              purrr::map(is.numeric) %>%
                              unlist() %>%
                              return()
                          }, #function

                          ####################
                          # graphical output #
                          ####################
                          #' @description
                          #' Creates and returns a scatter plot abscissa and ordinate value pairs.
                          #' @return
                          #' Scatter plot
                          #' (ggplot2::ggplot)
                          #' @examples
                          #' x$scatterPlot() %>%
                          #'  show()
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
                          #' @examples
                          #' x$abscissaBarPlot() %>%
                          #'  show()
                          abscissaBarPlot = function(){
                            p <- NULL
                            if(self$abscissaIsNumeric()){
                              p <- self$rawData %>%
                                ggplot2::ggplot(mapping = ggplot2::aes_string(x = as.name(self$abscissa)), na.rm=TRUE) +
                                ggplot2::geom_bar(stat = "bin") +
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
                          #' @examples
                          #' x$abscissaBoxPlot() %>%
                          #'  show()
                          abscissaBoxPlot = function(){
                            p <- NULL
                            if(self$abscissaIsNumeric()){
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
                          #' @examples
                          #' x$abscissaPlot() %>%
                          #'  show()
                          abscissaPlot = function(){
                            p <- NULL
                            if(self$abscissaIsNumeric()){
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
                          #' @examples
                          #' x$ordinateBarPlot() %>%
                          #'  show()
                          ordinateBarPlot = function(){
                            p <- NULL
                            if(self$ordinateIsNumeric()){
                              p <- self$rawData %>%
                                ggplot2::ggplot(mapping = ggplot2::aes_string(x = as.name(self$ordinate)), na.rm=TRUE) +
                                ggplot2::geom_bar(stat = "bin") +
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
                          #' @examples
                          #' x$ordinateBoxPlot() %>%
                          #'  show()
                          ordinateBoxPlot = function(){
                            p <- NULL
                            if(self$ordinateIsNumeric()){
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
                          #' @examples
                          #' x$ordinatePlot() %>%
                          #'  show()
                          ordinatePlot = function(){
                            p <- NULL
                            if(self$ordinateIsNumeric()){
                              p1 <- self$ordinateBoxPlot()
                              limits <- ggplot2::layer_scales(p1)$y$range$range
                              p2 <- self$ordinateBarPlot() +
                                ggplot2::scale_x_continuous(position = "top", limits=limits) +
                                ggplot2::coord_flip()
                              p <- gridExtra::grid.arrange(p1,p2, layout_matrix = rbind(c(1,2),c(1,2)),
                                                           top = textGrob(label = sprintf("Distribution of %s", self$ordinate)))
                            }# if
                            return(p)
                          }, #function

                          ##################
                          # numeric output #
                          ##################
                          #' @description
                          #' Summarizes the numeric values of a vector.
                          #' @param val
                          #' values to analyze
                          #' (numeric)
                          #' @return
                          #' summary
                          #' (tibble::tibble)
                          #' @examples
                          #' x$summarizeNumeric(val) %>%
                          #'  print()
                          summarizeNumeric = function(val = "numeric"){
                            if(!any(is.na(val))){
                              res <- c(summary(val),"NA's"=0)
                            }#if
                            else{
                              res <- summary(val)
                            }#else
                            return(res)
                          }, #function

                          #' @description
                          #' Calculates and returns the statistics of the abscissa values.
                          #' @return
                          #' statistics
                          #' (tibble::tibble)
                          #' @examples
                          #' x$abscissaStatistic() %>%
                          #'  print()
                          abscissaStatistic = function(){
                            t <- NULL
                            if(self$abscissaIsNumeric()){
                              t <- self$rawData %>%
                                dplyr::select(self$abscissa) %>%
                                apply(MARGIN=2, FUN=self$summarizeNumeric) %>%
                                as.data.frame() %>%
                                tibble::rownames_to_column("Value") %>%
                                tibble::as_tibble()
                            }#if
                            return(t)
                          }, #function

                          #' @description
                          #' Calculates and returns the statistics of the ordinate values.
                          #' @return
                          #' statistics
                          #' (tibble::tibble)
                          #' @examples
                          #' x$ordinateStatistic() %>%
                          #'  print()
                          ordinateStatistic = function(){
                            t <- NULL
                            if(self$abscissaIsNumeric()){
                              t <- self$rawData %>%
                                dplyr::select(self$ordinate) %>%
                                apply(MARGIN=2, FUN=self$summarizeNumeric) %>%
                                as.data.frame() %>%
                                tibble::rownames_to_column("Value") %>%
                                tibble::as_tibble()
                            }#if
                            return(t)
                          }#function
                        )#public
)#class
