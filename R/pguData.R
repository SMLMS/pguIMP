#' @title pgu.data
#' @description
#' Handles the pguIMP dataset.
#' @details
#' Stores the pguIMP dataset as instance variable and keeps track of the attributes of interest.
#' Provides additionally fast access to several statistical information about the data set.
#' This object is used by the shiny based gui and is not for use in individual R-scripts!
#'
#' @format [R6::R6Class] object.
#' @importFrom R6 R6Class
#' @importFrom magrittr %>%
#' @importFrom dplyr pull select mutate rename summarise summarise_all select_if all_of
#' @importFrom purrr negate
#' @importFrom tibble tibble as_tibble is_tibble rownames_to_column
#' @importFrom tidyr gather
#'
#' @include pguFile.R
#'
#' @examples
#' require(dplyr)
#' require(tibble)
#' data(iris)
#' data_df <- iris %>%
#'   tibble::as_tibble()
#' data_obj = pguIMP::pgu.data$new(data_df)
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#' @export
#'

pgu.data <- R6::R6Class("pgu.data",
                        ####################
                        # instance variables
                        ####################
                        private = list(
                          .rawData = "tbl_df",
                          .attributeNames = "character",
                          .numericalAttributeNames = "character",
                          .categoricalAttributeNames = "character",
                          .classInformation = "tbl_df",
                          .statistics = "tbl_df",
                          .reducedStatistics = "tbl_df",
                          .missingsStatistics = "tbl_df",

                          #' @description
                          #' Clears the heap and
                          #' indicates that instance of pguIMP::pgu.data is removed from heap.
                          finalize = function()
                          {
                            print("Instance of pgu.data removed from heap")
                          }, #end pguIMP::pgu.data$finalize

                          #' @description
                          #' Summarizes information on the instance variable rawData
                          #' and retruns it in form of a compact data frame.
                          analyze_classes = function(){
                            private$.classInformation <- self$rawData %>%
                              dplyr::summarise_all(class) %>%
                              tidyr::gather(variable, class)
                          }, #end pguIMP::pgi.data$analyze_classes


                          #' @description
                          #' Summarizes a vector of numericals and returns summary.
                          #' @param val
                          #' Vector of numericals to be summarized.
                          #' (numeric)
                          summarize_numerical_data = function(val = "numeric")
                          {
                            if(!any(is.na(val))){
                              res <- c(summary(val),"NA's"=0)
                            }#if
                            else{
                              res <- summary(val)
                            }#else
                            return(res)
                          }, #end pguIMP::pgi.data$summarize_numerical_data

                          #' @description
                          #' Iterativley calls the function summarize_numerical_data on all numerical attributes
                          #' of the instance variable rawData and returns the result in form of a data frame.
                          calculate_statistics = function()
                          {
                            if (length(self$numerical_data()) > 0)
                            {
                              private$.statistics <- self$numerical_data() %>%
                                apply(MARGIN=2, FUN=private$summarize_numerical_data) %>%
                                t() %>%
                                as.data.frame() %>%
                                tibble::rownames_to_column("Value") %>%
                                tibble::as_tibble()
                            }else{
                              private$.statistics <- tibble::tibble(Value = character(0))
                            }

                          }, #end pguIMP::pgu.data$calculate_statistics

                          #' @description
                          #' Calls the function calculate_statistics
                          #' filters the result for the attribute names and arithmetic mean values.
                          #' and returns the result in form of a data frame.
                          reduce_statistics = function()
                          {
                            if ("Mean" %in% colnames(self$statistics))
                            {
                              private$.reducedStatistics <- self$statistics %>%
                                dplyr::select(c("Value", "Mean"))
                            }else{
                              private$.reducedStatistics <- tibble::tibble(Value = character(0),
                                                                           Mean = numeric(0))
                            }

                          }, #end pguIMP::pgu.data$reduce_satatistics

                          #' @description
                          #' Calls the class' function dataStatistics
                          #' filters the result for the attribute names and information about missing values.
                          #' and returns the result in form of a data frame.
                          detect_missings = function()
                          {
                            if("NA's" %in% colnames(self$statistics))
                            {
                              len <- nrow(self$rawData)
                              private$.missingsStatistics <- self$statistics %>%
                                dplyr::select(c("Value", "NA's")) %>%
                                dplyr::rename(absolute = !!("NA's")) %>%
                                dplyr::mutate(fraction = absolute / len)
                            }else{
                              private$.missingsStatistics <- tibble::tibble(Value = character(0),
                                                                            absolute = numeric(0),
                                                                            fraction = numeric(0))
                            }

                          }#end pguIMP::pgu.data$detect_missings
                        ), #end pguIMP::pgu.data$private


                        ##################
                        # accessor methods
                        ##################
                        active = list(
                          #' @field rawData
                          #' Returns the instance variable rawData
                          #' (tibble::tibble)
                          rawData = function()
                          {
                            return(private$.rawData)
                          },
                          #' @field setRawData
                          #' Sets the instance variable rawData
                          #' (tibble::tibble)
                          setRawData = function(data_df = "tbl_df")
                          {
                            if (!tibble::is_tibble(data_df)){
                              data_df <- tibble::tibble("Sample Name" := c(character(0)))
                            }
                            if(!"Sample Name" %in% colnames(data_df)){
                              sampleName <- seq.int(from =1, to = nrow(data_df), by =1)
                              data_df <- data_df %>%
                                dplyr::mutate("Sample Name" := sampleName) %>%
                                dplyr::select("Sample Name", dplyr::everything())
                            }
                            private$.rawData <- data_df %>%
                              dplyr::mutate("Sample Name" := as.character(data_df[["Sample Name"]]))
                            private$.attributeNames <- colnames(data_df)
                            private$.numericalAttributeNames <- self$rawData %>%
                              dplyr::select_if(is.numeric) %>%
                              colnames()
                            private$.categoricalAttributeNames <- self$rawData %>%
                              dplyr::select_if(purrr::negate(is.numeric)) %>%
                              colnames()
                          },
                          #' @field attributeNames
                          #' Returns the instance variable attributeNames
                          #' (character)
                          attributeNames = function()
                          {
                            return(private$.attributeNames)
                          },
                          #' @field numericalAttributeNames
                          #' Returns the instance variable numericalAttributeNames
                          #' (character)
                          numericalAttributeNames = function()
                          {
                            return(private$.numericalAttributeNames)
                          },
                          #' @field categoricalAttributeNames
                          #' Returns the instance variable categoricalAttributeNames
                          #' (character)
                          categoricalAttributeNames = function()
                          {
                            return(private$.categoricalAttributeNames)
                          },
                          #' @field classInformation
                          #' Returns the instance variable classInformation
                          #' (tibble::tibble)
                          classInformation = function()
                          {
                            return(private$.classInformation)
                          },
                          #' @field statistics
                          #' Returns the instance variable statistics
                          #' (tibble::tibble)
                          statistics = function()
                          {
                            return(private$.statistics)
                          },
                          #' @field reducedStatistics
                          #' Returns the instance variable reducedStatistics
                          #' (tibble::tibble)
                          reducedStatistics = function()
                          {
                            return(private$.reducedStatistics)
                          },
                          #' @field missingsStatistics
                          #' Returns the instance variable missingsStatistics
                          #' (tibble::tibble)
                          missingsStatistics = function()
                          {
                            return(private$.missingsStatistics)
                          }
                        ), #end pguIMP::pgi.data$active

                        ####################
                        # public functions #
                        ####################
                        public = list(
                          #' @description
                          #' Creates and returns a new pguIMP::pgu.data object.
                          #' @param data_df
                          #' The data to be analyzed.
                          #' (tibble::tibble)
                          #' @return
                          #' A new pguIMP::pgu.data object.
                          #' (pguIMP::pgu.data)
                          initialize = function(data_df = "tbl_df")
                          {
                            self$setRawData <- data_df
                          }, #end pgiIMP::pgu.data$initialize

                          #' @description
                          #' Prints instance variables of a pguIMP::pgu.data object.
                          #' @return
                          #' string
                          print = function()
                          {
                            sprintf("\npgu.data\nrawData:\n") %>%
                              cat()
                            print(head(self$rawData))
                            sprintf("\nattributeNames:\n") %>%
                              cat()
                            print(self$attributeNames)
                            sprintf("\nnumericalAttributeNames:\n") %>%
                              cat()
                            print(self$numericalAttributeNames)
                            sprintf("\ncategoricalAttributeNames:\n") %>%
                              cat()
                            print(self$categoricalAttributeNames)
                            sprintf("\nclassInfromation:\n") %>%
                              cat()
                            print(self$classInformation)
                            sprintf("\nstatistics:\n") %>%
                              cat()
                            print(self$statistics)
                            sprintf("\nreducedStatistics:\n") %>%
                              cat()
                            print(self$reducedStatistics)
                            sprintf("\nmissingsStatistics:\n") %>%
                              cat()
                            print(self$missingsStatistics)
                            cat("\n\n")
                            invisible(self)
                          }, #end pguIMP::pgu.data$print

                          #' @description
                          #' Extracts information about the instance variable rawData.
                          fit = function()
                          {
                            private$analyze_classes()
                            private$calculate_statistics()
                            private$reduce_statistics()
                            private$detect_missings()
                          }, #end pguIMP::pgi.data$fit()


                          #' @description
                          #' Returns the index of an attribute within the instance variable attributeNames.
                          #' @param attribute
                          #' Attribute's name.
                          #' (character)
                          #' @return
                          #' Index of attribute's name in rawData
                          #' (numeric)
                          attribute_index = function(attribute = "character")
                          {
                            idx <- match(attribute, self$attributeNames)
                            if(is.na(idx)){
                              rString <- sprintf("\nWarning in pgu.data: attribute %s is not known\n",
                                                 attribute)
                              cat(rString)
                            }#if
                            return(idx)
                          }, #end pguIMP::pgu.data$attribute_idx()

                          #' @description
                          #' Returns the numeric attributes of the instance variable rawData.
                          #' @return
                          #' A data frame
                          #' (tibble::tibble)
                          numerical_data = function()
                          {
                            self$rawData %>%
                              dplyr::select_if(is.numeric) %>%
                              return()
                          }, #end pguIMP::pgu.data$numerical_data()

                          #' @description
                          #' Returns the categorical attributes of the instance variable rawData.
                          #' @return
                          #' A data frame
                          #' (tibble::tibble)
                          categorical_data = function()
                          {
                            self$rawData %>%
                              dplyr::select_if(purrr::negate(is.numeric)) %>%
                              return()
                          } #end pguIMP::pgi.data$categorical_data()


                        )#end pguIMP::pgi.data$public
)#end pguIMP::pgu.data
