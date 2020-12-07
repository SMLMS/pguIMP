#' @title pgu.data
#'
#' @description
#' Handles the pguIMP dataset.
#'
#' @details
#' Stores the pguIMP dataset as instance variable and keeps track of the attributes of interest.
#' Provides additionally fast access to several statistical information about the data set.
#'
#' @format [R6::R6Class] object.
#' @section Construction:
#' x <- pguIMP::pgu.data$new(data)
#'
#' @import R6
#' @import tidyverse
#'
#' @include pguFile.R
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pgu.data <- R6::R6Class("pgu.data",
                        ####################
                        # instance variables
                        ####################
                        private = list(
                          .rawData = "tbl_df",
                          .featureNames = "character",
                          .numericFeatureNames = "character",
                          .nonNumericFeatureNames = "character",
                          .activeFeatureNames = "character",
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
                          setRawData = function(data = "tbl_df"){
                            if (!tibble::is_tibble(data)){
                              data <- tibble::tibble("Sample Name" := c(character(0)))
                            }
                            if(!"Sample Name" %in% colnames(data)){
                              sampleName <- seq.int(from =1, to = nrow(data), by =1)
                              data <- data %>%
                                dplyr::mutate("Sample Name" := sampleName) %>%
                                dplyr::select("Sample Name", dplyr::everything())
                            }
                            private$.rawData <- data %>%
                              dplyr::mutate("Sample Name" := as.character(data[["Sample Name"]]))
                            private$.featureNames <- colnames(data)
                            private$.numericFeatureNames <- data %>%
                              dplyr::select_if(is.numeric) %>%
                              colnames()
                            private$.nonNumericFeatureNames <- data %>%
                              # dplyr::select_if(purrr::negate(is.numeric)) %>%
                              dplyr::select(-self$numericFeatureNames) %>%
                              colnames()
                            private$.abscissa <- self$featureNames[1]
                            private$.ordinate <- self$featureNames[1]
                            self$resetActiveFeatureNames()
                          },
                          #' @field featureNames
                          #' Returns the instance variable featureNames
                          #' (character)
                          featureNames = function(){
                            return(private$.featureNames)
                          },
                          #' @field numericFeatureNames
                          #' Returns the insatnce variable numericFeatureNames
                          #' (character)
                          numericFeatureNames = function(){
                            return(private$.numericFeatureNames)
                          },
                          #' @field nonNumericFeatureNames
                          #' Returns the instance variable nonNumericFeatureNames
                          #' (character)
                          nonNumericFeatureNames = function(){
                            return(private$.nonNumericFeatureNames)
                          },
                          #' @field activeFeatureNames
                          #' Returns the instance variable activeFeatureNames
                          #' (character)
                          activeFeatureNames = function(){
                            return(private$.activeFeatureNames)
                          },
                          #' @field setActiveFeatureNames
                          #' Sets the instance variable activeFeatureNames
                          #' (character)
                          setActiveFeatureNames = function(val = "character"){
                            idx = 0
                            for(feature in val){
                              idx <- idx + self$featureIdx(feature)
                            }
                            if(is.na(idx)){
                              self$resetActiveFeatureNames()
                            }
                            else{
                              private$.activeFeatureNames <- val
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
                          setAbscissa = function(feature = "character"){
                            idx <- self$featureIdx(feature)
                            if(!is.na(idx)){
                              private$.abscissa <- feature
                            }
                            else{
                              private$.abscissa <- self$featureNames[1]
                            }
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
                          setOrdinate = function(feature = "character"){
                            idx <- self$featureIdx(feature)
                            if(!is.na(idx)){
                              private$.ordinate <- feature
                            }
                            else{
                              private$.ordinate<- self$featureNames[1]
                            }
                          }
                        ),
                        ###################
                        # memory management
                        ###################
                        public = list(
                          #' @description
                          #' Creates and returns a new `pgu.data` object.
                          #' @param data
                          #' The data to be analyzed.
                          #' (tibble::tibble)
                          #' @return
                          #' A new `pgu.data` object.
                          #' (pguIMP::pgu.data)
                          #' @examples
                          #' y <- tibble::tibble()
                          #' x <- pguIMP:pgu.data$new(data = y)
                          initialize = function(data = "tbl_df") {
                            # if(class(data) != "tbl_df"){
                            #   data <- tibble::tibble(names <- "none",
                            #                          values <- c(NA))
                            # }#if
                            self$setRawData <- data
                          }, #function

                          #' @description
                          #' Clears the heap and
                          #' indicates that instance of `pgu.data` is removed from heap.
                          finalize = function() {
                            print("Instance of pgu.data removed from heap")
                          }, #function

                          ##########################
                          # print instance variables
                          ##########################
                          #' @description
                          #' Prints instance variables of a `pgu.data` object.
                          #' @return
                          #' string
                          #' @examples
                          #' x$print()
                          #' print(x)
                          print = function() {
                            rString <- sprintf("\npgu.data\n")
                            cat(rString)
                            print(head(self$rawData))
                            fString <- sprintf("\nfeatureNames:\n")
                            cat(fString)
                            print(self$featureNames)
                            nString <- sprintf("\nnumeric featureNames:\n")
                            cat(nString)
                            print(self$numericFeatureNames)
                            noString <- sprintf("\nnon numeric featureNames:\n")
                            cat(noString)
                            print(self$nonNumericFeatureNames)
                            aString <- sprintf("\nactive featureNames:\n")
                            cat(aString)
                            print(self$activeFeatureNames)
                            cat("\n\n")
                            invisible(self)
                          }, #functon
                          ####################
                          # public functions #
                          ####################
                          #' @description
                          #' Sets all attribute feature names as active.
                          #' @examples
                          #' x$resetActiveFeatureNames()
                          resetActiveFeatureNames = function(){
                            private$.activeFeatureNames <- self$featureNames
                          }, #function

                          ####################
                          # helper functions #
                          ####################
                          #' @description
                          #' Returns the index of an attributes feature name within the object's instance variable `featureNames`.
                          #' @param feature
                          #' Attribute's name.
                          #' (character)
                          #' @return
                          #' Index of attribute's feature name in dataframe
                          #' (numeric)
                          #' @examples
                          #' idx <- x$featureIdx(feature = "infected")
                          featureIdx = function(feature = "character"){
                            idx <- match(feature, self$featureNames)
                            if(is.na(idx)){
                              rString <- sprintf("\nWarning in pgu.data: feature %s is not known\n",
                                                 feature)
                              cat(rString)
                            }#if
                            return(idx)
                          }, #function

                          #' @description
                          #' Returns a copy of the instance variable rawData comprising solely the numeric attributes.
                          #' @return
                          #' A data frame
                          #' (tibble::tibble)
                          #' @examples
                          #' y <- x$numericData()
                          numericData = function(){
                            self$rawData %>%
                              dplyr::select_if(is.numeric) %>%
                              return()
                          }, #function

                          #' @description
                          #' Returns a copy of the instance variable rawData comprising solely the attributes of non numeric types.
                          #' @return
                          #' A data frame
                          #' (tibble::tibble)
                          #' @examples
                          #' y <- x$nonNumericData()
                          nonNumericData = function(){
                            self$rawData %>%
                              dplyr::select_if(!is.numeric) %>%
                              return()
                          }, #function

                          ####################
                          # data information #
                          ####################
                          #' @description
                          #' Summarizes information on the instance variable rawData
                          #' and retruns it in form of a compact data frame.
                          #' @return
                          #' Information data frame
                          #' (tibble::tibble)
                          #' @examples
                          #' x$dataInformation() %>%
                          #'  print()
                          dataInformation = function(){
                            self$rawData %>%
                              dplyr::summarise_all(class) %>%
                              tidyr::gather(variable, class) %>%
                              return()
                          }, #function

                          #############################
                          # data statistics functions #
                          #############################
                          #' @description
                          #' Summarizes a vector of numerics and returns summary.
                          #' @param val
                          #' Vector of numerics to be summarized.
                          #' (numeric)
                          #' @return
                          #' Summary
                          #' @examples
                          #' x$summarizeNumeric(val = c(1,2,3,4,5))
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
                          #' Iterativly calls the class' function summarizeNumeric on all numeric attributes
                          #' of the instance variable rawData and returns the result in form of a data frame.
                          #' @return
                          #' Summary
                          #' (tibble:tibble)
                          #' @examples
                          #' x$dataStatistics() %>%
                          #'  print()
                          dataStatistics = function(){
                            self$numericData() %>%
                              apply(MARGIN=2, FUN=self$summarizeNumeric) %>%
                              t() %>%
                              as.data.frame() %>%
                              tibble::rownames_to_column("Value") %>%
                              tibble::as_tibble() %>%
                              return()
                          }, #function

                          #' @description
                          #' Calls the class' function dataStatistics
                          #' filters the result for the attributes feature names and arithmetric mean values.
                          #' and returns the result in form of a data frame.
                          #' @return
                          #' Summary
                          #' (tibble:tibble)
                          #' @examples
                          #' x$reducedDataStatistics() %>%
                          #'  print()
                          reducedDataStatistics = function(){
                            self$dataStatistics() %>%
                              dplyr::select(c("Value", "Mean")) %>%
                              return()
                          }, #function

                          #' @description
                          #' Calls the class' function dataStatistics
                          #' filters the result for the attributes feature names and information about missing values.
                          #' and returns the result in form of a data frame.
                          #' @return
                          #' Summary
                          #' (tibble:tibble)
                          #' @examples
                          #' x$misings() %>%
                          #'  print()
                          missings = function(){
                            len <- nrow(self$rawData)
                            self$dataStatistics() %>%
                              dplyr::select(c("Value", "NA's")) %>%
                              dplyr::rename(absolute = !!("NA's")) %>%
                              dplyr::mutate(fraction = absolute / len) %>%
                              return()
                          }#function
                        )#public
)#class
