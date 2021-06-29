#' @title pgu.limitsOfQuantification
#'
#' @description
#' Handles values in the pguIMP dataset that exceed the limits of quantification.
#' This object is used by the shiny based gui and is not for use in individual R-scripts!
#'
#' @details
#' more information
#'
#' @format [R6::R6Class] object.
#'
#' @importFrom dplyr bind_cols filter if_else mutate mutate_if pull
#' @importFrom dplyr right_join select slice summarise_all sym
#' @importFrom DT datatable formatStyle styleEqual
#' @importFrom ggplot2 aes aes_string coord_flip element_blank
#' @importFrom ggplot2 element_rect element_text geom_bar geom_boxplot
#' @importFrom ggplot2 geom_col geom_hline geom_jitter geom_vline
#' @importFrom ggplot2 ggplot ggtitle layer_scales
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous theme theme_linedraw
#' @importFrom ggplot2 xlab ylab
#' @importFrom gridExtra grid.arrange
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @importFrom stringr str_detect
#' @importFrom tibble add_row is_tibble tibble
#' @importFrom tidyr gather gather_
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pgu.limitsOfQuantification <- R6::R6Class('pgu.limitsOfQuantification',
                                          ####################
                                          # instance variables
                                          ####################
                                          private = list(
                                            .loq = "tbl_df",
                                            .outliers = "tbl_df",
                                            .lloqSubstituteAlphabet = "character",
                                            .lloqSubstituteAgent = "factor",
                                            .uloqSubstituteAlphabet = "character",
                                            .uloqSubstituteAgent = "factor",
                                            .naHandlingAlphabet = "character",
                                            .naHandlingAgent = "factor",
                                            .loqStatistics = "tbl_df",


                                            #' @description
                                            #' Mutates outlier candidates characterized as below LLOQ based on user defined actions.
                                            mutate_lloq_outliers = function(data_df = "tbl_df")
                                            {
                                              attributeNames <- colnames(data_df)
                                              cleanObj <- data_df
                                              if(private$attributes_are_known(attributeNames))
                                              {
                                                value = NA
                                                for (attribute in attributeNames)
                                                {
                                                  switch(
                                                    self$lloqSubstituteAgent,
                                                    "NA" = {value <- NA},
                                                    "LLOQ" = {value <- self$attribute_lloq(attribute)},
                                                    "0.5 LLOQ" = {value <- 0.5 * self$attribute_lloq(attribute)}
                                                  )#switch
                                                  idx <- self$outliers %>%
                                                    dplyr::filter(attribute == !!attribute) %>%
                                                    dplyr::filter(type == "<lloq") %>%
                                                    dplyr::pull(instance) %>%
                                                    as.integer()
                                                  if(self$lloqSubstituteAgent != "keep")
                                                  {
                                                    cleanObj <- cleanObj %>%
                                                      dplyr::mutate(!!as.symbol(attribute) := replace(x = !!as.symbol(attribute), list = idx, values = value))
                                                  } #if
                                                }#for
                                              }#if
                                              return(cleanObj)
                                            }, #function


                                            #' @description
                                            #' Mutates outlier candidates characterized as above ULOQ based on user defined actions.
                                            mutate_uloq_outliers = function(data_df = "tbl_df")
                                            {
                                              attributeNames <- colnames(data_df)
                                              cleanObj <- data_df
                                              if(private$attributes_are_known(attributeNames))
                                              {
                                                value = NA
                                                for (attribute in attributeNames)
                                                {
                                                  switch(
                                                    self$uloqSubstituteAgent,
                                                    "NA" = {value <- NA},
                                                    "ULOQ" = {value <- self$attribute_uloq(attribute)}
                                                  )#switch
                                                  idx <- self$outliers %>%
                                                    dplyr::filter(attribute == !!attribute) %>%
                                                    dplyr::filter(type == ">uloq") %>%
                                                    dplyr::pull(instance) %>%
                                                    as.integer()
                                                  if (self$uloqSubstituteAgent != "keep")
                                                  {
                                                    cleanObj <- cleanObj %>%
                                                      dplyr::mutate(!!as.symbol(attribute) := replace(x = !!as.symbol(attribute), list = idx, values = value))
                                                  }
                                                }#for
                                              }#if
                                              return(cleanObj)
                                            }, #function


                                            #' @description
                                            #' Searches for outliers in the given data frame.
                                            #' If an outlier was found, it is appended to the instance variable outliers.
                                            #' Indicates if an outlier was found.
                                            find_outliers = function(data_df = "tbl_df"){
                                              private$reset_outliers()
                                              if(tibble::is_tibble(data_df))
                                              {
                                                attributes <- colnames(data_df)
                                                if(private$attributes_are_known(attributes = attributes))
                                                {
                                                  for (attribute in attributes)
                                                  {
                                                    lloq <-  self$attribute_lloq(attribute = attribute)
                                                    uloq <- self$attribute_uloq(attribute = attribute)
                                                    attributeData <- data_df %>%
                                                      dplyr::pull(attribute) %>%
                                                      as.numeric()
                                                    private$append_outlier(attribute = attribute, values = attributeData,  lloq = lloq, uloq = uloq)
                                                  }#for
                                                  private$.outliers <-self$outliers %>%
                                                    dplyr::mutate(instance = as.integer(instance))
                                                }
                                              }
                                            }, #pguIMP::pgu.limitsOfQuantification$find_loq_outliers

                                            #' @description
                                            #' Extends the instance variable outliers by one entry.
                                            append_outlier = function(attribute = "character", values = "numeric", lloq = "numeric", uloq = "numeric")
                                            {
                                              idx_lloq <- integer()
                                              if(!is.na(lloq))
                                              {
                                                idx_lloq <- which(values < lloq)
                                              }
                                              if(length(idx_lloq) > 0)
                                              {
                                                private$.outliers <- tibble::add_row(self$outliers,
                                                                                     instance = idx_lloq,
                                                                                     attribute = rep(attribute, length(idx_lloq)),
                                                                                     value = values[idx_lloq],
                                                                                     type = rep("<lloq", length(idx_lloq)),
                                                                                     color = rep("blue", length(idx_lloq)))
                                              }#if

                                              idx_uloq <- integer()
                                              if(!is.na(uloq))
                                              {
                                                idx_uloq <- which(values > lloq)
                                              }
                                              if(length(idx_uloq) > 0)
                                              {
                                                private$.outliers <- tibble::add_row(self$outliers,
                                                                                     instance = idx_uloq,
                                                                                     attribute = rep(attribute, length(idx_uloq)),
                                                                                     value = values[idx_uloq],
                                                                                     type = rep("<uloq", length(idx_uloq)),
                                                                                     color = rep("red", length(idx_uloq)))
                                              }#if
                                            }, #function

                                            #' @description
                                            #' Tests if the provided attributes are known to the class.
                                            attributes_are_known = function(attributes = "charater"){
                                              all(unique(attributes) %in% unique(self$loq$attribute)) %>%
                                                return()
                                            }, #function

                                            #' @description
                                            #' Resets the class' instance variable outliers
                                            reset_outliers = function(){
                                              private$.outliers <- tibble::tibble(instance = integer(0),
                                                                                  attribute = character(0),
                                                                                  value = numeric(0),
                                                                                  type = character(0),
                                                                                  color = character(0)) %>%
                                                dplyr::mutate_if(is.numeric, round, 8)
                                            }, #function


                                            #' @description
                                            #' Calculates statistics of outlier appearance.
                                            #' Stores it into the instance variable loqStatistics
                                            collect_statistics = function(data_df = "tbl_df")
                                            {
                                              private$reset_loq_statistics(data_df)
                                              attributeNames <- colnames(data_df)
                                              if(private$attributes_are_known(attributes = attributeNames))
                                              {
                                                lowerOutlier <- as.integer(c(rep(0, length(attributeNames))))
                                                upperOutlier <- as.integer(c(rep(0, length(attributeNames))))
                                                for (i in seq_along(attributeNames))
                                                {
                                                  attribute <- attributeNames[i]
                                                  lloq <-  self$attribute_lloq(attribute = attribute)
                                                  uloq <- self$attribute_uloq(attribute = attribute)
                                                  lowerOutlier[i] <- data_df %>%
                                                    dplyr::mutate(below = as.numeric(!!dplyr::sym(attribute) < lloq)) %>%
                                                    dplyr::select(below) %>%
                                                    sum(na.rm = TRUE)
                                                  upperOutlier[i] <- data_df %>%
                                                    dplyr::mutate(above = as.numeric(!!dplyr::sym(attribute) > uloq)) %>%
                                                    dplyr::select(above) %>%
                                                    sum(na.rm = TRUE)
                                                }#for
                                                private$.loqStatistics <- switch(self$naHandlingAgent,
                                                                                 "keep" = {
                                                                                   private$.loqStatistics %>%
                                                                                     dplyr::mutate(belowLloq = lowerOutlier,
                                                                                                   aboveUloq = upperOutlier)
                                                                                 },
                                                                                 "<LLOQ" = {
                                                                                   private$.loqStatistics %>%
                                                                                     dplyr::mutate(belowLloq = lowerOutlier + missings,
                                                                                                   aboveUloq = upperOutlier)
                                                                                 },
                                                                                 ">ULOQ" = {
                                                                                   private$.loqStatistics %>%
                                                                                     dplyr::mutate(belowLloq = lowerOutlier,
                                                                                                   aboveUloq = upperOutlier + missings)
                                                                                 }
                                                )#switch

                                                private$.loqStatistics <- private$.loqStatistics %>%
                                                  dplyr::mutate(loqOutliers = belowLloq + aboveUloq,
                                                                fractionBelowLloq = belowLloq / instances,
                                                                fractionAboveUloq = aboveUloq / instances,
                                                                fractionLoqOutliers = loqOutliers / instances)
                                              }#if
                                            }, #function

                                            #' @description
                                            #' Resets the class' instance variable loqStatistics
                                            reset_loq_statistics = function(data_df = 'tbl_df'){
                                              if (tibble::is_tibble(data_df))
                                              {
                                                attributeNames <- colnames(data_df)
                                                if(private$attributes_are_known(attributeNames))
                                                {
                                                  private$.loqStatistics <- self$loq %>%
                                                    dplyr::mutate(instances = c(rep(nrow(data_df), length(attributeNames))),
                                                                  belowLloq = as.integer(c(rep(0, length(attributeNames)))),
                                                                  aboveUloq = as.integer(c(rep(0, length(attributeNames)))),
                                                                  loqOutliers = as.integer(c(rep(0, length(attributeNames)))),
                                                                  fractionBelowLloq = as.numeric(c(rep(0, length(attributeNames)))),
                                                                  fractionAboveUloq = as.numeric(c(rep(0, length(attributeNames)))),
                                                                  fractionLoqOutliers = as.numeric(c(rep(0, length(attributeNames))))
                                                    )
                                                }else{
                                                  private$.loqStatistics <- tibble::tibble(attribute =character(0),
                                                                                           instances = integer(0),
                                                                                           belowLloq = integer(0),
                                                                                           abloveUloq = integer(0),
                                                                                           loqOutliers = integer(0),
                                                                                           fractionBelowLloq = numeric(0),
                                                                                           fractionAboveUloq = numeric(0),
                                                                                           fractionLoqOutliers = numeric(0))
                                                }
                                              }else{
                                                private$.loqStatistics <- tibble::tibble(attribute =character(0),
                                                                                         instances = integer(0),
                                                                                         belowLloq = integer(0),
                                                                                         abloveUloq = integer(0),
                                                                                         loqOutliers = integer(0),
                                                                                         fractionBelowLloq = numeric(0),
                                                                                         fractionAboveUloq = numeric(0),
                                                                                         fractionLoqOutliers = numeric(0))

                                              }#if
                                            }, #function

                                            #' @description
                                            #' Resets the class by a data frame comprising information about LOQs.
                                            reset_by_data = function(data_df = "tbl_df")
                                            {
                                              if(tibble::is_tibble(data_df))
                                              {
                                                tryCatch(
                                                  {
                                                    private$.loq <- data_df %>%
                                                      dplyr::select(c("attribute", "LLOQ", "ULOQ")) %>%
                                                      dplyr::right_join(self$loq, by=c("attribute")) %>%
                                                      dplyr::mutate(LLOQ = LLOQ.x) %>%
                                                      dplyr::mutate(ULOQ = ULOQ.x) %>%
                                                      dplyr::select(c("attribute", "LLOQ", "ULOQ"))
                                                  },
                                                  error = function(e) {
                                                    print("Error in pguIMP::pgu.limitsOfQuantification")
                                                    print(e)
                                                    }#error
                                                )#tryCatch
                                              }#if
                                            },#pguIMP.pgu.limitsOfQuantification$reset_by_data

                                            #' @description
                                            #' Resets the class by a vector of attribute names.
                                            #' The Attributes' LOQs are initially assigned to na.
                                            reset_by_attribute_names = function(attribute_names = "character")
                                            {
                                              if(!is.character(attribute_names))
                                              {
                                                attribute_names <- c("Sample Name")
                                              }#if
                                              private$.loq <- tibble::tibble(attribute = attribute_names,
                                                                             LLOQ = as.numeric(rep(NA, length(attribute_names))),
                                                                             ULOQ = as.numeric(rep(NA, length(attribute_names))))
                                            },

                                            #' @description
                                            #' Clears the heap and
                                            #' indicates that instance of `pgu.limitaOfQuantification` is removed from heap.
                                            finalize = function()
                                              {
                                              print("Instance of pgu.limitsOfQuantification removed from heap")
                                            } #pguIMP::pgu.limitsOfQuantification$finalize
                                          ),
                                          ##################
                                          # accessor methods
                                          ##################
                                          active = list(
                                            #' @field loq
                                            #' Returns the instance variable loq
                                            #' (tibble::tibble)
                                            loq = function()
                                            {
                                              return(private$.loq)
                                            },
                                            #' @field setLoq
                                            #' Sets the instance variable loq.
                                            #' (tibble::tibble)
                                            setLoq = function(obj = 'tbl_df')
                                            {
                                              private$.loq <- obj
                                            },
                                            #' @field outliers
                                            #' Returns instance variable outliers
                                            #' (tibble::tibble)
                                            outliers = function()
                                            {
                                              return(private$.outliers)
                                            },
                                            #' @field lloqSubstituteAlphabet
                                            #' Returns the instance variable lloqSubstititeAlphabet
                                            #' (character)
                                            lloqSubstituteAlphabet = function()
                                            {
                                              return(private$.lloqSubstituteAlphabet)
                                            },
                                            #' @field lloqSubstituteAgent
                                            #' Returns the instance variable lloqSubstituteAgent
                                            #' (character)
                                            lloqSubstituteAgent = function()
                                            {
                                              return(as.character(private$.lloqSubstituteAgent))
                                            },
                                            #' @field setLloqSubstituteAgent
                                            #' Sets the instance variable lloqSubstituteAgent.
                                            #' (character)
                                            setLloqSubstituteAgent = function(agent = "character")
                                            {
                                              private$.lloqSubstituteAgent <- factor(agent, levels = self$lloqSubstituteAlphabet)
                                            },
                                            #' @field uloqSubstituteAlphabet
                                            #' Returns the instance variable uloqSubstititeAlphabet
                                            #' (character)
                                            uloqSubstituteAlphabet = function()
                                            {
                                              return(private$.uloqSubstituteAlphabet)
                                            },
                                            #' @field uloqSubstituteAgent
                                            #' Returns the instance variable uloqSubstituteAgent
                                            #' (character)
                                            uloqSubstituteAgent = function()
                                            {
                                              return(as.character(private$.uloqSubstituteAgent))
                                            },
                                            #' @field setUloqSubstituteAgent
                                            #' Sets the instance variable uloqSubstituteAgent.
                                            #' (character)
                                            setUloqSubstituteAgent = function(agent = "character")
                                            {
                                              private$.uloqSubstituteAgent <- factor(agent, levels = self$uloqSubstituteAlphabet)
                                            },
                                            #' @field naHandlingAlphabet
                                            #' Returns the instance variable naHandlingAlphabet
                                            #' (character)
                                            naHandlingAlphabet = function()
                                            {
                                              return(private$.naHandlingAlphabet)
                                            },
                                            #' @field naHandlingAgent
                                            #' Returns the instance variable naHandlingAgentt
                                            #' (character)
                                            naHandlingAgent = function()
                                            {
                                              return(as.character(private$.naHandlingAgent))
                                            },
                                            #' @field setNaHandlingAgent
                                            #' Sets the instance variable naHandlingAgentt
                                            #' (character)
                                            setNaHandlingAgent = function(agent = "character")
                                            {
                                              private$.naHandlingAgent <- factor(agent, levels = self$naHandlingAlphabet)
                                            },
                                            #' @field loqStatistics
                                            #' Returns the instance variable loqStatistics
                                            loqStatistics = function()
                                            {
                                              return(private$.loqStatistics)
                                            }
                                          ),

                                          ####################
                                          # public functions #
                                          ####################
                                          public = list(
                                            #' @description
                                            #' Creates and returns a new `pgu.limitsOfQuantification` object.
                                            #' @param attribute_names
                                            #' Vector of attribute names with to be analyzed by the loq object.
                                            #' (character)
                                            #' @return
                                            #' A new `pgu.limitsOfQuantification` object.
                                            #' (pguIMP::pgu.limitsOfQuantification)
                                            initialize = function(attribute_names = 'character')
                                              {
                                              private$.lloqSubstituteAlphabet <- c("keep","NA", "LLOQ", "0.5 LLOQ")
                                              self$setLloqSubstituteAgent <- self$lloqSubstituteAlphabet[1]
                                              private$.uloqSubstituteAlphabet <- c("keep", "NA", "ULOQ")
                                              self$setUloqSubstituteAgent <- self$uloqSubstituteAlphabet[1]
                                              private$.naHandlingAlphabet <- c("keep", "<LLOQ", ">ULOQ")
                                              self$setNaHandlingAgent <- self$naHandlingAlphabet[1]
                                              if (!is.character(attribute_names)){
                                                attribute_names <- c("Sample Name")
                                              }
                                              self$reset(attribute_names)
                                            }, #pguIMP::pgu.limitsOfQuantification$initialize

                                            #' @description
                                            #' Prints instance variables of a `pgu.limitsOfQuantification` object.
                                            #' @return
                                            #' string
                                            print = function() {
                                              rString <- sprintf("\npgu.limitsOfQuantification\n")
                                              cat(rString)
                                              ustring <- sprintf("\nlloqSubstituteAgent: %s\nuloqSubstituteAgent: %s\nnaHandlingAgent: %s\n", self$lloqSubstituteAgent, self$uloqSubstituteAgent, self$naHandlingAgent)
                                              cat(ustring)
                                              cat("\n\n")
                                              print(self$loq)
                                              invisible(self)
                                            }, #function

                                            #' @description
                                            #' Resets the pguIMP::pgu.limitsOfQuantification object
                                            #' on the given parameters attribute_names and data_df
                                            #' @param attribute_names
                                            #' Vector of attribute names with to be analyzed by the loq object.
                                            #' (character)
                                            #' @param data_df
                                            #' Dataframe comprising loq information.
                                            #' Feature names need to be 'attribute', 'LLOQ' and 'ULOQ'.
                                            #' (tibble::tibble)
                                            reset = function(attribute_names = "character", data_df = "tbl_df")
                                            {
                                              private$reset_by_attribute_names(attribute_names = attribute_names)
                                              if(tibble::is_tibble(data_df))
                                              {
                                                private$reset_by_data(data_df = data_df)
                                              }
                                              private$reset_outliers()
                                              private$reset_loq_statistics(data_df = data_df)
                                            }, #pguIMP::pgu.limitsOfQuantification$reset


                                            #' @description
                                            #' Analyses the data dets for instances outside of the LOQ defined value interval.
                                            #' @param data_df
                                            #' Dataframe to be analyzed
                                            fit = function(data_df = "tbl_df")
                                            {
                                              # private$reset_outliers()
                                              # private$reset_loq_statistics(data_df = data_df)
                                              private$find_outliers(data_df = data_df)
                                              private$collect_statistics(data_df = data_df)
                                            },

                                            #' @description
                                            #' Mutates all outlier candidates based on user defined actions.
                                            #' @param data_df
                                            #' The data to be analyzed.
                                            #' (tibble::tibble)
                                            #' @return
                                            #' The revised data frame
                                            #' (tibble::tibble)
                                            predict = function(data_df = "tbl_df")
                                            {
                                              if(tibble::is_tibble(data_df))
                                              {
                                                data_df %>%
                                                  private$mutate_lloq_outliers() %>%
                                                  private$mutate_uloq_outliers() %>%
                                                  return()
                                              }else{
                                                warning("Error in pguIMP::pgu.lilimOfWuantification$predict. Variable data_df needs to be of type tibble::tibble")
                                                return(data_df)
                                              }#else
                                            }, #function

                                            ####################
                                            # helper functions #
                                            ####################
                                            #' @description
                                            #' Returns the attribute's specific lloq.
                                            #' @param attribute
                                            #' The attribute to be analyzed
                                            #' (character)
                                            #' @return
                                            #' The attribute's lloq
                                            #' (numeric)
                                            attribute_lloq = function(attribute = "character"){
                                              limit <- NA
                                              if(private$attributes_are_known(attributes = attribute)){
                                                limit <-  self$loq %>%
                                                  dplyr::filter(attribute == !!attribute) %>%
                                                  dplyr::pull(LLOQ) %>%
                                                  as.numeric()
                                              }# if
                                              return(limit)
                                            }, #function

                                            #' @description
                                            #' Returns the attribute's specific uloq.
                                            #' @param attribute
                                            #' The attribute to be analyzed
                                            #' (character)
                                            #' @return
                                            #' The attribute's uloq
                                            #' (numeric)
                                            attribute_uloq = function(attribute = "character"){
                                              limit <- NA
                                              if(private$attributes_are_known(attributes = attribute)){
                                                limit <-  self$loq %>%
                                                  dplyr::filter(attribute == !!attribute) %>%
                                                  dplyr::pull(ULOQ) %>%
                                                  as.numeric()
                                              }# if
                                              return(limit)
                                            }, #function

                                            #' @description
                                            #' sets the attribute's specific lloq to value.
                                            #' @param attribute
                                            #' The attribute to be updated
                                            #' (character)
                                            #' @param value
                                            #' The value parsed to the attributes lloq
                                            #' (numeric)
                                            set_attribute_lloq = function(attribute = "character", value = NA){
                                              limit <- NA
                                              if(is.numeric(value))
                                              {
                                                limit <- value
                                              }
                                              if(private$attributes_are_known(attributes = attribute)){
                                                attributeIdx <- stringr::str_detect(self$loq$attribute, attribute)
                                                private$.loq$LLOQ[attributeIdx] <- as.numeric(limit)
                                              }# if
                                            }, #function

                                            #' @description
                                            #' sets the attribute's specific uloq to value.
                                            #' @param attribute
                                            #' The attribute to be updated
                                            #' (character)
                                            #' @param value
                                            #' The value parsed to the attributes lloq
                                            #' (numeric)
                                            set_attribute_uloq = function(attribute = "character", value = NA){
                                              limit <- NA
                                              if(is.numeric(value))
                                              {
                                                limit <- value
                                              }
                                              if(private$attributes_are_known(attributes = attribute)){
                                                attributeIdx <- stringr::str_detect(self$loq$attribute, attribute)
                                                private$.loq$ULOQ[attributeIdx] <- as.numeric(limit)
                                              }# if
                                            }, #function

                                            #' @description
                                            #' Returns the detected outliers of a given attribute.
                                            #' @param attribute
                                            #' The attribute to be analyzed
                                            #' (character)
                                            #' @return
                                            #' The attribute's outliers
                                            #' (tibble::tibble)
                                            attribute_outliers = function(attribute = "character")
                                              {
                                              t <- NULL
                                              if(private$attributes_are_known(attributes = attribute))
                                                {
                                                t <- self$outliers %>%
                                                  dplyr::filter(attribute == !!attribute)
                                              }# if
                                              return(t)
                                            }, #function
#'
#'                                             ####################
#'                                             # data information #
#'                                             ####################
#'                                             #' @description
#'                                             #' Gathers and returns class information
#'                                             dataInformation = function(){
#'                                               self$loq %>%
#'                                                 dplyr::summarise_all(class) %>%
#'                                                 tidyr::gather(variable, class) %>%
#'                                                 return()
#'                                             }, #function
#'
#'                                             ####################
#'                                             # output functions #
#'                                             ####################
#'                                             #' @description
#'                                             #' Merges dfData and dfMetadata and returns a fromatted data table.
#'                                             #' @param dfData
#'                                             #' The data to be analyzed.
#'                                             #' (tibble::tibble)
#'                                             #' @param dfMetadata
#'                                             #' The data frame containing metadata.
#'                                             #' (tibble::tibble)
#'                                             #' @return
#'                                             #' A formatted data table
#'                                             #' (DT::datatable)
#'                                             loqDataTable = function(dfData = "tbl_df", dfMetadata = "tbl_df"){
#'                                               options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
#'                                               t <- NULL
#'                                               featureNames <- colnames(dfData)
#'                                               tryCatch(
#'                                                 dfMerge <- dplyr::bind_cols(dfMetadata, dfData),
#'                                                 error = function(e){
#'                                                   print("error")
#'                                                   print(e)
#'                                                   dfMerge <- dfData
#'                                                 }#error
#'                                               )#tryCatch
#'                                               if(self$checkValidity(featureNames)){
#'                                                 t <- dfMerge %>%
#'                                                   dplyr::mutate_if(is.numeric, round, 3) %>%
#'                                                   DT::datatable(options = list(scrollX = TRUE,
#'                                                                                scrollY = '350px',
#'                                                                                paging = FALSE))
#'                                                 for (featureName in featureNames){
#'                                                   featureOutlier <- self$outliers %>%
#'                                                     dplyr::filter(feature == featureName) %>%
#'                                                     dplyr::mutate_if(is.numeric, round, 3)
#'                                                   if (nrow(featureOutlier) > 0){
#'                                                     t <- DT::formatStyle(t,
#'                                                                          featureName,
#'                                                                          backgroundColor = DT::styleEqual(dfMerge %>%
#'                                                                                                             dplyr::select(!!featureName) %>%
#'                                                                                                             dplyr::slice(featureOutlier[["measurement"]]) %>%
#'                                                                                                             unlist() %>%
#'                                                                                                             round(digits = 3),
#'                                                                                                           featureOutlier[["color"]]))
#'                                                   }#if
#'                                                 }#for
#'                                               }#if
#'                                               return(t)
#'                                             }, #function
#'
#'                                             #' @description
#'                                             #' Returns a formatted data table with comrising the information of a user defined attribute's outliers.
#'                                             #' @param obj
#'                                             #' The data to be analyzed.
#'                                             #' (tibble::tibble)
#'                                             #' @param feature
#'                                             #' The attribute to be analyzed
#'                                             #' (character)
#'                                             #' @return
#'                                             #' A formatted data table
#'                                             #' (DT::datatable)
#'                                             loqFeatureTable = function(obj = "tbl_df", feature = "character"){
#'                                               options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
#'                                               t <- NULL
#'                                               if(self$checkValidity(feature)){
#'                                                 featureOutlier <- self$outliers %>%
#'                                                   dplyr::filter(feature == !!feature) %>%
#'                                                   dplyr::mutate_if(is.numeric, round, 3)
#'
#'                                                 dfFeature <- obj %>%
#'                                                   dplyr::mutate_if(is.numeric, round, 3)
#'
#'                                                 print(dfFeature)
#'
#'                                                 t <- dfFeature %>%
#'                                                   DT::datatable(options = list(scrollX = TRUE,
#'                                                                                scrollY = '350px',
#'                                                                                paging = FALSE))
#'                                                 if (nrow(featureOutlier) > 0){
#'                                                   t <- DT::formatStyle(
#'                                                     t,
#'                                                     feature,
#'                                                     backgroundColor = DT::styleEqual(dfFeature %>%
#'                                                                                        dplyr::select(!!feature) %>%
#'                                                                                        dplyr::slice(featureOutlier[["measurement"]]) %>%
#'                                                                                        unlist() %>%
#'                                                                                        round(digits = 3),
#'                                                                                      featureOutlier[["color"]]))
#'                                                 }#if
#'                                               }#if
#'                                               return(t)
#'                                             }, #function
#'
                                            #' @description
                                            #' Creates a plot of the instance variable loqStatistics.
                                            #' @return
                                            #' A plot.
                                            #' (ggplot2::ggplot)
                                            plot_loq_distribution = function()
                                              {
                                              p <- self$loqStatistics %>%
                                                tidyr::gather('belowLloq', 'aboveUloq', key = "type", value="typeCount") %>%
                                                dplyr::mutate(fraction = 100 * typeCount/instances) %>%
                                                ggplot2::ggplot(mapping = ggplot2::aes_string(x = "attribute", y = "fraction", fill = "type"), na.rm=TRUE)+
                                                ggplot2::geom_col()+
                                                ggplot2::ggtitle("LOQ Distribution") +
                                                ggplot2::theme_linedraw() +
                                                ggplot2::theme(
                                                  panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                                                  plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                                                  legend.background = ggplot2::element_rect(fill = "transparent"),
                                                  legend.key = ggplot2::element_rect(fill = "transparent"),
                                                  axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
                                                )
                                              return(p)
                                            }, #function

                                            #' @description
                                            #' Creates a bar plot of a user defined attribute's value distribution.
                                            #' LOQs are indicated as dotted lines
                                            #' @param data_df
                                            #' The data to be analyzed.
                                            #' (tibble::tibble)
                                            #' @param attribute
                                            #' The attribute to be analyzed
                                            #' (character)
                                            #' @return
                                            #' A bar plot.
                                            #' (ggplot2::ggplot)
                                            attribute_bar_plot = function(data_df = "tbl_df", attribute = "character"){
                                              p <- NULL
                                              if(private$attributes_are_known(attributes = attribute)){
                                                lloq <-  self$attribute_lloq(attribute)
                                                uloq <- self$attribute_uloq(attribute)
                                                p <- data_df %>%
                                                  ggplot2::ggplot(mapping = ggplot2::aes_string(x = as.name(attribute)), na.rm=TRUE) +
                                                  ggplot2::geom_bar(stat = "bin", bins = 30, na.rm = TRUE) +
                                                  ggplot2::geom_vline(xintercept=lloq, linetype="dashed", na.rm=TRUE) +
                                                  ggplot2::geom_vline(xintercept=uloq, linetype="dashed", na.rm=TRUE) +
                                                  ggplot2::xlab("value") +
                                                  ggplot2::ylab("counts") +
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
                                            #' Creates a box plot of a user defined attribute's value distribution.
                                            #' LOQs are indicated as dotted lines
                                            #' @param data_df
                                            #' The data to be analyzed.
                                            #' (tibble::tibble)
                                            #' @param attribute
                                            #' The attribute to be analyzed
                                            #' (character)
                                            #' @return
                                            #' A box plot.
                                            #' (ggplot2::ggplot)
                                            attribute_box_plot_with_subset = function(data_df = "tbl_df", attribute = "character"){
                                              p <- NULL
                                              if(private$attributes_are_known(attributes = attribute)){
                                                lloq <-  self$attribute_lloq(attribute)
                                                uloq <- self$attribute_uloq(attribute)
                                                p <- data_df %>%
                                                  dplyr::select(attribute) %>%
                                                  dplyr::mutate(LOQ = dplyr::if_else(condition = !!dplyr::sym(attribute) < lloq, true = "< LLOQ", dplyr::if_else(condition = !!dplyr::sym(attribute) > uloq, true = "> ULOQ", false = "quantitative", missing = "quantitative"), missing = "quantitative")) %>%
                                                  tidyr::gather_(key="attribute", value="instance", attribute) %>%
                                                  ggplot2::ggplot(mapping=ggplot2::aes_string(x="attribute",y="instance"), na.rm=TRUE)+
                                                  ggplot2::geom_boxplot(na.rm=TRUE, outlier.shape = NA)+
                                                  ggplot2::geom_jitter(ggplot2::aes(colour=LOQ), na.rm=TRUE) +
                                                  ggplot2::geom_hline(yintercept=lloq, linetype="dashed", na.rm=TRUE) +
                                                  ggplot2::geom_hline(yintercept=uloq, linetype="dashed", na.rm = TRUE) +
                                                  ggplot2::xlab("attribute") +
                                                  ggplot2::ylab("value") +
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
                                            #' Creates and returns a composite graphical analysis
                                            #' of the outlier analysis of a user defined attribute.
                                            #' @param data_df
                                            #' The data to be analyzed.
                                            #' (tibble::tibble)
                                            #' @param attribute
                                            #' Attribute's name.
                                            #' (character)
                                            #' @return
                                            #' Composite result plot.
                                            #' (gridExtra::grid.arrange)
                                            attribute_plot = function(data_df = "tbl_df", attribute = "character"){
                                              p1 <- self$attribute_box_plot_with_subset(data_df, attribute) +
                                                ggplot2::theme(legend.position = c(0.9, 0.9),
                                                               legend.key = ggplot2::element_blank(),
                                                               legend.background = ggplot2::element_blank())

                                              limits1 <- ggplot2::layer_scales(p1)$y$range$range

                                              p2 <- self$attribute_bar_plot(data_df, attribute)
                                              limits2 <- ggplot2::layer_scales(p2)$x$range$range

                                              limits <- c(min(c(limits1[1], limits2[1])),
                                                          max(c(limits1[2], limits2[2]))
                                              )

                                              p1 <- p1 +
                                                ggplot2::scale_y_continuous(limits=limits)

                                              p2 <- p2 +
                                                ggplot2::scale_x_continuous(position = "top", limits=limits) +
                                                ggplot2::coord_flip()

                                              p <- gridExtra::grid.arrange(p1,p2, layout_matrix = rbind(c(1,2),c(1,2)),
                                                                           top = textGrob(label = sprintf("Distribution of %s", attribute)))
                                              return(p)
                                            } #function
                                          )#public
)#class
