#' @title pgu.limitsOfQuantification
#'
#' @description
#' Handles values in the pguIMP dataset that exceed the limits of quantification.
#'
#' @details
#' more information
#'
#' @format [R6::R6Class] object.
#' @section Construction:
#' x <- pguIMP::pgu.limitsOfQuantification$new(data)
#'
#' @import R6
#' @import tidyverse
#' @import pracma
#' @import DT
#' @import gridExtra
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pgu.limitsOfQuantification <- R6::R6Class(
  'pgu.limitsOfQuantification',
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
    .loqStatistics = "tbl_df"
  ),
  ##################
  # accessor methods
  ##################
  active = list(
    #' @field loq
    #' Returns the instance variable loq
    #' (tibble::tibble)
    loq = function(){
      return(private$.loq)
    },
    #' @field setLoq
    #' Sets the instance variable loq.
    #' (tibble::tibble)
    setLoq = function(obj = 'tbl_df'){
      private$.loq <- obj
    },
    #' @field outliers
    #' Returns instance variable outliers
    #' (tibble::tibble)
    outliers = function(){
      return(private$.outliers)
    },
    #' @field lloqSubstituteAlphabet
    #' Returns the instance variable lloqSubstititeAlphabet
    #' (character)
    lloqSubstituteAlphabet = function(){
      return(private$.lloqSubstituteAlphabet)
    },
    #' @field lloqSubstituteAgent
    #' Returns the instance variable lloqSubstituteAgent
    #' (character)
    lloqSubstituteAgent = function(){
      return(as.character(private$.lloqSubstituteAgent))
    },
    #' @field setLloqSubstituteAgent
    #' Sets the instance variable lloqSubstituteAgent.
    #' (character)
    setLloqSubstituteAgent = function(agent = "character"){
      private$.lloqSubstituteAgent <- factor(agent, levels = self$lloqSubstituteAlphabet)
    },
    #' @field uloqSubstituteAlphabet
    #' Returns the instance variable uloqSubstititeAlphabet
    #' (character)
    uloqSubstituteAlphabet = function(){
      return(private$.uloqSubstituteAlphabet)
    },
    #' @field uloqSubstituteAgent
    #' Returns the instance variable uloqSubstituteAgent
    #' (character)
    uloqSubstituteAgent = function(){
      return(as.character(private$.uloqSubstituteAgent))
    },
    #' @field setUloqSubstituteAgent
    #' Sets the instance variable uloqSubstituteAgent.
    #' (character)
    setUloqSubstituteAgent = function(agent = "character"){
      private$.uloqSubstituteAgent <- factor(agent, levels = self$uloqSubstituteAlphabet)
    },
    #' @field naHandlingAlphabet
    #' Returns the instance variable naHandlingAlphabet
    #' (character)
    naHandlingAlphabet = function(){
      return(private$.naHandlingAlphabet)
    },
    #' @field naHandlingAgent
    #' Returns the instance variable naHandlingAgentt
    #' (character)
    naHandlingAgent = function(){
      return(as.character(private$.naHandlingAgent))
    },
    #' @field setNaHandlingAgent
    #' Sets the instance variable naHandlingAgentt
    #' (character)
    setNaHandlingAgent = function(agent = "character"){
      private$.naHandlingAgent <- factor(agent, levels = self$naHandlingAlphabet)
    },
    #' @field loqStatistics
    #' Returns the instance variable loqStatistics
    loqStatistics = function(){
      return(private$.loqStatistics)
    }
  ),
  ###################
  # memory management
  ###################
  public = list(
    #' @description
    #' Creates and returns a new `pgu.limitsOfQuantification` object.
    #' @param obj
    #' The data to be analyzed.
    #' (tibble::tibble)
    #' @return
    #' A new `pgu.limitsOfQuantification` object.
    #' (pguIMP::pgu.optimizer)
    #' @examples
    #' y <- tibble:tibble()
    #' x <- pguIMP:pgu.limitsOfQuantification$new(data = y)
    initialize = function(obj = 'tbl_df'){
      print("Created  instance of pgu.limitsOfQuantification in heap")
      private$.lloqSubstituteAlphabet <- c("keep","NA", "LLOQ", "0.5 LLOQ")
      self$setLloqSubstituteAgent <- self$lloqSubstituteAlphabet[1]
      private$.uloqSubstituteAlphabet <- c("keep", "NA", "ULOQ")
      self$setUloqSubstituteAgent <- self$uloqSubstituteAlphabet[1]
      private$.naHandlingAlphabet <- c("keep", "<LLOQ", ">ULOQ")
      self$setNaHandlingAgent <- self$naHandlingAlphabet[1]
      if(class(obj)[1] != "tbl_df"){
        obj <- tibble::tibble(names <- "none",
                               values <- c(NA))
      }#if
      self$setLoq <- obj
      self$resetLoq()
    }, #function

    #' @description
    #' Clears the heap and
    #' indicates that instance of `pgu.limitaOfQuantification` is removed from heap.
    finalize = function(){
      print("Instance of pgu.limitsOfQuantification removed from heap")
    }, #function

    ##########################
    # print instance variables
    ##########################
    #' @description
    #' Prints instance variables of a `pgu.limitsOfQuantification` object.
    #' @return
    #' string
    #' @examples
    #' x$print()
    #' print(x)
    print = function() {
      rString <- sprintf("\npgu.limitsOfQuantification\n")
      cat(rString)
      ustring <- sprintf("\nlloqSubstituteAgent: %s\nuloqSubstituteAgent: %s\nnaHandlingAgent: %s\n", self$lloqSubstituteAgent, self$uloqSubstituteAgent, self$naHandlingAgent)
      cat(ustring)
      cat("\n\n")
      print(self$loq)
      invisible(self)
    }, #function

    ####################
    # public functions
    ####################
    #' @description
    #' Tests if the provided attributes are known to the class.
    #' @param features
    #' A vector of attributes to be testes
    #' (character)
    #' @return
    #' Test result
    #' (logical)
    #' @examples
    #' y <- x$checkValidity(features)
    checkValidity = function(features = "charater"){
      validity <- FALSE
      tryCatch({
        self$loq %>%
          dplyr::select(features)
        validity <- TRUE
      },
      error = function(e) {
        print("error")
        print(e)
      }, #error
      warning = function(w) {
        print("warning")
        print(e)
      } #warning
      )#tryCatch
      return(validity)
    }, #function

    #' @description
    #' Resets the class' instance variable loqStatistics
    #' @param obj
    #' The data to be analyzed.
    #' (tibble::tibble)
    #' @examples
    #' x$resetLoqStatistics(obj)
    resetLoqStatistics = function(obj = 'tbl_df'){
      featureNames <- colnames(obj)
      if(self$checkValidity(featureNames)){
        private$.loqStatistics <- self$loq %>%
          dplyr::select(featureNames) %>%
          t() %>%
          as.data.frame() %>%
          tibble::rownames_to_column() %>%
          tibble::as_tibble() %>%
          purrr::set_names(c("features", dplyr::pull(self$loq, c(1)))) %>%
          dplyr::mutate(quality = as.integer(quality),
                        # quantitative = as.character(quantitative),
                        measurements = c(rep(nrow(obj), length(featureNames))),
                        missings = as.integer(dplyr::summarise_all(obj, list(~sum(is.na(.))))),
                        belowLloq = as.integer(c(rep(0, length(featureNames)))),
                        aboveUloq = as.integer(c(rep(0, length(featureNames)))),
                        loqOutliers = as.integer(c(rep(0, length(featureNames)))),
                        fractionBelowLloq = as.integer(c(rep(0, length(featureNames)))),
                        fractionAboveUloq = as.integer(c(rep(0, length(featureNames)))),
                        fractionLoqOutliers = as.integer(c(rep(0, length(featureNames))))
          ) %>%
          dplyr::select(features, dplyr::everything())
      }#if
    }, #function

    #' @description
    #' Resets the class' instance variable outliers
    #' @examples
    #' x$resetLoq()
    resetLoq = function(){
      private$.outliers <- tibble::tibble(measurement = numeric(0),
                                          feature = character(0),
                                          value = numeric(0),
                                          type = character(0),
                                          color = character(0)) %>%
        dplyr::mutate_if(is.numeric, round, 8)
    }, #function

    ####################
    # helper functions #
    ####################
    #' @description
    #' Returns the attribute's specific lloq.
    #' @param feature
    #' The attribute to be analyzed
    #' (character)
    #' @return
    #' The attribute's lloq
    #' (numeric)
    #' @examples
    #' lloq <- x$featureLloq(feature = "infected")
    featureLloq = function(feature = "character"){
      limit <- NA
      if(self$checkValidity(features = feature)){
        limit <-  self$loq %>%
          dplyr::filter(LOQ == 'LLOQ') %>%
          dplyr::select(feature) %>%
          as.numeric()
      }# if
      return(limit)
    }, #function

    #' @description
    #' Returns the attribute's specific uloq.
    #' @param feature
    #' The attribute to be analyzed
    #' (character)
    #' @return
    #' The attribute's uloq
    #' (numeric)
    #' @examples
    #' uloq <- x$featureUloq(feature = "infected")
    featureUloq = function(feature = "character"){
      limit <- NA
      if(self$checkValidity(features = feature)){
        limit <-  self$loq %>%
          dplyr::filter(LOQ == 'ULOQ') %>%
          dplyr::select(feature) %>%
          as.numeric()
      }# if
      return(limit)
    }, #function

    #' @description
    #' Extends the instance variable outliers by one entry.
    #' @param feature
    #' The attribute the value is assigned to.
    #' (character)
    #' @param value
    #' The value to append
    #' (numeric)
    #' @param idx
    #' The index of the measruement the value is originated from.
    #' (numeric)
    #' @param lloq
    #' The attributes lloq
    #' (numeric)
    #' @param uloq
    #' The attributes uloq
    #' (numeric)
    #' @examples
    #' x$appendOutlier(feature = "infected", value = 501, idx = 200, lloq=0, uloq=100)
    appendOutlier = function(feature = "character", value = "numeric", idx = "numeric", lloq = "numeric", uloq = "numeric"){
      isOutlier <- FALSE
      if(is.na(value)){
        switch(self$naHandlingAgent,
               "keep" = {isOutlier <- FALSE},
               "<LLOQ" = {
                 isOutlier <- TRUE
                 outlierType <- "LLOQ"
                 outlierColor <- "blue"
               },
               ">ULOQ" = {
                 isOutlier <- TRUE
                 outlierType <- "ULOQ"
                 outlierColor <- "firebrick"
               }
        )#switc
      }#if
      else if (!is.na(lloq)){
        if(value < lloq){
          isOutlier <- TRUE
          outlierType <- "LLOQ"
          outlierColor <- "blue"
        } #if
      }#else if
      else if (!is.na(uloq)){
        if(value > uloq){
          isOutlier <- TRUE
          outlierType <- "ULOQ"
          outlierColor <- "firebrick"
        }
      }#else if
      if(isOutlier){
        private$.outliers <- tibble::add_row(self$outliers,
                                             measurement = idx,
                                             feature = feature,
                                             value = value,
                                             type = as.character(outlierType),
                                             color = as.character(outlierColor))
      }#if
    }, #function

    #' @description
    #' Searches for outliers in the given data frame.
    #' If an outlier was found, it is appended to the instance variable outliers.
    #' Indicates if an outlier was found.
    #' @param obj
    #' The data to be analyzed.
    #' (tibble::tibble)
    #' @return
    #' Test result
    #' (logical)
    #' @examples
    #' y <- x$findOutliers(obj)
    findOutliers = function(obj = "tbl_df"){
      self$resetLoq()
      features <- colnames(obj)
      if(self$checkValidity(features)){
        for (feature in features){
          lloq <-  self$featureLloq(feature)
          uloq <- self$featureUloq(feature)
          featureData <- obj %>%
            dplyr::pull(feature) %>%
            as.numeric()
          for (i in seq_along(featureData)){
            self$appendOutlier(feature = feature, value = featureData[i], idx = i, lloq = lloq, uloq = uloq)
          }#for
        }#for
        return(TRUE)
      }#if
      else{
        return(FALSE)
      }#else
    }, #function

    #' @description
    #' Calculates statistics of outlier appearance.
    #' Stores it into the instance variable loqStatistics
    #' @param obj
    #' The data to be analyzed.
    #' (tibble::tibble)
    #' @examples
    #' x$collectStatistics(obj)
    collectStatistics = function(obj = "tbl_df"){
      self$resetLoqStatistics(obj)
      featureNames <- colnames(obj)
      if(self$checkValidity(featureNames)){
        lowerOutlier <- as.integer(c(rep(0, length(featureNames))))
        upperOutlier <- as.integer(c(rep(0, length(featureNames))))
        for (i in seq_along(colnames(obj))){
          feature <- featureNames[i]
          lloq <-  self$featureLloq(feature)
          uloq <- self$featureUloq(feature)
          lowerOutlier[i] <- obj %>%
            dplyr::mutate(below = as.numeric(!!dplyr::sym(feature) < lloq)) %>%
            dplyr::select(below) %>%
            sum(na.rm = TRUE)
          upperOutlier[i] <- obj %>%
            dplyr::mutate(above = as.numeric(!!dplyr::sym(feature) > uloq)) %>%
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
                        fractionBelowLloq = belowLloq / measurements,
                        fractionAboveUloq = aboveUloq / measurements,
                        fractionLoqOutliers = loqOutliers / measurements)
      }#if
    }, #function

    #' @description
    #' Mutates all outlier candidates based on user defined actions.
    #' @param obj
    #' The data to be analyzed.
    #' (tibble::tibble)
    #' @return
    #' Te revised data frame
    #' (tibble::tibble)
    #' @examples
    #' y <- x$mutateLoqOutliers(obj)
    mutateLoqOutliers = function(obj = "tbl_df"){
      if(self$checkValidity(colnames(obj))){
        obj %>%
          self$mutateLloqOutliers() %>%
          self$mutateUloqOutliers() %>%
          return()
      }#if
      else{
        print("error")
        return(obj)
      }#else
    }, #function

    #' @description
    #' Mutates outlier candidates characterized as below LLOQ based on user defined actions.
    #' @param obj
    #' The data to be analyzed.
    #' (tibble::tibble)
    #' @return
    #' Te revised data frame
    #' (tibble::tibble)
    #' @examples
    #' y <- x$mutateLloqOutliers(obj)
    mutateLloqOutliers = function(obj = "tbl_df"){
      featureNames <- colnames(obj)
      cleanObj <- obj
      if(self$checkValidity(featureNames)){
        value = NA
        for (feature in featureNames){
          switch(
            self$lloqSubstituteAgent,
            "NA" = {value <- NA},
            "LLOQ" = {value <- self$featureLloq(feature)},
            "0.5 LLOQ" = {value <- 0.5 * self$featureLloq(feature)}
          )#switch
          idx <- self$outliers %>%
            dplyr::filter(feature == !!feature) %>%
            dplyr::filter(type == "LLOQ") %>%
            dplyr::pull(measurement) %>%
            as.integer()
          if(self$lloqSubstituteAgent != "keep"){
            cleanObj <- cleanObj %>%
              dplyr::mutate(!!as.symbol(feature) := replace(x = !!as.symbol(feature), list = idx, values = value))
          } #if
        }#for
      }#if
      return(cleanObj)
    }, #function

    #' @description
    #' Mutates outlier candidates characterized as above ULOQ based on user defined actions.
    #' @param obj
    #' The data to be analyzed.
    #' (tibble::tibble)
    #' @return
    #' Te revised data frame
    #' (tibble::tibble)
    #' @examples
    #' y <- x$mutateUloqOutliers(obj)
    mutateUloqOutliers = function(obj = "tbl_df"){
      featureNames <- colnames(obj)
      cleanObj <- obj
      if(self$checkValidity(featureNames)){
        value = NA
        for (feature in featureNames){
          switch(
            self$uloqSubstituteAgent,
            "NA" = {value <- NA},
            "ULOQ" = {value <- self$featureUloq(feature)}
          )#switch
          idx <- self$outliers %>%
            dplyr::filter(feature == !!feature) %>%
            dplyr::filter(type == "ULOQ") %>%
            dplyr::pull(measurement) %>%
            as.integer()
          if (self$uloqSubstituteAgent != "keep"){
            cleanObj <- cleanObj %>%
              dplyr::mutate(!!as.symbol(feature) := replace(x = !!as.symbol(feature), list = idx, values = value))
          }
        }#for
      }#if
      return(cleanObj)
    }, #function

    #' @description
    #' Returns the detected outliers of a given attribute.
    #' @param feature
    #' The attribute to be analyzed
    #' (character)
    #' @return
    #' The attribute's outliers
    #' (tibble::tibble)
    #' @examples
    #' f <- x$featureOutlier(feature = "infected")
    featureOutlier = function(feature = "character"){
      t <- NULL
      if(self$checkValidity(feature)){
        t <- self$outliers %>%
          dplyr::filter(feature == !!feature)
      }# if
      return(t)
    }, #function

    ####################
    # data information #
    ####################
    #' @description
    #' Gathers and returns class information
    #' @examples
    #' x$dataInformation() %>%
    #'  print()
    dataInformation = function(){
      self$loq %>%
        dplyr::summarise_all(class) %>%
        tidyr::gather(variable, class) %>%
        return()
    }, #function

    ####################
    # output functions #
    ####################
    #' @description
    #' Merges dfData and dfMetadata and returns a fromatted data table.
    #' @param dfData
    #' The data to be analyzed.
    #' (tibble::tibble)
    #' @param dfMetadata
    #' The data frame containing metadata.
    #' (tibble::tibble)
    #' @return
    #' A formatted data table
    #' (DT::datatable)
    #' @examples
    #' x$loqDataTable(dfData, dfMetadata) %>%
    #'  show()
    loqDataTable = function(dfData = "tbl_df", dfMetadata = "tbl_df"){
      options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
      t <- NULL
      featureNames <- colnames(dfData)
      tryCatch(
        dfMerge <- dplyr::bind_cols(dfMetadata, dfData),
        error = function(e){
          print("error")
          print(e)
          dfMerge <- dfData
        }#error
      )#tryCatch
      if(self$checkValidity(featureNames)){
        t <- dfMerge %>%
          dplyr::mutate_if(is.numeric, round, 3) %>%
          DT::datatable(options = list(scrollX = TRUE,
                                       scrollY = '350px',
                                       paging = FALSE))
        for (featureName in featureNames){
          featureOutlier <- self$outliers %>%
            dplyr::filter(feature == featureName) %>%
            dplyr::mutate_if(is.numeric, round, 3)
          if (nrow(featureOutlier) > 0){
            t <- DT::formatStyle(t,
                                 featureName,
                                 backgroundColor = DT::styleEqual(dfMerge %>%
                                                                    dplyr::select(!!featureName) %>%
                                                                    dplyr::slice(featureOutlier[["measurement"]]) %>%
                                                                    unlist() %>%
                                                                    round(digits = 3),
                                                                  featureOutlier[["color"]]))
          }#if
        }#for
      }#if
      return(t)
    }, #function

    #' @description
    #' Returns a formatted data table with comrising the information of a user defined attribute's outliers.
    #' @param obj
    #' The data to be analyzed.
    #' (tibble::tibble)
    #' @param feature
    #' The attribute to be analyzed
    #' (character)
    #' @return
    #' A formatted data table
    #' (DT::datatable)
    #' @examples
    #' x$loqFeatureTable(obj, feature = "infected") %>%
    #'  show()
    loqFeatureTable = function(obj = "tbl_df", feature = "character"){
      options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
      t <- NULL
      if(self$checkValidity(feature)){
        featureOutlier <- self$outliers %>%
          dplyr::filter(feature == !!feature) %>%
          dplyr::mutate_if(is.numeric, round, 3)

        dfFeature <- obj %>%
          dplyr::mutate_if(is.numeric, round, 3)

        print(dfFeature)

        t <- dfFeature %>%
          DT::datatable(options = list(scrollX = TRUE,
                                       scrollY = '350px',
                                       paging = FALSE))
        if (nrow(featureOutlier) > 0){
          t <- DT::formatStyle(
            t,
            feature,
            backgroundColor = DT::styleEqual(dfFeature %>%
                                               dplyr::select(!!feature) %>%
                                               dplyr::slice(featureOutlier[["measurement"]]) %>%
                                               unlist() %>%
                                               round(digits = 3),
                                             featureOutlier[["color"]]))
        }#if
      }#if
      return(t)
    }, #function

    #' @description
    #' Creates a plot of the instance variable loqStatistics.
    #' @return
    #' A plot.
    #' (ggplot2::ggplot)
    #' @examples
    #' x$plotLoqDistribution() %>%
    #'  show()
    plotLoqDistribution = function(){
      p <- self$loqStatistics %>%
        tidyr::gather('belowLloq', 'aboveUloq', key = "type", value="typeCount") %>%
        dplyr::mutate(fraction = 100 * typeCount/measurements) %>%
        ggplot2::ggplot(mapping = ggplot2::aes_string(x = "features", y = "fraction", fill = "type"), na.rm=TRUE)+
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
    #' @param obj
    #' The data to be analyzed.
    #' (tibble::tibble)
    #' @param feature
    #' The attribute to be analyzed
    #' (character)
    #' @return
    #' A bar plot.
    #' (ggplot2::ggplot)
    #' @examples
    #' x$featureBarPlot(obj, feature = "infected") %>%
    #'  show()
    featureBarPlot = function(obj = "tbl_df", feature = "character"){
      p <- NULL
      if(self$checkValidity(feature)){
        lloq <-  self$featureLloq(feature)
        uloq <- self$featureUloq(feature)
        p <- obj %>%
          # dplyr::select(feature) %>%
          ggplot2::ggplot(mapping = ggplot2::aes_string(x = as.name(feature)), na.rm=TRUE) +
          ggplot2::geom_bar(stat = "bin") +
          ggplot2::geom_vline(xintercept=lloq, linetype="dashed") +
          ggplot2::geom_vline(xintercept=uloq, linetype="dashed") +
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
    #' @param obj
    #' The data to be analyzed.
    #' (tibble::tibble)
    #' @param feature
    #' The attribute to be analyzed
    #' (character)
    #' @return
    #' A box plot.
    #' (ggplot2::ggplot)
    #' @examples
    #' x$featureBoxPlotWithSubset(obj, feature = "infected") %>%
    #'  show()
    featureBoxPlotWithSubset = function(obj = "tbl_df", feature = "character"){
      p <- NULL
      if(self$checkValidity(feature)){
        lloq <-  self$featureLloq(feature)
        uloq <- self$featureUloq(feature)
        p <- obj %>%
          dplyr::select(feature) %>%
          dplyr::mutate(LOQ = dplyr::if_else(condition = !!dplyr::sym(feature) < lloq, true = "< LLOQ", dplyr::if_else(condition = !!dplyr::sym(feature) > uloq, true = "> ULOQ", false = "quantitative", missing = "quantitative"), missing = "quantitative")) %>%
          tidyr::gather_(key="feature", value="measurement", feature) %>%
          ggplot2::ggplot(mapping=ggplot2::aes_string(x="feature",y="measurement"), na.rm=TRUE)+
          ggplot2::geom_boxplot(na.rm=TRUE, outlier.shape = NA)+
          ggplot2::geom_jitter(ggplot2::aes(colour=LOQ), na.rm=TRUE) +
          ggplot2::geom_hline(yintercept=lloq, linetype="dashed") +
          ggplot2::geom_hline(yintercept=uloq, linetype="dashed") +
          ggplot2::xlab("feature") +
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
    #' @param obj
    #' The data to be analyzed.
    #' (tibble::tibble)
    #' @param feature
    #' Attribute's name.
    #' (character)
    #' @return
    #' Composite result plot.
    #' (gridExtra::grid.arrange)
    #' @examples
    #' x$featurePlot(obj, feature = "infected") %>%
    #'  show()
    featurePlot = function(obj = "tbl_df", feature = "character"){
      p1 <- self$featureBoxPlotWithSubset(obj, feature) +
        ggplot2::theme(legend.position = c(0.9, 0.9),
                       legend.key = ggplot2::element_blank(),
                       legend.background = ggplot2::element_blank())

      limits1 <- ggplot2::layer_scales(p1)$y$range$range

      p2 <- self$featureBarPlot(obj, feature)
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
                                   top = textGrob(label = sprintf("Distribution of %s", feature)))
      return(p)
    } #function
  )#public
)#class
